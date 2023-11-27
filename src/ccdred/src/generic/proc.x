include	<imhdr.h>
include	"../ccdred.h"


.help proc Feb87 noao.imred.ccdred
.nf ----------------------------------------------------------------------------
proc -- Process CCD images

These are the main CCD reduction procedures.  There is one for each
readout axis (lines or columns) and one for short and real image data.
They apply corrections for bad pixels, overscan levels, zero levels,
dark counts, flat field response, illumination response, and fringe
effects.  The image is also trimmed if it was mapped with an image
section.  The mean value for the output image is computed when the flat
field or illumination image is processed to form the scale factor for
these calibrations in order to avoid reading through these image a
second time.

The processing information and parameters are specified in the CCD
structure. The processing operations to be performed are specified by
the correction array CORS in the ccd structure.  There is one array
element for each operation with indices defined symbolically by macro
definitions (see ccdred.h); i.e.  FLATCOR.  The value of the array
element is an integer bit field in which the bit set is the same as the
array index; i.e element 3 will have the third bit set for an operation
with array value 2**(3-1)=4.  If an operation is not to be performed
the bit is not set and the array element has the numeric value zero.
Note that the addition of several correction elements gives a unique
bit field describing a combination of operations.  For efficiency the
most common combinations are implemented as separate units.

The CCD structure also contains the correction or calibration data
consisting either pointers to data, IMIO pointers for the calibration
images, and scale factors.

The processing is performed line-by-line.  The procedure CORINPUT is
called to get an input line.  This procedure trims and fixes bad pixels by
interpolation.  The output line and lines from the various calibration
images are read.  The image vectors as well as the overscan vector and
the scale factors are passed to the procedure COR (which also
dereferences the pointer data into simple arrays and variables).  That
procedure does the actual corrections apart from bad pixel
corrections.

The final optional step is to add each corrected output line to form a
mean.  This adds efficiency since the operation is done only if desired
and the output image data is already in memory so there is no I/O
penalty.

SEE ALSO
    ccdred.h, cor, fixpix, setfixpix, setoverscan, settrim,
    setzero, setdark, setflat, setsflat, setillum, setfringe
.endhelp ----------------------------------------------------------------------



# PROC1 -- Process CCD images with readout axis 1 (lines).

procedure proc1s (ccd)

pointer	ccd		# CCD structure

bool	dosat, dointerp, rep, findmean
int	l, nc, ncols, coff, loff, nmean, nsum
int	overscan_type, overscan_c1, noverscan
real	overscan, darkscale, gainscale, flatscale, sflatscale
real	illumscale, frgscale, minflat, maxflat
double	sum, mean
short	minrep
pointer	sp, nflatbuf
pointer	inbuf, outbuf, outbuf1, noibuf, noibuf1
pointer	overscan_vec, zerobuf, darkbuf, flatbuf, sflatbuf, illumbuf, fringebuf
pointer	in, bpin, zeroim, darkim, flatim, flatim2, illumim, fringeim
pointer	out, bpout, fdnoi
pointer	outbufr, noibufr

double	procmeans()
real	find_overscans()
pointer	imgl2s(), impl2s(), ccd_gls(), xx_fpss()

errchk	bld_open

begin
	call smark (sp)

	# Set input.
	in = IN_IM(ccd)
	bpin = BPIN_IM(ccd)

	# Set output.
	out = OUT_IM(ccd)
	bpout = BPOUT_IM(ccd)
	fdnoi = NOIOUT_IM(ccd)
	ncols = OUT_C2(ccd) - OUT_C1(ccd) + 1
	coff = IN_C1(ccd) - OUT_C1(ccd)
	loff = IN_L1(ccd) - OUT_L1(ccd)
	if (out == NULL)
	    nc = IM_LEN(in,1)
	else
	    nc = IM_LEN(out,1)

	# If there is no output image allocate a buffer.
	if (out == NULL)
	    call salloc (outbuf, nc, TY_SHORT)
	noibuf = NULL
	noibufr = NULL

	# Set saturated and bleed pixel parameters.  If interpolating,
	# the output I/O is done in BLD_INTERP and we dont't use the
	# IMIO buffer so we must allocate memory.

	dosat = false
	dointerp = false
	if (CORS(ccd,SATURATE) == YES &&
	    (CORS(ccd,FIXPIX) == YES || bpout != NULL || fdnoi != NULL)) {
	    dosat = true
	    if (CORS(ccd,FIXPIX) == YES)
		dointerp = true
	    if (dointerp && out != NULL)
		call salloc (outbuf, nc, TY_SHORT)
	    call salloc (outbufr, nc, TY_REAL)

	    if (dointerp && fdnoi != NULL)
		call salloc (noibuf, nc, TY_SHORT)
	    call salloc (noibufr, nc, TY_REAL)

	    # Initialize the saturation/bleed pixel routines.
	    call bld_open (out, fdnoi, bpout, bpin, dointerp, SATVAL(ccd), 4,
	        SATGROW(ccd), BLDVAL(ccd), 5, BLDGROW(ccd), BLDTRAIL(ccd),
		IN_C1(ccd), OUT_C1(ccd))
	}
	
	# Initialize mean value computation.
	findmean = (CORS(ccd, FINDMEAN) == YES)
	if (findmean) {
	    sum = 0.
	    nsum = 0.
	}

	# Set lower threshold replacement parameters.
	rep = (CORS(ccd, MINREP) == YES)
	if (rep)
	    minrep = MINREPLACE(ccd)

	# Set overscan parameters.
	if (CORS(ccd, OVERSCAN) == 0)
	    overscan_type = 0
	else {
	    overscan_type = OVERSCAN_TYPE(ccd)
	    overscan_vec = OVERSCAN_VEC(ccd)
	    overscan_c1 = BIAS_C1(ccd) - 1
	    noverscan = BIAS_C2(ccd) - overscan_c1
	}

	# Set calibration images.
	# If the calibration image is 1D then just get the data once.
	if (CORS(ccd, ZEROCOR) == 0) {
	    zeroim = NULL
	    zerobuf = 1
	} else if (IM_LEN(ZERO_IM(ccd),2) == 1) {
	    zeroim = NULL
	    zerobuf = ccd_gls (ZERO_IM(ccd), ZERO_C1(ccd), ZERO_C2(ccd), 1)
	} else
	    zeroim = ZERO_IM(ccd)

	if (CORS(ccd, DARKCOR) == 0) {
	    darkim = NULL
	    darkbuf = 1
	} else if (IM_LEN(DARK_IM(ccd),2) == 1) {
	    darkim = NULL
	    darkbuf = ccd_gls (DARK_IM(ccd), DARK_C1(ccd), DARK_C2(ccd), 1)
	    darkscale = DARKSCALE(ccd)
	} else {
	    darkim = DARK_IM(ccd)
	    darkscale = DARKSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) == 0) {
	    flatim = NULL
	    flatbuf = 1
	} else if (IM_LEN(FLAT_IM(ccd),2) == 1) {
	    flatim = NULL
	    flatbuf = ccd_gls (FLAT_IM(ccd), FLAT_C1(ccd), FLAT_C2(ccd), 1)
	    gainscale = GAINSCALE(ccd)
	    flatscale = FLATSCALE(ccd) * gainscale
	    minflat = 0.01 / gainscale
	    maxflat = 100. / gainscale
	} else {
	    flatim = FLAT_IM(ccd)
	    gainscale = GAINSCALE(ccd)
	    flatscale = FLATSCALE(ccd) * gainscale
	    minflat = 0.01 / gainscale
	    maxflat = 100. / gainscale
	}

	if (CORS(ccd, SFLATCOR) == 0) {
	    flatim2 = NULL
	    sflatbuf = 1
	} else if (IM_LEN(SFLAT_IM(ccd),2) == 1) {
	    flatim2 = NULL
	    sflatbuf = ccd_gls (SFLAT_IM(ccd), SFLAT_C1(ccd), SFLAT_C2(ccd), 1)
	    sflatscale = SFLATSCALE(ccd)
	    minflat = 0.01
	    maxflat = 100.
	} else {
	    flatim2 = SFLAT_IM(ccd)
	    sflatscale = SFLATSCALE(ccd)
	    minflat = 0.01
	    maxflat = 100.
	}

	if (CORS(ccd, FLATCOR) != 0 || CORS(ccd, SFLATCOR) != 0) {
	    call salloc (nflatbuf, ncols, TY_REAL)
	    if (flatim == NULL && flatim2 == NULL)
		call cor1flats (CORS(ccd,1), Memr[nflatbuf], Mems[flatbuf],
		    Mems[sflatbuf], ncols, flatscale, sflatscale, minflat,
		    maxflat)
	} else
	    nflatbuf = 1

	if (CORS(ccd, ILLUMCOR) == 0) {
	    illumim = NULL
	    illumbuf = 1
	} else {
	    illumim = ILLUM_IM(ccd)
	    illumscale = ILLUMSCALE(ccd)
	}

	if (CORS(ccd, FRINGECOR) == 0) {
	    fringeim = NULL
	    fringebuf = 1
	} else {
	    fringeim = FRINGE_IM(ccd)
	    frgscale = FRINGESCALE(ccd)
	}

	# For each output line read line from the input.  Data outside the
	# trim region is simply copied to the output.  Procedure XT_FPS
	# replaces input bad pixels by interpolation.  The BLD procedures
	# find saturated and bleed pixels which are added to the output bad
	# pixel mask and replaced by interpolation in the output image.
	# Procedure COR1 does the pixel corrections.  A mean of the output
	# data is computed.
	#
	# If not interpolating saturated or bleed pixels this routine does
	# the output I/O otherwise it is done in BLD_INTERP.

	# Copy initial untrimmed lines.
	if (out != NULL) {
	    do l = 1, OUT_L1(ccd)-1 {
		call amovs (Mems[imgl2s(in,l+loff)+coff],
		    Mems[impl2s(out,l)], nc)
		if (fdnoi != NULL)
		    call amovs (Mems[imgl2s(in,l+loff)+coff],
			Mems[impl2s(fdnoi,l)], nc)
	    }
	} else if (fdnoi != NULL) {
	    do l = 1, OUT_L1(ccd)-1
		call amovs (Mems[imgl2s(in,l+loff)+coff],
		    Mems[impl2s(fdnoi,l)], nc)
	}
	if (bpout != NULL) {
	    do l = 1, OUT_L1(ccd)-1
		call aclrs (Mems[impl2s(bpout,l)], nc)
	}

	# Process output lines.
	do l = OUT_L1(ccd), OUT_L2(ccd) {
	    # Set output line buffer.  Use IMIO buffer if an output image
	    # is being created and if not interpolating.
	    if (out != NULL && !dointerp)
		outbuf = impl2s (out, l)
	    outbuf1 = outbuf + OUT_C1(ccd) - 1
	    if (fdnoi != NULL && !dointerp)
		noibuf = impl2s (fdnoi, l)
	    noibuf1 = noibuf + OUT_C1(ccd) - 1

	    # Get input data, fix bad pixels, and copy to output line buffer.
	    if (BPIN_FP(ccd) != NULL && noibuf != NULL)
	        call amovs (Mems[imgl2s(in,l+loff)+coff], Mems[noibuf], nc)
	    inbuf = xx_fpss (BPIN_FP(ccd), in, l+loff, IN_C1(ccd), IN_C2(ccd),
	        IN_L1(ccd), IN_L2(ccd), NULL)
	    call amovs (Mems[inbuf+coff], Mems[outbuf], nc)
	    if (BPIN_FP(ccd) == NULL && noibuf != NULL)
	        call amovs (Mems[outbuf], Mems[noibuf], nc)

	    # Set the calibration data.
	    if (overscan_type != 0) {
		if (overscan_type < OVERSCAN_FIT)
		    overscan = find_overscans (Mems[inbuf+overscan_c1],
			noverscan, overscan_type)
		else
		    overscan = Memr[overscan_vec+l-OUT_L1(ccd)]
	    }
	    if (zeroim != NULL)
		zerobuf = ccd_gls (zeroim, ZERO_C1(ccd), ZERO_C2(ccd),
		    l+ZERO_L1(ccd)-OUT_L1(ccd))
	    if (darkim != NULL)
		darkbuf = ccd_gls (darkim, DARK_C1(ccd), DARK_C2(ccd),
		    l+DARK_L1(ccd)-OUT_L1(ccd))
	    if (flatim != NULL)
		flatbuf = ccd_gls (flatim, FLAT_C1(ccd), FLAT_C2(ccd),
		    l+FLAT_L1(ccd)-OUT_L1(ccd))
	    if (flatim2 != NULL)
		sflatbuf = ccd_gls (flatim2, SFLAT_C1(ccd), SFLAT_C2(ccd),
		    l+SFLAT_L1(ccd)-OUT_L1(ccd))
	    if (illumim != NULL)
		illumbuf = ccd_gls (illumim, ILLUM_C1(ccd), ILLUM_C2(ccd),
		    l+ILLUM_L1(ccd)-OUT_L1(ccd))
	    if (fringeim != NULL)
		fringebuf = ccd_gls (fringeim, FRINGE_C1(ccd), FRINGE_C2(ccd),
		    l+FRINGE_L1(ccd)-OUT_L1(ccd))
	    if (flatim != NULL || flatim2 != NULL)
		call cor1flats (CORS(ccd,1), Memr[nflatbuf], Mems[flatbuf],
		    Mems[sflatbuf], ncols, flatscale, sflatscale, minflat,
		    maxflat)

	    # Find the saturated and bleed pixels before other processing.
	    if (dosat) {
		call achtsr (Mems[outbuf], Memr[outbufr], nc)
		call bld_mask (bpout, l, Memr[outbufr], bpin)
	    } else if (bpout != NULL) {
		if (bpin != NULL)
		    call amovs (Mems[imgl2s(bpin,l+loff)+coff],
			Mems[impl2s(bpout,l)], nc)
		else
		    call aclrs (Mems[impl2s(bpout,l)], nc)
	    }

	    # Process the line.
	    call cor1s (CORS(ccd,1), Mems[outbuf1], overscan,
		Mems[zerobuf], Mems[darkbuf], Memr[nflatbuf],
		Mems[illumbuf], Mems[fringebuf], ncols, darkscale,
		flatscale, illumscale, frgscale)
	    if (noibuf != NULL)
		call cor1s (CORS(ccd,1), Mems[noibuf1], overscan,
		    Mems[zerobuf], Mems[darkbuf], Memr[nflatbuf],
		    Mems[illumbuf], Mems[fringebuf], ncols, darkscale,
		    flatscale, illumscale, frgscale)

	    # Interpolate the saturated and bleed pixels and set the
	    # uninterpolated image and output mask if desired.
	    if (dointerp) {
		call achtsr (Mems[outbuf], Memr[outbufr], nc)
		if (noibuf != NULL)
		    call achtsr (Mems[noibuf], Memr[noibufr], nc)
		call bld_interp (out, fdnoi, l, noibufr, Memr[outbufr])
	    }

	    # Apply a lower threshold to the output.
	    if (rep)
		call amaxks (Mems[outbuf1], minrep, Mems[outbuf1], ncols)

	    # Compute the mean.
	    if (findmean) {
		mean = procmeans (Mems[outbuf1], ncols, 2., nmean)
		sum = sum + nmean * mean
		nsum = nsum + nmean
	    }
	}

	# Copy final untrimmed lines.
	if (out != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2) {
		call amovs (Mems[imgl2s(in,l+loff)+coff],
		    Mems[impl2s(out,l)], nc)
		if (fdnoi != NULL)
		    call amovs (Mems[imgl2s(in,l+loff)+coff],
			Mems[impl2s(fdnoi,l)], nc)
	    }
	} else if (fdnoi != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2)
		call amovs (Mems[imgl2s(in,l+loff)+coff],
		    Mems[impl2s(fdnoi,l)], nc)
	}
	if (bpout != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2)
		call aclrs (Mems[impl2s(bpout,l)], nc)
	}

	# Compute the mean from the sum of the output pixels.
	if (findmean) {
	    if (nsum > 0)
		MEAN(ccd) = sum / nsum
	    else
		MEAN(ccd) = 1.
	}

	# Finish up.
	if (dosat)
	    call bld_close ()
	call sfree (sp)
end


# PROC2 -- Process CCD images with readout axis 2 (columns).

procedure proc2s (ccd)

pointer	ccd		# CCD structure

bool	dosat, dointerp, rep, findmean
int	l, nc, ncols, coff, loff, nmean, nsum
real	darkscale, gainscale, flatscale, sflatscale
real	illumscale, frgscale, minflat, maxflat
double	sum, mean
short	minrep
pointer	sp, nflatbuf
pointer	inbuf, outbuf, outbuf1, noibuf, noibuf1
pointer	overscan_vec, zerobuf, darkbuf, flatbuf, sflatbuf, illumbuf, fringebuf
pointer	in, bpin, zeroim, darkim, flatim, flatim2, illumim, fringeim
pointer	out, bpout, fdnoi
pointer	outbufr, noibufr

double	procmeans()
pointer	imgl2s(), impl2s(), imgs2s(), ccd_gls(), xx_fpss()

errchk	bld_open

begin
	call smark (sp)

	# Set input.
	in = IN_IM(ccd)
	bpin = BPIN_IM(ccd)

	# Set output.
	out = OUT_IM(ccd)
	bpout = BPOUT_IM(ccd)
	fdnoi = NOIOUT_IM(ccd)
	ncols = OUT_C2(ccd) - OUT_C1(ccd) + 1
	coff = IN_C1(ccd) - OUT_C1(ccd)
	loff = IN_L1(ccd) - OUT_L1(ccd)
	if (out == NULL)
	    nc = IM_LEN(in,1)
	else
	    nc = IM_LEN(out,1)

	# If there is no output image allocate a buffer.
	if (out == NULL)
	    call salloc (outbuf, nc, TY_SHORT)
	noibuf = NULL
	noibufr = NULL

	# Set saturated and bleed pixel parameters.  If interpolating,
	# the output I/O is done in BLD_INTERP and we dont't use the
	# IMIO buffer so we must allocate memory.

	dosat = false
	dointerp = false
	if (CORS(ccd,SATURATE) == YES &&
	    (CORS(ccd,FIXPIX) == YES || bpout != NULL || fdnoi != NULL)) {
	    dosat = true
	    if (CORS(ccd,FIXPIX) == YES)
		dointerp = true
	    if (dointerp && out != NULL)
		call salloc (outbuf, nc, TY_SHORT)
	    call salloc (outbufr, nc, TY_REAL)

	    if (dointerp && fdnoi != NULL)
		call salloc (noibuf, nc, TY_SHORT)
	    call salloc (noibufr, nc, TY_REAL)

	    # Initialize the saturation/bleed pixel routines.
	    call bld_open (out, fdnoi, bpout, bpin, dointerp, SATVAL(ccd), 4,
	        SATGROW(ccd), BLDVAL(ccd), 5, BLDGROW(ccd), BLDTRAIL(ccd),
		IN_C1(ccd), OUT_C1(ccd))
	}
	
	# Initialize mean value computation.
	findmean = (CORS(ccd, FINDMEAN) == YES)
	if (findmean) {
	    sum = 0.
	    nsum = 0.
	}

	# Set lower threshold replacement parameters.
	rep = (CORS(ccd, MINREP) == YES)
	if (rep)
	    minrep = MINREPLACE(ccd)

	# Set overscan parameters.
	overscan_vec = OVERSCAN_VEC(ccd)

	# Set calibration images.
	# If the calibration image is 1D then just get the data once.
	if (CORS(ccd, ZEROCOR) == 0) {
	    zeroim = NULL
	    zerobuf = 1
	} else if (IM_LEN(ZERO_IM(ccd),1) == 1) {
	    zeroim = NULL
	    zerobuf = imgs2s (ZERO_IM(ccd), 1, 1, ZERO_L1(ccd), ZERO_L2(ccd))
	} else
	    zeroim = ZERO_IM(ccd)

	if (CORS(ccd, DARKCOR) == 0) {
	    darkim = NULL
	    darkbuf = 1
	} else if (IM_LEN(DARK_IM(ccd),1) == 1) {
	    darkim = NULL
	    darkbuf = imgs2s (DARK_IM(ccd), 1, 1, DARK_L1(ccd), DARK_L2(ccd))
	    darkscale = DARKSCALE(ccd)
	} else {
	    darkim = DARK_IM(ccd)
	    darkscale = DARKSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) == 0) {
	    flatim = NULL
	    flatbuf = 1
	} else if (IM_LEN(FLAT_IM(ccd),1) == 1) {
	    flatim = NULL
	    flatbuf = imgs2s (FLAT_IM(ccd), 1, 1, FLAT_L1(ccd), FLAT_L2(ccd))
	    gainscale = GAINSCALE(ccd)
	    flatscale = FLATSCALE(ccd) * gainscale
	    minflat = 0.01 / gainscale
	    maxflat = 100. / gainscale
	} else {
	    flatim = FLAT_IM(ccd)
	    gainscale = GAINSCALE(ccd)
	    flatscale = FLATSCALE(ccd) * gainscale
	    minflat = 0.01 / gainscale
	    maxflat = 100. / gainscale
	}

	if (CORS(ccd, SFLATCOR) == 0) {
	    flatim2 = NULL
	    sflatbuf = 1
	} else if (IM_LEN(SFLAT_IM(ccd),1) == 1) {
	    flatim2 = NULL
	    sflatbuf = imgs2s (SFLAT_IM(ccd), 1, 1, SFLAT_L1(ccd),
		SFLAT_L2(ccd))
	    sflatscale = SFLATSCALE(ccd)
	} else {
	    flatim2 = SFLAT_IM(ccd)
	    sflatscale = SFLATSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) != 0 || CORS(ccd, SFLATCOR) != 0)
	    call malloc (nflatbuf, ncols, TY_REAL)
	else
	    nflatbuf = 1

	if (CORS(ccd, ILLUMCOR) == 0) {
	    illumim = NULL
	    illumbuf = 1
	} else {
	    illumim = ILLUM_IM(ccd)
	    illumscale = ILLUMSCALE(ccd)
	}

	if (CORS(ccd, FRINGECOR) == 0) {
	    fringeim = NULL
	    fringebuf = 1
	} else {
	    fringeim = FRINGE_IM(ccd)
	    frgscale = FRINGESCALE(ccd)
	}

	# For each output line read line from the input.  Data outside the
	# trim region is simply copied to the output.  Procedure XT_FPS
	# replaces input bad pixels by interpolation.  The BLD procedures
	# find saturated and bleed pixels which are added to the output bad
	# pixel mask and replaced by interpolation in the output image.
	# Procedure COR1 does the pixel corrections.  A mean of the output
	# data is computed.
	#
	# If not interpolating saturated or bleed pixels this routine does
	# the output I/O otherwise it is done in BLD_INTERP.

	# Copy initial untrimmed lines.
	if (out != NULL) {
	    do l = 1, OUT_L1(ccd)-1 {
		call amovs (Mems[imgl2s(in,l+loff)+coff],
		    Mems[impl2s(out,l)], nc)
		if (fdnoi != NULL)
		    call amovs (Mems[imgl2s(in,l+loff)+coff],
			Mems[impl2s(fdnoi,l)], nc)
	    }
	} else if (fdnoi != NULL) {
	    do l = 1, OUT_L1(ccd)-1
		call amovs (Mems[imgl2s(in,l+loff)+coff],
		    Mems[impl2s(fdnoi,l)], nc)
	}
	if (bpout != NULL)
	    call aclrs (Mems[impl2s(bpout,l)], nc)

	# Process output lines.
	do l = OUT_L1(ccd), OUT_L2(ccd) {
	    # Set output line buffer.  Use IMIO buffer if an output image
	    # is being created and if not interpolating.
	    if (out != NULL && !dointerp)
		outbuf = impl2s (out, l)
	    outbuf1 = outbuf + OUT_C1(ccd) - 1
	    if (fdnoi != NULL && !dointerp)
		noibuf = impl2s (fdnoi, l)
	    noibuf1 = noibuf + OUT_C1(ccd) - 1

	    # Get input data, fix bad pixels, and copy to output line buffer.
	    if (BPIN_FP(ccd) != NULL && noibuf != NULL)
	        call amovs (Mems[imgl2s(in,l+loff)+coff], Mems[noibuf], nc)
	    inbuf = xx_fpss (BPIN_FP(ccd), in, l+loff, IN_C1(ccd), IN_C2(ccd),
	        IN_L1(ccd), IN_L2(ccd), NULL)
	    call amovs (Mems[inbuf+coff], Mems[outbuf], nc)
	    if (BPIN_FP(ccd) == NULL && noibuf != NULL)
	        call amovs (Mems[outbuf], Mems[noibuf], nc)

	    # Set the calibration data.
	    if (zeroim != NULL)
		zerobuf = ccd_gls (zeroim, ZERO_C1(ccd), ZERO_C2(ccd),
		    l+ZERO_L1(ccd)-OUT_L1(ccd))
	    if (darkim != NULL)
		darkbuf = ccd_gls (darkim, DARK_C1(ccd), DARK_C2(ccd),
		    l+DARK_L1(ccd)-OUT_L1(ccd))
	    if (flatim != NULL)
		flatbuf = ccd_gls (flatim, FLAT_C1(ccd), FLAT_C2(ccd),
		    l+FLAT_L1(ccd)-OUT_L1(ccd))
	    if (flatim2 != NULL)
		sflatbuf = ccd_gls (flatim2, SFLAT_C1(ccd), SFLAT_C2(ccd),
		    l+SFLAT_L1(ccd)-OUT_L1(ccd))
	    if (illumim != NULL)
		illumbuf = ccd_gls (illumim, ILLUM_C1(ccd), ILLUM_C2(ccd),
		    l+ILLUM_L1(ccd)-OUT_L1(ccd))
	    if (fringeim != NULL)
		fringebuf = ccd_gls (fringeim, FRINGE_C1(ccd), FRINGE_C2(ccd),
		    l+FRINGE_L1(ccd)-OUT_L1(ccd))
	    call cor2flats (CORS(ccd,1), Memr[nflatbuf], Mems[flatbuf],
		Mems[sflatbuf], ncols, flatim, flatim2, flatscale, sflatscale,
		minflat, maxflat)

	    # Find the saturated and bleed pixels before other processing.
	    if (dosat) {
		call achtsr (Mems[outbuf], Memr[outbufr], nc)
		call bld_mask (bpout, l, Memr[outbufr], bpin)
	    } else if (bpout != NULL) {
		if (bpin != NULL)
		    call amovs (Mems[imgl2s(bpin,l+loff)+coff],
			Mems[impl2s(bpout,l)], nc)
		else
		    call aclrs (Mems[impl2s(bpout,l)], nc)
	    }

	    # Process the line.
	    call cor2s (l-OUT_L1(ccd)+1, CORS(ccd,1), Mems[outbuf1],
		Memr[overscan_vec], Mems[zerobuf], Mems[darkbuf],
		Memr[flatbuf], Mems[illumbuf], Mems[fringebuf],
		ncols, zeroim,  darkscale, illumscale, frgscale)
	    if (noibuf != NULL)
		call cor2s (l-OUT_L1(ccd)+1, CORS(ccd,1), Mems[noibuf1],
		    Memr[overscan_vec], Mems[zerobuf], Mems[darkbuf],
		    Memr[flatbuf], Mems[illumbuf], Mems[fringebuf],
		    ncols, zeroim,  darkscale, illumscale, frgscale)

	    # Interpolate the saturated and bleed pixels and set the
	    # output no interplation image and output mask.
	    if (dointerp) {
		call achtsr (Mems[outbuf], Memr[outbufr], nc)
		if (noibuf != NULL)
		    call achtsr (Mems[noibuf], Memr[noibufr], nc)
		call bld_interp (out, fdnoi, l, noibufr, Memr[outbufr])
	    }

	    # Apply a lower threshold to the output.
	    if (rep)
		call amaxks (Mems[outbuf1], minrep, Mems[outbuf1], ncols)

	    # Compute the mean.
	    if (findmean) {
		mean = procmeans (Mems[outbuf1], ncols, 2., nmean)
		sum = sum + nmean * mean
		nsum = nsum + nmean
	    }
	}

	# Copy final untrimmed lines.
	if (out != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2) {
		call amovs (Mems[imgl2s(in,l+loff)+coff],
		    Mems[impl2s(out,l)], nc)
		if (fdnoi != NULL)
		    call amovs (Mems[imgl2s(in,l+loff)+coff],
			Mems[impl2s(fdnoi,l)], nc)
	    }
	} else if (fdnoi != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2)
		call amovs (Mems[imgl2s(in,l+loff)+coff],
		    Mems[impl2s(fdnoi,l)], nc)
	}
	if (bpout != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2)
		call aclrs (Mems[impl2s(bpout,l)], nc)
	}

	# Compute the mean from the sum of the output pixels.
	if (findmean) {
	    if (nsum > 0)
		MEAN(ccd) = sum / nsum
	    else
		MEAN(ccd) = 1.
	}

	# Finish up.
	if (dosat)
	    call bld_close ()
	call sfree (sp)
end


# FIND_OVERSCAN -- Find the overscan value for a line.
# No check is made on the number of pixels.
# The median is the (npix+1)/2 element.

real procedure find_overscans (data, npix, type)

short	data[npix]	#I Overscan data
int	npix		#I Number of overscan points
int	type		#I Type of overscan calculation

int	i
real	overscan, d, dmin, dmax
short	asoks()

begin
	if (type == OVERSCAN_MINMAX) {
	    overscan = data[1]
	    dmin = data[1]
	    dmax = data[1]
	    do i = 2, npix {
		d = data[i]
		overscan = overscan + d
		if (d < dmin)
		    dmin = d
		else if (d > dmax)
		    dmax = d
	    }
	    if (npix > 2)
		overscan = (overscan - dmin - dmax) / (npix - 2)
	    else
		overscan = overscan / npix
	} else if (type == OVERSCAN_MEDIAN)
	    overscan = asoks (data, npix, (npix + 1) / 2)
	else {
	    overscan = data[1]
	    do i = 2, npix
		overscan = overscan + data[i]
	    overscan = overscan / npix
	}

	return (overscan)
end


# PROCMEAN -- Find mean of data.

double procedure procmeans (pix, n, ksig, nmean)

short	pix[n]			#I Pixels
int	n			#I Number of pixels
real	ksig			#I Sigma clipping factor
int	nmean			#O Number of pixels in the mean

real	mean, sigma, lcut, hcut
int	awvgs()

real	asums()

begin
	if (ksig <= 0.) {
	    mean = asums (pix, n) / n
	    nmean = n
	} else {
	    lcut = 0.
	    hcut = 0.
	    nmean = awvgs (pix, n, mean, sigma, lcut, hcut)
	    lcut = mean - abs (ksig) * sigma
	    hcut = mean + abs (ksig) * sigma
	    nmean = awvgs (pix, n, mean, sigma, lcut, hcut)
	}

	return (double (mean))
end

# PROC1 -- Process CCD images with readout axis 1 (lines).

procedure proc1r (ccd)

pointer	ccd		# CCD structure

bool	dosat, dointerp, rep, findmean
int	l, nc, ncols, coff, loff, nmean, nsum
int	overscan_type, overscan_c1, noverscan
real	overscan, darkscale, gainscale, flatscale, sflatscale
real	illumscale, frgscale, minflat, maxflat
double	sum, mean
real	minrep
pointer	sp, nflatbuf
pointer	inbuf, outbuf, outbuf1, noibuf, noibuf1
pointer	overscan_vec, zerobuf, darkbuf, flatbuf, sflatbuf, illumbuf, fringebuf
pointer	in, bpin, zeroim, darkim, flatim, flatim2, illumim, fringeim
pointer	out, bpout, fdnoi

double	procmeanr()
real	find_overscanr()
pointer	imgl2r(), impl2r(), ccd_glr(), xx_fpsr()
pointer	impl2s(), imgl2s()

errchk	bld_open

begin
	call smark (sp)

	# Set input.
	in = IN_IM(ccd)
	bpin = BPIN_IM(ccd)

	# Set output.
	out = OUT_IM(ccd)
	bpout = BPOUT_IM(ccd)
	fdnoi = NOIOUT_IM(ccd)
	ncols = OUT_C2(ccd) - OUT_C1(ccd) + 1
	coff = IN_C1(ccd) - OUT_C1(ccd)
	loff = IN_L1(ccd) - OUT_L1(ccd)
	if (out == NULL)
	    nc = IM_LEN(in,1)
	else
	    nc = IM_LEN(out,1)

	# If there is no output image allocate a buffer.
	if (out == NULL)
	    call salloc (outbuf, nc, TY_REAL)
	noibuf = NULL

	# Set saturated and bleed pixel parameters.  If interpolating,
	# the output I/O is done in BLD_INTERP and we dont't use the
	# IMIO buffer so we must allocate memory.

	dosat = false
	dointerp = false
	if (CORS(ccd,SATURATE) == YES &&
	    (CORS(ccd,FIXPIX) == YES || bpout != NULL || fdnoi != NULL)) {
	    dosat = true
	    if (CORS(ccd,FIXPIX) == YES)
		dointerp = true
	    if (dointerp && out != NULL)
		call salloc (outbuf, nc, TY_REAL)

	    if (dointerp && fdnoi != NULL)
		call salloc (noibuf, nc, TY_REAL)

	    # Initialize the saturation/bleed pixel routines.
	    call bld_open (out, fdnoi, bpout, bpin, dointerp, SATVAL(ccd), 4,
	        SATGROW(ccd), BLDVAL(ccd), 5, BLDGROW(ccd), BLDTRAIL(ccd),
		IN_C1(ccd), OUT_C1(ccd))
	}
	
	# Initialize mean value computation.
	findmean = (CORS(ccd, FINDMEAN) == YES)
	if (findmean) {
	    sum = 0.
	    nsum = 0.
	}

	# Set lower threshold replacement parameters.
	rep = (CORS(ccd, MINREP) == YES)
	if (rep)
	    minrep = MINREPLACE(ccd)

	# Set overscan parameters.
	if (CORS(ccd, OVERSCAN) == 0)
	    overscan_type = 0
	else {
	    overscan_type = OVERSCAN_TYPE(ccd)
	    overscan_vec = OVERSCAN_VEC(ccd)
	    overscan_c1 = BIAS_C1(ccd) - 1
	    noverscan = BIAS_C2(ccd) - overscan_c1
	}

	# Set calibration images.
	# If the calibration image is 1D then just get the data once.
	if (CORS(ccd, ZEROCOR) == 0) {
	    zeroim = NULL
	    zerobuf = 1
	} else if (IM_LEN(ZERO_IM(ccd),2) == 1) {
	    zeroim = NULL
	    zerobuf = ccd_glr (ZERO_IM(ccd), ZERO_C1(ccd), ZERO_C2(ccd), 1)
	} else
	    zeroim = ZERO_IM(ccd)

	if (CORS(ccd, DARKCOR) == 0) {
	    darkim = NULL
	    darkbuf = 1
	} else if (IM_LEN(DARK_IM(ccd),2) == 1) {
	    darkim = NULL
	    darkbuf = ccd_glr (DARK_IM(ccd), DARK_C1(ccd), DARK_C2(ccd), 1)
	    darkscale = DARKSCALE(ccd)
	} else {
	    darkim = DARK_IM(ccd)
	    darkscale = DARKSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) == 0) {
	    flatim = NULL
	    flatbuf = 1
	} else if (IM_LEN(FLAT_IM(ccd),2) == 1) {
	    flatim = NULL
	    flatbuf = ccd_glr (FLAT_IM(ccd), FLAT_C1(ccd), FLAT_C2(ccd), 1)
	    gainscale = GAINSCALE(ccd)
	    flatscale = FLATSCALE(ccd) * gainscale
	    minflat = 0.01 / gainscale
	    maxflat = 100. / gainscale
	} else {
	    flatim = FLAT_IM(ccd)
	    gainscale = GAINSCALE(ccd)
	    flatscale = FLATSCALE(ccd) * gainscale
	    minflat = 0.01 / gainscale
	    maxflat = 100. / gainscale
	}

	if (CORS(ccd, SFLATCOR) == 0) {
	    flatim2 = NULL
	    sflatbuf = 1
	} else if (IM_LEN(SFLAT_IM(ccd),2) == 1) {
	    flatim2 = NULL
	    sflatbuf = ccd_glr (SFLAT_IM(ccd), SFLAT_C1(ccd), SFLAT_C2(ccd), 1)
	    sflatscale = SFLATSCALE(ccd)
	    minflat = 0.01
	    maxflat = 100.
	} else {
	    flatim2 = SFLAT_IM(ccd)
	    sflatscale = SFLATSCALE(ccd)
	    minflat = 0.01
	    maxflat = 100.
	}

	if (CORS(ccd, FLATCOR) != 0 || CORS(ccd, SFLATCOR) != 0) {
	    call salloc (nflatbuf, ncols, TY_REAL)
	    if (flatim == NULL && flatim2 == NULL)
		call cor1flatr (CORS(ccd,1), Memr[nflatbuf], Memr[flatbuf],
		    Memr[sflatbuf], ncols, flatscale, sflatscale, minflat,
		    maxflat)
	} else
	    nflatbuf = 1

	if (CORS(ccd, ILLUMCOR) == 0) {
	    illumim = NULL
	    illumbuf = 1
	} else {
	    illumim = ILLUM_IM(ccd)
	    illumscale = ILLUMSCALE(ccd)
	}

	if (CORS(ccd, FRINGECOR) == 0) {
	    fringeim = NULL
	    fringebuf = 1
	} else {
	    fringeim = FRINGE_IM(ccd)
	    frgscale = FRINGESCALE(ccd)
	}

	# For each output line read line from the input.  Data outside the
	# trim region is simply copied to the output.  Procedure XT_FPS
	# replaces input bad pixels by interpolation.  The BLD procedures
	# find saturated and bleed pixels which are added to the output bad
	# pixel mask and replaced by interpolation in the output image.
	# Procedure COR1 does the pixel corrections.  A mean of the output
	# data is computed.
	#
	# If not interpolating saturated or bleed pixels this routine does
	# the output I/O otherwise it is done in BLD_INTERP.

	# Copy initial untrimmed lines.
	if (out != NULL) {
	    do l = 1, OUT_L1(ccd)-1 {
		call amovr (Memr[imgl2r(in,l+loff)+coff],
		    Memr[impl2r(out,l)], nc)
		if (fdnoi != NULL)
		    call amovr (Memr[imgl2r(in,l+loff)+coff],
			Memr[impl2r(fdnoi,l)], nc)
	    }
	} else if (fdnoi != NULL) {
	    do l = 1, OUT_L1(ccd)-1
		call amovr (Memr[imgl2r(in,l+loff)+coff],
		    Memr[impl2r(fdnoi,l)], nc)
	}
	if (bpout != NULL) {
	    do l = 1, OUT_L1(ccd)-1
		call aclrs (Mems[impl2s(bpout,l)], nc)
	}

	# Process output lines.
	do l = OUT_L1(ccd), OUT_L2(ccd) {
	    # Set output line buffer.  Use IMIO buffer if an output image
	    # is being created and if not interpolating.
	    if (out != NULL && !dointerp)
		outbuf = impl2r (out, l)
	    outbuf1 = outbuf + OUT_C1(ccd) - 1
	    if (fdnoi != NULL && !dointerp)
		noibuf = impl2r (fdnoi, l)
	    noibuf1 = noibuf + OUT_C1(ccd) - 1

	    # Get input data, fix bad pixels, and copy to output line buffer.
	    if (BPIN_FP(ccd) != NULL && noibuf != NULL)
	        call amovr (Memr[imgl2r(in,l+loff)+coff], Memr[noibuf], nc)
	    inbuf = xx_fpsr (BPIN_FP(ccd), in, l+loff, IN_C1(ccd), IN_C2(ccd),
	        IN_L1(ccd), IN_L2(ccd), NULL)
	    call amovr (Memr[inbuf+coff], Memr[outbuf], nc)
	    if (BPIN_FP(ccd) == NULL && noibuf != NULL)
	        call amovr (Memr[outbuf], Memr[noibuf], nc)

	    # Set the calibration data.
	    if (overscan_type != 0) {
		if (overscan_type < OVERSCAN_FIT)
		    overscan = find_overscanr (Memr[inbuf+overscan_c1],
			noverscan, overscan_type)
		else
		    overscan = Memr[overscan_vec+l-OUT_L1(ccd)]
	    }
	    if (zeroim != NULL)
		zerobuf = ccd_glr (zeroim, ZERO_C1(ccd), ZERO_C2(ccd),
		    l+ZERO_L1(ccd)-OUT_L1(ccd))
	    if (darkim != NULL)
		darkbuf = ccd_glr (darkim, DARK_C1(ccd), DARK_C2(ccd),
		    l+DARK_L1(ccd)-OUT_L1(ccd))
	    if (flatim != NULL)
		flatbuf = ccd_glr (flatim, FLAT_C1(ccd), FLAT_C2(ccd),
		    l+FLAT_L1(ccd)-OUT_L1(ccd))
	    if (flatim2 != NULL)
		sflatbuf = ccd_glr (flatim2, SFLAT_C1(ccd), SFLAT_C2(ccd),
		    l+SFLAT_L1(ccd)-OUT_L1(ccd))
	    if (illumim != NULL)
		illumbuf = ccd_glr (illumim, ILLUM_C1(ccd), ILLUM_C2(ccd),
		    l+ILLUM_L1(ccd)-OUT_L1(ccd))
	    if (fringeim != NULL)
		fringebuf = ccd_glr (fringeim, FRINGE_C1(ccd), FRINGE_C2(ccd),
		    l+FRINGE_L1(ccd)-OUT_L1(ccd))
	    if (flatim != NULL || flatim2 != NULL)
		call cor1flatr (CORS(ccd,1), Memr[nflatbuf], Memr[flatbuf],
		    Memr[sflatbuf], ncols, flatscale, sflatscale, minflat,
		    maxflat)

	    # Find the saturated and bleed pixels before other processing.
	    if (dosat) {
		call bld_mask (bpout, l, Memr[outbuf], bpin)
	    } else if (bpout != NULL) {
		if (bpin != NULL)
		    call amovs (Mems[imgl2s(bpin,l+loff)+coff],
			Mems[impl2s(bpout,l)], nc)
		else
		    call aclrs (Mems[impl2s(bpout,l)], nc)
	    }

	    # Process the line.
	    call cor1r (CORS(ccd,1), Memr[outbuf1], overscan,
		Memr[zerobuf], Memr[darkbuf], Memr[nflatbuf],
		Memr[illumbuf], Memr[fringebuf], ncols, darkscale,
		flatscale, illumscale, frgscale)
	    if (noibuf != NULL)
		call cor1r (CORS(ccd,1), Memr[noibuf1], overscan,
		    Memr[zerobuf], Memr[darkbuf], Memr[nflatbuf],
		    Memr[illumbuf], Memr[fringebuf], ncols, darkscale,
		    flatscale, illumscale, frgscale)

	    # Interpolate the saturated and bleed pixels and set the
	    # uninterpolated image and output mask if desired.
	    if (dointerp) {
		call bld_interp (out, fdnoi, l, noibuf, Memr[outbuf])
	    }

	    # Apply a lower threshold to the output.
	    if (rep)
		call amaxkr (Memr[outbuf1], minrep, Memr[outbuf1], ncols)

	    # Compute the mean.
	    if (findmean) {
		mean = procmeanr (Memr[outbuf1], ncols, 2., nmean)
		sum = sum + nmean * mean
		nsum = nsum + nmean
	    }
	}

	# Copy final untrimmed lines.
	if (out != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2) {
		call amovr (Memr[imgl2r(in,l+loff)+coff],
		    Memr[impl2r(out,l)], nc)
		if (fdnoi != NULL)
		    call amovr (Memr[imgl2r(in,l+loff)+coff],
			Memr[impl2r(fdnoi,l)], nc)
	    }
	} else if (fdnoi != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2)
		call amovr (Memr[imgl2r(in,l+loff)+coff],
		    Memr[impl2r(fdnoi,l)], nc)
	}
	if (bpout != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2)
		call aclrs (Mems[impl2s(bpout,l)], nc)
	}

	# Compute the mean from the sum of the output pixels.
	if (findmean) {
	    if (nsum > 0)
		MEAN(ccd) = sum / nsum
	    else
		MEAN(ccd) = 1.
	}

	# Finish up.
	if (dosat)
	    call bld_close ()
	call sfree (sp)
end


# PROC2 -- Process CCD images with readout axis 2 (columns).

procedure proc2r (ccd)

pointer	ccd		# CCD structure

bool	dosat, dointerp, rep, findmean
int	l, nc, ncols, coff, loff, nmean, nsum
real	darkscale, gainscale, flatscale, sflatscale
real	illumscale, frgscale, minflat, maxflat
double	sum, mean
real	minrep
pointer	sp, nflatbuf
pointer	inbuf, outbuf, outbuf1, noibuf, noibuf1
pointer	overscan_vec, zerobuf, darkbuf, flatbuf, sflatbuf, illumbuf, fringebuf
pointer	in, bpin, zeroim, darkim, flatim, flatim2, illumim, fringeim
pointer	out, bpout, fdnoi

double	procmeanr()
pointer	imgl2r(), impl2r(), imgs2r(), ccd_glr(), xx_fpsr()
pointer	impl2s(), imgl2s()

errchk	bld_open

begin
	call smark (sp)

	# Set input.
	in = IN_IM(ccd)
	bpin = BPIN_IM(ccd)

	# Set output.
	out = OUT_IM(ccd)
	bpout = BPOUT_IM(ccd)
	fdnoi = NOIOUT_IM(ccd)
	ncols = OUT_C2(ccd) - OUT_C1(ccd) + 1
	coff = IN_C1(ccd) - OUT_C1(ccd)
	loff = IN_L1(ccd) - OUT_L1(ccd)
	if (out == NULL)
	    nc = IM_LEN(in,1)
	else
	    nc = IM_LEN(out,1)

	# If there is no output image allocate a buffer.
	if (out == NULL)
	    call salloc (outbuf, nc, TY_REAL)
	noibuf = NULL

	# Set saturated and bleed pixel parameters.  If interpolating,
	# the output I/O is done in BLD_INTERP and we dont't use the
	# IMIO buffer so we must allocate memory.

	dosat = false
	dointerp = false
	if (CORS(ccd,SATURATE) == YES &&
	    (CORS(ccd,FIXPIX) == YES || bpout != NULL || fdnoi != NULL)) {
	    dosat = true
	    if (CORS(ccd,FIXPIX) == YES)
		dointerp = true
	    if (dointerp && out != NULL)
		call salloc (outbuf, nc, TY_REAL)

	    if (dointerp && fdnoi != NULL)
		call salloc (noibuf, nc, TY_REAL)

	    # Initialize the saturation/bleed pixel routines.
	    call bld_open (out, fdnoi, bpout, bpin, dointerp, SATVAL(ccd), 4,
	        SATGROW(ccd), BLDVAL(ccd), 5, BLDGROW(ccd), BLDTRAIL(ccd),
		IN_C1(ccd), OUT_C1(ccd))
	}
	
	# Initialize mean value computation.
	findmean = (CORS(ccd, FINDMEAN) == YES)
	if (findmean) {
	    sum = 0.
	    nsum = 0.
	}

	# Set lower threshold replacement parameters.
	rep = (CORS(ccd, MINREP) == YES)
	if (rep)
	    minrep = MINREPLACE(ccd)

	# Set overscan parameters.
	overscan_vec = OVERSCAN_VEC(ccd)

	# Set calibration images.
	# If the calibration image is 1D then just get the data once.
	if (CORS(ccd, ZEROCOR) == 0) {
	    zeroim = NULL
	    zerobuf = 1
	} else if (IM_LEN(ZERO_IM(ccd),1) == 1) {
	    zeroim = NULL
	    zerobuf = imgs2r (ZERO_IM(ccd), 1, 1, ZERO_L1(ccd), ZERO_L2(ccd))
	} else
	    zeroim = ZERO_IM(ccd)

	if (CORS(ccd, DARKCOR) == 0) {
	    darkim = NULL
	    darkbuf = 1
	} else if (IM_LEN(DARK_IM(ccd),1) == 1) {
	    darkim = NULL
	    darkbuf = imgs2r (DARK_IM(ccd), 1, 1, DARK_L1(ccd), DARK_L2(ccd))
	    darkscale = DARKSCALE(ccd)
	} else {
	    darkim = DARK_IM(ccd)
	    darkscale = DARKSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) == 0) {
	    flatim = NULL
	    flatbuf = 1
	} else if (IM_LEN(FLAT_IM(ccd),1) == 1) {
	    flatim = NULL
	    flatbuf = imgs2r (FLAT_IM(ccd), 1, 1, FLAT_L1(ccd), FLAT_L2(ccd))
	    gainscale = GAINSCALE(ccd)
	    flatscale = FLATSCALE(ccd) * gainscale
	    minflat = 0.01 / gainscale
	    maxflat = 100. / gainscale
	} else {
	    flatim = FLAT_IM(ccd)
	    gainscale = GAINSCALE(ccd)
	    flatscale = FLATSCALE(ccd) * gainscale
	    minflat = 0.01 / gainscale
	    maxflat = 100. / gainscale
	}

	if (CORS(ccd, SFLATCOR) == 0) {
	    flatim2 = NULL
	    sflatbuf = 1
	} else if (IM_LEN(SFLAT_IM(ccd),1) == 1) {
	    flatim2 = NULL
	    sflatbuf = imgs2r (SFLAT_IM(ccd), 1, 1, SFLAT_L1(ccd),
		SFLAT_L2(ccd))
	    sflatscale = SFLATSCALE(ccd)
	} else {
	    flatim2 = SFLAT_IM(ccd)
	    sflatscale = SFLATSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) != 0 || CORS(ccd, SFLATCOR) != 0)
	    call malloc (nflatbuf, ncols, TY_REAL)
	else
	    nflatbuf = 1

	if (CORS(ccd, ILLUMCOR) == 0) {
	    illumim = NULL
	    illumbuf = 1
	} else {
	    illumim = ILLUM_IM(ccd)
	    illumscale = ILLUMSCALE(ccd)
	}

	if (CORS(ccd, FRINGECOR) == 0) {
	    fringeim = NULL
	    fringebuf = 1
	} else {
	    fringeim = FRINGE_IM(ccd)
	    frgscale = FRINGESCALE(ccd)
	}

	# For each output line read line from the input.  Data outside the
	# trim region is simply copied to the output.  Procedure XT_FPS
	# replaces input bad pixels by interpolation.  The BLD procedures
	# find saturated and bleed pixels which are added to the output bad
	# pixel mask and replaced by interpolation in the output image.
	# Procedure COR1 does the pixel corrections.  A mean of the output
	# data is computed.
	#
	# If not interpolating saturated or bleed pixels this routine does
	# the output I/O otherwise it is done in BLD_INTERP.

	# Copy initial untrimmed lines.
	if (out != NULL) {
	    do l = 1, OUT_L1(ccd)-1 {
		call amovr (Memr[imgl2r(in,l+loff)+coff],
		    Memr[impl2r(out,l)], nc)
		if (fdnoi != NULL)
		    call amovr (Memr[imgl2r(in,l+loff)+coff],
			Memr[impl2r(fdnoi,l)], nc)
	    }
	} else if (fdnoi != NULL) {
	    do l = 1, OUT_L1(ccd)-1
		call amovr (Memr[imgl2r(in,l+loff)+coff],
		    Memr[impl2r(fdnoi,l)], nc)
	}
	if (bpout != NULL)
	    call aclrs (Mems[impl2s(bpout,l)], nc)

	# Process output lines.
	do l = OUT_L1(ccd), OUT_L2(ccd) {
	    # Set output line buffer.  Use IMIO buffer if an output image
	    # is being created and if not interpolating.
	    if (out != NULL && !dointerp)
		outbuf = impl2r (out, l)
	    outbuf1 = outbuf + OUT_C1(ccd) - 1
	    if (fdnoi != NULL && !dointerp)
		noibuf = impl2r (fdnoi, l)
	    noibuf1 = noibuf + OUT_C1(ccd) - 1

	    # Get input data, fix bad pixels, and copy to output line buffer.
	    if (BPIN_FP(ccd) != NULL && noibuf != NULL)
	        call amovr (Memr[imgl2r(in,l+loff)+coff], Memr[noibuf], nc)
	    inbuf = xx_fpsr (BPIN_FP(ccd), in, l+loff, IN_C1(ccd), IN_C2(ccd),
	        IN_L1(ccd), IN_L2(ccd), NULL)
	    call amovr (Memr[inbuf+coff], Memr[outbuf], nc)
	    if (BPIN_FP(ccd) == NULL && noibuf != NULL)
	        call amovr (Memr[outbuf], Memr[noibuf], nc)

	    # Set the calibration data.
	    if (zeroim != NULL)
		zerobuf = ccd_glr (zeroim, ZERO_C1(ccd), ZERO_C2(ccd),
		    l+ZERO_L1(ccd)-OUT_L1(ccd))
	    if (darkim != NULL)
		darkbuf = ccd_glr (darkim, DARK_C1(ccd), DARK_C2(ccd),
		    l+DARK_L1(ccd)-OUT_L1(ccd))
	    if (flatim != NULL)
		flatbuf = ccd_glr (flatim, FLAT_C1(ccd), FLAT_C2(ccd),
		    l+FLAT_L1(ccd)-OUT_L1(ccd))
	    if (flatim2 != NULL)
		sflatbuf = ccd_glr (flatim2, SFLAT_C1(ccd), SFLAT_C2(ccd),
		    l+SFLAT_L1(ccd)-OUT_L1(ccd))
	    if (illumim != NULL)
		illumbuf = ccd_glr (illumim, ILLUM_C1(ccd), ILLUM_C2(ccd),
		    l+ILLUM_L1(ccd)-OUT_L1(ccd))
	    if (fringeim != NULL)
		fringebuf = ccd_glr (fringeim, FRINGE_C1(ccd), FRINGE_C2(ccd),
		    l+FRINGE_L1(ccd)-OUT_L1(ccd))
	    call cor2flatr (CORS(ccd,1), Memr[nflatbuf], Memr[flatbuf],
		Memr[sflatbuf], ncols, flatim, flatim2, flatscale, sflatscale,
		minflat, maxflat)

	    # Find the saturated and bleed pixels before other processing.
	    if (dosat) {
		call bld_mask (bpout, l, Memr[outbuf], bpin)
	    } else if (bpout != NULL) {
		if (bpin != NULL)
		    call amovs (Mems[imgl2s(bpin,l+loff)+coff],
			Mems[impl2s(bpout,l)], nc)
		else
		    call aclrs (Mems[impl2s(bpout,l)], nc)
	    }

	    # Process the line.
	    call cor2r (l-OUT_L1(ccd)+1, CORS(ccd,1), Memr[outbuf1],
		Memr[overscan_vec], Memr[zerobuf], Memr[darkbuf],
		Memr[flatbuf], Memr[illumbuf], Memr[fringebuf],
		ncols, zeroim,  darkscale, illumscale, frgscale)
	    if (noibuf != NULL)
		call cor2r (l-OUT_L1(ccd)+1, CORS(ccd,1), Memr[noibuf1],
		    Memr[overscan_vec], Memr[zerobuf], Memr[darkbuf],
		    Memr[flatbuf], Memr[illumbuf], Memr[fringebuf],
		    ncols, zeroim,  darkscale, illumscale, frgscale)

	    # Interpolate the saturated and bleed pixels and set the
	    # output no interplation image and output mask.
	    if (dointerp) {
		call bld_interp (out, fdnoi, l, noibuf, Memr[outbuf])
	    }

	    # Apply a lower threshold to the output.
	    if (rep)
		call amaxkr (Memr[outbuf1], minrep, Memr[outbuf1], ncols)

	    # Compute the mean.
	    if (findmean) {
		mean = procmeanr (Memr[outbuf1], ncols, 2., nmean)
		sum = sum + nmean * mean
		nsum = nsum + nmean
	    }
	}

	# Copy final untrimmed lines.
	if (out != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2) {
		call amovr (Memr[imgl2r(in,l+loff)+coff],
		    Memr[impl2r(out,l)], nc)
		if (fdnoi != NULL)
		    call amovr (Memr[imgl2r(in,l+loff)+coff],
			Memr[impl2r(fdnoi,l)], nc)
	    }
	} else if (fdnoi != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2)
		call amovr (Memr[imgl2r(in,l+loff)+coff],
		    Memr[impl2r(fdnoi,l)], nc)
	}
	if (bpout != NULL) {
	    do l = OUT_L2(ccd)+1, IM_LEN(out,2)
		call aclrs (Mems[impl2s(bpout,l)], nc)
	}

	# Compute the mean from the sum of the output pixels.
	if (findmean) {
	    if (nsum > 0)
		MEAN(ccd) = sum / nsum
	    else
		MEAN(ccd) = 1.
	}

	# Finish up.
	if (dosat)
	    call bld_close ()
	call sfree (sp)
end


# FIND_OVERSCAN -- Find the overscan value for a line.
# No check is made on the number of pixels.
# The median is the (npix+1)/2 element.

real procedure find_overscanr (data, npix, type)

real	data[npix]	#I Overscan data
int	npix		#I Number of overscan points
int	type		#I Type of overscan calculation

int	i
real	overscan, d, dmin, dmax
real	asokr()

begin
	if (type == OVERSCAN_MINMAX) {
	    overscan = data[1]
	    dmin = data[1]
	    dmax = data[1]
	    do i = 2, npix {
		d = data[i]
		overscan = overscan + d
		if (d < dmin)
		    dmin = d
		else if (d > dmax)
		    dmax = d
	    }
	    if (npix > 2)
		overscan = (overscan - dmin - dmax) / (npix - 2)
	    else
		overscan = overscan / npix
	} else if (type == OVERSCAN_MEDIAN)
	    overscan = asokr (data, npix, (npix + 1) / 2)
	else {
	    overscan = data[1]
	    do i = 2, npix
		overscan = overscan + data[i]
	    overscan = overscan / npix
	}

	return (overscan)
end


# PROCMEAN -- Find mean of data.

double procedure procmeanr (pix, n, ksig, nmean)

real	pix[n]			#I Pixels
int	n			#I Number of pixels
real	ksig			#I Sigma clipping factor
int	nmean			#O Number of pixels in the mean

real	mean, sigma, lcut, hcut
int	awvgr()

real	asumr()

begin
	if (ksig <= 0.) {
	    mean = asumr (pix, n) / n
	    nmean = n
	} else {
	    lcut = 0.
	    hcut = 0.
	    nmean = awvgr (pix, n, mean, sigma, lcut, hcut)
	    lcut = mean - abs (ksig) * sigma
	    hcut = mean + abs (ksig) * sigma
	    nmean = awvgr (pix, n, mean, sigma, lcut, hcut)
	}

	return (double (mean))
end

