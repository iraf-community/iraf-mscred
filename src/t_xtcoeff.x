include	<error.h>
include	<syserr.h>
include	<fset.h>
include	<gset.h>
include	<imhdr.h>
include	<mach.h>
include	<math/iminterp.h>
include	<pkg/gtools.h>


# XTCOEFF -- Crosstalk coefficient determination.
# The purpose of this procedure is to characterize level of electronics
# ghosts in mosaic images.  These ghosts are produced by some sort
# of crosstalk in the readout electronics shared by pairs of chips.
#
# For pixels in the source CCD that are within a specified range (typically)
# near saturation, find the ratio to the corresponding background subtracted
# pixels in the victim CCD.  The background is determined by a selecting
# Nth brightest pixel in the row.  The set of ratios are fit using iterative
# rejection and optional interactive selection of sample regions and point
# rejection.  The output is to a crosstalk file in the format used by
# XTALKCOR and to the terminal.
#
# More than one exposure may be specified and the individual ratios are
# combined before fitting.  Multiple pairs of CCDs may be specified.

procedure t_xtcoeff ()

pointer	inlist		# List of input mosaic images
pointer	output		# Output file
pointer	vlist		# List of victim extensions
pointer	slist		# List of source extensions
pointer	bkvlist		# List of victim backgrounds
pointer	bkslist		# List of source backgrounds
pointer	mlist		# List of victim masks
real	smin		# Source minimum
real	smax		# Source maximum
real	medfactor	# Median factor
real	maxcoeff	# Maximum coefficient expected
int	niterate	# Number of rejection iterations
real	low		# Low rejection sigma factor
real	high		# High rejection sigma factor
bool	interactive	# Interactive?
bool	verbose		# Verbose?

bool	xflip, yflip
int	i, j, c, l, nc, nl, ncoeffs, nimages, out, fd
real	coeff, dcoeff, vsky, ssky, atmvictim, atmsource
pointer	sp, image, bkvname, bksname, mname, sextn, vextn, fname, str, strbuf
pointer	ptr, svals, coeffs, wts, sdata, vdata, bkvdata, bksdata, mdata
pointer	vbuf, sbuf
pointer	source, victim, bkv, bks, pm, gp, gt, ic, cv

bool	clgetb()
int	clgeti()
int	fntopnb(), fntlenb(), fntgfnb(), errcode()
int	imtopenp(), imtgetim(), imtlen()
int	open(), stropen(), fscan(), nscan(), nowhite(), ctor(), strlen()
real	clgetr(), asokr(), imgetr()
pointer	imgl2r(), imgs2r(), imgl2i(), imgs2i(), map_glr()
pointer	immap(), map_open(), yt_pmmap(), gopen(), gt_init()
errchk	delete, open, immap, map_open, yt_pmmap, map_open
errchk	imgl2r, imgs2r, imgl2i(), imgs2i, map_glr
errchk	malloc, realloc

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (bkvname, SZ_FNAME, TY_CHAR)
	call salloc (bksname, SZ_FNAME, TY_CHAR)
	call salloc (mname, SZ_FNAME, TY_CHAR)
	call salloc (sextn, SZ_FNAME, TY_CHAR)
	call salloc (vextn, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (strbuf, 1600, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get parameters.
	inlist = imtopenp ("input")
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("victim", Memc[vextn], SZ_FNAME)
	call clgstr ("source", Memc[sextn], SZ_FNAME)
	bkvlist = imtopenp ("vbkg")
	bkslist = imtopenp ("sbkg")
	mlist = imtopenp ("masks")
	smin = clgetr ("smin")
	smax = clgetr ("smax")
	medfactor = clgetr ("medfactor")
	maxcoeff = clgetr ("maxcoeff")
	niterate = clgeti ("niterate")
	low = clgetr ("low")
	high = clgetr ("high")
	interactive = clgetb ("interactive")
	verbose = clgetb ("verbose")

	# Check image lists.
	i = imtlen (inlist)
	if (i == 0)
	    call error (1, "No input images")
	j = imtlen (bkvlist)
	if (j > 0 && j != i)
	    call error (2, "Victim background list doesn't match input list")
	j = imtlen (bkslist)
	if (j > 0 && j != i)
	    call error (2, "Source background list doesn't match input list")
	j = imtlen (mlist)
	if (j > 0 && j != i)
	    call error (2, "Victim mask list doesn't match input list")

	# Open extension lists.
	iferr (vlist = fntopnb (Memc[vextn], NO)) {
	    i = nowhite (Memc[vextn], Memc[vextn], SZ_FNAME)
	    if (Memc[vextn] == '@') {
		call sprintf (Memc[fname], SZ_FNAME, "@xtcoeff$%s")
		    call pargstr (Memc[vextn+1])
		iferr (vlist = fntopnb (Memc[fname], NO))
		    vlist = fntopnb (Memc[vextn], NO)
	    } else
		call erract (EA_ERROR)
	}
	iferr (slist = fntopnb (Memc[sextn], NO)) {
	    i = nowhite (Memc[sextn], Memc[sextn], SZ_FNAME)
	    if (Memc[sextn] == '@') {
		call sprintf (Memc[fname], SZ_FNAME, "@mscred$lib/xtcoeff/%s")
		    call pargstr (Memc[sextn+1])
		iferr (slist = fntopnb (Memc[fname], NO))
		    slist = fntopnb (Memc[sextn], NO)
	    } else
		call erract (EA_ERROR)
	}

	# Check the list of victim and source extensions match in number.
	if (fntlenb (vlist) != fntlenb (slist))
	    call error (1, "Extension lists don't match")

	# Open the output crosstalk file if one is specified.
	out = NULL
	if (nowhite (Memc[output], Memc[output], SZ_FNAME) > 0) {
	    iferr (out = open (Memc[output], NEW_FILE, TEXT_FILE)) {
		if (errcode () == SYS_FCLOBBER) {
		    call erract (EA_WARN)
		    if (clgetb ("clobber"))
			call delete (Memc[output])
		    else
			return
		} else
		    call erract (EA_ERROR)
		out = open (Memc[output], NEW_FILE, TEXT_FILE)
	    }
	}

	# Set default limits for source pixels.
	if (IS_INDEF(smax))
	    smax = MAX_REAL
	if (IS_INDEF(smin))
	    smin = 10000

	# Set header information of output.
	call sysid (Memc[image], SZ_FNAME)
	call sprintf (Memc[str], SZ_LINE, "# XTCOEFF: %s\n")
	    call pargstr (Memc[image])
	if (out != NULL)
	    call fprintf (out, Memc[str])
	if (verbose) {
	    call fseti (STDOUT, F_FLUSHNL, YES)
	    call printf ("\n")
	    call printf (Memc[str])
	}
	do i = 1, ARB {
	    if (imtgetim (inlist, Memc[image], SZ_FNAME) == EOF)
		break
	    if (imtgetim (bkvlist, Memc[bkvname], SZ_FNAME) == EOF)
	        call strcpy ("Line median", Memc[bkvname], SZ_FNAME)
	    if (imtgetim (bkslist, Memc[bksname], SZ_FNAME) == EOF)
	        call strcpy ("Line median", Memc[bksname], SZ_FNAME)
	    if (imtgetim (mlist, Memc[mname], SZ_FNAME) == EOF)
	        Memc[mname] = EOS
	    if (i == 1) {
		call sprintf (Memc[str], SZ_LINE, "# %18s %18s %18s %18s\n")
		    call pargstr ("Images")
		    call pargstr ("Victim Bkg")
		    call pargstr ("Source Bkg")
		    call pargstr ("Victim Mask")
		if (out != NULL)
		    call fprintf (out, Memc[str])
		if (verbose)
		    call printf (Memc[str])
	    }
	    call sprintf (Memc[str], SZ_LINE, "# %18s %18s %18s %18s\n")
		call pargstr (Memc[image])
		call pargstr (Memc[bkvname])
		call pargstr (Memc[bksname])
		call pargstr (Memc[mname])
	    if (out != NULL)
		call fprintf (out, Memc[str])
	    if (verbose)
		call printf (Memc[str])
	}
	if (out != NULL)
	    call fprintf (out, "\n")
	if (verbose)
	    call printf ("\n")

	# Set the initial fitting parameters.
	call ic_open (ic)
	call ic_pstr (ic, "function", "chebyshev")
	call ic_puti (ic, "order", 1)
	call ic_pstr (ic, "xlabel", "Source pixel")
	call ic_pstr (ic, "ylabel", "Crosstalk coefficient")
	call ic_puti (ic, "niterate", niterate)
	call ic_putr (ic, "low", low)
	call ic_putr (ic, "high", high)

	# Set graphics for interactive fitting.
	gp = NULL; gt = NULL
	if (interactive) {
	    gp = gopen ("stdgraph", NEW_FILE+AW_DEFER, STDGRAPH)
	    gt = gt_init ()
	}

	# Because ic_fvshow requires gt in versions through V2.11.3p1.
	# This is a kludge to be removed someday.
	if (!interactive) {
	    gt = gt_init ()
	    Memi[ic+31] = gt
	}

	# Loop through each pair of victim and source extensions.
	while (fntgfnb (vlist, Memc[vextn], SZ_FNAME) != EOF) {
	    if (fntgfnb (slist, Memc[sextn], SZ_FNAME) == EOF)
		break

	    svals = NULL
	    coeffs = NULL
	    wts = NULL
	    ncoeffs = 0

	    # Accumulate data from all input exposures.
	    nimages = 0
	    call imtrew (inlist)
	    call imtrew (bkvlist)
	    call imtrew (bkslist)
	    call imtrew (mlist)
	    while (imtgetim (inlist, Memc[image], SZ_FNAME) != EOF) {
	        if (imtgetim (bkvlist, Memc[bkvname], SZ_FNAME) == EOF)
		    Memc[bkvname] = EOS
	        if (imtgetim (bkslist, Memc[bksname], SZ_FNAME) == EOF)
		    Memc[bksname] = EOS
	        if (imtgetim (mlist, Memc[mname], SZ_FNAME) == EOF)
		    Memc[mname] = EOS

		iferr {
		    source = NULL; victim = NULL
		    bkv = NULL; bks = NULL; pm = NULL
		    vbuf = NULL; sbuf = NULL

		    # Open victim extension.
		    call sprintf (Memc[fname], SZ_FNAME, "%s[%s]")
			call pargstr (Memc[image])
			call pargstr (Memc[vextn])
		    ptr = immap (Memc[fname], READ_ONLY, 0); victim = ptr

		    i = strlen (Memc[fname])
		    ifnoerr (call imgstr (victim, "DATASEC", Memc[fname+i],
			SZ_FNAME-i)) {
			call imunmap (victim)
			ptr = immap (Memc[fname], READ_ONLY, 0); victim = ptr
		    }

		    # Open source extension.
		    call sprintf (Memc[fname], SZ_FNAME, "%s[%s]")
			call pargstr (Memc[image])
			call pargstr (Memc[sextn])
		    ptr = immap (Memc[fname], READ_ONLY, 0); source = ptr

		    i = strlen (Memc[fname])
		    ifnoerr (call imgstr (source, "DATASEC", Memc[fname+i],
			SZ_FNAME-i)) {
			call imunmap (source)
			ptr = immap (Memc[fname], READ_ONLY, 0); source = ptr
		    }

		    # Set background or open background extension.
		    if (Memc[bkvname] != EOS) {
			i = 1
			if (ctor (Memc[bkvname], i, vsky) == 0) {
			    call sprintf (Memc[fname], SZ_FNAME, "%s[%s]")
				call pargstr (Memc[bkvname])
				call pargstr (Memc[vextn])
			    ptr = map_open (Memc[fname], victim); bkv = ptr
			}
		    } else
			call malloc (vbuf, IM_LEN(victim,1), TY_REAL)
		    if (Memc[bksname] != EOS) {
			i = 1
			if (ctor (Memc[bksname], i, ssky) == 0) {
			    call sprintf (Memc[fname], SZ_FNAME, "%s[%s]")
				call pargstr (Memc[bksname])
				call pargstr (Memc[sextn])
			    ptr = map_open (Memc[fname], source); bks = ptr
			}
		    } else
			call malloc (sbuf, IM_LEN(victim,1), TY_REAL)

		    # Open mask.
		    if (Memc[mname] != EOS) {
			call sprintf (Memc[fname], SZ_FNAME, "%s[%s]")
			    call pargstr (Memc[mname])
			    call pargstr (Memc[vextn])
			call xt_maskname (Memc[fname], "mask", READ_ONLY,
			    Memc[fname], SZ_FNAME)
			ptr = yt_pmmap (Memc[fname], victim,
			    Memc[str], SZ_LINE); pm = ptr
		    }

		    # Keep track of number of image pairs successfully opened.
		    nimages = nimages + 1

		    # Check sizes.
		    nc = IM_LEN(source,1)
		    nl = IM_LEN(source,2)
		    if (nc != IM_LEN(victim,1) || nl != IM_LEN(victim,2))
			call error (1, "Extension sizes do not match")

		    # Determine flips.
		    iferr (atmvictim = imgetr (victim, "atm1_1"))
			atmvictim = 1
		    iferr (atmsource = imgetr (source, "atm1_1"))
			atmsource = 1
		    xflip = (atmvictim * atmsource < 0)
		    iferr (atmvictim = imgetr (victim, "atm2_2"))
			atmvictim = 1
		    iferr (atmsource = imgetr (source, "atm2_2"))
			atmsource = 1
		    yflip = (atmvictim * atmsource < 0)

		    # Accumulate crosstalk coefficient measurements.
		    do l = 1, nl {
			sdata = imgl2r (source, l) - 1
			if (bks != NULL)
			    bksdata = map_glr (bks, l, READ_ONLY) - 1
			vdata = NULL
			do c = 1, nc {
			    if (Memr[sdata+c]<smin || Memr[sdata+c]>smax)
				next
			    if (svals == NULL) {
				call malloc (svals, 2000, TY_REAL)
				call malloc (coeffs, 2000, TY_REAL)
				call malloc (wts, 2000, TY_REAL)
			    } else if (mod (max(1,ncoeffs), 2000) == 0) {
				call realloc (svals, ncoeffs+2000, TY_REAL)
				call realloc (coeffs, ncoeffs+2000, TY_REAL)
				call realloc (wts, ncoeffs+2000, TY_REAL)
			    }
			    if (vdata == NULL) {
				if (xflip && yflip) {
				    vdata = imgs2r (victim, nc, 1,
					nl-l+1, nl-l+1) - 1
				    if (bkv != NULL) {
				        bkvdata = map_glr (bkv, nl-l+1,
					    READ_WRITE) - 1
					do i = 1, nc/2 {
					    j = nc - i + 1
					    coeff = Memr[bkvdata+i]
					    Memr[bkvdata+i] = Memr[bkvdata+j]
					    Memr[bkvdata+j] = coeff
					}
				    }
				    if (pm != NULL)
					mdata = imgs2i (pm, nc, 1,
					    nl-l+1, nl-l+1) - 1
				} else if (xflip) {
				    vdata = imgs2r (victim, nc, 1, l, l) - 1
				    if (bkv != NULL) {
				        bkvdata = map_glr (bkv, l,
					    READ_WRITE) - 1
					do i = 1, nc/2 {
					    j = nc - i + 1
					    coeff = Memr[bkvdata+i]
					    Memr[bkvdata+i] = Memr[bkvdata+j]
					    Memr[bkvdata+j] = coeff
					}
				    }
				    if (pm != NULL)
					mdata = imgs2i (pm, nc, 1, l, l) - 1
				} else if (yflip) {
				    vdata = imgl2r (victim, nl-l+1) - 1
				    if (bkv != NULL)
					bkvdata = map_glr (bkv, nl-l+1,
					    READ_ONLY) - 1
				    if (pm != NULL)
					mdata = imgl2i (pm, nl-l+1) - 1
				} else {
				    vdata = imgl2r (victim, l) - 1
				    if (bkv != NULL)
					bkvdata = map_glr (bkv, l,
					    READ_ONLY) - 1
				    if (pm != NULL)
					mdata = imgl2i (pm, l) - 1
				}
				if (Memc[bkvname] == EOS) {
				    call amovr (Memr[vdata+1], Memr[vbuf], nc)
				    vsky = asokr (Memr[vbuf], nc,
				        int(medfactor*nc))
				}
				if (Memc[bksname] == EOS) {
				    call amovr (Memr[sdata+1], Memr[sbuf], nc)
				    ssky = asokr (Memr[sbuf], nc,
				        int(medfactor*nc))
				}
			    }
			    if (pm != NULL) {
				if (Memi[mdata+c] != 0)
				    next
			    }
			    if (bkv == NULL)
				coeff = (Memr[vdata+c] - vsky)
			    else
				coeff = (Memr[vdata+c] - Memr[bkvdata+c])
			    if (bks == NULL)
				coeff = coeff / (Memr[sdata+c] - ssky)
			    else
				coeff = coeff /
				    (Memr[sdata+c] - Memr[bksdata+c])
			    if (coeff > maxcoeff)
				next
			    Memr[svals+ncoeffs] = Memr[sdata+c]
			    Memr[coeffs+ncoeffs] = coeff
			    Memr[wts+ncoeffs] = 1.
			    ncoeffs = ncoeffs + 1
			}
		    }
		} then
		    call erract (EA_WARN)

		call mfree (sbuf, TY_REAL)
		call mfree (vbuf, TY_REAL)
		if (pm != NULL)
		    call imunmap (pm)
		if (bks != NULL)
		    call map_close (bks)
		if (bkv != NULL)
		    call map_close (bkv)
		if (victim != NULL)
		    call imunmap (victim)
		if (source != NULL)
		    call imunmap (source)
	    }

	    # If no images were opened continue with next extensions.
	    # The error checking will have notified the users.
	    if (nimages == 0)
		next

	    # Compute final coefficient and uncertainty.
	    if (ncoeffs < 10) {
		call eprintf (
		    "  Insufficient source data within limits (%s)\n")
		    call pargstr (Memc[sextn])
		call mfree (svals, TY_REAL)
		call mfree (coeffs, TY_REAL)
		call mfree (wts, TY_REAL)
		next
	    }

	    call xtsort3 (Memr[svals], Memr[coeffs], Memr[wts], ncoeffs)
	    if (interactive) {
		call sprintf (Memc[str], SZ_LINE, "Crosstalk for %s to %s")
		    call pargstr (Memc[sextn])
		    call pargstr (Memc[vextn])
		call gt_sets (gt, GTTITLE, Memc[str])
		call gt_sets (gt, GTTYPE, "point")

		call icg_fit (ic, gp, "gcur", gt, cv, Memr[svals],
		    Memr[coeffs], Memr[wts], ncoeffs)
		call gdeactivate (gp, 0)
	    } else {
		call ic_fit (ic, cv, Memr[svals], Memr[coeffs], Memr[wts],
		    ncoeffs, YES, YES, YES, YES)
	    }

	    # Extract the coefficient and uncertainty from ICFIT.
	    fd = stropen (Memc[strbuf], 1600, WRITE_ONLY)
	    call ic_fvshowr (ic, cv, Memr[svals], Memr[coeffs], Memr[wts],
		ncoeffs, fd)
	    call close (fd) 

	    fd = stropen (Memc[strbuf], 1600, READ_ONLY)
	    while (fscan (fd) != EOF) {
		call gargwrd (Memc[str], SZ_LINE)
		call gargr (coeff)
		call gargr (dcoeff)
		if (nscan() == 3)
		    break
	    }
	    call close (fd)

	    # Output the coefficient.
	    call sprintf (Memc[str], SZ_LINE, "%s\t%s\t%9.6f (%8.6f, %4.1f)\n")
		call pargstr (Memc[vextn])
		call pargstr (Memc[sextn])
		call pargr (coeff)
		call pargr (dcoeff)
		call pargr (abs(coeff/dcoeff))

	    if (out != NULL)
		call fprintf (out, Memc[str])
	    if (verbose)
		call printf (Memc[str])

	    call mfree (svals, TY_REAL)
	    call mfree (coeffs, TY_REAL)
	    call mfree (wts, TY_REAL)
	}

	# Done.
	if (gt != NULL)
	    call gt_free (gt)
	if (gp != NULL)
	    call gclose (gp)
	call ic_closer (ic)
	if (out != NULL)
	    call close (out)
	call imtclose (mlist)
	call imtclose (bkslist)
	call imtclose (bkvlist)
	call fntclsb (slist)
	call fntclsb (vlist)
	call imtclose (inlist)

	call sfree (sp)
end


# XT_MODE -- Compute the mode of an array.

real procedure xt_mode (data, npix, bins, nbins, binstart, binwidth, ncentroid)

real	data[npix]		#I Data array
int	npix			#I Number of pixels
int	bins[nbins]		#O Bin array
int	nbins			#I Number of bins
real	binstart		#I Value for first bin	
real	binwidth		#I Width of bins
int	ncentroid		#I Number of bins to centroid

int	i, n, bin, binmax
real	val, mode

begin
	call aclri (bins, nbins)
	do i = 1, npix {
	    bin = (data[i] - binstart) / binwidth + 1
	    if (bin < 1 || bin > nbins)
		next
	    bins[bin] = bins[bin] + 1
	}

	binmax = 1
	n = bins[binmax]
	do i = 1, nbins {
	    if (bins[i] > n) {
		binmax = i
		n = bins[binmax]
	    }
	}

	do i = -ncentroid/2, ncentroid/2 {
	    bin = binmax + i
	    if (bin < 1 || bin > nbins)
		next
	    val = (bin - 0.5) * binwidth + binstart
	    mode = mode + val * bins[bin]
	    n = n + bins[bin]
	}
	mode = mode / n

	return (mode)
end
