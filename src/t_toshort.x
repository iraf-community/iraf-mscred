include	<error.h>
include	<imhdr.h>
include	<mach.h>


# T_TOSHORT -- Convert images to scaled short.

procedure t_toshort ()

int	inlist			# List of input images
int	outlist			# List of output images
int	bpmlist			# List of bad pixel masks
real	datamin			# Minimum data value for scaling
real	datamax			# Maximum data value for scaling
real	maxbscale		# Maximum bscale allowed
int	logfile			# Logfile descriptor	

int	i
real	bscale, bzero
pointer	sp, input, output, bpmask, inbpmask, outbpmask, temp, bpmtemp
pointer	in, out, inbpm, outbpm, tmp

bool	streq()
int	imtopenp(), imtlen(), imtgetim()
int	nowhite(), open(), fnextn(), strlen()
real	clgetr()
pointer	immap(), yt_pmmap()
errchk	open, immap, yt_pmmap, toshort

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (bpmask, SZ_FNAME, TY_CHAR)
	call salloc (inbpmask, SZ_FNAME, TY_CHAR)
	call salloc (outbpmask, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (bpmtemp, SZ_FNAME, TY_CHAR)

	# Get input parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	bpmlist = imtopenp ("bpmasks")
	datamin = clgetr ("datamin")
	datamax = clgetr ("datamax")
	maxbscale = clgetr ("maxbscale")
	call clgstr ("logfile", Memc[temp], SZ_FNAME)

	if (nowhite (Memc[temp], Memc[temp], SZ_FNAME) == 0)
	    logfile = NULL
	else
	    logfile = open (Memc[temp], APPEND, TEXT_FILE)

	# Check lists match.
	Memc[output] = EOS
	Memc[bpmask] = EOS
	if ((imtlen (outlist) > 0 && imtlen(inlist) != imtlen(outlist)) ||
	    (imtlen (bpmlist) > 1 && imtlen(inlist) != imtlen(bpmlist))) {
	    call sfree (sp)
	    call error (1, "Input and output lists do not match")
	}
	if (imtlen (bpmlist) == 1 && imtlen(inlist) > 1) {
	    i = imtgetim (bpmlist, Memc[bpmask], SZ_FNAME)
	    if (!streq (Memc[bpmask], "BPM")) {
		call sfree (sp)
		call error (1, "Bad pixel file must be 'BPM'")
	    }
	}

	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[temp], SZ_FNAME) != EOF)
		call strcpy (Memc[temp], Memc[output], SZ_FNAME)
	    if (imtgetim (bpmlist, Memc[temp], SZ_FNAME) != EOF) {
		call strcpy (Memc[temp], Memc[bpmask], SZ_FNAME)
		if (!streq (Memc[bpmask], "BPM")) {
		    if (fnextn (Memc[bpmask], Memc[temp], SZ_FNAME) > 0) {
			i = strlen (Memc[bpmask])
			if (streq (Memc[temp], "pl"))
			    Memc[bpmask+i-3] = EOS
			else if (streq (Memc[temp], "fits"))
			    Memc[bpmask+i-4] = EOS
		    }
		    call strcat (".pl", Memc[bpmask], SZ_FNAME)
		    call strcpy (Memc[bpmask], Memc[outbpmask], SZ_FNAME)
		}
	    }

	    if (Memc[output] == EOS)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    call xt_mkimtemp (Memc[input], Memc[output], Memc[temp], SZ_FNAME)

	    iferr {
		in = NULL; out = NULL; inbpm = NULL; outbpm = NULL

		tmp = immap (Memc[input], READ_ONLY, 0); in = tmp
		tmp = yt_pmmap ("BPM", in, Memc[inbpmask],SZ_FNAME); inbpm = tmp
		tmp = immap (Memc[output], NEW_COPY, in); out = tmp
		IM_PIXTYPE(out) = TY_SHORT
		if (streq (Memc[bpmask], "BPM")) {
		    if (IS_INDEFR(datamin) && IS_INDEFR(datamax))
			Memc[outbpmask] = EOS
		    else if (Memc[inbpmask] == NULL)
			call error (1, "BPM not defined in the header")
		    else
			call strcpy (Memc[inbpmask], Memc[outbpmask], SZ_FNAME)
		}
		if (Memc[outbpmask] != EOS) {
		    if (Memc[inbpmask] == EOS) {
			call strcpy (Memc[outbpmask], Memc[bpmtemp], SZ_FNAME)
			tmp = immap (Memc[outbpmask], NEW_COPY, in)
		    } else {
			call xt_mkimtemp (Memc[inbpmask], Memc[outbpmask],
			    Memc[bpmtemp], SZ_FNAME)
			tmp = immap (Memc[outbpmask], NEW_COPY, inbpm)
		    }
		    outbpm = tmp
		    IM_PIXTYPE(outbpm) = TY_SHORT
		}

		call toshort (in, out, inbpm, outbpm, datamin, datamax,
		    maxbscale, bscale, bzero)

		if (outbpm != NULL)
		    call imunmap (outbpm)
		if (out != NULL)
		    call imunmap (out)
		if (inbpm != NULL)
		    call imunmap (inbpm)
		if (in != NULL)
		    call imunmap (in)

		# Update header.
		out = immap (Memc[output], READ_WRITE, 0)
		call imaddr (out, "BSCALE", bscale)
		call imaddr (out, "BZERO", bzero)
		if (Memc[outbpmask] != EOS)
		    call imastr (out, "BPM", Memc[bpmtemp])
		call imunmap (out)

		call xt_delimtemp (Memc[output], Memc[temp])
		if (Memc[outbpmask] != EOS)
		    call xt_delimtemp (Memc[outbpmask], Memc[bpmtemp])
	    } then {
		call eprintf ("%s - ")
		    call pargstr (Memc[input])
		call flush (STDERR)
		call erract (EA_WARN)
		if (outbpm != NULL) {
		    call imunmap (outbpm)
		    call imdelete (Memc[bpmtemp])
		}
		if (out != NULL) {
		    call imunmap (out)
		    call imdelete (Memc[output])
		}
		if (inbpm != NULL)
		    call imunmap (inbpm)
		if (in != NULL)
		    call imunmap (in)
	    }
	}

	if (logfile != NULL)
	    call close (logfile)
	call imtclose (bpmlist)
	call imtclose (outlist)
	call imtclose (inlist)
	call sfree (sp)
end


# TOSHORT -- Scale input data and output scaled data and optional mask.

procedure toshort (in, out, inbpm, outbpm, datamin, datamax, maxbscale,
	bscale, bzero)

pointer	in		#I Input image pointer
pointer	out		#I Output image pointer
pointer	inbpm		#I Input mask pointer
pointer	outbpm		#I Output imask pointer
real	datamin		#I Data minimum for scaling
real	datamax		#I Data maximum for scaling
real	maxbscale	#I Maximum bscale allowed
real	bscale		#O Scaling value
real	bzero		#O Scaling value

int	i, nc
real	dmin, dmax, val
pointer	sp, vin, vout, vinbpm, voutbpm, str
pointer	datain, dataout, datainbpm, dataoutbpm

int	imgnlr(), imgnls(), impnls()
errchk	toshort_minmax

begin
	call smark (sp)
	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)
	call salloc (vinbpm, IM_MAXDIM, TY_LONG)
	call salloc (voutbpm, IM_MAXDIM, TY_LONG)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Set data range if not already specified.
	if (IS_INDEFR (datamin) || IS_INDEFR (datamax))
	    call toshort_minmax (in, inbpm, dmin, dmax)
	if (!IS_INDEFR (datamin))
	    dmin = datamin
	if (!IS_INDEFR (datamax))
	    dmax = datamax

	# Set scaling parameters.
	bscale = (dmax - dmin) / 65520
	bzero = (dmax + dmin) / 2

	# Check bscale limit.
	if (!IS_INDEFR(maxbscale) && bscale > maxbscale) {
	    call sprintf (Memc[str], SZ_FNAME,
		"Maximum bscale exceeded (%.4g), output canceled.")
		call pargr (bscale)
	    call error (1, Memc[str])
	}

	# Initialize I/O.
	nc = IM_LEN(in,1)
	call amovkl (long (1), Meml[vin], IM_MAXDIM)
	call amovkl (long (1), Meml[vout], IM_MAXDIM)
	call amovkl (long (1), Meml[vinbpm], IM_MAXDIM)
	call amovkl (long (1), Meml[voutbpm], IM_MAXDIM)

	while (imgnlr (in, datain, Meml[vin]) != EOF) {
	    i  = impnls (out, dataout, Meml[vout])

	    if (outbpm == NULL) {
		do i = 1, nc {
		    val = max (dmin, min (dmax, Memr[datain]))
		    val = (val - bzero) / bscale
		    if (val >= 0.)
			val = val + 0.5
		    else
			val = val - 0.5
		    Mems[dataout] = val
		    datain = datain + 1
		    dataout = dataout + 1
		}
	    } else {
		i = impnls (outbpm, dataoutbpm, Meml[voutbpm])
		if (inbpm == NULL)
		    call aclrs (Mems[dataoutbpm], nc)
		else {
		    i = imgnls (inbpm, datainbpm, Meml[vinbpm])
		    call amovs (Mems[datainbpm], Mems[dataoutbpm], nc)
		}
		do i = 1, nc {
		    val = Memr[datain]
		    if (val < dmin || val > dmax) {
			val = max (dmin, min (dmax, Memr[datain]))
			Mems[dataoutbpm] = 2
		    }
		    val = (val - bzero) / bscale
		    if (val >= 0.)
			val = val + 0.5
		    else
			val = val - 0.5
		    Mems[dataout] = val
		    datain = datain + 1
		    dataout = dataout + 1
		    dataoutbpm = dataoutbpm + 1
		}
	    }
	}

	call sfree (sp)
end


# TOSHORT_MINMAX -- Find range of data which is not masked.

procedure toshort_minmax (in, bpm, dmin, dmax)

pointer	in			#I Input image pointer
pointer	bpm			#I Input mask pointer
real	dmin			#O Output data minimum
real	dmax			#O Output data maximum

pointer	sp, v, vbpm, data, databpm
int	i, nc, imgnlr(), imgnls()

begin
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call salloc (vbpm, IM_MAXDIM, TY_LONG)

	# Initialize
	nc = IM_LEN(in,1)
	dmin = MAX_REAL
	dmax = -MAX_REAL
	call amovkl (long (1), Meml[v], IM_MAXDIM)
	call amovkl (long (1), Meml[vbpm], IM_MAXDIM)

	# Find possibly masked minimum and maximum.
	while (imgnlr (in, data, Meml[v]) != EOF) {
	    if (bpm != NULL) {
		if (imgnls (bpm, databpm, Meml[vbpm]) != EOF) {
		    do i = 1, nc {
			if (Mems[databpm] == 0) {
			    dmin = min (dmin, Memr[data])
			    dmax = max (dmax, Memr[data])
			}
			data = data + 1
			databpm = databpm + 1
		    }
		} else {
		    do i = 1, nc {
			dmin = min (dmin, Memr[data])
			dmax = max (dmax, Memr[data])
			data = data + 1
		    }
		}
	    } else {
		do i = 1, nc {
		    dmin = min (dmin, Memr[data])
		    dmax = max (dmax, Memr[data])
		    data = data + 1
		}
	    }
	}

	if (dmax < dmin)
	    call error (1, "No unmasked data")

	call sfree (sp)
end
