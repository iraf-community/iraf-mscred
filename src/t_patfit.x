include	<error.h>
include	<imhdr.h>

define	OUTTYPES	"|none|fit|diff|ratio|flat|pfit|pdiff|pratio|pflat|\
			 |sfit|sdiff|sratio|sflat|"
define	NONE	1
define	FIT	2
define	DIFF	3
define	RATIO	4
define	FLAT	5
define	PFIT	6
define	PDIFF	7
define	PRATIO	8
define	PFLAT	9
define	SFIT	11
define	SDIFF	12
define	SRATIO	13
define	SFLAT	14

define	INIT	1		# Initialize accumulations
define	ACCUM	2		# Accumulate statistics
define	RESULT	3		# Compute result

# Accumulation elements
define	A	1
define	B	2
define	P	3
define	Q	4
define	W	5
define	V	6
define	AW	7
define	BW	8
define	PW	9
define	QW	10
define	AV	11
define	BV	12
define	PV	13
define	QV	14
define	N	15
define	NMEAN	14
define	SZ_SUM	15


# T_PATFIT -- Fit a pattern image to a data image.
# The input and output may be MEF files in which case the fit is done over
# all the extensions.

procedure t_patfit ()

pointer	inlist			# List of input files
pointer	outlist			# List of output files
pointer	patlist			# List of pattern files
pointer	wtlist			# List of weight files
int	ncblk, nlblk		# Weight blocking factors
pointer	bkglist			# List of background files
pointer	bkgplist		# List of pattern  background files
pointer	bkgwlist		# List of weight  background files
pointer	inmlist			# List of mask files for input
pointer	patmlist		# List of mask files for pattern
pointer	extfit			# Extension names to fit
pointer	extout			# Extension names to output
pointer	outtype			# Type of output
pointer	logname			# Name for log output
pointer	logfile			# Logfile
bool	verbose			# Verbose?

int	i, j, type, logfd[2]
pointer	sp, input, output, pattern, weight, bkg, bkgpat, bkgwt, mask, patmask, temp

bool	clgetb(), streq()
int	clgeti(), clgwrd(), imtlen(), imtgetim(), nowhite(), open()
pointer	imtopenp()
errchk	patfit1, imdelete, imrename, open

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (pattern, SZ_FNAME, TY_CHAR)
	call salloc (weight, SZ_FNAME, TY_CHAR)
	call salloc (bkg, SZ_FNAME, TY_CHAR)
	call salloc (bkgpat, SZ_FNAME, TY_CHAR)
	call salloc (bkgwt, SZ_FNAME, TY_CHAR)
	call salloc (mask, SZ_FNAME, TY_CHAR)
	call salloc (patmask, SZ_FNAME, TY_CHAR)
	call salloc (extfit, SZ_LINE, TY_CHAR)
	call salloc (extout, SZ_LINE, TY_CHAR)
	call salloc (outtype, SZ_FNAME, TY_CHAR)
	call salloc (logname, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Get parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	patlist = imtopenp ("pattern")
	wtlist = imtopenp ("weight")
	ncblk = clgeti ("ncblk")
	nlblk = clgeti ("nlblk")
	bkglist = imtopenp ("background")
	bkgplist = imtopenp ("bkgpattern")
	bkgwlist = imtopenp ("bkgweight")
	inmlist = imtopenp ("masks")
	patmlist = imtopenp ("patmasks")
	call clgstr ("extfit", Memc[extfit], SZ_FNAME)
	call clgstr ("extout", Memc[extout], SZ_FNAME)
	type = clgwrd ("outtype", Memc[outtype], SZ_FNAME, OUTTYPES)
	call clgstr ("logname", Memc[logname], SZ_FNAME)
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	verbose = clgetb ("verbose")

	if (type == 0)
	    call error (1, "Unknown output type")

	# Check lists.
	j = imtlen (inlist)
	if (j > 0) {
	    if (type != NONE) {
		i = imtlen (outlist)
		if (i > 0 && i != j)
		    call error (1, "Output list doesn't match input list")
	    }
	    i = imtlen (patlist)
	    if (i == 0)
		call error (2, "No pattern specified")
	    else if (i > 1 && i != j)
		call error (3, "Pattern list doesn't match input list")
	    i = imtlen (wtlist)
	    if (i > 1 && i != j)
		call error (4, "Weight list doesn't match input list")
	    i = imtlen (bkglist)
	    if (i > 1 && i != j)
		call error (5, "Input background list doesn't match input list")
	    i = imtlen (bkgplist)
	    if (i > 1 && i != j)
		call error (6,
		    "Pattern background list doesn't match input list")
	    i = imtlen (bkgwlist)
	    if (i > 1 && i != j)
		call error (7,
		    "Weight background list doesn't match input list")
	    i = imtlen (inmlist)
	    if (i > 1 && i != j)
		call error (8, "Input mask list doesn't match input list")
	    i = imtlen (patmlist)
	    if (i > 1 && i != j)
		call error (9, "Pattern mask list doesn't match input list")
	}

	# Setup log output.
	if (nowhite (Memc[logfile], Memc[logfile], SZ_LINE) != 0)
	    logfd[1] = open (Memc[logfile], APPEND, TEXT_FILE)
	else
	    logfd[1] = NULL
	if (verbose)
	    logfd[2] = STDOUT
	else
	    logfd[2] = NULL

	# Initialize file names.
	Memc[input] = EOS;  Memc[output] = EOS; Memc[pattern] = EOS
	Memc[weight] = EOS; Memc[bkg] = EOS;    Memc[bkgpat] = EOS
	Memc[bkgwt] = EOS;  Memc[mask] = EOS;   Memc[patmask] = EOS

	# Process input to output.
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (type != NONE) {
		if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		    call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    } else
		Memc[output] = EOS
	    if (imtgetim (patlist, Memc[temp], SZ_LINE) != EOF)
	        call strcpy (Memc[temp], Memc[pattern], SZ_LINE)
	    if (imtgetim (wtlist, Memc[weight], SZ_LINE) == EOF)
	        call strcpy (Memc[pattern], Memc[weight], SZ_LINE)
	    if (imtgetim (bkglist, Memc[temp], SZ_FNAME) != EOF)
	        call strcpy (Memc[temp], Memc[bkg], SZ_LINE)
	    if (imtgetim (bkgplist, Memc[temp], SZ_FNAME) != EOF)
	        call strcpy (Memc[temp], Memc[bkgpat], SZ_LINE)
	    if (imtgetim (bkgwlist, Memc[temp], SZ_FNAME) != EOF)
	        call strcpy (Memc[temp], Memc[bkgwt], SZ_LINE)
	    if (imtgetim (inmlist, Memc[temp], SZ_FNAME) != EOF)
	        call strcpy (Memc[temp], Memc[mask], SZ_LINE)
	    if (imtgetim (patmlist, Memc[temp], SZ_FNAME) != EOF)
	        call strcpy (Memc[temp], Memc[patmask], SZ_LINE)

	    call strcpy (Memc[output], Memc[temp], SZ_LINE)
	    if (streq (Memc[input], Memc[output]))
		call mktemp ("tmp", Memc[temp], SZ_FNAME)

	    iferr {
		call patfit1 (Memc[input], Memc[output], Memc[temp],
		    Memc[pattern], Memc[weight], ncblk, nlblk, Memc[bkg],
		    Memc[bkgpat], Memc[bkgwt], Memc[mask], Memc[patmask],
		    Memc[extfit], Memc[extout], Memc[outtype],
		    Memc[logname], logfd)
		if (!streq (Memc[output], Memc[temp])) {
		    call imdelete (Memc[output])
		    call imrename (Memc[temp], Memc[output])
		}
	    } then {
		call erract (EA_WARN)
		iferr (call imdelete (Memc[temp]))
		    ;
	    }
	}

	# Finish up.
	do i = 1, 2 {
	    if (logfd[i] != NULL || logfd[i] == STDERR)
		call close (logfd[i])
	}
	call imtclose (patmlist)
	call imtclose (inmlist)
	call imtclose (bkgwlist)
	call imtclose (bkgplist)
	call imtclose (bkglist)
	call imtclose (wtlist)
	call imtclose (patlist)
	call imtclose (outlist)
	call imtclose (inlist)
	call sfree (sp)
end


# PATFIT1 -- Fit a pattern image to a data image.
# The input and output may be MEF files in which case the fit is done over
# all the extensions.

procedure patfit1 (input, output, temp, pattern, weight, ncblk, nlblk,
	bkg, bkgpat, bkgwt, mask, patmask, extfit, extout, outtype,
	logname, logfd)

char	input[ARB]		#I Input filename
char	output[ARB]		#I Output filename
char	temp[ARB]		#I Temporary output filename
char	pattern[ARB]		#I Pattern filename
char	weight[ARB]		#I Weight filename
int	ncblk, nlblk		#I Weight blocking factors
char	bkg[ARB]		#I Input background filename
char	bkgpat[ARB]		#I Pattern background filename
char	bkgwt[ARB]		#I Weight background filename
char	mask[ARB]		#I Mask filename
char	patmask[ARB]		#I Pattern mask filename
char	extfit[ARB]		#I Extensions names to fit
char	extout[ARB]		#I Extensions names to output
char	outtype[ARB]		#I Output type
char	logname[ARB]		#I Log name for keyword
int	logfd[ARB]		#I Log file descriptors

int	i, j, nc, nl, meflist1, meflist2, imext, type
real	s, f
double	scale, flatscale, flatbkg, stat[SZ_SUM]
pointer	sp, inext, outext, tempext, patext, wtext, ghdr
pointer	bkgext, bkgpext, bkgwext, mext, patmext, str
pointer	in, out, pat, wt, bkin, bkpat, bkwt, pm, pmpat, tmp
pointer	inbuf, outbuf, patbuf, bkinbuf, bkpatbuf

bool	streq()
int	xt_extns(), imtgetim(), strldxs(), strdic(), imaccess()
pointer	immap(), yt_pmmap(), map_open()
pointer	imgl2r(), impl2r(), map_glr()
errchk	immap, yt_pmmap, map_open, patfit2

begin
	call smark (sp)
	call salloc (inext, SZ_FNAME, TY_CHAR)
	call salloc (outext, SZ_FNAME, TY_CHAR)
	call salloc (tempext, SZ_FNAME, TY_CHAR)
	call salloc (patext, SZ_FNAME, TY_CHAR)
	call salloc (wtext, SZ_FNAME, TY_CHAR)
	call salloc (ghdr, SZ_FNAME, TY_CHAR)
	call salloc (bkgext, SZ_FNAME, TY_CHAR)
	call salloc (bkgpext, SZ_FNAME, TY_CHAR)
	call salloc (bkgwext, SZ_FNAME, TY_CHAR)
	call salloc (mext, SZ_FNAME, TY_CHAR)
	call salloc (patmext, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Set output type.
	type = strdic (outtype, Memc[str], SZ_FNAME, OUTTYPES)
	if (type == 0)
	    call error (10, "Unknown output type")

	# Write log output.
	do i = 1, 2 {
	    if (logfd[i] == NULL || logfd[i] == STDERR)
		next
	    call sysid (Memc[str], SZ_LINE)
	    call fprintf (logfd[i], "%s: %s\n")
		call pargstr (logname)
		call pargstr (Memc[str])
	    call fprintf (logfd[i], "  input = %s\n")
		call pargstr (input)
	    call fprintf (logfd[i], "  pattern = %s\n")
		call pargstr (pattern)
	    call fprintf (logfd[i], "  weight = %s\n")
		call pargstr (weight)
	    if (ncblk > 1) {
		call fprintf (logfd[i], "  ncblk = %d\n")
		    call pargi (ncblk)
	    }
	    if (nlblk > 1) {
		call fprintf (logfd[i], "  nlblk = %d\n")
		    call pargi (nlblk)
	    }
	    if (bkg[1] != EOS) {
		call fprintf (logfd[i], "  input background = %s\n")
		    call pargstr (bkg)
	    } else
		call fprintf (logfd[i], "  input background = <input>\n")
	    if (bkgpat[1] != EOS) {
		call fprintf (logfd[i], "  pattern background = %s\n")
		    call pargstr (bkgpat)
	    } else
		call fprintf (logfd[i], "  pattern background = <pattern>\n")
	    if (bkgwt[1] != EOS) {
		call fprintf (logfd[i], "  weight background = %s\n")
		    call pargstr (bkgwt)
	    } else
		call fprintf (logfd[i], "  weight background = <weight>\n")
	    if (mask[1] != EOS) {
		call fprintf (logfd[i], "  input mask = %s\n")
		    call pargstr (mask)
	    }
	    if (patmask[1] != EOS) {
		call fprintf (logfd[i], "  pattern mask = %s\n")
		    call pargstr (patmask)
	    }
	    if (type != NONE) {
		call fprintf (logfd[i], "  output = %s\n")
		    call pargstr (output)
		call fprintf (logfd[i], "  outtype = %s\n")
		    call pargstr (outtype)
	    }
	    call flush (logfd[i])
	}

	# Initialize fitting.
	call patfit2 (NULL, NULL, NULL, ncblk, nlblk, NULL, NULL, NULL, NULL,
	    NULL, INIT, scale, stat, logfd)

	# Expand clusters to images.  As a special case if the input is
	# an explicit extension image then don't treat the input as MEF.
	meflist1 = xt_extns (input, "IMAGE", "0-", extfit, "", NO, YES, NO,
	    NO, "", NO, imext)
	meflist2 = xt_extns (input, "IMAGE", "0-", extout, "", NO, YES, NO,
	    NO, "", NO, imext)
	if (strldxs ("[", input) != 0)
	    imext = NO
	while (imtgetim (meflist1, Memc[inext], SZ_FNAME) != EOF) {
	    call strcpy (pattern, Memc[patext], SZ_FNAME)
	    call strcpy (weight, Memc[wtext], SZ_FNAME)
	    call strcpy (bkg, Memc[bkgext], SZ_FNAME)
	    call strcpy (bkgpat, Memc[bkgpext], SZ_FNAME)
	    call strcpy (bkgwt, Memc[bkgwext], SZ_FNAME)
	    call strcpy (mask, Memc[mext], SZ_FNAME)
	    call strcpy (patmask, Memc[patmext], SZ_FNAME)

	    # Add extension if needed.
	    i = strldxs ("[", Memc[inext])
	    if (imext == YES && i > 0) {
		i = inext + i - 1
		if (strldxs ("[", pattern) == 0)
		    call strcat (Memc[i], Memc[patext], SZ_FNAME)
		if (strldxs ("[", pattern) == 0)
		    call strcat (Memc[i], Memc[wtext], SZ_FNAME)
		if (bkg[1] != EOS && strldxs ("[", pattern) == 0)
		    call strcat (Memc[i], Memc[bkgext], SZ_FNAME)
		if (bkgpat[1] != EOS && strldxs ("[", pattern) == 0)
		    call strcat (Memc[i], Memc[bkgpext], SZ_FNAME)
		if (bkgwt[1] != EOS && strldxs ("[", pattern) == 0)
		    call strcat (Memc[i], Memc[bkgwext], SZ_FNAME)
		if (mask[1] != EOS && strldxs ("[", mask) == 0)
		    call strcat (Memc[i], Memc[mext], SZ_FNAME)
		if (patmask[1] != EOS && strldxs ("[", patmask) == 0)
		    call strcat (Memc[i], Memc[patmext], SZ_FNAME)
	    }

	    # Set pixel mask names.
	    if (Memc[mext] != EOS)
		call xt_maskname (Memc[mext], "mask", READ_ONLY,
		    Memc[mext], SZ_FNAME)
	    if (Memc[patmext] != EOS)
		call xt_maskname (Memc[patmext], "mask", READ_ONLY,
		    Memc[patmext], SZ_FNAME)

	    # Fit image.
	    iferr {
		in = NULL; out = NULL; pat = NULL; wt = NULL
		bkin = NULL; bkpat = NULL; bkwt = NULL; pm = NULL; pmpat = NULL

		tmp = immap (Memc[inext], READ_ONLY, 0); in = tmp
		tmp = immap (Memc[patext], READ_ONLY, 0); pat = tmp
		if (streq (Memc[patext], Memc[wtext]))
		    wt = pat
		else {
		    tmp = map_open (Memc[wtext], in); wt = tmp
		}
		if (Memc[bkgext] != EOS) {
		    tmp = map_open (Memc[bkgext], in); bkin = tmp
		}
		if (Memc[bkgpext] != EOS) {
		    tmp = map_open (Memc[bkgpext], in); bkpat = tmp
		}
		if (streq (Memc[bkgpext], Memc[bkgwext]))
		    bkwt = bkpat
		else if (Memc[bkgwext] != EOS) {
		    tmp = map_open (Memc[bkgwext], in); bkwt = tmp
		}
		if (Memc[mext] != EOS) {
		    tmp = yt_pmmap (Memc[mext], in, Memc[str], SZ_FNAME)
		    pm = tmp
		}
		if (Memc[patmext] != EOS) {
		    tmp = yt_pmmap (Memc[patmext], pat, Memc[str], SZ_FNAME)
		    pmpat = tmp
		}

		call patfit2 (in, pat, wt, ncblk, nlblk, bkin, bkpat, bkwt,
		    pm, pmpat, ACCUM, scale, stat, logfd)

		if (pmpat != NULL)
		    call imunmap (pmpat)
		if (pm != NULL)
		    call imunmap (pm)
		if (bkwt != NULL && bkwt != bkpat)
		    call map_close (bkwt)
		if (bkpat != NULL)
		    call map_close (bkpat)
		if (bkin != NULL)
		    call map_close (bkin)
		if (wt == pat)
		    wt = NULL
		else
		    call map_close (wt)
		if (type == NONE || imext == YES) {
		    call imunmap (pat)
		    call imunmap (in)
		}

		# Write log output.
		if (imext == YES) {
		    do i = 1, 2 {
			if (logfd[i] == NULL || logfd[i] == STDERR)
			    next
			call fprintf (logfd[i],
			    "    %s: %.4g\n")
			    call pargstr (Memc[inext])
			    call pargd (scale)
			call flush (logfd[i])
		    }
		}
	    } then {
		if (pmpat != NULL)
		    call imunmap (pmpat)
		if (pm != NULL)
		    call imunmap (pm)
		if (bkwt != NULL && bkwt != bkpat)
		    call map_close (bkwt)
		if (bkpat != NULL)
		    call map_close (bkpat)
		if (bkin != NULL)
		    call map_close (bkin)
		if (wt != NULL && wt != pat)
		    call map_close (wt)
		if (pat != NULL)
		    call imunmap (pat)
		if (in != NULL)
		    call imunmap (in)
		call imtclose (meflist2)
		call imtclose (meflist1)
		call sfree (sp)
		call erract (EA_ERROR)
	    }
	}

	# Compute grand totals.
	call patfit2 (NULL, NULL, NULL, ncblk, nlblk, NULL, NULL, NULL, NULL,
	    NULL, RESULT, scale, stat, logfd)

	# Derive related quantities.
	switch (type) {
	case FLAT, PFLAT:
	    flatbkg = stat[A] - scale * (stat[P] - stat[Q])
	    flatscale = scale / flatbkg
	    s = flatscale
	    f = 1 / flatbkg
	case SFLAT:
	    flatbkg = stat[A] - scale * stat[P]
	    flatscale = scale / flatbkg
	    s = flatscale
	    f = 1 / flatbkg
	default:
	    s = scale
	}

	# Write log output.
	do i = 1, 2 {
	    if (logfd[i] == NULL || logfd[i] == STDERR)
		next
	    call fprintf (logfd[i], "  <input> = %.4g\n")
		call pargd (stat[A])
	    if (stat[B] != stat[A]) {
		call fprintf (logfd[i], "  <background> = %.4g\n")
		    call pargd (stat[B])
	    }
	    call fprintf (logfd[i], "  <pattern> = %.4g\n")
		call pargd (stat[P])
	    if (stat[Q] != stat[P]) {
		call fprintf (logfd[i], "  <bkgpattern> = %.4g\n")
		    call pargd (stat[Q])
	    }
	    call fprintf (logfd[i], "  <weight> = %.4g\n")
		call pargd (stat[W])
	    if (stat[V] != stat[W]) {
		call fprintf (logfd[i], "  <bkgweight> = %.4g\n")
		    call pargd (stat[V])
	    }
	    call fprintf (logfd[i], "  scale = %.4g\n")
		call pargd (scale)
	    switch (type) {
	    case FLAT, PFLAT, SFLAT:
	        call fprintf (logfd[i], "  flatscale = %.4g\n")
		    call pargd (flatscale)
	        call fprintf (logfd[i], "  flatbkg = %.4g\n")
		    call pargd (flatbkg)
	    }
	    call flush (logfd[i])
	}

	# Create output.
	if (type != NONE) {
	    call imtrew (meflist2)
	    while (imtgetim (meflist2, Memc[inext], SZ_FNAME) != EOF) {
		call strcpy (output, Memc[outext], SZ_FNAME)
		call strcpy (temp, Memc[tempext], SZ_FNAME)
		call strcpy (pattern, Memc[patext], SZ_FNAME)
		call strcpy (bkg, Memc[bkgext], SZ_FNAME)
		call strcpy (bkgpat, Memc[bkgpext], SZ_FNAME)

		# Add extensions if needed.
		i = strldxs ("[", Memc[inext])
		if (imext == YES && i > 0) {
		    i = inext + i - 1
		    if (strldxs ("[", output) == 0)
			call strcat (Memc[i], Memc[outext], SZ_FNAME)
		    if (strldxs ("[", temp) == 0)
			call strcat (Memc[i], Memc[tempext], SZ_FNAME)
		    if (strldxs ("[", pattern) == 0)
			call strcat (Memc[i], Memc[patext], SZ_FNAME)
		    if (bkg[1] != EOS && strldxs ("[", pattern) == 0)
			call strcat (Memc[i], Memc[bkgext], SZ_FNAME)
		    if (bkgpat[1] != EOS && strldxs ("[", pattern) == 0)
			call strcat (Memc[i], Memc[bkgpext], SZ_FNAME)
		}

		# Substitute means for backgrounds if needed.
		if (Memc[bkgext] == EOS) {
		    call sprintf (Memc[bkgext], SZ_FNAME, "%.5g")
		        call pargd (stat[B])
		}
		if (Memc[bkgpext] == EOS) {
		    call sprintf (Memc[bkgpext], SZ_FNAME, "%.5g")
		        call pargd (stat[Q])
		}

		# Optimize cases where backgrounds are zero.
		if (streq (Memc[bkgext], "0") || streq (Memc[bkgext], "0.")) {
		    switch (type) {
		    case FIT:
		        type = PFIT
		    case DIFF:
		        type = PDIFF
		    case RATIO:
		        type = PRATIO
		    case FLAT:
		        type = PFLAT
		    }
		}
		if (streq (Memc[bkgpext], "0") || streq (Memc[bkgpext], "0.")) {
		    switch (type) {
		    case PFIT:
		        type = SFIT
		    case PDIFF:
		        type = SDIFF
		    case PRATIO:
		        type = SRATIO
		    case PFLAT:
		        type = SFLAT
		    }
		}

		# Add append/inherit if needed.
		i = strldxs ("]", Memc[tempext])
		if (i > 0 && imext == YES) {
		    Memc[tempext+i-1] = EOS
		    call strcat (",append,inherit]", Memc[tempext], SZ_FNAME)
		}

		# Set output.
		switch (type) {
		case FIT:
		    call sprintf (Memc[str], SZ_LINE,
			"%.5g (%s - %s) + %s")
			call pargd (scale)
			call pargstr (Memc[patext])
			call pargstr (Memc[bkgpext])
			call pargstr (Memc[bkgext])
		case PFIT:
		    call sprintf (Memc[str], SZ_LINE,
			"%.5g (%s - %s)")
			call pargd (scale)
			call pargstr (Memc[patext])
			call pargstr (Memc[bkgpext])
		case SFIT:
		    call sprintf (Memc[str], SZ_LINE,
			"%.5g %s")
			call pargd (scale)
			call pargstr (Memc[patext])
		case DIFF:
		    call sprintf (Memc[str], SZ_LINE,
			"%s - (%.5g (%s - %s) + %s)")
			call pargstr (Memc[inext])
			call pargd (scale)
			call pargstr (Memc[patext])
			call pargstr (Memc[bkgpext])
			call pargstr (Memc[bkgext])
		case PDIFF:
		    call sprintf (Memc[str], SZ_LINE,
			"%s - %.5g (%s - %s)")
			call pargstr (Memc[inext])
			call pargd (scale)
			call pargstr (Memc[patext])
			call pargstr (Memc[bkgpext])
		case SDIFF:
		    call sprintf (Memc[str], SZ_LINE,
			"%s - %.5g %s")
			call pargstr (Memc[inext])
			call pargd (scale)
			call pargstr (Memc[patext])
		case RATIO:
		    call sprintf (Memc[str], SZ_LINE,
			"%s / (%.5g (%s - %s) + %s)")
			call pargstr (Memc[inext])
			call pargd (scale)
			call pargstr (Memc[patext])
			call pargstr (Memc[bkgpext])
			call pargstr (Memc[bkgext])
		case PRATIO:
		    call sprintf (Memc[str], SZ_LINE,
			"%s / (%.5g (%s - %s))")
			call pargstr (Memc[inext])
			call pargd (scale)
			call pargstr (Memc[patext])
			call pargstr (Memc[bkgpext])
		case SRATIO:
		    call sprintf (Memc[str], SZ_LINE,
			"%s / (%.5g %s)")
			call pargstr (Memc[inext])
			call pargd (scale)
			call pargstr (Memc[patext])
		case FLAT:
		    call sprintf (Memc[str], SZ_LINE,
			"%s / (%.5g %s + %.5g %s)")
			call pargstr (Memc[inext])
			call pargr (s)
			call pargstr (Memc[patext])
			call pargr (f)
			call pargstr (Memc[bkgext])
		case PFLAT:
		    call sprintf (Memc[str], SZ_LINE,
			"%s / (%.5g (%s - %s) + 1)")
			call pargstr (Memc[inext])
			call pargr (s)
			call pargstr (Memc[patext])
			call pargstr (Memc[bkgpext])
		case SFLAT:
		    call sprintf (Memc[str], SZ_LINE,
			"%s / (%.5g %s + 1)")
			call pargstr (Memc[inext])
			call pargr (s)
			call pargstr (Memc[patext])
		}

		# Write log output.
		do i = 1, 2 {
		    if (logfd[i] == NULL || logfd[i] == STDERR)
			next
		    call fprintf (logfd[i], "    %s = %s\n")
			call pargstr (Memc[outext])
			call pargstr (Memc[str])
		    call flush (logfd[i])
		}

		# Create output extension.
		iferr {
		    if (imext == YES) {
			call sprintf (Memc[ghdr], SZ_LINE, "%s[0]")
			    call pargstr (temp)
			if (imaccess (Memc[ghdr], 0) == NO) {
			    call sprintf (Memc[ghdr], SZ_LINE, "%s[0]")
				call pargstr (input)
			    tmp = immap (Memc[ghdr], READ_ONLY, 0); in = tmp
			    out = immap (temp, NEW_COPY, in)
			    call imunmap (out)
			    call imunmap (in)
			}
		    }

		    if (in == NULL) {
			tmp = immap (Memc[inext], READ_ONLY, 0); in = tmp
		    }
		    if (pat == NULL) {
			tmp = immap (Memc[patext], READ_ONLY, 0); pat = tmp
		    }
		    switch (type) {
		    case FIT, DIFF, RATIO, FLAT:
			if (Memc[bkgext] != EOS) {
			    tmp = map_open (Memc[bkgext], in); bkin = tmp
			}
			if (Memc[bkgpext] != EOS) {
			    tmp = map_open (Memc[bkgpext], in); bkpat = tmp
			}
		    case PFIT, PDIFF, PRATIO, PFLAT:
			if (Memc[bkgpext] != EOS) {
			    tmp = map_open (Memc[bkgpext], in); bkpat = tmp
			}
		    }
		    tmp = immap (Memc[tempext], NEW_COPY, in); out = tmp
		    call imastr (out, logname, Memc[str])

		    nc = IM_LEN(in,1)
		    nl = IM_LEN(in,2)
		    do i = 1, nl {
			patbuf = imgl2r (pat, i)
			outbuf = impl2r (out, i)
			switch (type) {
			case FIT:
			    bkpatbuf = map_glr (bkpat, i, READ_ONLY)
			    bkinbuf = map_glr (bkin, i, READ_ONLY)
			    do j = 0, nc-1
				Memr[outbuf+j] = s * (Memr[patbuf+j] -
				    Memr[bkpatbuf+j]) + Memr[bkinbuf+j]
			case PFIT:
			    bkpatbuf = map_glr (bkpat, i, READ_ONLY)
			    do j = 0, nc-1
				Memr[outbuf+j] = s * (Memr[patbuf+j] -
				    Memr[bkpatbuf+j])
			case SFIT:
			    do j = 0, nc-1
				Memr[outbuf+j] = s * Memr[patbuf+j]
			case DIFF:
			    inbuf = imgl2r (in, i)
			    bkpatbuf = map_glr (bkpat, i, READ_ONLY)
			    bkinbuf = map_glr (bkin, i, READ_ONLY)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] -
				    (s * (Memr[patbuf+j] - Memr[bkpatbuf+j]) +
				    Memr[bkinbuf+j])
			case PDIFF:
			    inbuf = imgl2r (in, i)
			    bkpatbuf = map_glr (bkpat, i, READ_ONLY)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] -
				    s * (Memr[patbuf+j] - Memr[bkpatbuf+j])
			case SDIFF:
			    inbuf = imgl2r (in, i)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] -
				    s * Memr[patbuf+j]
			case RATIO:
			    inbuf = imgl2r (in, i)
			    bkpatbuf = map_glr (bkpat, i, READ_ONLY)
			    bkinbuf = map_glr (bkin, i, READ_ONLY)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] /
				    (s * (Memr[patbuf+j] - Memr[bkpatbuf+j]) +
				    Memr[bkinbuf+j])
			case PRATIO:
			    inbuf = imgl2r (in, i)
			    bkpatbuf = map_glr (bkpat, i, READ_ONLY)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] /
				    (s * (Memr[patbuf+j] - Memr[bkpatbuf+j]))
			case SRATIO:
			    inbuf = imgl2r (in, i)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] /
				    (s * Memr[patbuf+j])
			case FLAT:
			    inbuf = imgl2r (in, i)
			    bkinbuf = map_glr (bkin, i, READ_ONLY)
			    bkpatbuf = map_glr (bkpat, i, READ_ONLY)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] /
				    (s * (Memr[patbuf+j] - Memr[bkpatbuf]) +
				    f * Memr[bkinbuf])
			case PFLAT:
			    inbuf = imgl2r (in, i)
			    bkpatbuf = map_glr (bkpat, i, READ_ONLY)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] /
				    (s * (Memr[patbuf+j] - Memr[bkpatbuf]) + 1)
			case SFLAT:
			    inbuf = imgl2r (in, i)
			    do j = 0, nc-1
				Memr[outbuf+j] = Memr[inbuf+j] /
				    (s * Memr[patbuf+j] + 1)
			}
		    }

		    if (bkpat != NULL)
			call map_close (bkpat)
		    if (bkin != NULL)
			call map_close (bkin)
		    call imunmap (pat)
		    call imunmap (out)
		    call imunmap (in)
		} then {
		    if (bkpat != NULL)
			call map_close (bkpat)
		    if (bkin != NULL)
			call map_close (bkin)
		    if (pat != NULL)
			call imunmap (pat)
		    if (out != NULL)
			call imunmap (out)
		    if (in != NULL)
			call imunmap (in)
		    call imtclose (meflist2)
		    call imtclose (meflist1)
		    call sfree (sp)
		    call erract (EA_ERROR)
		}
	    }
	}

	call imtclose (meflist2)
	call imtclose (meflist1)
	call sfree (sp)
end


# PATFIT2: Solve <((A - B) -  s (P - Q)) (W - V)> = 0 
# There are three modes: INIT, ACCUM, and RESULT.  The accumulation is
# to compute a scale across multiple images.  A scale is returned for
# each image in ACCUM mode and the grand result is returned in RESULT mode.
# This is optimized for special cases of no bad pixel mask, no background
# image, and a background image of 0.


procedure patfit2 (in, pat, wt, ncblk, nlblk, bkin, bkpat, bkwt, pm, pmpat,
	mode, scale, stat, logfd)

pointer	in				#I Input image (imio)
pointer	pat				#I Pattern image (imio)
pointer	wt				#I Weight image (mapio)
int	ncblk, nlblk			#I Weight blocking factors
pointer	bkin				#I Input background image (mapio)
pointer	bkpat				#I Pattern background image (mapio)
pointer	bkwt				#I Weight background image (mapio)
pointer	pm				#I Pixel mask (imio)
pointer	pmpat				#I Pattern pixel mask (imio)
int	mode				#I Mode
double	scale				#O Scale
double	stat[SZ_SUM]			#O Statistics
int	logfd[ARB]			#I Log file descriptors

int	i, j, n, nc, nl, nc1, nl1, nc2, nl2, nlbuf, nblk, op
real	rval
double	a, b, p, q, w, v, sum[SZ_SUM], sumtot[SZ_SUM]
pointer	inbuf, patbuf, wtbuf, bkinbuf, bkpatbuf, bkwtbuf, pmbuf, pmpbuf
pointer	sp, bufs[6], sumbuf[6]

bool	fp_equald()
pointer	map_glr()

#pointer	out, immap(), impl2i()

begin
	# Initialize
	if (mode == INIT) {
	    call aclrd (sumtot, SZ_SUM)
	    return

	# Compute result from accumulation
	} else if (mode == RESULT) {
	    if (sumtot[N] > 0D0)
		call adivkd (sumtot, sumtot[N], sum, NMEAN)
	    a = sum[AW] - sum[BW] - sum[AV] + sum[BV]
	    b = sum[PW] - sum[QW] - sum[PV] + sum[QV]
	    if (fp_equald (b, 0D0))
		scale = 0.
	    else
		scale = a / b
	    call amovd (sum, stat, SZ_SUM)
	    #stat[W] = stat[W] / nblk
	    #stat[V] = stat[V] / nblk
	    return
	}

	call smark (sp)

	# Initialize input, pattern, and weight I/O.
	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	nc2 = min (nc, ncblk) / 2
	nc1 = nc2 - min (nc, ncblk) + 1
	nc = nc - (nc2 - nc1)
	nl2 = min (nl, nlblk) / 2
	nl1 = nl2 - min (nl, nlblk) + 1
	nlbuf = nl2 - nl1 + 1
	nblk = (nc2 - nc1 + 1) * (nl2 - nl1 + 1)

	call pblk_initr (in, NO, bufs[1], sumbuf[1], nc,
	    nc1, nc2, nlbuf)
	call pblk_initr (pat, NO, bufs[2], sumbuf[2], nc,
	    nc1, nc2, nlbuf)
	wtbuf = NULL
	if (wt != pat) {
	    iferr (call map_getr (wt, "constant", rval))
		call pblk_initr (wt, YES, bufs[3], sumbuf[3], nc,
		    nc1, nc2, nlbuf)
	}
	if (pm != NULL)
	    call pblk_initi (pm, NO, bufs[4], sumbuf[4], nc,
		nc1, nc2, nlbuf)
	if (pmpat != NULL)
	    call pblk_initi (pmpat, NO, bufs[5], sumbuf[5], nc,
		nc1, nc2, nlbuf)
	if (pm != NULL && pmpat != NULL)
	    call salloc (sumbuf[6], nc, TY_INT)

	call aclrd (sum, SZ_SUM)
	n = 0
	pmbuf = NULL; pmpbuf = NULL
	bkinbuf = NULL; bkpatbuf = NULL; bkwtbuf = NULL
	do i = 1-nl1, nl-nl2 {
	    # Get line of input data.  The pattern and weight data may be the
	    # same.  If the weight is a constant the weight buffer is NULL.

	    j = i + nl2
	    call patblkr (in, j, NO, inbuf, bufs[1], sumbuf[1], nc,
	        nc1, nc2, nlbuf)
	    call patblkr (pat, j, NO, patbuf, bufs[2], sumbuf[2], nc,
	        nc1, nc2, nlbuf)
	    if (wt == pat)
	        wtbuf = patbuf
	    else if (wtbuf != NULL)
		call patblkr (wt, j, YES, wtbuf, bufs[3], sumbuf[3], nc,
		    nc1, nc2, nlbuf)
	    if (pm != NULL && pmpat != NULL) {
		call patblki (pm, j, NO, pmbuf, bufs[4], sumbuf[4], nc,
		    nc1, nc2, nlbuf)
		call patblki (pmpat, j, NO, pmpbuf, bufs[5], sumbuf[5], nc,
		    nc1, nc2, nlbuf)
		call aaddi (Memi[pmbuf], Memi[pmpbuf], Memi[sumbuf[6]], nc)
		pmbuf = sumbuf[6]
	    } else if (pm != NULL)
		call patblki (pm, j, NO, pmbuf, bufs[4], sumbuf[4], nc,
		    nc1, nc2, nlbuf)
	    else if (pmpat != NULL)
		call patblki (pmpat, j, NO, pmbuf, bufs[5], sumbuf[5], nc,
		    nc1, nc2, nlbuf)

	    # Debug code for outputing a block averaged image.
	    #if (out == NULL)
		#out = immap ("patfit.pl", NEW_COPY, pm)
	    #call amovr (Memi[pmbuf], Memi[impl2i(out,i)-nc1], nc)
	    #if (j == nl)
		#call imunmap (out)

	    # The backgrounds may be absent, be a constant, or be a map.
	    # When there is no background or the background is zero the
	    # buffer pointers are NULL.  Adjust for block summing since
	    # the backgrounds are not block summed.

		if (bkin != NULL) {
		# Optimize case of constant value of 0.
	        iferr (call map_getr (bkin, "constant", rval))
		    bkinbuf = map_glr (bkin, i, READ_ONLY) - nc1
		else {
		    if (rval != 0.)
			bkinbuf = map_glr (bkin, i, READ_ONLY) - nc1
		}
		if (bkinbuf != NULL && nblk > 1)
		    call amulkr (Memr[bkinbuf], real(nblk), Memr[bkinbuf], nc)
	    }
	    if (bkpat != NULL) {
		# Optimize case of constant value of 0.
	        iferr (call map_getr (bkpat, "constant", rval))
		    bkpatbuf = map_glr (bkpat, i, READ_ONLY) - nc1
		else {
		    if (rval != 0.)
			bkpatbuf = map_glr (bkpat, i, READ_ONLY) - nc1
		}
		if (bkpatbuf != NULL && nblk > 1)
		    call amulkr (Memr[bkpatbuf], real(nblk), Memr[bkpatbuf], nc)
	    }
	    if (wtbuf != NULL) {
		if (bkwt == bkpat)
		    bkwtbuf = bkpatbuf
		else if (bkwt != NULL) {
		    # Optimize case of constant value of 0.
		    iferr (call map_getr (bkwt, "constant", rval))
			bkwtbuf = map_glr (bkwt, i, READ_ONLY) - nc1
		    else {
			if (rval != 0.)
			    bkwtbuf = map_glr (bkwt, i, READ_ONLY) - nc1
		    }
		    if (bkwtbuf != NULL && nblk > 1)
			call amulkr (Memr[bkwtbuf],real(nblk),Memr[bkwtbuf],nc)
		}
	    }

	    # Optimize accumulations.
	    op = 0
	    if (bkinbuf != NULL)
	        op = op + 1
	    if (bkpatbuf != NULL)
	        op = op + 2
	    if (bkwtbuf != NULL)
	        op = op + 4
	    if (wtbuf != NULL)
	        op = op + 8

	    if (pmbuf != NULL) {
		switch (op) {
		case 0:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
		    }
		case 1:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
		    }
		case 2:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
		    }
		case 3:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
		    }
		case 8:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			w = Memr[wtbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[W] = sum[W] + w
			sum[AW] = sum[AW] + a*w
			sum[PW] = sum[PW] + p*w
		    }
		case 9:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			w = Memr[wtbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[W] = sum[W] + w
			sum[AW] = sum[AW] + a*w
			sum[BW] = sum[BW] + b*w
			sum[PW] = sum[PW] + p*w
		    }
		case 10:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			w = Memr[wtbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
			sum[W] = sum[W] + w
			sum[AW] = sum[AW] + a*w
			sum[PW] = sum[PW] + p*w
			sum[QW] = sum[QW] + q*w
		    }
		case 11:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			w = Memr[wtbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
			sum[W] = sum[W] + w
			sum[AW] = sum[AW] + a*w
			sum[BW] = sum[BW] + b*w
			sum[PW] = sum[PW] + p*w
			sum[QW] = sum[QW] + q*w
		    }
		case 12:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			w = Memr[wtbuf+j]
			v = Memr[bkwtbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[W] = sum[W] + w
			sum[V] = sum[V] + v
			sum[AW] = sum[AW] + a*w
			sum[PW] = sum[PW] + p*w
			sum[AV] = sum[AV] + a*v
			sum[PV] = sum[PV] + p*v
		    }
		case 13:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			w = Memr[wtbuf+j]
			v = Memr[bkwtbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[W] = sum[W] + w
			sum[V] = sum[V] + v
			sum[AW] = sum[AW] + a*w
			sum[BW] = sum[BW] + b*w
			sum[PW] = sum[PW] + p*w
			sum[AV] = sum[AV] + a*v
			sum[BV] = sum[BV] + b*v
			sum[PV] = sum[PV] + p*v
		    }
		case 14:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			w = Memr[wtbuf+j]
			v = Memr[bkwtbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
			sum[W] = sum[W] + w
			sum[V] = sum[V] + v
			sum[AW] = sum[AW] + a*w
			sum[PW] = sum[PW] + p*w
			sum[QW] = sum[QW] + q*w
			sum[AV] = sum[AV] + a*v
			sum[PV] = sum[PV] + p*v
			sum[QV] = sum[QV] + q*v
		    }
		case 15:
		    do j = 0, nc-1 {
			if (Memi[pmbuf+j] != 0)
			    next
			n = n + nblk
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			w = Memr[wtbuf+j]
			v = Memr[bkwtbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
			sum[W] = sum[W] + w
			sum[V] = sum[V] + v
			sum[AW] = sum[AW] + a*w
			sum[BW] = sum[BW] + b*w
			sum[PW] = sum[PW] + p*w
			sum[QW] = sum[QW] + q*w
			sum[AV] = sum[AV] + a*v
			sum[BV] = sum[BV] + b*v
			sum[PV] = sum[PV] + p*v
			sum[QV] = sum[QV] + q*v
		    }
		}
	    } else {
		switch (op) {
		case 0:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
		    }
		case 1:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
		    }
		case 2:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
		    }
		case 3:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
		    }
		case 8:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			w = Memr[wtbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[W] = sum[W] + w
			sum[AW] = sum[AW] + a*w
			sum[PW] = sum[PW] + p*w
		    }
		case 9:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			w = Memr[wtbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[W] = sum[W] + w
			sum[AW] = sum[AW] + a*w
			sum[BW] = sum[BW] + b*w
			sum[PW] = sum[PW] + p*w
		    }
		case 10:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			w = Memr[wtbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
			sum[W] = sum[W] + w
			sum[AW] = sum[AW] + a*w
			sum[PW] = sum[PW] + p*w
			sum[QW] = sum[QW] + q*w
		    }
		case 11:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			w = Memr[wtbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
			sum[W] = sum[W] + w
			sum[AW] = sum[AW] + a*w
			sum[BW] = sum[BW] + b*w
			sum[PW] = sum[PW] + p*w
			sum[QW] = sum[QW] + q*w
		    }
		case 12:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			w = Memr[wtbuf+j]
			v = Memr[bkwtbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[W] = sum[W] + w
			sum[V] = sum[V] + v
			sum[AW] = sum[AW] + a*w
			sum[PW] = sum[PW] + p*w
			sum[AV] = sum[AV] + a*v
			sum[PV] = sum[PV] + p*v
		    }
		case 13:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			w = Memr[wtbuf+j]
			v = Memr[bkwtbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[W] = sum[W] + w
			sum[V] = sum[V] + v
			sum[AW] = sum[AW] + a*w
			sum[BW] = sum[BW] + b*w
			sum[PW] = sum[PW] + p*w
			sum[AV] = sum[AV] + a*v
			sum[BV] = sum[BV] + b*v
			sum[PV] = sum[PV] + p*v
		    }
		case 14:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			w = Memr[wtbuf+j]
			v = Memr[bkwtbuf+j]
			sum[A] = sum[A] + a
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
			sum[W] = sum[W] + w
			sum[V] = sum[V] + v
			sum[AW] = sum[AW] + a*w
			sum[PW] = sum[PW] + p*w
			sum[QW] = sum[QW] + q*w
			sum[AV] = sum[AV] + a*v
			sum[PV] = sum[PV] + p*v
			sum[QV] = sum[QV] + q*v
		    }
		case 15:
		    do j = 0, nc-1 {
			a = Memr[inbuf+j]
			b = Memr[bkinbuf+j]
			p = Memr[patbuf+j]
			q = Memr[bkpatbuf+j]
			w = Memr[wtbuf+j]
			v = Memr[bkwtbuf+j]
			sum[A] = sum[A] + a
			sum[B] = sum[B] + b
			sum[P] = sum[P] + p
			sum[Q] = sum[Q] + q
			sum[W] = sum[W] + w
			sum[V] = sum[V] + v
			sum[AW] = sum[AW] + a*w
			sum[BW] = sum[BW] + b*w
			sum[PW] = sum[PW] + p*w
			sum[QW] = sum[QW] + q*w
			sum[AV] = sum[AV] + a*v
			sum[BV] = sum[BV] + b*v
			sum[PV] = sum[PV] + p*v
			sum[QV] = sum[QV] + q*v
		    }
		}
		n = n + nc * nblk
	    }
	}
	sum[N] = n
	n = n / nblk

	# Compute background means if no background was specified.
	op = 0
	if (bkin == NULL)
	    op = op + 1
	if (bkpat == NULL)
	    op = op + 2
	if (bkwt == NULL)
	    op = op + 4
	if (wtbuf != NULL)
	    op = op + 8

	# If no background is specified use the mean.
	switch (op) {
	case 0:
	    sum[W] = sum[N]
	    sum[AW] = sum[A]
	    sum[BW] = sum[B]
	    sum[PW] = sum[P]
	    sum[QW] = sum[Q]
	case 1:
	    sum[W] = sum[N]
	    sum[B] = sum[A]
	    sum[AW] = sum[A]
	    sum[BW] = sum[A]
	    sum[PW] = sum[P]
	    sum[QW] = sum[Q]
	case 2:
	    sum[W] = sum[N]
	    sum[Q] = sum[P]
	    sum[AW] = sum[A]
	    sum[BW] = sum[B]
	    sum[PW] = sum[P]
	    sum[QW] = sum[P]
	case 3:
	    sum[W] = sum[N]
	    sum[B] = sum[A]
	    sum[Q] = sum[P]
	    sum[AW] = sum[A]
	    sum[BW] = sum[A]
	    sum[PW] = sum[P]
	    sum[QW] = sum[P]
	case 4:
	    sum[W] = sum[N]
	    sum[AW] = sum[A]
	    sum[BW] = sum[B]
	    sum[PW] = sum[P]
	    sum[QW] = sum[Q]
	case 5:
	    sum[W] = sum[N]
	    sum[B] = sum[A]
	    sum[AW] = sum[A]
	    sum[BW] = sum[A]
	    sum[PW] = sum[P]
	    sum[QW] = sum[Q]
	case 6:
	    sum[W] = sum[N]
	    sum[Q] = sum[P]
	    sum[AW] = sum[A]
	    sum[BW] = sum[B]
	    sum[PW] = sum[P]
	    sum[QW] = sum[P]
	case 7:
	    sum[W] = sum[N]
	    sum[B] = sum[A]
	    sum[Q] = sum[P]
	    sum[AW] = sum[A]
	    sum[BW] = sum[A]
	    sum[PW] = sum[P]
	    sum[QW] = sum[P]
	case 9:
	    sum[B] = sum[A]
	    sum[BW] = sum[A] / n * sum[W]
	    sum[BV] = sum[A] / n * sum[W]
	case 10:
	    sum[Q] = sum[P]
	    sum[QW] = sum[P] / n * sum[W]
	    sum[QV] = sum[P] / n * sum[V]
	case 11:
	    sum[B] = sum[A]
	    sum[BW] = sum[A] / n * sum[W]
	    sum[BV] = sum[A] / n * sum[V]
	    sum[Q] = sum[P]
	    sum[QW] = sum[P] / n * sum[W]
	    sum[QV] = sum[P] / n * sum[V]
	case 12:
	    sum[V] = sum[W]
	    sum[AV] = sum[A] / n * sum[W]
	    sum[BV] = sum[B] / n * sum[W]
	    sum[PV] = sum[P] / n * sum[W]
	    sum[QV] = sum[Q] / n * sum[W]
	case 13:
	    sum[B] = sum[A]
	    sum[BW] = sum[A] / n * sum[W]
	    sum[BV] = sum[A] / n * sum[W]
	    sum[V] = sum[W]
	    sum[AV] = sum[A] / n * sum[W]
	    sum[BV] = sum[B] / n * sum[W]
	    sum[PV] = sum[P] / n * sum[W]
	    sum[QV] = sum[Q] / n * sum[W]
	case 14:
	    sum[Q] = sum[P]
	    sum[QW] = sum[P] / n * sum[W]
	    sum[QV] = sum[P] / n * sum[W]
	    sum[V] = sum[W]
	    sum[AV] = sum[A] / n * sum[W]
	    sum[BV] = sum[B] / n * sum[W]
	    sum[PV] = sum[P] / n * sum[W]
	    sum[QV] = sum[P] / n * sum[W]
	case 15:
	    sum[B] = sum[A]
	    sum[BW] = sum[A] / n * sum[W]
	    sum[BV] = sum[A] / n * sum[W]
	    sum[Q] = sum[P]
	    sum[QW] = sum[P] / n * sum[W]
	    sum[QV] = sum[P] / n * sum[W]
	    sum[V] = sum[W]
	    sum[AV] = sum[A] / n * sum[W]
	    sum[BV] = sum[A] / n * sum[W]
	    sum[PV] = sum[P] / n * sum[W]
	    sum[QV] = sum[P] / n * sum[W]
	}

	# Accumulate grand total.
	call aaddd (sum, sumtot, sumtot, SZ_SUM)

	# Compute results for this image.
	if (sum[N] > 0D0)
	    call adivkd (sum, sum[N], sum, NMEAN)

	do i = 1, 2 {
	    if (logfd[i] != STDERR)
	        next
	    call eprintf ("N =%d\n")
	    call pargd (sum[N])
	    call eprintf (
	        "A =%8.5g  B =%8.5g  P =%8.5g  Q =%8.5g  W =%8.5g  V =%8.5g\n")
	    call pargd (sum[A])
	    call pargd (sum[B])
	    call pargd (sum[P])
	    call pargd (sum[Q])
	    call pargd (sum[W])
	    call pargd (sum[V])
	    call eprintf ("AW=%8.5g  BW=%8.5g  PW=%8.5g  QW=%8.5g\n")
	    call pargd (sum[AW])
	    call pargd (sum[BW])
	    call pargd (sum[PW])
	    call pargd (sum[QW])
	    call eprintf ("AV=%8.5g  BV=%8.5g  PV=%8.5g  QV=%8.5g\n")
	    call pargd (sum[AV])
	    call pargd (sum[BV])
	    call pargd (sum[PV])
	    call pargd (sum[QV])
	}

	a = sum[AW] - sum[BW] - sum[AV] + sum[BV]
	b = sum[PW] - sum[QW] - sum[PV] + sum[QV]
	if (fp_equald (b, 0D0))
	    scale = 0.
	else
	    scale = a / b
	call amovd (sum, stat, SZ_SUM)
	#stat[W] = stat[W] / nblk
	#stat[V] = stat[V] / nblk

	call sfree (sp)
end
