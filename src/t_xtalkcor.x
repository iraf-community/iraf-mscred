include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<time.h>


# XTALKCOR -- Apply a crosstalk correction.
# The output may be the crosstalk corrected data, masks flagging affected
# data or both.  The input is a list of input MEF exposures and a list
# of crosstalk coefficient files.

procedure t_xtalkcor ()

int	inlist			# List of input exposures
int	outlist			# List of output exposures
int	bpmlist			# List of bad pixel masks
pointer	section			# Section value or keyword
int	xtfiles			# List of crosstalk files
bool	split			# Split input?
real	bpmthresh		# BPM threshold
int	pixeltype		# Output pixel type
bool	noproc			# List operations only?
pointer	fextn			# File extension
int	verbose			# Verbose output?
int	logfd			# Logfile
int	bufsize			# I/O buffer size in Mb

int	i, j, k, nstps, nimages, pixtypes[7], ierr
pointer	stps, stp, sym, ins, outs, bpms
pointer	sp, infile, infile1, outfile, bpmfile, xtfile, xtfile1
pointer	pat, tempfile, str, err, ptr

bool	clgetb(), streq()
int	clpopnu(), clplen(), clgfil()
int	imtopenp(), imtlen(), imtgetim()
int	btoi(), open(), strdic(), strmatch(), access(), imaccess(), ctowrd()
int	errget(), strlen(), nowhite()
real	clgetr()
pointer	sthead(), stnext(), stname(), immap()

errchk	fcopy, open, immap, imdelete, imrename, ctcopy, ctread, ctalk

data	pixtypes /0, TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE/

begin
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (infile1, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (bpmfile, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (xtfile, SZ_FNAME, TY_CHAR)
	call salloc (xtfile1, SZ_FNAME, TY_CHAR)
	call salloc (fextn, SZ_FNAME, TY_CHAR)
	call salloc (pat, SZ_FNAME, TY_CHAR)
	call salloc (tempfile, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (err, SZ_LINE, TY_CHAR)

	# Get the task parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	bpmlist = imtopenp ("bpmasks")
	call clgstr ("section", Memc[section], SZ_FNAME)
	xtfiles = clpopnu ("xtalkfiles")
	split = clgetb ("split")
	bpmthresh = clgetr ("bpmthreshold")
	call clgstr ("fextn", Memc[fextn+1], SZ_FNAME)
	noproc = clgetb ("noproc")

	call clgstr ("pixeltype", Memc[str], SZ_FNAME)
	call clgstr ("logfile", Memc[infile], SZ_FNAME)
	verbose = btoi (clgetb ("verbose"))
	bufsize = max (1024., 1E6 * clgetr ("im_bufsize"))

	# Open logfile if requested.
	logfd = NULL
	if (nowhite (Memc[infile], Memc[infile], SZ_FNAME) != 0)
	    logfd = open (Memc[infile], APPEND, TEXT_FILE)

	# Check requested output pixel type.
	i = 1
	if (ctowrd (Memc[str], i, Memc[pat], SZ_FNAME) == 0)
	    call strcpy ("real", Memc[pat], SZ_FNAME)
	pixeltype = pixtypes[1+strdic (Memc[pat], Memc[str], SZ_LINE,
	    "|ushort|short|integer|long|real|double|"))
	if (pixeltype == 0) {
	    call sprintf (Memc[str], SZ_LINE,
		"xtalkcor: Unknown pixel type (%s)")
		call pargstr (Memc[pat])
	    call error (2, Memc[str])
	}

	# Set extension pattern.
	Memc[fextn] = '.'
	call sprintf (Memc[pat], SZ_FNAME, "%s$")
	    call pargstr (Memc[fextn])

	# Check the input lists match up.
	# If splitting and the output list is longer than the input then
	# the output list is assumed to contain all the output files.

	i = imtlen (inlist)
	j = imtlen (outlist)

	if (j > 0 && j != i)
	    call error (2, "xtalkcor: input and output lists don't match")
	j = imtlen (bpmlist)
	if (j > 0 && j != i)
	    call error (2, "xtalkcor: input and bad pixel lists don't match")
	j = clplen (xtfiles)
	if (j == 0)
	    call error (2, "xtalkcor: no crosstalk file specified")
	if (j > 1 && j != i)
	    call error (2,
		"xtalkcor: input and crosstalk file lists don't match")
	i = imtlen (outlist)
	j = imtlen (bpmlist)
	if (i == 0 && j == 0)
	    call error (2, "xtalkcor: no output images or masks specified")

	call mktemp ("tmp", Memc[tempfile], SZ_FNAME)

	# Loop through each input file and correct it to the output file.
	while (imtgetim (inlist, Memc[infile], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[outfile], SZ_FNAME) == EOF)
		Memc[outfile] = EOS
	    if (imtgetim (bpmlist, Memc[bpmfile], SZ_FNAME) == EOF)
		Memc[bpmfile] = EOS
	    if (clgfil (xtfiles, Memc[xtfile], SZ_FNAME) == EOF)
		;
	    if (strmatch (Memc[infile], Memc[pat]) == 0)
		call strcat (Memc[fextn], Memc[infile], SZ_FNAME)
	    if (Memc[outfile] != EOS) {
		if (strmatch (Memc[outfile], Memc[pat]) == 0)
		    call strcat (Memc[fextn], Memc[outfile], SZ_FNAME)
	    }
	    i = strlen (Memc[infile]) - strlen (Memc[fextn])
	    call strcpy (Memc[infile], Memc[infile1], i)
	    if (split) {
		i = strlen (Memc[outfile]) - strlen (Memc[fextn])
		call strcpy (Memc[outfile], Memc[tempfile], i)
	    }

	    ins = NULL; outs = NULL; bpms = NULL; stps = NULL
	    iferr {
		# Read the crosstalk file.
		call ctread (Memc[xtfile], Memc[infile], Memc[xtfile1],
		    stps, nimages, nstps)

		# Check for existing output.
		if (!split) {
		    if (Memc[outfile]!=EOS && access(Memc[outfile],0,0)==YES) {
			call sprintf (Memc[str], SZ_LINE,
			    "xtalkcor: Output already exists (%s)")
			    call pargstr (Memc[outfile])
			call error (2, Memc[str])
		    }
		}

		# Loop on the groups.
		do j = nstps, 1, -1 {
		    stp = Memi[stps+j-1]

		    # Open input and check for missing extensions, a previous
		    # correction.
		    k = 0; ierr = 0
		    call calloc (ins, nimages, TY_POINTER)
		    for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
			i = Memi[sym] - 1
			call sprintf (Memc[str], SZ_LINE, "%s[%s]")
			    call pargstr (Memc[infile])
			    call pargstr (Memc[stname(stp,sym)])
			iferr (ptr = immap (Memc[str], READ_ONLY, 0)) {
			    ierr = errget (Memc[err], SZ_LINE)
			    if (k > 0)
			        break
			    next
			}
			k = k + 1
			Memi[ins+i] = ptr
			if (ierr > 0)
			    break
			call imseti (ptr, IM_BUFSIZE, bufsize)
			ifnoerr (call imgstr (ptr, "XTALKCOR", Memc[str],
			    SZ_LINE)) {
			    call sprintf (Memc[str], SZ_LINE,
				"xtalkcor: Correction already done (%s)")
				call pargstr (Memc[infile1])
			    call error (1, Memc[str])
			}
		    }
		    if (ierr > 0 ) {
		        if (k > 0)
			    iferr (call error (ierr, Memc[err]))
			        call erract (EA_WARN)
		        call mfree (ins, TY_POINTER)
		        next
		    }

		    if (noproc) {
			call sprintf (Memc[str], SZ_LINE,
			    "%s\n  [TO BE DONE] Crosstalk file is %s\n")
			    call pargstr (Memc[infile1])
			    call pargstr (Memc[xtfile1])
			call printf (Memc[str])
			call error (1, Memc[str])
		    }

		    # Setup the output.
		    call calloc (outs, nimages, TY_POINTER)
		    if (Memc[outfile] != EOS) {
			for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
			    do i = 1, nimages {
				if (i != Memi[sym] && Memr[P2R(sym+i)] != 0.)
				    break
			    }
			    if (i > nimages)
				next
			    i = Memi[sym] - 1
			    call sprintf (Memc[str], SZ_LINE, "%s_%s")
				call pargstr (Memc[tempfile])
				call pargstr (Memc[stname(stp,sym)])
			    if (imaccess (Memc[str], 0) == YES) {
				call sprintf (Memc[tempfile], SZ_LINE,
				    "xtalkcor: Output already exists (%s)")
				    call pargstr (Memc[str])
				call error (2, Memc[tempfile])
			    }
			    ptr = immap (Memc[str], NEW_COPY, Memi[ins+i])
			    Memi[outs+i] = ptr
			    call imseti (ptr, IM_BUFSIZE, bufsize)
			    IM_PIXTYPE(ptr) = pixeltype
			}
		    }

		    # Setup the bad pixel masks.
		    call calloc (bpms, nimages, TY_POINTER)
		    if (Memc[bpmfile] != EOS) {
			for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
			    do i = 1, nimages {
				if (i != Memi[sym] && Memr[P2R(sym+i)] != 0.)
				    break
			    }
			    if (i > nimages)
				next
			    i = Memi[sym] - 1
			    if (Memi[ins+i] == NULL)
				next
			    call sprintf (Memc[str], SZ_LINE, "%s_%s.pl")
				call pargstr (Memc[bpmfile])
				call pargstr (Memc[stname(stp,sym)])
			    if (imaccess (Memc[str], 0) == YES) {
				call sprintf (Memc[tempfile], SZ_LINE,
				    "xtalkcor: Output already exists (%s)")
				    call pargstr (Memc[str])
				call error (2, Memc[tempfile])
			    }
			    ptr = immap (Memc[str], NEW_COPY, Memi[ins+i])
			    Memi[bpms+i] = ptr
			}
		    }

		    # Do the crosstalk corrections.
		    call ctalk (stp, Memi[ins], Memi[outs], Memi[bpms], nimages,
			Memc[infile1], Memc[xtfile1], Memc[section],
			bpmthresh, logfd, verbose)

		    # Close images.
		    do i = 1, nimages
			if (Memi[outs+i-1] != NULL)
			    call imunmap (Memi[outs+i-1])
		    call mfree (outs, TY_POINTER)
		    do i = 1, nimages
			if (Memi[bpms+i-1] != NULL)
			    call imunmap (Memi[bpms+i-1])
		    call mfree (bpms, TY_POINTER)
		    do i = 1, nimages
			if (Memi[ins+i-1] != NULL)
			    call imunmap (Memi[ins+i-1])
		    call mfree (ins, TY_POINTER)
		}

		# Join single images to MEF if not splitting.
		if (!split && Memc[outfile] != EOS) {
		    if (streq (Memc[outfile], Memc[infile]))
			call imdelete (Memc[infile])
		    call ctjoin (Memc[infile], Memc[outfile], Memc[tempfile],
			bufsize, Memc[xtfile1])

		# Copy the images without a correction if splitting.
		} else if (split && Memc[outfile] != EOS)
		    call ctsplit (Memc[infile], Memc[outfile], Memc[tempfile],
		        bufsize, Memc[xtfile1], outlist)
	    } then {
		ierr = errget (Memc[err], SZ_LINE)
		if (ierr > 1) {
		    do i = 1, nimages {
			if (outs != NULL) {
			    ptr = Memi[outs+i-1]
			    if (ptr != NULL) {
				call imstats (ptr, IM_IMAGENAME,
				    Memc[str], SZ_LINE)
				call imunmap (Memi[outs+i-1])
				iferr (call imdelete (Memc[str]))
				    ;
			    }
			}
			if (bpms != NULL) {
			    ptr = Memi[bpms+i-1]
			    if (ptr != NULL) {
				call imstats (ptr, IM_IMAGENAME,
				    Memc[str], SZ_LINE)
				call imunmap (Memi[bpms+i-1])
				iferr (call imdelete (Memc[str]))
				    ;
			    }
			}
		    }
		    iferr (call error (ierr, Memc[err]))
			call erract (EA_WARN)
		}
	    }

	    # Finish up and clean up after an error.
	    # If the correction was successful the temporary files will
	    # have been joined to create the output so the deletes will
	    # be no-ops.  However if there is an error we want to get rid
	    # of the temporary files.
	    if (outs != NULL) {
		do i = 1, nimages {
		    if (Memi[outs+i-1] != 0)
			call imunmap (Memi[outs+i-1])
		}
		call mfree (outs, TY_POINTER)
	    }
	    if (bpms != NULL) {
		do i = 1, nimages
		    if (Memi[bpms+i-1] != NULL)
			call imunmap (Memi[bpms+i-1])
	    }
	    if (ins != NULL) {
		do i = 1, nimages {
		    if (Memi[ins+i-1] != 0)
			call imunmap (Memi[ins+i-1])
		}
		call mfree (ins, TY_POINTER)
	    }
	    if (stps != NULL) {
		do i = 1, nstps {
		    stp = Memi[stps+i-1]
		    if (stp != NULL) {
			if (!split) {
			    for (sym=sthead(stp); sym!=NULL;
				sym=stnext(stp,sym)) {
				call sprintf (Memc[str], SZ_LINE, "%s_%s")
				    call pargstr (Memc[tempfile])
				    call pargstr (Memc[stname(stp,sym)])
				iferr (call imdelete (Memc[str]))
				    ;
			    }
			}
			call stclose (stp)
		    }
		}
		call mfree (stps, TY_POINTER)
	    }
	}

	# Close the lists.
	if (logfd != NULL)
	    call close (logfd)
	call clpcls (xtfiles)
	call imtclose (outlist)
	call imtclose (inlist)
	call sfree (sp)
end


# CTREAD -- Read the crosstalk file and setup a symbol table.
# The symbol table is keyed by the names in the first column.
# Images are assigned integer IDs and the array of coefficients
# are ordered by the IDs.  The symbol data structure is the integer
# ID for the image followed by an array of real coefficients indexed
# by the image IDs.  The coefficient for the image itself is set to 1
# and any source images which do not affect the target image have
# coefficients of 0.

procedure ctread (xtfile, infile, xtfile1, stps, nimages, nstps)

char	xtfile[ARB]		#I Crosstalk file reference
char	infile[ARB]		#I Input file
char	xtfile1[SZ_FNAME]	#O Crosstalk file used
pointer	stps			#O Pointer to array of symbol tables
int	nimages			#O Number of images
int	nstps			#I Number of groups

int	i, j, k, xt, nalloc, symlen
real	coeff1
pointer	im, stp, stp1, sym1, sym2, sym3, extname
pointer	sp, ext1, ext2, extnames, syms

int	open(), errget(), fscan(), nscan()
pointer	immap(), stopen(), stfind(), stenter(), sthead(), stnext(), stname()

errchk	immap, open, stopen

begin
	call smark (sp)
	call salloc (ext1, SZ_FNAME, TY_CHAR)
	call salloc (ext2, SZ_FNAME, TY_CHAR)

	# Open the file.  If an error occurs the routine will return.
	if (xtfile[1] != '!')
	    call strcpy (xtfile, xtfile1, SZ_FNAME)
	else {
	    call sprintf (Memc[ext1], SZ_FNAME, "%s[1]")
		call pargstr (infile)
	    im = immap (Memc[ext1], READ_ONLY, 0)
	    iferr (call imgstr (im, xtfile[2], xtfile1, SZ_FNAME)) {
		xt = errget (Memc[ext2], SZ_FNAME)
		call imunmap (im)
		call error (xt, Memc[ext2])
	    }
	    call imunmap (im)
	}
	xt = open (xtfile1, READ_ONLY, TEXT_FILE)

	# Read through the crosstalk file and create a symbol table.
	# We don't know in advance how many images might be referenced
	# so we start with up to 32 and then redo the reading if more
	# are found.

	for (nalloc=32;; nalloc=2*nalloc) {
	    symlen = 1 + nalloc
	    stp = stopen (xtfile1, nalloc, symlen, 1024)
	    nimages = 0
	    while (fscan (xt) != EOF) {
		call gargwrd (Memc[ext1], SZ_FNAME)
		call gargwrd (Memc[ext2], SZ_FNAME)
		call gargr (coeff1)
		if (nscan() < 3)
		    next
		if (Memc[ext1] == '#')
		    next

		# Don't use aclrr below because of the way we address reals.
		sym1 = stfind (stp, Memc[ext1])
		if (sym1 == NULL) {
		    sym1 = stenter (stp, Memc[ext1], symlen)
		    nimages = nimages + 1
		    Memi[sym1] = nimages
		    do i = 1, 1+nalloc
		        Memr[P2R(sym1+i)] = 0.
		    #call aclrr (Memr[P2R(sym1+1)], nalloc)
		}
		sym2 = stfind (stp, Memc[ext2])
		if (sym2 == NULL) {
		    sym2 = stenter (stp, Memc[ext2], symlen)
		    nimages = nimages + 1
		    Memi[sym2] = nimages
		    do i = 1, 1+nalloc
		        Memr[P2R(sym2+i)] = 0.
		    #call aclrr (Memr[P2R(sym2+1)], nalloc)

		    # We must get the pointer again because adding a new
		    # symbol may reallocate memory and change the pointer. 
		    sym1 = stfind (stp, Memc[ext1])
		}
		if (nimages > nalloc)
		    break
		Memr[P2R(sym1+Memi[sym1])] = 1.
		Memr[P2R(sym1+Memi[sym2])] = coeff1
		Memr[P2R(sym2+Memi[sym2])] = 1.
	    }

	    if (nimages <= nalloc)
		break

	    call stclose (stp)
	    call seek (xt, BOF)
	}
	call close (xt)

	call salloc (extnames, nimages, TY_POINTER)
	call salloc (syms, nimages, TY_POINTER)
	for (sym1 = sthead (stp); sym1 != NULL; sym1 = stnext (stp, sym1)) {
	    i = Memi[sym1]
	    Memi[extnames+i-1] = stname(stp,sym1)
	    Memi[syms+i-1] = sym1
	}

	nstps = 0
	call calloc (stps, nimages, TY_POINTER)
	do i = nimages, 1, -1 {
	    sym1 = Memi[syms+i-1]
	    sym2 = NULL
	    sym3 = NULL
	    extname = Memi[extnames+i-1]
	    do j = 1, nstps {
		stp1 = Memi[stps+j-1]
		sym2 = stfind (stp1, Memc[extname])
		if (sym2 != NULL)
		    break
		do k = 1, nimages {
		    if (Memr[P2R(sym1+k)] == 0.)
			next
		    sym3 = stfind (stp1, Memc[Memi[extnames+k-1]])
		    if (sym3 != NULL)
			break
		}
		if (sym3 != NULL)
		    break
	    }
	    if (sym2 != NULL)
		next

	    if (j > nstps) {
		stp1 = stopen (xtfile1, nimages, symlen, 1024)
		Memi[stps+nstps] = stp1
		nstps = nstps + 1
	    }

	    sym2 = stenter (stp1, Memc[extname], symlen)
	    call amovi (Memi[sym1], Memi[sym2], symlen)
	    repeat {
		for (sym1=sthead(stp1); sym1!=NULL; sym1=stnext(stp1, sym1)) {
		    sym2 = NULL
		    do j = nimages, 1, -1 {
			if (Memr[P2R(sym1+j)] == 0.)
			    next
			extname = Memi[extnames+j-1]
			if (stfind (stp1, Memc[extname]) == NULL) {
			    sym2 = stenter (stp1, Memc[extname], symlen)
			    call amovi (Memi[Memi[syms+j-1]], Memi[sym2],
				symlen)
			}
		    }
		}
	    } until (sym2 == NULL)
	}

	call stclose (stp)
	call sfree (sp)
end
		

# CTALK -- Do the crosstalk correction from the set of input images to the
# set of output images.  The routine checks the sizes of the images match
# and sets up the relative flips of the readout direction.  The actual work
# is done in the type specific routines to minimize datatype conversions
# through IMIO.

procedure ctalk (stp, ins, outs, bpms, nimages, infile, xtfile, section,
	bpmthresh, logfd, verbose)

pointer	stp			#I Symbol table
pointer	ins[nimages]		#I Input image pointers
pointer	outs[nimages]		#I Output image pointers
pointer	bpms[nimages]		#I Output bad pixel masks
int	nimages			#I Number of images
char	infile[ARB]		#I Input file name
char	xtfile[ARB]		#I Crosstalk file name
char	section[ARB]		#I Section value or keyword
real	bpmthresh		#I Threshold for bpm flags
int	logfd			#I Logfile
int	verbose			#I Verbose?

bool	doout, dobpm, doxflip, doyflip
int	i, j, k, nc, nl, pixtype, fd, stropen()
long	clktime()
real	rval, atm, imgetr()
pointer	sp, inbufs, bufs, coeffs, x1, x2, y1, y2, str
pointer	sym1, sym2, sthead(), stnext(), stname()

errchk	ctalks, ctalkr, ccd_section

begin
	call smark (sp)
	call salloc (inbufs, 2*nimages, TY_POINTER)
	call salloc (bufs, nimages, TY_POINTER)
	call salloc (coeffs, nimages, TY_REAL)
	call salloc (x1, nimages, TY_INT)
	call salloc (x2, nimages, TY_INT)
	call salloc (y1, nimages, TY_INT)
	call salloc (y2, nimages, TY_INT)
	call salloc (str, 1024, TY_CHAR)

	# Select processing.
	doout = false
	dobpm = false
	do i = 1, nimages {
	    if (outs[i] != NULL)
		doout = true
	    if (bpms[i] != NULL)
		dobpm = true
	}
	if (!(doout || dobpm))
	    call error (2, "No output defined")

	# Check image dimensions and pixel types.
	nc = INDEFI
	do i = 1, nimages {
	    if (ins[i] == NULL)
		next
	    if (IS_INDEFI(nc)) {
		nc = IM_LEN(ins[i],1)
		nl = IM_LEN(ins[i],2)
		pixtype = IM_PIXTYPE(ins[i])
	    } else {
		if (IM_LEN(ins[i],1) != nc || IM_LEN(ins[i],2) != nl)
		    call error (2, "xtalkcor: Image dimensions don't match")
		if (IM_PIXTYPE(ins[i]) != pixtype)
		    pixtype = TY_REAL
	    }
	}

	# Get data pixel limits.
	call amovki (1, Memi[x1], nimages)
	call amovki (nc, Memi[x2], nimages)
	call amovki (1, Memi[y1], nimages)
	call amovki (nl, Memi[y2], nimages)
	do i = 1, nimages {
	    if (ins[i] == NULL)
	        next
	    if (section[1] != EOS) {
	        if (section[1] == '!') {
		    ifnoerr (call imgstr (ins[i], section[2],
		        Memc[str], SZ_FNAME))
			call ccd_section (Memc[str], Memi[x1+i-1], Memi[x2+i-1],
			    j, Memi[y1+i-1], Memi[y2+i-1], k)
		} else
		    call ccd_section (section, Memi[x1+i-1], Memi[x2+i-1],
			j, Memi[y1+i-1], Memi[y2+i-1], k)
	    }
	    atm = 1.
	    ifnoerr (rval = imgetr (ins[i], "atm1_1"))
	        atm = rval
	    if (atm < 0.) {
	        j = Memi[x1+i-1]
		Memi[x1+i-1] = Memi[x2+i-1]
		Memi[x2+i-1] = j
	    }
	    atm = 1.
	    ifnoerr (rval = imgetr (ins[i], "atm2_2"))
	        atm = rval
	    if (atm < 0.) {
	        j = Memi[y1+i-1]
		Memi[y1+i-1] = Memi[y2+i-1]
		Memi[y2+i-1] = j
	    }
	}

	# If all dependent images have the same flip then don't flip.
	doxflip = false
	doyflip = false
	for (sym1 = sthead (stp); sym1 != NULL; sym1 = stnext (stp, sym1)) {
	    i = Memi[sym1]
	    do j = 1, nimages {
		if (Memr[P2R(sym1+j)] == 0.)
		    next
		if (Memi[x2+i-1]-Memi[x1+i-1] != Memi[x2+j-1]-Memi[x1+j-1])
		    doxflip = true
		if (Memi[y2+i-1]-Memi[y1+i-1] != Memi[y2+j-1]-Memi[y1+j-1])
		    doyflip = true
	    }
	}
	if (!doxflip) {
	    i = min (Memi[x1], Memi[x2])
	    j = max (Memi[x1], Memi[x2])
	    call amovki (i, Memi[x1], nimages)
	    call amovki (j, Memi[x2], nimages)
	}
	if (!doyflip) {
	    i = min (Memi[y1], Memi[y2])
	    j = max (Memi[y1], Memi[y2])
	    call amovki (i, Memi[y1], nimages)
	    call amovki (j, Memi[y2], nimages)
	}

	# Update the output headers.
	for (sym1 = sthead (stp); sym1 != NULL; sym1 = stnext (stp, sym1)) {
	    i = Memi[sym1]
	    call cnvdate (clktime(0), Memc[str], 1024)
	    fd = stropen (Memc[str], 1024, APPEND)
	    k = 0
	    for (sym2 = sthead (stp); sym2 != NULL; sym2 = stnext (stp, sym2)) {
		j = Memi[sym2]
		if (j == i || Memr[P2R(sym1+j)] == 0.)
		    next
		k = k + 1
		if (k == 1)
		    call fprintf (fd, " Crosstalk is ")
		if (k > 1 && Memr[P2R(sym1+j)] > 0.)
		    call fprintf (fd, "+")
		call fprintf (fd, "%.3g*%s")
		    call pargr (Memr[P2R(sym1+j)])
		    call pargstr (Memc[stname(stp,sym2)])
	    }
	    if (k == 0)
		call fprintf (fd, " No crosstalk correction required")
	    call close (fd)
	    if (logfd != NULL) {
		call fprintf (logfd, "%s[%s]: %s\n")
		    call pargstr (infile)
		    call pargstr (Memc[stname(stp,sym1)])
		    call pargstr (Memc[str])
		call flush (logfd)
	    }
	    if (verbose == YES) {
		call printf ("%s[%s]: %s\n")
		    call pargstr (infile)
		    call pargstr (Memc[stname(stp,sym1)])
		    call pargstr (Memc[str])
		call flush (STDOUT)
	    }
	    if (outs[i] != NULL) {
		call imastr (outs[i], "XTALKCOR", Memc[str])
		call imastr (outs[i], "XTALKFILE", xtfile)
		if (bpms[i] != NULL) {
		    call imstats (bpms[i], IM_IMAGENAME, Memc[str], 1024)
		    call imastr (outs[i], "XTALKBPM", Memc[str])
		}
	    }
	}

	# Do the corrections.
	switch (pixtype) {
	case TY_SHORT:
	    if (doout && dobpm)
		call ctobs (stp, ins, outs, bpms, Memi[x1], Memi[x2],
		    Memi[y1], Memi[y2], Memi[inbufs], Memi[bufs],
		    Memr[coeffs], bpmthresh, nimages, nc, nl)
	    else if (doout)
		call ctalks (stp, ins, outs, Memi[x1], Memi[x2],
		    Memi[y1], Memi[y2], Memi[inbufs], Memi[bufs],
		    Memr[coeffs], nimages, nc, nl)
	    else if (dobpm)
		call ctbpms (stp, ins, bpms, Memi[x1], Memi[x2],
		    Memi[y1], Memi[y2], Memi[inbufs], Memi[bufs],
		    Memr[coeffs], bpmthresh, nimages, nc, nl)
	default:
	    if (doout && dobpm)
		call ctobr (stp, ins, outs, bpms, Memi[x1], Memi[x2],
		    Memi[y1], Memi[y2], Memi[inbufs], Memi[bufs],
		    Memr[coeffs], bpmthresh, nimages, nc, nl)
	    else if (doout)
		call ctalkr (stp, ins, outs, Memi[x1], Memi[x2],
		    Memi[y1], Memi[y2], Memi[inbufs], Memi[bufs],
		    Memr[coeffs], nimages, nc, nl)
	    else if (dobpm)
		call ctbpmr (stp, ins, bpms, Memi[x1], Memi[x2],
		    Memi[y1], Memi[y2], Memi[inbufs], Memi[bufs],
		    Memr[coeffs], bpmthresh, nimages, nc, nl)
	}

	call sfree (sp)
end


# CTJOIN -- Join the separate temporary images into the output MEF file.
# The input file is used to maintain the order of the extensions and to
# supply data when the extension does not have a crosstalk correction.

procedure ctjoin (infile, outfile, tempfile, bufsize, xtfile)

char	infile[ARB]			#I Input MEF file
char	outfile[ARB]			#I Output MEF file
char	tempfile[ARB]			#I Temporary file rootname
int	bufsize				#I I/O buffer size
char	xtfile[ARB]			#I Crosstalk file for header

int	i, j, nc, err, errget()
pointer	sp, inname, outname, extname, str
pointer	in, out, tmp, bufin, bufout, immap(), imgl2r(), impl2r()
long	clktime()
errchk	immap, imgl2r, impl2r

begin
	call smark (sp)
	call salloc (inname, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	iferr {
	    do i = 0, ARB {
		in = NULL; out = NULL
		call sprintf (Memc[inname], SZ_FNAME, "%s[%d]")
		    call pargstr (infile)
		    call pargi (i)
		iferr (in = immap (Memc[inname], READ_ONLY, 0))
		    break
		if (i == 0) {
		    # Copy the global header.
		    tmp = immap (outfile, NEW_COPY, in)
		    out = tmp
		    call imunmap (out)
		    call imunmap (in)
		    next
		}
		call imgstr (in, "extname", Memc[extname], SZ_FNAME)

		# Check if a crosstalk corrected image exists.
		call sprintf (Memc[inname], SZ_FNAME, "%s_%s")
		    call pargstr (tempfile)
		    call pargstr (Memc[extname])
		ifnoerr (tmp = immap (Memc[inname], READ_ONLY, 0)) {
		    call imunmap (in)
		    in = tmp
		    Memc[str] = EOS
		} else {
		    call cnvdate (clktime(0), Memc[str], SZ_LINE)
		    call strcat (" No crosstalk correction required",
			Memc[str], SZ_LINE)
		}

		# Open output and copy header.
		call sprintf (Memc[outname], SZ_FNAME, "%s[%s,append,inherit]")
		    call pargstr (outfile)
		    call pargstr (Memc[extname])
		tmp = immap (Memc[outname], NEW_COPY, in)
		out = tmp

		# Add keywords.
		if (Memc[str] != EOS)
		    call imastr (out, "XTALKCOR", Memc[str])
		call imastr (out, "XTALKFILE", xtfile)

		# Copy data.
		call imseti (in, IM_BUFSIZE, bufsize)
		call imseti (out, IM_BUFSIZE, bufsize)
		nc = IM_LEN(in,1)
		do j = 1, IM_LEN(in,2) {
		    bufin = imgl2r(in,j)
		    bufout = impl2r(out,j)
		    call amovr (Memr[bufin], Memr[bufout], nc)
		}
		call imunmap (out)
		call imunmap (in)
		iferr (call imdelete (Memc[inname]))
		    ;
	    }
	} then {
	    err = errget (Memc[extname], SZ_FNAME)
	    if (out != NULL) {
		call imunmap (out)
		iferr (call imdelete (outfile))
		    ;
	    }
	    if (in != NULL)
		call imunmap (in)
	    call error (err, Memc[extname])
	}

	call sfree (sp)
end


# CTSPLIT -- Split the images which did not require correction.

procedure ctsplit (infile, outfile, tempfile, bufsize, xtfile, outlist)

char	infile[ARB]			#I Input MEF file
char	outfile[ARB]			#I Output file root name
char	tempfile[ARB]			#I Temporary file rootname
int	bufsize				#I I/O buffer size
char	xtfile[ARB]			#I Crosstalk file for header
int	outlist				#I Output list

int	i, j, nc, err, imaccess(), errget()
pointer	sp, inname, outname, extname, str
pointer	in, out, tmp, bufin, bufout, immap(), imgl2r(), impl2r()
long	clktime()
errchk	immap, imgl2r, impl2r

begin
	call smark (sp)
	call salloc (inname, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	iferr {
	    do i = 0, ARB {
		in = NULL; out = NULL
		call sprintf (Memc[inname], SZ_FNAME, "%s[%d]")
		    call pargstr (infile)
		    call pargi (i)
		iferr (in = immap (Memc[inname], READ_ONLY, 0))
		    break
		if (i == 0) {
		    call imunmap (in)
		    next
		}
		
		# Check if a crosstalk corrected image exists.
		call imgstr (in, "extname", Memc[extname], SZ_FNAME)
		call sprintf (Memc[inname], SZ_FNAME, "%s_%s")
		    call pargstr (tempfile)
		    call pargstr (Memc[extname])
		if (imaccess (Memc[inname], 0) == YES) {
		    call imunmap (in)
		    next
		}

		# Open the output image.
		iferr (tmp = immap (Memc[inname], NEW_COPY, in)) {
		    call imunmap (in)
		    next
		}
		out = tmp

		# Add keywords.
		call cnvdate (clktime(0), Memc[str], SZ_LINE)
		call strcat (" No crosstalk correction required",
		    Memc[str], SZ_LINE)
		if (Memc[str] != EOS)
		    call imastr (out, "XTALKCOR", Memc[str])
		call imastr (out, "XTALKFILE", xtfile)

		# Copy data.
		call imseti (in, IM_BUFSIZE, bufsize)
		call imseti (out, IM_BUFSIZE, bufsize)
		nc = IM_LEN(in,1)
		do j = 1, IM_LEN(in,2) {
		    bufin = imgl2r(in,j)
		    bufout = impl2r(out,j)
		    call amovr (Memr[bufin], Memr[bufout], nc)
		}
		call imunmap (out)
		call imunmap (in)
	    }
	} then {
	    err = errget (Memc[extname], SZ_FNAME)
	    if (out != NULL) {
		call imunmap (out)
		iferr (call imdelete (outfile))
		    ;
	    }
	    if (in != NULL)
		call imunmap (in)
	    call error (err, Memc[extname])
	}

	call sfree (sp)
end
