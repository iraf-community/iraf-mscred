# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include <fset.h>
include <mach.h>

# FITS header data structure.
define	LEN_HDR		261
define	GROUP		Memi[$1]		# Group number
define	NAXIS		Memi[$1+$2+1]		# Axes sizes
define	BITPIX		Memi[$1+9]		# Bits per pixel
define	NEXTEND		Memi[$1+10]		# Bits per pixel
define	XTENSION	Memc[P2C($1+20)]	# Extension type
define	EXTNAME		Memc[P2C($1+60)]	# Extension name
define	EXTVER		Memc[P2C($1+100)]	# Extension version
define	OBJECT		Memc[P2C($1+140)]	# Object/title
define	FILENAME	Memc[P2C($1+180)]	# Filename
define	TEMP		Memc[P2C($1+220)]	# Temporary string


# T_FITSCOPY -- Copy FITS files.

procedure t_fitscopy ()

int	input			# Input list of FITS files
int	output			# Output list of FITS files
int	list			# List only?
int	slist			# Short listing?
int	llist			# Long listing?
pointer	extn			# Extension for output disk filenames
int	offset			# Offset for numbering of  output disk filenames
int	original		# Rename to  original filename?
int	intape			# Input files on tape?
int	outtape			# Output files to tape?
pointer	tapefiles		# List of tape file numbers
int	blockfac		# FITS tape blocking factor

int	i, j, innum, outnum
pointer	sp, in_root, out_root, in_fname, out_fname, pat, rngstr, junk

int	fits_copy()
int	fntopnb(), imtopen(), clgeti(), fntgfnb(), fntlenb()
int	fstati(), btoi(), mtneedfileno(), mtfile(), gstrmatch(), nowhite()
int	decode_ranges(), get_next_number()
bool	clgetb()
errchk	fits_copy

begin
	call smark (sp)
	call salloc (in_root, SZ_LINE, TY_CHAR)
	call salloc (out_root, SZ_LINE, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (in_fname, SZ_FNAME, TY_CHAR)
	call salloc (out_fname, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (pat, SZ_FNAME, TY_CHAR)
	call salloc (rngstr, SZ_LINE, TY_CHAR)
	call salloc (junk, SZ_LINE, TY_CHAR)

	# Flush on a newline if the output is not redirected.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get the parameters.
	call clgstr ("input", Memc[in_root], SZ_LINE)
	list = btoi (clgetb ("listonly"))
	slist = btoi (clgetb ("shortlist"))
	llist = btoi (clgetb ("longlist"))
	intape = btoi (clgetb ("intape"))
	if (list == NO) {
	    call clgstr ("output", Memc[out_root], SZ_LINE)
	    call clgstr ("extn", Memc[extn+1], SZ_FNAME)
	    outtape = btoi (clgetb ("outtape"))
	    offset = clgeti ("offset")
	    original = btoi (clgetb ("original"))
	    blockfac = clgeti ("blocking_factor")
	    if (nowhite (Memc[out_root], Memc[out_root], SZ_LINE) == 0) {
		if (original == YES)
		    call mktemp ("tmp$", Memc[out_root], SZ_LINE)
		else
		    call error (1, "No output name specified")
	    }
	}

	if (list == YES && llist == NO)
	    slist = YES
	if (llist == YES && slist == YES)
	    slist = NO

	# Set the file name expansions.
	Memc[extn] = '.'
	call sprintf (Memc[pat], SZ_FNAME, "%s$")
	    call pargstr (Memc[extn])

	if (intape == NO) {
	    input = fntopnb (Memc[in_root], NO)
	} else {
	    input = imtopen (Memc[in_root])
	    if (fntgfnb (input, Memc[in_root], SZ_LINE) == EOF)
		call error (1, "No input tape name")
	    if (mtneedfileno (Memc[in_root]) == YES) {
		call fntclsb (input)
		call salloc (tapefiles, 100*3, TY_INT)
		call clgstr ("tapefiles", Memc[rngstr], SZ_LINE)
		if (decode_ranges (Memc[rngstr], Memi[tapefiles], 100,
		    innum) == ERR)
		    call error (1, "Error in tape file list")
	    } else
		call fntrewb (input)
	}

	output = NULL
	if (list == YES ||
	    nowhite (Memc[out_root], Memc[out_fname], SZ_FNAME) == 0) {
	    Memc[out_root] = EOS
	    Memc[out_fname] = EOS
	    outtape = NO
	} else {
	    if (outtape == YES) {
		if (mtneedfileno (Memc[out_root]) == YES) {
		    if (!clgetb ("newtape"))
			call mtfname (Memc[out_root], EOT, Memc[out_fname],
			    SZ_FNAME)
		    else
			call mtfname (Memc[out_root], 1, Memc[out_fname],
			    SZ_FNAME)
		}
		Memc[out_root] = EOS
	    } else {
		output = fntopnb (Memc[out_root], NO)
		if (input != NULL) {
		    if (fntlenb (output) != fntlenb (input)) {
			if (fntlenb (output) > 1)
			    call error (1, "Input and output lists don't match")
			if (fntgfnb (output, Memc[out_root], SZ_LINE) == EOF)
			    call error (1, "Error in output name")
			call fntclsb (output)
		    }
		} else {
		    if (fntgfnb (output, Memc[out_root], SZ_FNAME) == EOF)
			call error (1, "Error in output name")
		    call fntclsb (output)
		}
		if (gstrmatch (Memc[out_root], Memc[pat], i, j) != 0)
		    Memc[out_root+i-1] = EOS
	    }
	}

	# Copy the FITS files.
	innum = 0
	outnum = offset
	repeat {
	    if (input != NULL) {
		if (fntgfnb (input, Memc[in_fname], SZ_FNAME) == EOF)
		    break
		outnum = outnum + 1
		if (intape == NO) {
		    if (gstrmatch (Memc[in_fname], Memc[pat], i, j) == 0)
			call strcat (Memc[extn], Memc[in_fname], SZ_FNAME)
		} else if (mtfile (Memc[in_fname]) == YES) {
		    call mtparse (Memc[in_fname], Memc[junk], SZ_LINE,
			innum, i, Memc[junk], SZ_LINE)
		    if (innum != ERR)
			outnum = innum + offset 
		}
	    } else {
		if (get_next_number (Memi[tapefiles], innum) == EOF)
		    break
		call sprintf (Memc[in_fname], SZ_FNAME, "%s[%d]")
		    call pargstr (Memc[in_root])
		    call pargi (innum)
		outnum = innum + offset
	    }

	    if (list == NO && outtape == NO) {
		if (output != NULL) {
		    if (fntgfnb (output, Memc[out_fname], SZ_FNAME) == EOF)
			break
		    if (gstrmatch (Memc[out_fname], Memc[pat], i, j) == 0)
			call strcat (Memc[extn], Memc[out_fname], SZ_FNAME)
		} else if (Memc[out_root] != EOS) {
		    call sprintf (Memc[out_fname], SZ_FNAME, "%s%04d%s")
			call pargstr (Memc[out_root])
			call pargi (outnum)
			call pargstr (Memc[extn])
		}
	    }

	    iferr (i = fits_copy (Memc[in_fname], Memc[out_fname], list, slist,
		llist, original, Memc[extn], intape, outtape, blockfac)) {
		if (intape == YES || outtape == YES)
		    call erract (EA_ERROR)
		else
		    call erract (EA_WARN)
	    }

	    if (i == EOF) {
		if (slist == YES || llist == YES) {
		    if (intape == YES)
			call printf ("  End of tape\n")
		    else
			call printf ("  Empty file\n")
		}
		if (intape == YES)
		    break
	    }
	    if (outtape == YES)
		call mtfname (Memc[out_fname], EOT, Memc[out_fname],
		    SZ_FNAME)
	}

	if (input != NULL)
	    call fntclsb (input)
	if (output != NULL)
	    call fntclsb (output)
	call sfree (sp)
end


# FITS_COPY -- Copy FITS files.

int procedure fits_copy (in_fname, out_fname, list, slist, llist, original,
	extn, intape, outtape, blocking)

char	in_fname[ARB]		#I input file name
char	out_fname[ARB]		#I output file name
int	list			#I List only?
int	slist			#I Short list?
int	llist			#I Long list?
int	original		#I restore original file name?
char	extn[ARB]		#I extension for disk images
int	intape			#I input tape?
int	outtape			#I output tape?
int	blocking		#I output FITS blocking factor

int	in, out, mov_nbytes, rem_in, rem_out
int	sz_charsin, sz_charsout, szb_inrecord, szb_outrecord, szb_outblock
int	bytes_read, ip, op, nchars, recnum, hdrrec, err
long	bytes_write, offset
pointer	sp, inbuf, outbuf, hdr, errmsg, tmp, fname

int	fnroot(), mtopen(), read(), fstati(), open(), strmatch()
long	awaitb()
bool	strne()
errchk	open, mtopen, read, awriteb, awaitb, close, mfree, malloc
errchk	open, mtopen, fits_hdr

begin
	call smark (sp)
	call salloc (hdr, LEN_HDR, TY_STRUCT)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Print filenames.
	if (slist == YES || llist == YES) {
	    call printf ("%s")
		call pargstr (in_fname)
	    if (out_fname[1] != EOS) {
		call printf ("  ->  %s")
		    call pargstr (out_fname)
	    }
	    call printf (":")
	    call flush (STDOUT)
	}

	# Setup the input.  The output file is setup after checking the
	# first record of the input file.

	if (intape == NO) {
	    nchars = fnroot (in_fname, FILENAME(hdr), 80)
	    in = open (in_fname, READ_ONLY, BINARY_FILE)
	} else {
	    FILENAME(hdr) = EOS
	    in = mtopen (in_fname, READ_ONLY, 0)
	}
	out = NULL

	sz_charsin = 1440
	szb_inrecord = sz_charsin * SZB_CHAR
	rem_in = szb_inrecord
	call salloc (inbuf, sz_charsin, TY_CHAR)

	err = NO
	iferr {
	    # Loop over the input blocks.
	    ip = 1
	    op = 1
	    offset = 1
	    recnum = 0
	    repeat {
		# Read a block and update block counter.
		nchars = read (in, Memc[inbuf], sz_charsin)
		if (nchars == EOF)
		    break
		bytes_read = nchars * SZB_CHAR
		if (mod (fstati (in, F_SZBBLK), SZB_CHAR) != 0)
		    bytes_read = bytes_read - mod (fstati (in, F_SZBBLK),
			SZB_CHAR)

		iferr (call fits_hdr (Memc[inbuf], hdr, recnum, hdrrec,
		    list, slist, llist, outtape)) {
		    call close (in)
		    call sfree (sp)
		    call erract (EA_FATAL)
		}

		if (list == YES && slist == YES) {
		    if (hdrrec == NO)
			break
		    else
			next
		}

		if (out == NULL) {
		    if (outtape == NO) {
			if (out_fname[1] == EOS)
			    next
			tmp = open (out_fname, NEW_FILE, BINARY_FILE)
			out = tmp
			szb_outblock = fstati (out, F_BUFSIZE) * SZB_CHAR
		    } else {
			tmp = mtopen (out_fname, WRITE_ONLY, 0)
			out = tmp
			szb_outblock = blocking * 2880
		    }
		    szb_outrecord = szb_outblock
		    rem_out = szb_outrecord

		    if (mod (szb_outblock, SZB_CHAR) == 0)
			sz_charsout = szb_outblock / SZB_CHAR
		    else
			sz_charsout = (szb_outblock / SZB_CHAR + 1) * SZB_CHAR
		    if (sz_charsout != sz_charsin)
			call salloc (outbuf, sz_charsout, TY_CHAR)
		    else
			outbuf = inbuf
		}

		if (outbuf == inbuf) {
		    call awriteb (out, Memc[inbuf], bytes_read, offset)
		    bytes_write = awaitb (out)
		    if (bytes_write == ERR) {
			call sprintf (Memc[errmsg], SZ_LINE,
			    "Write failed (%s -> %s)")
			    call pargstr (in_fname)
			    call pargstr (out_fname)
			call error (1, Memc[errmsg])
		    }
		    offset = offset + bytes_write
		} else {
		    repeat {
			# Calculate the number of bytes to be moved.
			mov_nbytes = min (bytes_read - ip + 1,
			    rem_in, rem_out, szb_outblock - op + 1)
			call bytmov (Memc[inbuf], ip, Memc[outbuf], op,
			    mov_nbytes)

			# Update the remainders
			rem_in = rem_in - mov_nbytes
			if (rem_in == 0)
			    rem_in = szb_inrecord
			rem_out = rem_out - mov_nbytes
			if (rem_out == 0)
			    rem_out = szb_outrecord

			# Update pointers
			ip = ip + mov_nbytes
			op = op + mov_nbytes

			# If output buffer is exhausted, output block of data.
			if (op > szb_outblock) {
			    call awriteb (out, Memc[outbuf], szb_outblock,
				offset)
			    bytes_write = awaitb (out)
			    if (bytes_write == ERR) {
				call sprintf (Memc[errmsg], SZ_LINE,
				    "Write failed:  %s -> %s")
				    call pargstr (in_fname)
				    call pargstr (out_fname)
				call error (1, Memc[errmsg])
			    }
			    offset = offset + bytes_write
			    op = 1
			}
		    } until (ip > bytes_read)

		    ip = 1
		}
	    }

	    # Output remainder of data
	    if (op > 1) {
		call awriteb (out, Memc[outbuf], op - 1, offset)
		if (awaitb (out) == ERR) {
		    call sprintf (Memc[errmsg], SZ_LINE,
			"Write failed:  %s -> %s")
			call pargstr (in_fname)
			call pargstr (out_fname)
		    call error (1, Memc[errmsg])
		}
	    }
	} then
	    err = YES

	call close (in)
	if (out != NULL) {
	    call close (out)
	    if (outtape == NO && original == YES &&
		strne (out_fname, FILENAME(hdr))) {
		if (FILENAME(hdr) != EOS) {
		    call salloc (fname, SZ_FNAME, TY_CHAR)
		    call sprintf (Memc[fname], SZ_FNAME, "%s$")
			call pargstr (extn)
		    if (strmatch (FILENAME(hdr), Memc[fname]) == 0) {
			call sprintf (Memc[fname], SZ_FNAME, "%s%s")
			    call pargstr (FILENAME(hdr))
			    call pargstr (extn)
		    } else
			call strcpy (FILENAME(hdr), Memc[fname], SZ_FNAME)
		    if (slist == YES || llist == YES) {
			call printf ("  Rename %s -> %s\n")
			    call pargstr (out_fname)
			    call pargstr (Memc[fname])
		    }
		    iferr (call rename (out_fname, Memc[fname]))
			call erract (EA_WARN)
		} else
		    call printf ("  Original filename not found\n")
	    }
	}
	call sfree (sp)

	if (err == YES)
	    call erract (EA_ERROR)
	else if (recnum == 0)
	    return (EOF)
	else
	    return (OK)
end


# FITS_HDR -- FITS header record.

procedure fits_hdr (inrec, hdr, recnum, hdrrec, list, slist, llist, filename)

char	inrec[ARB]	#I Input FITS record
pointer hdr		#O Header structure
int	recnum		#U Record number
int	hdrrec		#O Header record?
int	list		#I List only?
int	slist		#I Short list?
int	llist		#I Long list?
int	filename	#I Update file name?

int     i, j, strncmp()
bool	strne()
char    line[81]

begin
	recnum = recnum + 1
	if (recnum == 1)
	    hdrrec = NO
	line[81] = '\n'
	for (i=1; i<=1440; i=i+40) {
	    call achtbc(inrec[i], line, 80)
	    if (hdrrec == NO) {
		if (recnum == 1) {
		    if (strncmp (line, "SIMPLE", 6) != 0) {
			if (list == NO)
			    call error (1, "Not a FITS file - file not written")
			else
			    call error (1, "Not a FITS file")
		    }
		    call strcpy ("PRIMARY", XTENSION(hdr), 80)
		    GROUP(hdr) = 0
		    NEXTEND(hdr) = INDEFI
		} else {
		    if (strncmp (line, "XTENSION", 8) != 0)
			return
		    call fits_gstr (line, XTENSION(hdr), 80)
		    GROUP(hdr) = GROUP(hdr) + 1
		}
		NAXIS(hdr,0) = 0
		BITPIX(hdr,0) = 0
		EXTNAME(hdr) = EOS
		EXTVER(hdr) = EOS
		OBJECT(hdr) = EOS
		hdrrec = YES
		next
	    }

	    if (strncmp (line, "END", 3) == 0) {
		if (slist == YES || llist == YES) {
		    if (GROUP(hdr) == 0) {
			if (FILENAME(hdr) != EOF) {
			    call printf ("  %-20s")
				call pargstr (FILENAME(hdr))
			}
			if (!IS_INDEFI(NEXTEND(hdr))) {
			    call printf (" nextend=%d")
				call pargi (NEXTEND(hdr))
			}
			call printf ("  %s\n")
			    call pargstr (OBJECT(hdr))
		    }
		    if (llist == YES) {
			call printf ("  %2d %7s %3s %s")
			    call pargi (GROUP(hdr))
			    call pargstr (XTENSION(hdr))
			    call pargstr (EXTNAME(hdr))
			    call pargstr (EXTVER(hdr))
			if (NAXIS(hdr,0) > 0) {
			    call printf (" %2d %d")
				call pargi (BITPIX(hdr))
				call pargi (NAXIS(hdr,1))
			    do j = 2, NAXIS(hdr,0) {
				call printf ("x%d")
				    call pargi (NAXIS(hdr,j))
			    }
			}
			call printf ("\n")
		    }
		}
		hdrrec = NO
		return
	    }

	    if (strncmp (line, "NAXIS", 5) == 0) {
		if (line[6] == ' ')
		    call fits_geti (line, NAXIS(hdr,0))
		else if (IS_DIGIT(line[6]))
		    call fits_geti (line, NAXIS(hdr,TO_INTEG(line[6])))
	    } else if (strncmp (line, "BITPIX", 6) == 0) {
		call fits_geti (line, BITPIX(hdr))
	    } else if (strncmp (line, "NEXTEND", 7) == 0) {
		call fits_geti (line, NEXTEND(hdr))
	    } else if (strncmp (line, "EXTNAME", 7) == 0) {
		call fits_gstr (line, EXTNAME(hdr), 80)
	    } else if (strncmp (line, "EXTVER", 6) == 0) {
		call fits_gstr (line, EXTVER(hdr), 80)
	    } else if (strncmp (line, "OBJECT", 6) == 0) {
		call fits_gstr (line, OBJECT(hdr), 80)
	    } else if (strncmp (line, "FILENAME", 8) == 0) {
		call fits_gstr (line, TEMP(hdr), 80)
		if (FILENAME(hdr) == EOS)
		    call strcpy (TEMP(hdr), FILENAME(hdr), 80)
		if (strne (TEMP(hdr), FILENAME(hdr)) || line[80] == '\n') {
		    call sprintf (line[11], 71, "'%8s'%71t\n")
			call pargstr (FILENAME(hdr))
		    call achtcb (line, inrec[i], 80)
		}
	    } else if (strncmp (line, "IRAFNAME", 8) == 0) {
		call fits_gstr (line, TEMP(hdr), 80)
		if (FILENAME(hdr) == EOS)
		    call strcpy (TEMP(hdr), FILENAME(hdr), 80)
		if (strne (TEMP(hdr), FILENAME(hdr)) || line[80] == '\n') {
		    call sprintf (line[11], 71, "'%8s'%71t\n")
			call pargstr (FILENAME(hdr))
		    call achtcb (line, inrec[i], 80)
		}
	    }
	}
	hdrrec = YES
end


# FITS_GETS -- Get the string value of a FITS encoded card.  Strip leading
# and trailing whitespace and any quotes.

procedure fits_gstr (card, outstr, maxch)

char	card[ARB]		#I FITS card to be decoded
char	outstr[ARB]		#O output string to receive parameter value
int	maxch			#I length of outstr

int	ip, op
int	ctowrd(), strlen()

begin
	ip = 10
	if (ctowrd (card, ip, outstr, maxch) > 0) {
	    # Strip trailing whitespace.
	    op = strlen (outstr)
	    while (op > 0 && (IS_WHITE(outstr[op]) || outstr[op] == '\n'))
		op = op - 1
	    outstr[op+1] = EOS
	} else
	    outstr[1] = EOS
end

# FITS_GETI -- Return the integer value of a FITS encoded card.

procedure fits_geti (card, ival)

char	card[ARB]		#I card to be decoded
int	ival			#O receives integer value

int	ip, ctoi()
char	sval[68]

begin
	call fits_gstr (card, sval, 68)
	ip = 1
	if (ctoi (sval, ip, ival) <= 0)
	    ival = 0
end
