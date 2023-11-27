include	<imhdr.h>
include	<error.h>
include	<syserr.h>


# T_MKMSC -- Convert data to MSCRED format.

procedure t_mkmsc()

pointer	inlist				# Input list of image
pointer	outlist				# Output list of files
pointer	desc				# Description file
bool	verbose				# Verbose?

int	i, j, l, n
int	datasec[3,2], trimsec[3,2], biassec[3,2]
int	dsec[2,2], bsec[2,2], ndata[2], nbias[2]
double	dval
pointer	sp, val, input, output, temp, image, extname, ptr, ibuf, obuf
pointer	estp, esym, stp, sym
pointer	in, out

bool	clgetb(), streq()
int	strlen(), stridxs(), ctod(), ctoi(), errcode(), imaccess()
int	imtopenp(), imtgetim(), imtlen()
pointer	immap(), imgl2i(), impl2i(), imgl2r(), impl2r()
pointer	estp_open(), sthead(), stnext(), stname()

errchk	estp_open, immap, imgl2i, impl2i, imgl2r, impl2r

begin
	call smark (sp)
	call salloc (desc, SZ_FNAME, TY_CHAR)
	call salloc (val, SZ_LINE, TY_CHAR)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	call clgstr ("description", Memc[desc], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Check lists match.
	i = imtlen (inlist)
	j = imtlen (outlist)
	if (j > 0 && j != i)
	    call error (1, "Input and output lists do not match")

	# Read description file into symbol tables.
	estp = estp_open (Memc[desc], verbose)

	# Create the extensions.
	while (imtgetim (inlist, Memc[image], SZ_FNAME) != EOF) {
	    # Ignore any image sections in the input.
	    call imgimage (Memc[image], Memc[input], SZ_FNAME)
	    if (imtgetim (outlist, Memc[temp], SZ_FNAME) == EOF) {
		call mktemp ("tmp", Memc[temp], SZ_FNAME)
		call strcat (".fits", Memc[temp], SZ_FNAME)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    } else if (streq (Memc[input], Memc[output])) {
		call mktemp ("tmp", Memc[temp], SZ_FNAME)
		call strcat (".fits", Memc[temp], SZ_FNAME)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    } else
		call strcpy (Memc[temp], Memc[output], SZ_FNAME)

	    iferr {
		in = NULL
		out = NULL

		if (imaccess (Memc[temp], 0) == YES) {
		    call sprintf (Memc[val], SZ_LINE,
			"Output file already exists (%s)")
			call pargstr (Memc[temp])
		    call error (1, Memc[val])
		}

		ptr = immap (Memc[input], READ_ONLY, 0); in = ptr

		for (esym=sthead(estp); esym!=NULL; esym=stnext(estp,esym)){
		    stp = Memi[esym]
		    extname = stname (estp, esym)

		    # Check whether to create extension.
		    call mk_gkey (stp, in, "DATASEC", Memc[val], SZ_LINE)
		    if (Memc[val] == EOS)
			next

		    # Create output extension.
		    call sprintf (Memc[image], SZ_LINE, "%s[%s,append]")
			call pargstr (Memc[temp])
			call pargstr (Memc[extname])
		    iferr (ptr = immap (Memc[image], NEW_COPY, in)) {
			switch (errcode()) {
			case SYS_IKIKSECTNS:
			    # Try adding ".fits" to name.
			    call sprintf (Memc[image], SZ_LINE,
				"%s.fits[%s,append]")
				call pargstr (Memc[temp])
				call pargstr (Memc[extname])
			    ptr = immap (Memc[image], NEW_COPY, in)
			default:
			    call erract (EA_ERROR)
			}
		    }
		    out = ptr

		    # Determine data section.
		    datasec[1,1] = 1
		    datasec[2,1] = IM_LEN(in,1)
		    datasec[3,1] = 1
		    datasec[1,2] = 1
		    datasec[2,2] = IM_LEN(in,2)
		    datasec[3,2] = 1
		    call mk_gkey (stp, in, "DATASEC", Memc[val], SZ_LINE)
		    if (Memc[val] != EOS)
			call ccd_section (Memc[val], datasec[1,1],
			    datasec[2,1], datasec[3,1], datasec[1,2],
			    datasec[2,2], datasec[3,2])
		    dsec[1,1] = min (datasec[1,1], datasec[2,1])
		    dsec[2,1] = max (datasec[1,1], datasec[2,1])
		    dsec[1,2] = min (datasec[1,2], datasec[2,2])
		    dsec[2,2] = max (datasec[1,2], datasec[2,2])
		    ndata[1] = dsec[2,1] - dsec[1,1] + 1
		    ndata[2] = dsec[2,2] - dsec[1,2] + 1

		    # Determine trim section.
		    call mk_gkey (stp, in, "TRIMSEC", Memc[val], SZ_LINE)
		    if (Memc[val] != EOS) {
			trimsec[1,1] = 1
			trimsec[2,1] = IM_LEN(in,1)
			trimsec[3,1] = 1
			trimsec[1,2] = 1
			trimsec[2,2] = IM_LEN(in,2)
			trimsec[3,2] = 1
			call ccd_section (Memc[val], trimsec[1,1],
			    trimsec[2,1], trimsec[3,1], trimsec[1,2],
			    trimsec[2,2], trimsec[3,2])
		    } else
			trimsec[1,1] = INDEFI

		    # Determine bias section.
		    call mk_gkey (stp, in, "BIASSEC", Memc[val], SZ_LINE)
		    if (Memc[val] != EOS) {
			biassec[1,1] = 1
			biassec[2,1] = IM_LEN(in,1)
			biassec[3,1] = 1
			biassec[1,2] = 1
			biassec[2,2] = IM_LEN(in,2)
			biassec[3,2] = 1
			call ccd_section (Memc[val], biassec[1,1],
			    biassec[2,1], biassec[3,1], biassec[1,2],
			    biassec[2,2], biassec[3,2])
			bsec[1,1] = min (biassec[1,1], biassec[2,1])
			bsec[2,1] = max (biassec[1,1], biassec[2,1])
			bsec[1,2] = dsec[1,2]
			bsec[2,2] = dsec[2,2]
			nbias[1] = bsec[2,1] - bsec[1,1] + 1
			nbias[2] = bsec[2,2] - bsec[1,2] + 1
		    } else {
			nbias[1] = INDEFI
			nbias[2] = INDEFI
		    }

		    # Set output size.
		    IM_LEN(out,1) = ndata[1]
		    IM_LEN(out,2) = ndata[2]
		    if (!IS_INDEFI(nbias[1]))
			IM_LEN(out,1) = IM_LEN(out,1) + nbias[1]

		    # Set header.
		    call sprintf (Memc[val], SZ_LINE, "[%d:%d,%d:%d]")
			call pargi (datasec[1,1] - dsec[1,1] + 1)
			call pargi (datasec[2,1] - dsec[1,1] + 1)
			call pargi (datasec[1,2] - dsec[1,2] + 1)
			call pargi (datasec[2,2] - dsec[1,2] + 1)
		    call imastr (out, "DATASEC", Memc[val])
		    if (!IS_INDEFI(trimsec[1,1])) {
			call sprintf (Memc[val], SZ_LINE, "[%d:%d,%d:%d]")
			    call pargi (trimsec[1,1] - dsec[1,1] + 1)
			    call pargi (trimsec[2,1] - dsec[1,1] + 1)
			    call pargi (trimsec[1,2] - dsec[1,2] + 1)
			    call pargi (trimsec[2,2] - dsec[1,2] + 1)
			call imastr (out, "TRIMSEC", Memc[val])
		    }
		    if (!IS_INDEFI(nbias[1])) {
			i = min (biassec[1,1], biassec[2,1]) - 1 - ndata[1]
			j = min (biassec[1,2], biassec[2,2]) - 1
			call sprintf (Memc[val], SZ_LINE, "[%d:%d,%d:%d]")
			    call pargi (biassec[1,1] - bsec[1,1] + ndata[1] + 1)
			    call pargi (biassec[2,1] - bsec[1,1] + ndata[1] + 1)
			    call pargi (biassec[1,2] - bsec[1,2] + 1)
			    call pargi (biassec[2,2] - bsec[1,2] + 1)
			call imastr (out, "BIASSEC", Memc[val])
		    }
		    for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
			ptr = stname (stp, sym) 
			if (streq (Memc[ptr], "DATASEC"))
			    next
			else if (streq (Memc[ptr], "TRIMSEC"))
			    next
			else if (streq (Memc[ptr], "BIASSEC"))
			    next

			call mk_gkey (stp, in, Memc[ptr], Memc[val], SZ_LINE)
			n = strlen (Memc[val])
			i = 1
			j = 1
			if (ctoi (Memc[val], i, l) == n)
			    call imaddi (out, Memc[ptr], l)
			else if (ctod (Memc[val], j, dval) == n) {
			    if (stridxs (":", Memc[val]) > 0)
				call imastr (out, Memc[ptr], Memc[val])
			    else
				call imaddd (out, Memc[ptr], dval)
			} else
			    call imastr (out, Memc[ptr], Memc[val])
		    }

		    if (verbose) {
			call printf ("  Create %s[%s][%d,%d]: %s\n")
			    call pargstr (Memc[output])
			    call pargstr (Memc[extname])
			    call pargi (IM_LEN(out,1))
			    call pargi (IM_LEN(out,2))
			    call pargstr (IM_TITLE(out))
			call printf ("    %s[%d:%d,%d,%d]")
			    call pargstr (Memc[input])
			    call pargi (dsec[1,1])
			    call pargi (dsec[2,1])
			    call pargi (dsec[1,2])
			    call pargi (dsec[2,2])
			call printf (" -> %s[%s][%d:%d,%d:%d]\n")
			    call pargstr (Memc[output])
			    call pargstr (Memc[extname])
			    call pargi (1)
			    call pargi (ndata[1])
			    call pargi (1)
			    call pargi (ndata[2])
			if (!IS_INDEFI(nbias[1])) {
			    call printf ("    %s[%d:%d,%d,%d]")
				call pargstr (Memc[input])
				call pargi (bsec[1,1])
				call pargi (bsec[2,1])
				call pargi (bsec[1,2])
				call pargi (bsec[2,2])
			    call printf (" -> %s[%s][%d:%d,%d:%d]\n")
				call pargstr (Memc[output])
				call pargstr (Memc[extname])
				call pargi (ndata[1]+1)
				call pargi (ndata[1]+nbias[1])
				call pargi (1)
				call pargi (nbias[2])
			}
			call flush (STDOUT)
		    }

		    # Copy data.
		    switch (IM_PIXTYPE(in)) {
		    case TY_USHORT, TY_INT:
			do l = 1, IM_LEN(out,2) {
			    ibuf = imgl2i (in, l+dsec[1,2]-1)
			    obuf = impl2i (out, l)
			    call amovi (Memi[ibuf+dsec[1,1]-1],
				Memi[obuf], ndata[1])
			    if (!IS_INDEFI(nbias[1]))
				call amovi (Memi[ibuf+bsec[1,1]-1],
				    Memi[obuf+ndata[1]], nbias[1])
			}
		    default:
			do l = 1, IM_LEN(out,2) {
			    ibuf = imgl2r (in, l+dsec[1,2]-1)
			    obuf = impl2r (out, l)
			    call amovr (Memr[ibuf+dsec[1,1]-1],
				Memr[obuf], ndata[1])
			    if (!IS_INDEFI(nbias[1]))
				call amovr (Memr[ibuf+bsec[1,1]-1],
				    Memr[obuf+ndata[1]], nbias[1])
			}
		    }

		    call imunmap (out)
		}

		call imunmap (in)

		if (streq (Memc[output], Memc[input])) {
		    call imdelete (Memc[input])
		    call imrename (Memc[temp], Memc[input])
		}
	    } then {
		if (out != NULL) {
		    call imunmap (out)
		    call imdelete (Memc[temp])
		}
		if (in != NULL)
		    call imunmap (in)
		call erract (EA_WARN)
	    }
	}

	# Close the symbol tables.
	for (esym=sthead(estp); esym!=NULL; esym=stnext(estp,esym))
	    call stclose (Memi[esym])
	call stclose (estp)

	call imtclose (outlist)
	call imtclose (inlist)

	call sfree (sp)
end


# MK_GKEY -- Get keyword.

procedure mk_gkey (stp, im, key, val, maxchar)

pointer	stp			#I Symbol table
pointer	im			#I Image pointer
char	key[ARB]		#I Key
char	val[ARB]		#O Value
int	maxchar			#I Maximum characters to return

pointer	sym, cp, stfind(), strefsbuf()

begin
	# Initialize return value.
	val[1] = EOS

	# Find key in symbol table.
	sym = stfind (stp, key)
	if (sym == NULL)
	    return

	# Get key value.  If not an image reference return value.
	cp = strefsbuf (stp, Memi[sym])
	if (Memc[cp] != '!') {
	    call strcpy (Memc[cp], val, maxchar)
	    return
	}

	iferr (call imgstr (im, Memc[cp+1], val, maxchar))
	    val[1] = EOS
end


# ESTP_OPEN --  Read description file into symbol tables.  There is a symbol
# table of other symbol tables indexed by the extension names.
# Each extension name has a symbol table of keywords.

pointer procedure estp_open (desc, verbose)

char	desc[ARB]		#I Description file
bool	verbose			#I Verbose?
pointer	estp			#O Symbol table of extensions

int	ip, fd
pointer	sp, key, val, extname, ptr
pointer	esym, stp, sym, estp1, esym1, stp1, sym1

int	open(), fscan(), nscan(), stpstr()
pointer	stopen(), stenter(), stfind(), sthead(), stnext(), stname(), strefsbuf()

errchk	open

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (val, SZ_LINE, TY_CHAR)

	estp = stopen (desc, 32, 32, 32*16)

	if (verbose) {
	    call printf ("  Reading description file %s\n")
	        call pargstr (desc)
	    call flush (STDOUT)
	}

	fd = open (desc, READ_ONLY, TEXT_FILE)
	while (fscan(fd) != EOF) {
	    call gargwrd (Memc[key], SZ_FNAME)
	    call gargwrd (Memc[val], SZ_LINE)
	    if (nscan() != 2)
		next
	    if (Memc[key] == '#')
		next

	    # Separate key into keyword and extension name.
	    extname = NULL
	    for (ip=key; Memc[ip] != EOS; ip=ip+1) {
		if (Memc[ip] == '(') {
		    Memc[ip] = EOS
		    extname = ip + 1
		} else if (Memc[ip] == ')')
		    Memc[ip] = EOS
	    }

	    # Get extension symbol table.  Create one as needed.
	    esym = stfind (estp, Memc[extname])
	    if (esym == NULL) {
		esym = stenter (estp, Memc[extname], 1)
		Memi[esym] = stopen (Memc[extname], 32, 32, 32*SZ_LINE)
	    }
	    stp = Memi[esym]

	    # Enter keyword value.  Previous values are overridden.
	    call strupr (Memc[key])
	    sym = stfind (stp, Memc[key])
	    if (sym == NULL) {
		sym = stenter (stp, Memc[key], 1)
		Memi[sym] = stpstr (stp, Memc[val], SZ_LINE)
	    } else {
		ptr = strefsbuf (stp, Memi[sym])
		call strcpy (Memc[val], Memc[ptr], SZ_LINE)
	    }
	}
	call close (fd)

	# Reverse the symbol tables so that we can use sthead/stnext.
	estp1 = stopen (desc, 32, 32, 32*16)
	for (esym=sthead(estp); esym!=NULL; esym=stnext(estp,esym)) {
	    stp = Memi[esym]
	    extname = stname (estp, esym)

	    stp1 = stopen (Memc[extname], 32, 32, 32*SZ_LINE)
	    esym1 = stenter (estp1, Memc[extname], 1)
	    Memi[esym1] = stp1
	    for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
		ptr = strefsbuf (stp, Memi[sym])
		sym1 = stenter (stp1, Memc[stname(stp,sym)], 1)
		Memi[sym1] = stpstr (stp1, Memc[ptr], SZ_LINE)
	    }
	    call stclose (stp)
	}
	call stclose (estp)
	estp = estp1

	call sfree (sp)
	return (estp)
end
