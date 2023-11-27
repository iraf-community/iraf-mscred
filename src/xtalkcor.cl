# XTALKCOR -- Remove remove chip-to-chip crosstalk from Mosaic frames.
# This should be run either as the very first processing step,
# or failing that before any flatfield corrections are applied
# (the script if run after bias removal will produce functionally
# very similar results to running first thing, with only a small
# change in final sky level, of order bias*xtalk_coefficients)
#
# This task was derived from a contribution from James Rhoads.

procedure xtalkcor (input, output)

string	input		{prompt="List of mosaic files to correct"}
string	output		{prompt="List of corrected mosaic files"}
file	xtalkfiles = ""	{prompt="Crosstalk file"}
bool	noproc = no	{prompt="List only?"}

struct	*fd1, *fd2

begin
	int	nimages, ncoeff
	file	list1, list2, tmpout, xtfile, in, out, im1, im2, inextn, outextn
	real	coeff1, coeff2
	string	outtype, day, hms, date
	struct	logstr

	cache	sections

	list1 = mktemp ("tmp$iraf")
	list2 = mktemp ("tmp$iraf")
	tmpout = mktemp ("tmp")

	# Check coefficient file.
	if (substr (xtalkfile, 1, 1) != "!")
	    if (!access (xtalkfile))
		error (1, "Crosstalk file not found (" // xtalkfile // ")")

	# Expand input and output lists.
	sections (input, option="fullname", >> list2)
	nimages = sections.nimages
	sections (output, option="fullname") |
	joinlines (list2, "STDIN", output=list1, delim =" ", maxchars=161,
	    shortest+, verbose-)
	delete (list2, verify-)
	     
	# Check input and output lists match.
	if (sections.nimages != nimages) {
	    delete (list1, verify-)
	    error (1, "Input and output lists do not match")
	}

	# Set output type.
	outtype = "real"
	ncoeff = fscan (pixeltype, outtype)

	# Process the images.
	fd1 = list1
	while (fscan (fd1, in, out) != EOF) {
	    # Check for previous processing.
	    hselect (in//"[1]", "XTALKCOR", yes) | scan (im1)
	    if (nscan() != 0)
		next

	    xtfile = xtalkfile
	    if (substr (xtfile, 1, 1) == "!") {
		xtfile = substr (xtfile, 2, strlen(xtfile))
		hselect (in//"[1]", xtfile, yes) | scan (xtfile)
		if (nscan() == 0)
		    error (1, "Crosstalk file not found for " // in)
		if (!access (xtfile))
		    error (1, "Crosstalk file not found (" // xtfile // ")")
	    }

	    if (noproc) {
		printf ("%s:\n  [TO BE DONE] Crosstalk file is %s\n",
		    in, xtfile)
		next
	    }

	    # Check for input and output being the same.
	    if (in == out)
		out = tmpout

	    # Do the calibration.
	    fd2 = xtfile
	    while (fscan (fd2, im1, im2, coeff1, coeff2) != EOF) {
		ncoeff = nscan()
		if (ncoeff == 0)
		    next
		if (substr (im1, 1, 1) == "#")
		    next
		switch (ncoeff) {
		case 3:
		    if (coeff1 == 0.)
			ncoeff = 2
		case 4:
		    if (coeff1 == 0. && coeff2 == 0.)
			ncoeff = 2
		}

		if (!imaccess (out//"[0]"))
		    imcopy (in//"[0]", out, verbose-)
		time | scan (day, hms, date)
		inextn = in // "[" // im1 // "]"
		outextn = out // "[append,inherit]"
		switch (ncoeff) {
		case 1, 2: {
		    imcopy (inextn, outextn, verbose-)
		    printf ("%s %s No crosstalk correction\n", date, hms) |
			scan (logstr)
		    }
		case 3: {
		    imexpr( "a-b*c", outextn, inextn,
			in//"["//im2//"]", coeff1, dims="auto", intype="real",
			outtype=outtype, refim="auto", bwidth=0,
			btype="nearest", bpixval=0.,rangecheck=yes,
			verbose=no, exprdb="none" )
		    printf ("%s %s Crosstalk is %s * %.3g\n", date, hms,
			im2, coeff1) | scan (logstr)
		    }
		case 4: {
		    imexpr( "a-(b*c+d)", outextn, inextn,
			in//"["//im2//"]", coeff1, coeff2, dims="auto",
			intype="real", outtype=outtype, refim="auto",
			bwidth=0, btype="nearest", bpixval=0.,rangecheck=yes,
			verbose=no, exprdb="none")
		    printf ("%s %s Crosstalk is %s * %.3g + %.3g\n",
			date, hms, im2, coeff1, coeff2) | scan (logstr)
		    }
		}
		hedit (out//"["//im1//"]", "XTALKCOR", logstr,
		    add+, del-, ver-, show-, upd+)
		if (verbose)
		    printf ("%s: %s\n", inextn, logstr)
		if (logfile != "")
		    printf ("%s: %s\n", inextn, logstr, >> logfile)
	    }
	    fd2 = ""

	    if (out == tmpout) {
		ccddelete (in)
		if (defvar (in))
		    imrename (out, "./"//in, verbose-)
		else
		    imrename (out, in, verbose-)
	    }
	}
	fd1 = ""; delete (list1, verify-)
end
