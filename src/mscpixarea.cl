# MSCPIXAREA -- Compute/correct pixel area factors.

procedure mscpixarea (input, output)

string	input			{prompt="List of input MEF files"}
string	output			{prompt="List of output MEF files"}
string	outtype = "multiply"	{prompt="Output type",
				 enum="area|multiply|divide"}

real	norm = INDEF		{prompt="Area normalization"}
bool	verbose = no		{prompt="Verbose output?"}

struct	*fd1

begin
	file	inlist, outlist, extlist
	string	in, out, ext
	int	nin

	cache sections, mscextensions

	inlist = mktemp ("tmp$iraf")
	outlist = mktemp ("tmp$iraf")
	extlist = mktemp ("tmp$iraf")

	sections (input, option="fullname", >> extlist)
	nin = sections.nimages
	sections (output, option="fullname", >> outlist)
	if (sections.nimages != nin)
	    error (1, "Input and output lists don't match")

	joinlines (extlist, outlist, output=inlist, delim=" ",
	    maxchars=161, verbose-)
	delete (extlist, verify-)
	delete (outlist, verify-)

	if (verbose) {
	    printf ("MSCPIXAREA:  "); time
	}

	fd1 = inlist
	while (fscan (fd1, in, out) != EOF) {
	    if (imaccess (out//"[0]")) {
		printf ("WARNING: Output already exists (%s)\n", out)
		next
	    }

	    if (verbose)
		printf ("  %s: output=%s, outtype=%s\n", in, out, outtype)

	    # Input list of extensions.
	    mscextensions (in, output="file", index="1-", extname="", extver="",
		lindex+, lname-, lver-, ikparams="", > extlist)
	    nin = mscextensions.nimages

	    # Output list.
	    for (i=1; i<=nin; i+=1)
		printf ("%s[append,inherit]\n", out, >> outlist)

	    # Create output.
	    imcopy (in//"[0]", out, verbose-)
	    pixarea ("@"//extlist, "@"//outlist, outtype=outtype, norm=norm)

	    delete (extlist, verify-)
	    delete (outlist, verify-)
	}
	fd1 = ""; delete (inlist, verify-)
end
