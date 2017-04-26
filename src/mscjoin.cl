# MSCJOIN -- Join a split MEF file into an MEF file.
# This routine uses the extension name stored by MSCSPLIT.

procedure mscjoin (input)

string	input			{prompt="List of input root names"}
string	output = ""		{prompt="List of output MEF names"}
bool	delete = no		{prompt="Delete input images after joining?"}
bool	verbose = no		{prompt="Verbose?"}

struct	*fd1, *fd2

begin
	file	inlist, extlist, in, out, inext, extver
	int	index
	struct	extname

	# Temporary files.
	inlist = mktemp ("tmp$iraf")
	extlist = mktemp ("tmp$iraf")

	# Expand input and output lists.  Allow missing or short output list.
	sections (input, option="fullname", > extlist)
	sections (output, option="fullname") | joinlines (extlist, "STDIN",
	    output=inlist, delim=" ", missing="", maxchars=161,
	    shortest-, verbose-)
	delete (extlist, verify-)

	# Join each input.
	fd1 = inlist
	while (fscan (fd1, in, out) != EOF) {
	    # If no output rootname is given use the input name.
	    if (nscan() == 1)
		out = in

	    # Check for the existance of the input and  output.
	    if (!imaccess (in//"_0") || !imaccess (in//"_1")) {
		printf ("WARNING: Can't access %s_0 or %s_1\n", in, in)
		next
	    }
	    if (imaccess (out//"[0]")) {
		printf ("WARNING: Output already exists (%s)\n", out)
		next
	    }

	    # Copy the primary HDU.
	    imcopy (in//"_0[0]", out, verbose=verbose)

	    # Join the extensions.
	    for (index = 1;; index+=1) {
		inext = in // "_" // index
		if (!imaccess (inext))
		    break
		hselect (inext, "extnm", yes) | scan (extname)
		if (extname == "")
		    extname = "im" // index
		hselect (inext, "extvr", yes) | scan (extver)
		if (nscan() == 0)
		    imcopy (inext,
			out//"["//extname//",append,inherit]",
			verbose=verbose)
		else
		    imcopy (inext,
			out//"["//extname//","//extver//",append,inherit]",
			verbose=verbose)
		hedit (out//"["//extname//"]", "extnm,extvr",
		    add-, del+, verify-, update+, show-)
	    }

	    if (delete)
		imdelete (in//"_*", verify-)
	}
	fd1 = ""; delete (inlist, verify-)
end
