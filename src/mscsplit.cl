# MSCSPLIT -- Split an MEF file into separate images.
# This routine stores the extension name for use with MSCJOIN.

procedure mscsplit (input)

string	input			{prompt="List of input MEF files"}
string	output = ""		{prompt="List of output root names"}
string	mefext = ".fits"	{prompt="MEF filename extension"}
bool	delete = no		{prompt="Delete MEF file after splitting?"}
bool	verbose = no		{prompt="Verbose?"}

struct	*fd1, *fd2

begin
	file	inlist, extlist, in, out, inext, outext
	int	index, extver
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

	# Split each input.
	fd1 = inlist
	while (fscan (fd1, in, out) != EOF) {
	    # If no output rootname is given use the input name.
	    if (nscan() == 1)
		out = in

	    # Strip the mefext if present.
	    index = strlen (out) - strlen (mefext) + 1
	    if (substr (out, index, 1000) == mefext)
		out = substr (out, 1, index-1)

	    # Check for the existance of the input and output.
	    if (!imaccess (in//"[0]")) {
		printf ("WARNING: Can't access input (%s)\n", in)
		next
	    }
	    if (imaccess (out//"_1")) {
		printf ("WARNING: Output already exists (%s_*)\n", out)
		next
	    }

	    # Expand the extensions.
	    mscextensions (in, output="file", index="", extname="", extver="",
		lindex=no, lname=yes, lver=no, ikparams="", > extlist)
	    if (mscextensions.nimages == 0) {
		printf ("WARNING: No extensions found (%s)\n", in)
		delete (extlist, verify-)
		next
	    }

	    # Copy the primary HDU.
	    imcopy (in//"[0]", out//"_0", verbose=verbose)

	    # Split the extensions.
	    index = 0
	    fd2 = extlist
	    while (fscan (fd2, inext) != EOF) {
		index = index + 1
		outext = out // "_" // index
		imcopy (inext, outext, verbose=verbose)

		# Set the extname/extver so that output images can be
		# joined later.
		hselect (inext, "extname", yes) | scan (extname)
		if (extname != "")
		    hedit (outext, "extnm", extname, add+,
			del-, verify-, show-, update+)
		hselect (inext, "extver", yes) | scan (extver)
		if (nscan() == 1)
		    hedit (outext, "extvr", extver, add+,
			del-, verify-, show-, update+)
	    }
	    fd2 = ""; delete (extlist, verify-)

	    if (delete)
		imdelete (in, verify-)
	}
	fd1 = ""; delete (inlist, verify-)
end
