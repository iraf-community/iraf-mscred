# MSCTOSHORT -- Convert Mosaic data to scaled short.

procedure msctoshort (input, output)

string	input			{prompt="List of input Mosaic files"}
string	output			{prompt="List of output Mosaic files"}
real	maxbscale = INDEF	{prompt="Maximum BSCALE allowed\n"}

struct	*fd1, *fd2

begin
	file	inlist, extlist, temp1, temp2
	file	in, out, inextn, outextn
	int	nextn
	struct	extname

	inlist = mktemp ("tmp$iraf")
	extlist = mktemp ("tmp$iraf")
	temp1 = mktemp ("tmp") // ".fits"
	temp2 = mktemp ("tmp") // ".fits"

	# Query parameters.
	in = input
	out = output

	# Expand lists.
	sections (in, option="fullname", > extlist)
	nextn = sections.nimages
	sections (out, option="fullname") | joinlines (extlist, "STDIN",
	    output=inlist, delim=" ", missing="", maxch=181, shortest-,
	    verbose-)
	delete (extlist, verify-)
	if (sections.nimages > 0 && sections.nimages != nextn) {
	    delete (inlist, verify-)
	    error (1, "Input and output lists don't match")
	}

	fd1 = inlist
	while (fscan (fd1, in, out) != EOF) {
	    if (nscan() == 1)
		out = in
	    if (out == in)
		out = temp2
	    i = strlen (out)
	    if (i <= 5 || substr(out,i-4,i) != ".fits")
		out = out // ".fits"

	    imcopy (in//"[0]", out, verbose+)

	    mscextensions (in, output="file", index="1-", extname="", extver="",
		lindex-, lname+, lver-, ikparams="noinherit", > extlist)

	    fd2 = extlist
	    nextn = 0
	    while (fscan (fd2, inextn) != EOF) {
		nextn = nextn + 1
		printf ("%s[%d]\n", out, nextn) | scan (outextn)
		hselect (inextn, "extname", yes) | scan (extname)

		printf ("%s -> %s\n", inextn, outextn)
		toshort (inextn, temp1, bpmasks="", datamin=INDEF,
		    datamax=INDEF, maxbscale=maxbscale, logfile="")
		fxcopy (temp1, out, groups="", new-, verbose+)
		imdelete (temp1, verify-)
		hedit (outextn, "extname", extname,
		    add+, del-, verify-, show+, update+)
#		hedit (outextn, "inherit", "T",
#		    add+, del-, verify-, show+, update+)
	    }
	    fd2 = ""; delete (extlist, verify-)

	    if (out == temp2) {
		imdelete (in, verify-)
		imrename (out, in, verbose+)
	    }
	}
	fd1 = ""; delete (inlist, verify-)
end
