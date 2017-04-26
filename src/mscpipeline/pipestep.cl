# PIPESTEP - Run pipeline step.

procedure pipestep (command, input, imkey, logkey)

string	command			{prompt="Command"}
file	input			{prompt="Input filename"}
string	imkey			{prompt="Image header keyword"}
string	logkey			{prompt="Logfile keyword"}

begin
	file	in, temp
	string	cmd, ikey, lkey, str1
	struct	str2

	cmd = command
	in = input
	ikey = imkey
	lkey = logkey

	temp = mktemp ("tmp$iraf")

	# Check if step has been done.
	if (in != "" && ikey != "") {
	    hselect (in//"[0]", ikey, yes) | scan (str1, str2)
	    if (nscan() != 0)
		return
	}

	# Initialize logfile.
	if (logfile != "") {
	    if (!access (logfile))
		xlog ("<HTML><BODY><PRE>", output=logfile)
	}
		
	# Log start of command.
	if (lkey != "") {
	    xlog ("</PRE><H2>", lkey//":", in, "TIME", "</H2><PRE>",
		output=logfile)
	    lpar (cmd) | cmdstr (cmd, hidden+, > temp)
	    xlog ("@"//temp, output=logfile)
	    xlog ("\n", output=logfile)
	    delete (temp, verify-)
	}

	xlog ("*1*", "TIME", "run ", in, cmd, output=pipelog)
	dpar (cmd) |
	    xlog ("*2*", "TIME", "pars", in, cmd, "@STDIN", output=pipelog)

	# Run command.
	printf ("%s (mode='h', >> \"%s\")\n", cmd, logfile) | cl
	if (in != "" && ikey != "") {
	    xlog ("TIME", cmd, output="STDOUT") | scan (str2)
	    hedit (input//"[0]", ikey, str2, add+, verify-, show-, update+)
	}

	# Log end of command.
	xlog ("*1*", "TIME", "done", in, cmd, output=pipelog)
end
