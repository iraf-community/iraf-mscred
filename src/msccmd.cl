# MSCCMD -- Execute commands on each image extension of a multiextension file.

procedure msccmd (command, input, output)

string	command			{prompt="Command"}
string	input			{prompt="Input files"}
string	in2 = ""		{prompt="Second list of input files",
				 mode="q"}
string	output			{prompt="Output files"}
string	extname = ""		{prompt="Extension names"}
string	ikparams = ""		{prompt="Image kernel parameters\n"}

bool	alist = no		{prompt="Do all extensions as one list?"}
bool	flist = yes		{prompt="Do all extensions in one file as one list?"}
bool	dataless = no		{prompt="Include dataless image headers?"}
bool	verbose = no		{prompt="List commands to be executed?"}
bool	exec = yes		{prompt="Execute commands?\n"}

string	prompt			{prompt="msccmd", mode="q"}

struct	*fd1, *fd2, *fd3, *fd4, *fd5, *fd6, *fd7

begin
	bool	mef, doinput
	file	temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8
	file	infile, outfile, in, out, infile2, op2, dummy
	string	cmd, cmd1
	int	idx1, idx2, idx3

	cache	mscextensions

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")
	temp3 = mktemp ("tmp$iraf")
	temp4 = mktemp ("tmp$iraf")
	temp5 = mktemp ("tmp$iraf")
	temp6 = mktemp ("tmp$iraf")
	temp7 = mktemp ("tmp$iraf")
	temp8 = mktemp ("tmp$iraf")

	if ($nargs == 0) {
	    if (mode == "h")
		cmd = command
	    else {
		prompt = ""
		cmd = prompt
	    }
	} else
	    cmd = command

	while (!(cmd=="q" || cmd=="quit")) {
	    if (fscan (cmd, cmd1) == 0)
		goto newcmd
	    if (!deftask (cmd1)) {
		printf ("WARNING: Unknown task name `%s'\n", cmd1)
		goto newcmd
	    }

	    doinput = no
	    idx1 = 1
	    idx2 = idx1 + stridx ("$", cmd) - 1
	    idx3 = strlen (cmd)
	    while (idx2 >= idx1) {
		if (substr (cmd, idx2, idx2+5) == "$input") {
		    doinput = yes
		    break
		}
		idx1 = idx2 + 1
		if (idx1 < idx3) {
		    cmd1 = substr (cmd, idx1, idx3)
		    idx2 = idx1 + stridx ("$", cmd1) - 1
		}
	    }
	    if (!doinput) {
		print (cmd) | cl
		goto newcmd
	    }

	    if (alist)
		print (input, > temp1)
	    else
		sections (input, option="fullname", > temp1)

	    fd1 = temp1
	    while (fscan (fd1, infile) != EOF) {
		infile2 = ""
		out = ""
		
		if (alist || flist) {
		    mscextensions (infile, output="file", index="0-",
			extname=extname, extver="", lindex=no, lname=yes,
			lver=no, dataless=dataless, ikparams=ikparams, > temp4)
		    print ("@"//temp4, > temp3)
		} else
		    mscextensions (infile, output="file", index="0-",
			extname=extname, extver="", lindex=no, lname=yes,
			lver=no, dataless=dataless, ikparams=ikparams, > temp3)
		mef = mscextensions.imext

		fd3 = temp3
		while (fscan (fd3, in) != EOF) {
		    cmd1 = ""
		    idx1 = 1
		    idx2 = idx1 + stridx ("$", cmd) - 1
		    idx3 = strlen (cmd)
		    while (idx2 >= idx1) {
			cmd1 = cmd1 // substr (cmd, idx1, idx2-1)
			if (substr (cmd, idx2, idx2+5) == "$input") {
			    cmd1 = cmd1 // in
			    idx1 = idx2 + 6
			} else if (substr (cmd, idx2, idx2+6) == "$output") {
			    if (!access (temp2)) {
				sections (output, option="fullname", > temp2)
				fd2 = temp2
			    }
			    if (out == "") {
				if (fscan (fd2, outfile) == EOF)
				    error (1, "Error in output list")
				if (outfile == infile)
				    out = mktemp ("tmp")
				else
				    out = outfile
			    }
			    if (!imaccess (out) && mef)
				imcopy (infile//"[0]", out, verbose-)
			    if (alist || flist) {
				fd7 = temp4; touch (temp8)
				while (fscan (fd7, dummy) != EOF)
				    print (out//"[inherit]", >> temp8)
				fd7 = ""
				cmd1 = cmd1 // "@" // temp8
			    } else
				cmd1 = cmd1 // out // "[inherit]"
			    idx1 = idx2 + 7
			} else if (substr (cmd, idx2, idx2+3) == "$in2") {
			    if (!access (temp5)) {
				if (alist)
				    print (in2, > temp5)
				else
				    sections (in2, option="fullname", > temp5)
				fd4 = temp5
			    }
			    if (infile2 == "") {
				if (fscan (fd4, infile2) == EOF)
				    error (1, "Error in in2 list")
				if (alist || flist) {
				    mscextensions (infile2, output="file",
					index="0-", extname=extname, extver="",
					lindex=no, lname=yes, lver=no,
					dataless=dataless, ikparams=ikparams,
					> temp7)
				    print ("@"//temp7, > temp6)
				} else
				    mscextensions (infile2, output="file",
					index="0-", extname=extname, extver="",
					lindex=no, lname=yes, lver=no,
					dataless=dataless, ikparams=ikparams,
					> temp6)
				fd5 = temp6
			    }
			    if (fscan (fd5, op2) == EOF)
				error (1, "Error in operand2 list")
			    cmd1 = cmd1 // op2
			    idx1 = idx2 + 4
			} else {
			    cmd1 = cmd1 // "$"
			    idx1 = idx2 + 1
			}

			idx2 = idx1 + stridx ("$", substr (cmd, idx1, idx3)) - 1
		    }
		    cmd1 = cmd1 // substr (cmd, idx1, idx3)

		    if (verbose) {
			print (cmd1)
			if (access (temp4)) {
			    printf ("%s:\n", temp4)
			    type (temp4)
			}
			if (access (temp7)) {
			    printf ("%s:\n", temp7)
			    type (temp7)
			}
			if (access (temp8)) {
			    printf ("%s:\n", temp8)
			    type (temp8)
			}
		    }
		    if (exec)
			print (cmd1) | cl
		}
		if (access (temp3)) {
		    fd3 = ""; delete (temp3, verify-)
		    if (access (temp4))
			delete (temp4, verify-)
		}
		if (access (temp6)) {
		    fd5 = ""; delete (temp6, verify-)
		    if (access (temp7))
			delete (temp7, verify-)
		}
		if (access (temp8))
		    delete (temp8, verify-)

		if (out != "" && outfile == infile && imaccess (out)) {
		    imdelete (infile, verify-)
		    if (defvar (outfile))
			imrename (out, "./"//outfile, verbose-)
		    else
			imrename (out, outfile, verbose-)
		}
	    }
	    if (access (temp5)) {fd4 = ""; delete (temp5, verify-)}
	    if (access (temp2)) {fd2 = ""; delete (temp2, verify-)}
	    if (access (temp1)) {fd1 = ""; delete (temp1, verify-)}

newcmd:
	    if ($nargs > 0 || mode == "h")
		break
	    prompt = ""
	    cmd = prompt
	}
end
