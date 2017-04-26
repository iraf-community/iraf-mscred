# MSCPUPIL -- Fit and subtract the pupil image.

procedure mscpupil (input, output)

begin
	file	inlist, outlist, list, in, out
	string	inext
	int	nimages

	cache	sections, mscextensions
	inlist = mktemp ("tmp$iraf")
	outlist = mktemp ("tmp$iraf")
	list = mktemp ("tmp$iraf")

	# Get input query parameters.  Expand operand and result lists.
	sections (input, option="fullname", > inlist); 
	nimages = sections.nimages
	sections (output, option="fullname", > outlist)

	# Check for correct lists.
	if (nimages != sections.nimages && sections.nimages != 0) {
	    delete (inlist, verify-)
	    delete (outlist, verify-)
	    error (1, "Input and output lists don't match")
	}

	joinlines (inlist, outlist, output=list, delim=" ", missing="",
	    maxchars=161, shortest-, verbose-)
	delete (inlist, verify-)
	delete (outlist, verify-)

	if (verbose)
	    printf ("MSCPUPIL:\n")

	out = ""
	fd = list
	while (fscan (fd, in, out) != EOF) {
	    if (verbose) {
		if (out != "")
		    printf (" %s -> %s:\n", in, out)
		else
		    printf (" %s:\n", in)
	    }
	    mscextensions (in, output="list", index="0-",
		extname="", extver="", lindex=no, lname=yes,
		lver=no, ikparams="") | scan (inext)
	    if (mscextensions.nimages == 0) {
	    printf ("WARNING: File not found or contains no extensions (%s)\n",
		    in)
		next
	    }
	    mscextensions (in, output="file", index="0-",
		extname="", extver="", lindex=no, lname=yes,
		lver=no, ikparams="")
	    mscextensions (in, output="file", index="0-",
		extname="", extver="", lindex=no, lname=yes,
		lver=no, ikparams="", > inlist)
	    nimages = mscextensions.nimages

	    if (out == "") {
		pupilfit ("@"//inlist, "", masks=masks, lmedian=lmedian,
		    type=type, xc=xc, yc=yc, rin=rin, drin=drin, rout=rout,
		    drout=drout, funcin=funcin, orderin=orderin,
		    funcout=funcout, orderout=orderout,
		    rfunction=rfunction, rorder=rorder,
		    sfunction="chebyshev", sorder=1, niterate=niterate,
		    lreject=lreject, hreject=hreject, datamin=datamin,
		    datamax=datamax, verbose=verbose)
	    } else if (in == out) {
		pupilfit ("@"//inlist, "@"//inlist, masks=masks,
		    lmedian=lmedian, type=type, xc=xc, yc=yc, rin=rin,
		    drin=drin, rout=rout, drout=drout, funcin=funcin,
		    orderin=orderin, funcout=funcout, orderout=orderout,
		    rfunction=rfunction, rorder=rorder,
		    sfunction="chebyshev", sorder=1, niterate=niterate,
		    lreject=lreject, hreject=hreject, datamin=datamin,
		    datamax=datamax, verbose=verbose)
	    } else {
		if (imaccess (out//"[0]")) {
		    printf ("WARNING: Output %s already exists\n", out)
		    delete (inlist, verify-)
		    next
		}
		if (in != inext) {
		    for (i=1; i<=nimages; i+=1) {
			if (type == "mask")
			    print (out//"[append,inherit,type=mask]",
			        >> outlist)
			else
			    print (out//"[append,inherit]", >> outlist)
		    }
		    imcopy (in//"[0]", out, verbose-)
		} else {
		    if (type == "mask")
			print (out//"[append,inherit,type=mask]", >> outlist)
		    else
			print (out, > outlist)
		}
		pupilfit ("@"//inlist, "@"//outlist, masks=masks,
		    lmedian=lmedian, type=type, xc=xc, yc=yc, rin=rin,
		    drin=drin, rout=rout, drout=drout, funcin=funcin,
		    orderin=orderin, funcout=funcout, orderout=orderout,
		    rfunction=rfunction, rorder=rorder,
		    sfunction="chebyshev", sorder=1, niterate=niterate,
		    lreject=lreject, hreject=hreject, datamin=datamin,
		    datamax=datamax, verbose=verbose)
		delete (outlist, verify-)
	    }
	    delete (inlist, verify-)
	}
	fd = ""; delete (list, verify-)
end
