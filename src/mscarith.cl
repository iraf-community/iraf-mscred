# MSCARITH -- Image arithmetic on multiextension Mosaic files.

procedure mcsarith (operand1, op, operand2, result)

string	operand1	{prompt="Operand image or numerical constant"}
string	op		{prompt="Operator", enum="+|-|*|/|min|max"}
string	operand2	{prompt="Operand image or numerical constant"}
string	result		{prompt="Resultant image"}
string	extname = ""	{prompt="Extension names to select"}
string	title = ""	{prompt="Title for resultant image"}
real	divzero = 0.	{prompt="Replacement value for division by zero"}
string	hparams = ""	{prompt="List of header parameters"}
string	pixtype = ""	{prompt="Pixel type for resultant image"}
string	calctype = ""	{prompt="Calculation data type"}
bool	verbose = no	{prompt="Print operations?"}
bool	noact = no	{prompt="Print operations without performing them?\n"}

struct	*fd1, *fd2, *fd3

begin
	bool	mef1, mef2
	file	op1list, op2list, reslist, op1exts, op2exts, resexts
	file	out, opval1, opval2, result1
	string	op1, optype, op2, hp
	int	nop1, nop2, nresult, next1, next2, nexts, n
	real	ccdmean

	cache	sections, mscextensions
	op1list = mktemp ("tmp$iraf")
	op2list = mktemp ("tmp$iraf")
	reslist = mktemp ("tmp$iraf")
	op1exts = mktemp ("tmp$iraf")
	op2exts = mktemp ("tmp$iraf")
	resexts = mktemp ("tmp$iraf")
	ccdmean = 0.

	# Get input query parameters.  Expand operand and result lists.
	sections (operand1, option="fullname", > op1list); 
	nop1 = sections.nimages
	optype = op
	sections (operand2, option="fullname", > op2list)
	nop2 = sections.nimages
	sections (result, option="fullname", > reslist)
	nresult = sections.nimages

	# Check for correct lists.
	if (nresult<1 || (nop1!=nresult&&nop1!=1) || (nop2!=nresult&&nop2!=1))
	    goto listerr

	# Create each output file.
	fd1 = op1list
	fd2 = op2list
	fd3 = reslist
	while (fscan (fd3, out) != EOF) {
	    n = strlen (out)
	    if (substr (out, n-4, n) == ".fits")
		out = substr (out, 1, n-5)

	    # Expand the operand files into a list of extensions.
	    hp = ""
	    next1 = INDEF
	    n = fscan (fd1, opval1)
	    if (imaccess (opval1)) {
		n = strlen (opval1)
		if (substr (opval1, n-4, n) == ".fits")
		    opval1 = substr (opval1, 1, n-5)
		mscextensions (opval1, output="file", index="0-",
		    extname=extname, extver="", lindex=no, lname=yes,
		    lver=no, ikparams="", > op1exts)
		next1 = mscextensions.nimages
		mef1 = mscextensions.imext
		if (next1 < 1)
		    goto exterr
		nexts = next1
		op1 = "@" // op1exts
		if (next1 > 1) {
		    hselect (op1, "ccdmean", yes) | scan (ccdmean)
		    if (nscan() > 0) {
			hedit (op1, "ccdmntmp", "(ccdmean)", add+, del-,
			    verify-, show-, update+)
			if (hp == "")
			    hp = "ccdmntmp,"//hparams
		    }
		}
	    } else
		op1 = opval1

	    next2 = INDEF
	    n = fscan (fd2, opval2)
	    if (imaccess (opval2)) {
		n = strlen (opval2)
		if (substr (opval2, n-4, n) == ".fits")
		    opval2 = substr (opval2, 1, n-5)
		mscextensions (opval2, output="file", index="0-",
		    extname=extname, extver="", lindex=no, lname=yes,
		    lver=no, ikparams="", > op2exts)
		next2 = mscextensions.nimages
		mef2 = mscextensions.imext
		if (next2 < 1 || (next1 != INDEF && next2 != next1))
		    goto exterr
		nexts =  next2
		op2 = "@" // op2exts
		if (next2 > 1) {
		    hselect (op2, "ccdmean", yes) | scan (ccdmean)
		    if (nscan() > 0) {
			hedit (op2, "ccdmntmp", "(ccdmean)", add+, del-,
			    verify-, show-, update+)
			if (hp == "")
			    hp = "ccdmntmp,"//hparams
		    }
		}
	    } else
		op2 = opval2

	    # Allow output to be the same as one of the input operands.
	    if ((out == opval1 || out == opval2) && !noact)
		result1 = mktemp ("tmp")
	    else
		result1 = out

	    # Create the global output header.
	    if (imaccess (opval1)) {
		if (!noact && mef1)
		    imcopy (opval1//"[0]", result1, verbose-)
	    } else if (imaccess (opval2)) {
		if (!noact && mef2)
		    imcopy (opval2//"[0]", result1, verbose-)
	    } else
		goto listerr
	    if (imaccess (result1))
		hedit (result1, "ccdmean,ccdmeant", add-, addonly-,
		    del+, verify-, show-, update+)

	    # Create the output extension list.
	    if (imaccess (result1) || noact) {
		for (n=1; n<=nexts; n=n+1)
			print (result1//"[inherit]", >> resexts)
	    } else
		print (result1, >> resexts)

	    # Do the arithmetic.
	    if (hp == "")
		imarith (op1, optype, op2, "@"//resexts, title=title,
		    divzero=divzero, hparams=hparams, pixtype=pixtype,
		    calctype=calctype, verbose=verbose, noact=noact)
	    else
		imarith (op1, optype, op2, "@"//resexts, title=title,
		    divzero=divzero, hparams=hp, pixtype=pixtype,
		    calctype=calctype, verbose=verbose, noact=noact)

	    delete (resexts, verify-, >& "dev$null")
	    mscextensions (result1, output="file", index="0-",
		extname=extname, extver="", lindex=no, lname=yes,
		lver=no, ikparams="", > resexts)
	    if (nexts > 1) {
		hselect (op1, "ccdmntmp", yes) | scan (ccdmean)
		if (nscan() > 0) {
		    hedit ("@"//resexts, "ccdmean", "(ccdmntmp)", add+, del-,
			update+, show-, verify-)
		    hedit ("@"//resexts, "ccdmntmp,ccdmeant", add-, addonly-,
		        del+, update+, show-, verify-)
		}
	    }

	    # Delete temporary lists.
	    delete (op1exts, verify-, >& "dev$null")
	    delete (op2exts, verify-, >& "dev$null")
	    delete (resexts, verify-, >& "dev$null")

	    # If the result is the same as an input replace the input.
	    if (imaccess (result1)) {
		if (out == opval1) {
		    imdelete (opval1, verify-)
		    if (defvar (opval1))
			imrename (result1, "./"//opval1, verbose-)
		    else
			imrename (result1, opval1, verbose-)
		} else if (out == opval2) {
		    imdelete (opval2, verify-)
		    if (defvar (opval2))
			imrename (result1, "./"//opval2, verbose-)
		    else
			imrename (result1, opval2, verbose-)
		}
	    }
	}

	fd1 = ""; delete (op1list, verify-)
	fd2 = ""; delete (op2list, verify-)
	fd3 = ""; delete (reslist, verify-)
	return

listerr:
	fd1 = ""; delete (op1list, verify-)
	fd2 = ""; delete (op2list, verify-)
	fd3 = ""; delete (reslist, verify-)
	delete (op1exts, verify-, >& "dev$null")
	delete (op2exts, verify-, >& "dev$null")
	delete (resexts, verify-, >& "dev$null")
	error (1, "Error in operand and result lists")

exterr:
	fd1 = ""; delete (op1list, verify-)
	fd2 = ""; delete (op2list, verify-)
	fd3 = ""; delete (reslist, verify-)
	delete (op1exts, verify-, >& "dev$null")
	delete (op2exts, verify-, >& "dev$null")
	delete (resexts, verify-, >& "dev$null")
	error (1, "Error in number of extensions")
end
