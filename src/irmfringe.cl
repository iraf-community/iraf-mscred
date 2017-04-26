# RMFRINGE -- Remove fringe template.

procedure rmfringe (input, output, template)

string	input			{prompt="List of input mosaic exposures"}
string	output			{prompt="List of output mosaic exposures"}
file	template		{prompt="Template mosaic exposure"}

string	extname = ""		{prompt="Extensions for fit"}
int	blkavg = 4		{prompt="Block average factor"}
real	scale = 0.5		{prompt="Scale"}
bool	interactive = yes	{prompt="Interactive?"}
bool	mscexam = no		{prompt="Examine corrections with MSCEXAM?"}
bool	verbose = yes		{prompt="Verbose output?"}

real	newscale = 1.	{prompt="Scale (0=done, -1=abort, -2=new blkavg)",
					mode="q"}
int	newblk			{prompt="New block average factor", mode="q"}

struct	*fd1, *fd2, *fd3

begin
	bool	mef
	file	in, out, tmplt
	file	tmpout, im1, im2, im3, im4, im5
	file	meflist, alllist, inlist, temp, temp2, inblk, outblk, tblk
	string	masks
	int	blk, nsum, nextn
	real	s, s2, sbest, psky1, psum1, psky2, psum2, pmax2
	real	xc, yc, rin, drin, rout, drout
	struct	pupilpar, logstr

	cache	mscextensions

	# Define temporary files and images.
	meflist = mktemp ("tmp$iraf")
	alllist = mktemp ("tmp$iraf")
	inlist = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")
	tmpout = mktemp ("tmp")

	# Expand lists.
	sections (input, option="fullname", > inlist)
	nsum = sections.nimages
	sections (output, option="fullname", > temp)
	tmplt = template

	joinlines (inlist, temp, output=meflist, delim=" ", missing="",
	    maxchars=161, shortest-, verbose-)
	delete (inlist, verify-)
	delete (temp, verify-)

	if (sections.nimages != 0 && nsum != sections.nimages) {
            delete (meflist, verify-)
	    error (1, "Input and output lists do not match")
	}
	if (!imaccess (tmplt//"[1]")) {
            delete (meflist, verify-)
	    error (1, "Can't access template image ("//tmplt//")")
	}

	if (verbose) {
	    printf ("RMFRINGE: ")
	    time
	}

	fd3 = meflist
	while (fscan (fd3, in, out) != EOF) {
	    if (nscan() == 1)
		out = in

	    # Check images.
	    if (!imaccess (in//"[1]")) {
		print ("  WARNING: Can't access input image ("//in//")")
		next
	    }
	    logstr = ""
	    hselect (in//"[1]", "FRINGCOR", yes) | scan (logstr)
	    if (logstr != "") {
		print ("  WARNING: Data already corrected ("//in//")")
		next
	    }
	    if (out == in)
		out = tmpout
	    else if (imaccess (out//"[1]")) {
		print ("  WARNING: Output image already exists ("//out//")")
		next
	    }

	    # Expand extensions.
	    mscextensions (in, output="file", index="0-", extname="", extver="",
		    lindex=no, lname=yes, lver=no, ikparams="", > temp)
	    mef = mscextensions.imext
	    mscextensions (tmplt, output="file", index="0-", extname="",
		extver="", lindex=no, lname=yes, lver=no, ikparams="") |
	    joinlines (temp, "STDIN", output=alllist, delim=" ",
		missing="Missing", maxchars=161, shortest+, verbose-)
	    delete (temp, verify-)
	    nextn = mscextensions.nimages

	    # Initial scale.
	    s = scale

	    # Block average to make things go faster.
	    if (interactive)
		blk = blkavg
	    else
		blk = 1

newblk:
	    mscextensions (in, output="file", index="0-", extname=extname,
		extver="", lindex=no, lname=yes, lver=no, ikparams="", > temp)
	    if (mscextensions.nimages == 0) {
		printf ("WARNING: ")
		printf ("No extensions in list, using all extensions\n")
		extname = ""
		delete (temp, verify-)
		mscextensions (in, output="file", index="0-", extname=extname,
		    extver="", lindex=no, lname=yes, lver=no, ikparams="",
		    > temp)
	    }
	    mscextensions (tmplt, output="file", index="0-", extname=extname,
		extver="", lindex=no, lname=yes, lver=no, ikparams="") |
	    joinlines (temp, "STDIN", output=inlist, delim=" ",
		missing="Missing", maxchars=161, shortest+, verbose-)
	    delete (temp, verify-)

	    if (blk > 1) {
		printf ("Block averaging %s and %s by a factor of %d ...\n",
		    in, tmplt, blk)
		inblk = mktemp ("tmp")
		tblk = mktemp ("tmp")
		outblk = mktemp ("tmp")
		fd1 = inlist
		if (mef) {
		    imcopy (in//"[0]", inblk, verbose-)
		    imcopy (tmplt//"[0]", tblk, verbose-)
		    while (fscan (fd1, im1, im2) != EOF) {
			blkavg (im1, inblk//"[append,inherit]", blk, blk,
			    option = "average")
			blkavg (im2, tblk//"[append,inherit]", blk, blk,
			    option = "average")
		    }
		} else {
		    while (fscan (fd1, im1, im2) != EOF) {
			blkavg (im1, inblk, blk, blk, option = "average")
			blkavg (im2, tblk, blk, blk, option = "average")
		    }
		}
		fd1 = ""
	    } else {
		inblk = in
		tblk = tmplt
		outblk = out
	    }

	    # Expand block average extensions.
	    rename (inlist, temp, field="all")
	    mscextensions (inblk, output="file", index="0-", extname=extname,
		extver="", lindex=no, lname=yes, lver=no, ikparams="") |
	    joinlines (temp, "STDIN", output=inlist, delim=" ",
		missing="Missing", maxchars=161, shortest+, verbose-)
	    delete (temp, verify-)
	    rename (inlist, temp, field="all")
	    mscextensions (tblk, output="file", index="0-", extname=extname,
		extver="", lindex=no, lname=yes, lver=no, ikparams="") |
	    joinlines (temp, "STDIN", output=inlist, delim=" ",
		missing="Missing", maxchars=161, shortest+, verbose-)
	    delete (temp, verify-)
	    sbest = s

	    # Scale loop.
	    if (interactive) {
		printf ("Displaying %s ...\n", in)
		mscdisplay (inblk, 1, extname=extname, >> "dev$null")
		sbest = INDEF
		while (s > 0.) {
		    printf ("Scaling %s by %.3g and subtracting from %s ...\n",
			tmplt, s, in)
		    imdelete (outblk, verify-, >& "dev$null")
		    if (mef)
			imcopy (inblk//"[0]", outblk, verbose-)
		    fd1 = inlist
		    while (fscan (fd1, im1, im2, im3, im4) != EOF) {
			imexpr ("a-(c*b)", outblk//"[append,inherit]",
			    im3, im4, s, dims="auto", intype="auto",
			    outtype="real", refim="auto", bwidth=0,
			    btype="nearest", bpixval=0., rangecheck+,
			    verbose-, exprdb="none")
		    }
		    fd1 = ""
		    printf ("Displaying corrected version of %s ...\n", in)
		    mscdisplay (outblk, 2, extname=extname, >> "dev$null")
		    if (mscexam) {
			printf ("Entering MSCEXAM (quit with 'q') ...\n")
			mscexamine
		    }
		    sbest = s

		    newscale = s
		    s = newscale
		}
	    }
	    delete (inlist, verify-)
	    if (inblk != in)
		imdelete (inblk, verify-)
	    if (tblk != tmplt)
		imdelete (tblk, verify-)
	    if (s == -1) {
		sbest = INDEF
		imdelete (outblk, verify-)
	    } else if (s == -2) {
		imdelete (outblk, verify-)
		newblk = blk
		blk = newblk
		s = sbest
		goto newblk
	    }

	    # Create output corrected image.
	    if (sbest!=INDEF &&
		(!interactive  || outblk!=out ||
		mscextensions.nimages!=nextn)) {
		if (verbose) {
		    if (out == tmpout)
			printf ("  Correcting image %s with scale %.3g ...\n",
			    in, sbest)
		    else
			printf ("  Creating output image %s with scale %.3g ...\n",
			    out, sbest)
		}

		if (interactive || outblk != out)
		    imdelete (outblk, verify-)
		if (mef)
		    imcopy (in//"[0]", out, verbose-)
		fd1 = alllist
		while (fscan (fd1, im1, im2) != EOF) {
		    imexpr ("a-(c*b)", out//"[append,inherit]", im1, im2,
			sbest, dims="auto", intype="auto", outtype="real",
			refim="auto", bwidth=0, btype="nearest",
			bpixval=0., rangecheck+, verbose-, exprdb="none")
		}
		fd1 = ""

printf ("hedit $input fringcor \"[%s]-(%.4g*[%s])\" add+ del- ver- upd+ show-\n",
			in, sbest, tmplt) | scan (logstr)
		msccmd (logstr, out, extname="", alist-, flist-, verbose-,
		    exec+)
		if (out == tmpout) {
		    imdelete (in, verify-)
		    imrename (out, in, verbose-)
		}
	    }
	    delete (alllist, verify-)
	}
	fd3 = ""; delete (meflist, verify-)
end
