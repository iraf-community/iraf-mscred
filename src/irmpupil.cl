# RMPUPIL -- Remove pupil template.

procedure rmpupil (input, output, template)

string	input			{prompt="List of input mosaic exposures"}
string	output			{prompt="List of output mosaic exposures"}
file	template		{prompt="Template mosaic exposure"}

string	type = "difference"	{prompt="Type of removal",
				 enum="difference|ratio"}
string	extname = "im[2367]"	{prompt="Extensions for fit"}
int	blkavg = 8		{prompt="Block average factor"}
real	fudge = 1.6		{prompt="Fudge factor"}
real	scale = INDEF		{prompt="Scale (INDEF for automatic estimate)"}
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
	int	blk, blkt, nsum, nextn
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
	    printf ("RMPUPIL: ")
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
	    hselect (in//"[1]", "RMPUPIL", yes) | scan (logstr)
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
	    if (interactive || s == INDEF)
		blk = blkavg
	    else
		blk = 1
	    if (interactive)
		blkt = blkavg
	    else
		blkt = 1

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

	    # Block average data.
	    if (blk > 1) {
		printf ("Block averaging %s by a factor of %d ...\n", in, blk)
		inblk = mktemp ("tmp")
		fd1 = inlist
		if (mef) {
		    imcopy (in//"[0]", inblk, verbose-)
		    while (fscan (fd1, im1, im2) != EOF)
			blkavg (im1, inblk//"[append,inherit]", blk, blk,
			    option = "average")
		} else {
		    while (fscan (fd1, im1, im2) != EOF)
			blkavg (im1, inblk, blk, blk, option = "average")
		}
		fd1 = ""
	    } else
		inblk = in

	    # Block average template.
	    if (blkt > 1) {
		printf ("Block averaging %s by a factor of %d ...\n",
		    tmplt, blkt)
		tblk = mktemp ("tmp")
		outblk = mktemp ("tmp")
		fd1 = inlist
		if (mef) {
		    imcopy (tmplt//"[0]", tblk, verbose-)
		    while (fscan (fd1, im1, im2) != EOF)
			blkavg (im2, tblk//"[append,inherit]", blk, blk,
			    option = "average")
		} else {
		    while (fscan (fd1, im1, im2) != EOF)
			blkavg (im2, tblk, blk, blk, option = "average")
		}
		fd1 = ""
	    } else {
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

	    # Automatic estimate.
	    if (verbose)
		printf ("  Estimating scale factor for %s ...\n", in)

	    if (s == INDEF) {
		sbest = 0
		nsum = 0
		fd1 = inlist
		while (fscan (fd1, im1, im2, im3, im4) != EOF) {
		    hselect (im4, "pupilsky,pupilsum,pupilpar", yes) |
			scan (psky1, psum1,pupilpar)
		    if (nscan() != 3)
			next
		    if (fscan (pupilpar, xc, yc, rin, drin, rout, drout,
			masks) < 6)
			next
		    pupilfit (im3, "", masks=masks, type="fit", xc=xc, yc=yc,
			rin=rin, drin=drin, rout=rout, drout=drout,
			abin=360., rorder=10, verbose-) |
			scan (psky2, psum2, pmax2)
		    if (nscan() != 3)
			next
		    if (type == "difference") {
			s = psum2 / psum1 * blk**2
			s = s / fudge		# fudge factor
		    } else {
			s = psum2 / psum1 / psky2 * blk**2
			s = s / fudge		# fudge factor
		    }

		    sbest = sbest + s
		    nsum = nsum + 1
		}
		if (nsum > 0) {
		    sbest = sbest / nsum
		    printf ("%.3g\n", sbest) | scan (sbest)
		}
		if (sbest <= 0.) {
		    if (type == "ratio")
			sbest = 0.0001
		    else
			sbest = 1.
		}
	    } else
		sbest = s
	    s = sbest

	    # Scale loop.
	    if (interactive) {
		printf ("Displaying %s ...\n", in)
		mscdisplay (inblk, 1, extname=extname,  >> "dev$null")
		sbest = INDEF
		while (s > 0.) {
		    if (type == "difference")
			printf ("Scaling %s by %.3g and subtracting from %s ...\n",
			    tmplt, s, in)
		    else
			printf ("Scaling %s by %.3g and dividing from %s ...\n",
			    tmplt, s, in)
		    imdelete (outblk, verify-, >& "dev$null")
		    if (mef)
			imcopy (inblk//"[0]", outblk, verbose-)
		    fd1 = inlist
		    while (fscan (fd1, im1, im2, im3, im4) != EOF) {
			if (type == "difference")
			    imexpr ("a-(c*b)", outblk//"[append,inherit]",
				im3, im4, s, dims="auto", intype="auto",
				outtype="real", refim="auto", bwidth=0,
				btype="nearest", bpixval=0., rangecheck+,
				verbose-, exprdb="none")
			else
			    imexpr ("a/(c*b+1)", outblk//"[append,inherit]",
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
		blkt = newblk
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
		    if (type == "difference") {
			imexpr ("a-(c*b)", out//"[append,inherit]", im1, im2,
			    sbest, dims="auto", intype="auto", outtype="real",
			    refim="auto", bwidth=0, btype="nearest",
			    bpixval=0., rangecheck+, verbose-, exprdb="none")
		    } else {
			imexpr ("a/(c*b+1)", out//"[append,inherit]", im1, im2,
			    sbest, dims="auto", intype="auto", outtype="real",
			    refim="auto", bwidth=0, btype="nearest",
			    bpixval=0., rangecheck+, verbose-, exprdb="none")
		    }
		}
		fd1 = ""

		if (type == "difference")
printf ("hedit $input rmpupil \"[%s]-(%.4g*[%s])\" add+ del- ver- upd+ show-\n",
			in, sbest, tmplt) | scan (logstr)
		else
printf ("hedit $input rmpupil \"[%s]/(%.4g*[%s]+1)\" add+ del- ver- upd+ show-\n",
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
