# FFPUPILCOR -- Correct broad band flat field for the pupil image in mosaic
# data.  This uses a narrow band flat field as the template for the pupil
# ghost image.

procedure ffpupilcor (input, output, template)

file	input			{prompt="Input mosaic exposure"}
file	output			{prompt="Output mosaic exposure"}
file	template		{prompt="Template mosaic exposure"}

string	extname = "[2367]"	{prompt="Extensions for fit"}
file	statsec = "mscred$noao/kpno/4meter/ffpupilcor.dat"	{prompt="List of image sections"}
int	blkavg = 8		{prompt="Block average factor"}
real	radius = INDEF		{prompt="Correction circle radius (pixels)"}
real	xcenter = 0.		{prompt="Center of correction circle (pixels)"}
real	ycenter = 0.		{prompt="Center of correction circle (pixels)"}
bool	mscexam = no		{prompt="Examine corrections with MSCEXAM?"}
real	scale = 1.		{prompt="Scale (0=quit, -1=abort)", mode="q"}

struct	*fd1, *fd2, *fd3

begin
	file	in, out, tmplt
	file	tmpout, im1, im2, im3, im4, im5
	file	alllist, inlist, temp, inblk, outblk, tblk
	int	nextn
	real	mean, xc, yc, r, s, sbest, x, y
	struct	str
	string	junk

	cache	imextensions

	# Define temporary files and images.
	alllist = mktemp ("tmp$iraf")
	inlist = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")

	# Get query parameters.
	in = input
	out = output
	tmplt = template

	# Check images.
	if (!imaccess (in//"[1]"))
	    error (1, "Can't access input image ("//in//")")
	if (!imaccess (tmplt//"[1]"))
	    error (1, "Can't access template image ("//tmplt//")")
	if (imaccess (out//"[1]"))
	    error (1, "Output image already exists ("//out//")")

	# Check statsec
	if (!access (statsec))
	    error (1, "Can't access statistics file ("//statsec//")")

	# Expand extensions.
	imextensions (in, output="file", index="1-", extname="", extver="",
		lindex=no, lname=yes, lver=no, ikparams="", > alllist)
	imextensions (in, output="file", index="1-", extname=extname,
	    extver="", lindex=no, lname=yes, lver=no, ikparams="", > temp)
	imextensions (tmplt, output="file", index="1-", extname=extname,
	    extver="", lindex=no, lname=yes, lver=no, ikparams="") |
	joinlines (temp, "STDIN", output=inlist, delim=" ", missing="Missing",
	    maxchars=161, shortest+, verbose-)
	delete (temp, verify-)
	nextn = imextensions.nimages

	# Block average to make things go faster.
	if (blkavg > 1) {
	    printf ("Block averaging %s and %s by a factor of %d ...\n",
		in, tmplt, blkavg)
	    inblk = mktemp ("tmp")
	    tblk = mktemp ("tmp")
	    outblk = mktemp ("tmp")
	    imcopy (in//"[0]", inblk, verbose-)
	    imcopy (tmplt//"[0]", tblk, verbose-)
	    fd1 = inlist
	    while (fscan (fd1, im1, im2) != EOF) {
		blkavg (im1, inblk//"[append,inherit]", blkavg, blkavg,
		    option = "average")
		blkavg (im2, tblk//"[append,inherit]", blkavg, blkavg,
		    option = "average")
	    }
	    fd1 = ""
	} else {
	    inblk = in
	    tblk = tmplt
	    outblk = out
	}

	# Expand block average extensions.
	rename (inlist, temp, field="all")
	imextensions (inblk, output="file", index="1-", extname=extname,
	    extver="", lindex=no, lname=yes, lver=no, ikparams="") |
	joinlines (temp, "STDIN", output=inlist, delim=" ",
	    missing="Missing", maxchars=161, shortest+, verbose-)
	delete (temp, verify-)
	rename (inlist, temp, field="all")
	imextensions (tblk, output="file", index="1-", extname=extname,
	    extver="", lindex=no, lname=yes, lver=no, ikparams="") |
	joinlines (temp, "STDIN", output=inlist, delim=" ",
	    missing="Missing", maxchars=161, shortest+, verbose-)
	delete (temp, verify-)

	# Compute replacement circle and normalization factor.
	printf ("Computing normalizations ...\n")
	fd1 = inlist
	fd2 = statsec
	fd3 = alllist
	i = fscan (fd1, im1, im2, im3, im4)
	while (fscan (fd3, im5) != EOF) {
	    if (fscan (fd2, str) == EOF) {
		delete (alllist, verify-)
		delete (inlist, verify-)
		delete (temp, verify-, >> "dev$null")
		if (inblk != in)
		    imdelete (inblk, verify-)
		if (tblk != tmplt)
		    imdelete (tblk, verify-)
		error (1, "Missing statistic section for " // im1)
	    }

	    if (im5 != im1)
		next

	    # Compute replacement circle in block averaged image.
	    if (radius == INDEF) {
		xc = INDEF
		yc = INDEF
		r = INDEF
	    } else {
		printf ("0 0\n") | mscctran ("STDIN", "STDOUT", im3,
		    "astrometry", "physical", columns="1 2", units="",
		    formats="", min_sig=9, verbose-) | scan (x, y)
		x = x + xcenter
		y = y + ycenter
		printf ("%g %g\n", x, y) | mscctran ("STDIN", "STDOUT", im3,
		    "physical", "logical", columns="1 2", units="",
		    formats="", min_sig=9, verbose-) | scan (xc, yc)
		x = x + radius + blkavg - 1
		printf ("%g %g\n", x, y) | mscctran ("STDIN", "STDOUT", im3,
		    "physical", "logical", columns="1 2", units="",
		    formats="", min_sig=9, verbose-) | scan (x, y)
		r = (x-xc)**2 + (y-yc)**2
	    }

	    # Compute normalization.
	    imstatistics (im2//str, fields="mean", lower=INDEF, upper=INDEF,
		binwidth=0.1, format=no) | scan (mean)

	    printf ("%s %s %s %s %.2f %.2f %.2f %g\n",
		im1, im2, im3, im4, xc, yc, r, mean, >> temp)

	    i = fscan (fd1, im1, im2, im3, im4)
	}
	fd1 = ""; fd2 = ""; fd3 = ""
	delete (inlist, verify-)
	rename (temp, inlist, field="all")

	# Scale loop.
	printf ("Displaying %s ...\n", in)
	mscdisplay (inblk, 1, extname=extname, >> "dev$null")
	sbest = INDEF
	for (s=scale; s > 0.; s=scale) {
	    printf ("Scaling %s by %.3g and dividing into %s ...\n",
		tmplt, s, in)
	    imdelete (outblk, verify-, >& "dev$null")
	    imcopy (inblk//"[0]", outblk, verbose-)
	    fd1 = inlist
	    while (fscan (fd1, im1, im2, im3, im4, xc, yc, r, mean) != EOF) {
		if (radius == INDEF) {
		    printf ("a/((b/%g-1)*%g+1)\n", mean, s) | scan (str)
		} else {
		    printf ("((I-%g)**2+(J-%g)**2<%g)?a/((b/%g-1)*%g+1):a\n",
			xc, yc, r, mean, s) | scan (str)
		}
		imexpr (str, outblk//"[append,inherit]", im3, im4, dims="auto",
		    intype="auto", outtype="auto", refim="auto", bwidth=0,
		    btype="nearest", bpixval=0., rangecheck+, verbose-,
		    exprdb="none")
	    }
	    fd1 = ""
	    printf ("Displaying corrected version of %s ...\n", in)
	    mscdisplay (outblk, 2, extname=extname, >> "dev$null")
	    if (mscexam) {
		printf ("Entering MSCEXAM (quit with 'q') ...\n")
		mscexamine
	    }
	    sbest = s
	}
	if (inblk != in)
	    imdelete (inblk, verify-)
	if (tblk != tmplt)
	    imdelete (tblk, verify-)
	if (s == -1) {
	    sbest = INDEF
	    if (outblk == out)
		imdelete (outblk, verify-)
	}

	# Create output corrected image.
	if (sbest!=INDEF && (outblk!=out || imextensions.nimages!=nextn)) {
	    printf ("Creating corrected output image %s with scale %.3g ...\n",
		out, sbest)
	    imdelete (outblk, verify-)
	    imcopy (in//"[0]", out, verbose-)
	    fd1 = inlist
	    fd2 = alllist
	    i = fscan (fd1, im1, im2, im3, im4, xc, yc, r, mean)
	    while (fscan (fd2, im5) != EOF) {
		if (im5 != im1)
		    imcopy (im5, out//"[append,inherit]", verbose-)
		else {
		    if (radius == INDEF) {
			printf ("a/((b/%g-1)*%g+1)\n", mean, sbest) | scan (str)
		    } else {
			# Compute replacement circle in full image.
			printf ("0 0\n") |
			mscctran ("STDIN", "STDOUT", im1, "astrometry",
			    "physical", columns="1 2", units="", formats="",
			    min_sig=9, verbose-) | scan (x, y)
			x = x + xcenter
			y = y + ycenter
			printf ("%g %g\n", x, y) |
			mscctran ("STDIN", "STDOUT", im1, "physical",
			    "logical", columns="1 2", units="", formats="",
			    min_sig=9, verbose-) | scan (xc, yc)

			x = x + radius + blkavg - 1
			printf ("%g %g\n", x, y) |
			mscctran ("STDIN", "STDOUT", im1, "physical",
			    "logical", columns="1 2", units="", formats="",
			    min_sig=9, verbose-) | scan (x, y)
			r = (x-xc)**2 + (y-yc)**2

			printf ("((I-%g)**2+(J-%g)**2<%g)?a/((b/%g-1)*%g+1):a\n",
			    xc, yc, r, mean, sbest) | scan (str)
		    }

		    imexpr (str, out//"[append,inherit]", im1, im2, dims="auto",
			intype="auto", outtype="auto", refim="auto", bwidth=0,
			btype="nearest", bpixval=0., rangecheck+, verbose-,
			exprdb="none")
		    i = fscan (fd1, im1, im2, im3, im4, xc, yc, r, mean)
		}
	    }
	    fd1 = ""; fd2 = ""
	}

	delete (alllist, verify-)
	delete (inlist, verify-)
end
