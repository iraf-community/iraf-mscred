procedure mscimatch (input, coords)

string	input			{prompt="List of input images"}
file	coords			{prompt="Coordinates"}
file	reference		{prompt="Reference image"}
int	box1 = 21		{prompt="Box size for statistics"}
int	box2 = 51		{prompt="Box size for statistics"}
real	lower = 1.		{prompt="Lower limit for good data"}
real	upper = INDEF		{prompt="Upper limit for good data"}
bool	update = yes		{prompt="Update images?"}
bool	interactive = yes	{prompt="Interactive?"}
bool	fit = yes		{prompt="Fit interactively?"}
bool	verbose = yes		{prompt="Verbose?"}
bool	accept = yes		{prompt="Accept scaling?", mode="q"}

struct	*fd1, *fd2

begin
	file	images, refstats, fitstats, results, temp
	file	ref, in, coord, refpix, inpix
	string	image
	int	nc, nl, x1, x2, y1, y2, nbox1, nbox2, npix, nstat, nfit
	real	ra, dec, xc, yc, refstat1, refstat2, instat1, instat2, sky
	real	msczero, mscscale, dsky
	bool	ifit

	images = mktemp ("tmp$iraf")
	refstats = mktemp ("tmp$iraf")
	fitstats = mktemp ("tmp$iraf")
	results = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")

	# Expand the input list.
	sections (input, option="fullname", > images)

	# Set the reference image.
	fd1 = images
	if (fscan (fd1, ref) == EOF)
	    error (1, "No input images")
	if (fscan (reference, ref) == 0)
	    ;

	# Set the coordinate file.
	if (fscan (coords, coord) == 0)
	    error (1, "No coordinate file")
	fd2 = coord

	if (verbose) {
	    printf ("MSCIMATCH:\n")
	    printf ("  Using %s as the reference image\n", ref)
	    printf ("    Computing statistics for %s...\n", ref)
	}

	# Measure the reference image.
	nbox1 = box1 * box1
	nbox2 = box2 * box2
	sky = INDEF
	hselect (ref, "naxis1,naxis2,pixfile", yes) | scan (nc, nl, refpix)
	while (fscan (fd2, ra, dec) != EOF) {
	    print (ra, dec) |
	    mscctran ("STDIN", "STDOUT", ref, "world", "logical", col="1 2",
		units="hours native", formats="", min_sig=9,
		verbose=no) | scan (xc, yc)
	    x1 = nint (xc - box1 / 2)
	    x2 = x1 + box1 - 1
	    y1 = nint (yc - box1 / 2)
	    y2 = y1 + box1 - 1
	    if (x1 < 1 || x2 > nc || y1 < 1 || y2 > nl)
		next
	    printf ("%s[%d:%d,%d:%d]\n", ref, x1, x2, y1, y2) | scan (image)
	    imstat (image, fields="npix,mean", lower=lower, upper=upper,
		binwidth=0.1, format=no) | scan (npix, refstat1)
	    if (npix != nbox1 || refstat1 == INDEF)
		next

	    if (sky == INDEF)
	        sky = refstat1
	    else
	        sky = min (refstat1, sky)

	    if (box2 <= box1) {
		print (ra, dec, refstat1, >> refstats)
		next
	    }

	    x1 = nint (xc - box2 / 2)
	    x2 = x1 + box2 - 1
	    y1 = nint (yc - box2 / 2)
	    y2 = y1 + box2 - 1
	    if (x1 < 1 || x2 > nc || y1 < 1 || y2 > nl) {
		print (ra, dec, refstat1, >> refstats)
		next
	    }
	    printf ("%s[%d:%d,%d:%d]\n", ref, x1, x2, y1, y2) | scan (image)
	    imstat (image, fields="npix,mean", lower=lower, upper=upper,
		binwidth=0.1, format=no) | scan (npix, refstat2)
	    if (npix != nbox2 || refstat2 == INDEF) {
		print (ra, dec, refstat1, >> refstats)
		next
	    }
	    refstat2 = (refstat2 * nbox2 - refstat1 * nbox1) / (nbox2 - nbox1)
	    print (ra, dec, refstat1, refstat2, >> refstats)
	    sky = min (refstat2, sky)
	}
	if (!access (refstats))
	    error (1, "No coordinates in " // ref)

	if (update) {
	    hedit (ref, "msczero", 0., add+, del-, show-, verify-, update+)
	    hedit (ref, "mscscale", 1., add+, del-, show-, verify-, update+)
	    mscstack.zero = "!msczero"
	    mscstack.scale = "!mscscale"
	}

	fd1 = images
	while (fscan (fd1, in) != EOF) {
	    hselect (in, "naxis1,naxis2,pixfile", yes) | scan (nc, nl, inpix)
	    if (refpix == inpix)
		next
	    if (verbose) {
		printf ("  Matching %s to %s\n", in, ref)
		printf ("    Computing statistics for %s...\n", in)
	    }

	    # Measure input image.
	    nfit = 0
	    fd2 = refstats
	    while (fscan (fd2, ra, dec, refstat1, refstat2) != EOF) {
		nstat = nscan() - 2
		print (ra, dec) |
		mscctran ("STDIN", "STDOUT", in, "world", "logical", col="1 2",
		    units="hours native", formats="", min_sig=9,
		    verbose=no) | scan (xc, yc)
		x1 = nint (xc - box1 / 2)
		x2 = x1 + box1 - 1
		y1 = nint (yc - box1 / 2)
		y2 = y1 + box1 - 1
		if (x1 < 1 || x2 > nc || y1 < 1 || y2 > nl)
		    next
		printf ("%s[%d:%d,%d:%d]\n", in, x1, x2, y1, y2) |
		    scan (image)
		imstat (image, fields="npix,mean", lower=lower, upper=upper,
		    binwidth=0.1, format=no) | scan (npix, instat1)
		if (npix != nbox1 || instat1 == INDEF)
		    next
		refstat1 = refstat1 - instat1
		print (instat1, refstat1, >> fitstats)
		nfit = nfit + 1

		if (nstat < 2)
		    next

		x1 = nint (xc - box2 / 2)
		x2 = x1 + box2 - 1
		y1 = nint (yc - box2 / 2)
		y2 = y1 + box2 - 1
		if (x1 < 1 || x2 > nc || y1 < 1 || y2 > nl)
		    next
		printf ("%s[%d:%d,%d:%d]\n", in, x1, x2, y1, y2) |
		    scan (image)
		imstat (image, fields="npix,mean", lower=lower, upper=upper,
		    binwidth=0.1, format=no) | scan (npix, instat2)
		if (npix != nbox2 || instat2 == INDEF)
		    next
		instat2 = (instat2 * nbox2 - instat1 * nbox1) / (nbox2 - nbox1)
		refstat2 = refstat2 - instat2
		print (instat2, refstat2, >> fitstats)
		nfit = nfit + 1
	    }
	    if (!access (fitstats)) {
		printf ("WARNING: No matching coordinates in %s\n", in)
		next
	    }
	    if (nfit < 2) {
		printf ("WARNING: Insufficient measurements for fit (%d)\n",
		    nfit)
		delete (fitstats, verify-)
		next
	    }

	    if (verbose)
		printf ("    Fitting statistics...\n")

	    if (interactive && fit) {
		msccurfit (fitstats, results, function="legendre",
		    weighting="uniform", order=2, interactive=yes, axis=1,
		    listdata=no, verbose=no, calctype="double", power=yes,
		    device="stdgraph", cursor="")
	    } else if (verbose) {
		print ("q", > temp)
		msccurfit (fitstats, results, function="legendre",
		    weighting="uniform", order=2, interactive=yes, axis=1,
		    listdata=no, verbose=no, calctype="double", power=yes,
		    device="stdgraph", cursor=temp)
		delete (temp, verify-)
	    } else {
		msccurfit (fitstats, results, function="legendre",
		    weighting="uniform", order=2, interactive=no, axis=1,
		    listdata=no, verbose=no, calctype="double", power=yes,
		    device="stdgraph", cursor="")
	    }
	    delete (fitstats, verify-)

	    tail (results, nlines=1) | scan (image, nstat, msczero)
	    if (nscan() != 3 || nstat > 2) {
		printf ("WARNING: Error fitting %s\n", in)
		next
	    }
	    if (nstat == 1)
		mscscale = 0
	    else {
		mscscale = msczero
		tail (results, nlines=2) | scan (image, nstat, msczero)
		if (nscan() != 3 || nstat != 1) {
		    printf ("WARNING: Error fitting %s\n", in)
		    next
		}
	    }
	    delete (results, verify-)

	    mscscale = mscscale + 1
	    msczero = msczero / mscscale
	    if (interactive || verbose) {
		dsky = sky * (1/mscscale - 1) - msczero
		printf ("  %s: SCALE = %.8g, OFFSET(@%.8g) = %.8g\n",
		    in, mscscale, sky, dsky)
	    }

	    if (update) {
		if (interactive) {
		    if (accept) {
			hedit (in, "msczero", msczero, add+, del-, show-,
			    verify-, update+)
			hedit (in, "mscscale", mscscale, add+, del-, show-,
			    verify-, update+)
		    }
		} else {
		    hedit (in, "msczero", msczero, add+, del-, show-,
			verify-, update+)
		    hedit (in, "mscscale", mscscale, add+, del-, show-,
			verify-, update+)
		}
	    }


	}
	fd1 = ""; delete (images, verify-)
	fd2 = ""; delete (refstats, verify-)
end
