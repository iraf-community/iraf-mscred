# MSCQPHOT -- Quick photometric calibration from list of stars.

procedure mscqphot (input, stars, obsfiles, photconf)

string	input			{prompt="List of input mosaic files"}
string	stars			{prompt="List of star datafiles"}
string	obsfiles		{prompt="List of observation files"}
file	photconf = ""		{prompt="Photometry configuration file"}
real	scale = 0.26		{prompt="Scale in arcsec/pixel"}
string	apertures = "1,3,5"	{prompt="List of photometry apertures(arcsec)"}
string	imag = "-10 -4"		{prompt="Instrumental magnitude range"}
string	cmag1 = "15 20"		{prompt="Catalog magnitude 1 range"}
string	cmag2 = "15 20"		{prompt="Catalog magnitude 2 range"}
string	plotfile = ""		{prompt="Output metacode file"}
bool	update = yes		{prompt="Update image?"}
bool	verbose = yes		{prompt="Verbose?"}

struct	*fd1, *fd2, *fd3

begin
	file	in, star, photcf, im
	file	inlist, extlist
	file	coords, catalog, phot, apfile, aplog, applot
	file	imsets, obsfile, fitpar, lfile, temp
	string	name, filter
	int	nc, nl, nstars, nextn
	real	c, l, otime, airmass, mag, emag, mag1, mag2, seeing, eseeing
	real	imagmin, imagmax, mag1min, mag1max, mag2min, mag2max
	struct	str, photfunc

	inlist = mktemp ("tmp$iraf")
	extlist = mktemp ("tmp$iraf")
	coords = mktemp ("tmp$iraf")
	catalog = mktemp ("tmp$iraf")
	phot = mktemp ("tmp$iraf")
	apfile = mktemp ("tmp$iraf")
	aplog = mktemp ("tmp$iraf")
	applot = ""
	imsets = mktemp ("tmp$iraf")
	fitpar = mktemp ("tmp$iraf")
	lfile = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")

	# Expand lists and check for number.
	sections (input, option="fullname", > inlist)
	nc = sections.nimages
	sections (stars, option="fullname") |
	joinlines (inlist, "STDIN", output=extlist, delim=" ", missing="",
	    maxch=181, shortest-, verbose-)
	delete (inlist, verify-)
	if (nc != sections.nimages && sections.nimages != 1) {
	    delete (extlist, verify-)
	    error (1, "Input list and star list do not match")
	}
	sections (obsfiles, option="fullname") |
	joinlines (extlist, "STDIN", output=inlist, delim=" ", missing="",
	    maxch=181, shortest-, verbose-)
	delete (extlist, verify-)
	if (nc != sections.nimages && sections.nimages != 1) {
	    delete (inlist, verify-)
	    error (1, "Input list and observation file list do not match")
	}

	# Get parameters and do some checking.
	photcf = photconf
	if (!access (photcf))
	    error (1,
		"Photometry configurations file not found ("//photcf//")")
	if (fscan (imag, imagmin, imagmax) != 2)
	    error (1, "Bad syntax in instrumental magnitude range")
	mag = imagmin
	imagmin = min (mag, imagmax)
	imagmax = max (mag, imagmax)
	if (fscan (cmag1, mag1min, mag1max) != 2)
	    error (1, "Bad syntax in catalog magnitude 1 range")
	mag = mag1min
	mag1min = min (mag, mag1max)
	mag1max = max (mag, mag1max)
	if (fscan (cmag2, mag2min, mag2max) != 2)
	    error (1, "Bad syntax in catalog magnitude 1 range")
	mag = mag2min
	mag2min = min (mag, mag2max)
	mag2max = max (mag, mag2max)

	if (plotfile != "")
	    set stdvdm = (plotfile)

	if (verbose) {
	    time | scan (str)
	    printf ("MSCQPHOT: %s\n", str)
	}

	fd1 = inlist
	while (fscan (fd1, in, star, obsfile) != EOF) {
	    i = strlen (in)
	    if (substr (in,i-4,i) == ".fits")
		in = substr (in,1,i-5)

	    if (verbose)
		printf ("  %s:\n", in)

	    mscextensions (in, output="file", index="1-", extname="", extver="",
		lindex-, lname+, lver-, ikparams="", > extlist)
	    nextn = mscextensions.nimages

	    fd2 = extlist
	    while (fscan (fd2, im) != EOF) {
		mscctran (star, temp, im, "world", "logical", columns="1 2",
		    units="hours native", formats="", min_sig=7, verbose=no)

		hselect (im, "extname,naxis1,naxis2", yes) |
		    scan (name,nc, nl)
		printf ("%s : %s\n", name, im, >> imsets)

		nstars = 0
		fd3 = temp
		while (fscan (fd3, c, l, mag1, mag2) != EOF) {
		    if (c < 1 || c > nc || l < 1 || l > nl)
			next
		    if (mag1 < mag1min || mag1 > mag1max)
			next
		    if (mag2 < mag2min || mag2 > mag2max)
			next
		    nstars = nstars + 1
		    printf ("%.2f %.2f %.2f %.2f\n",
			c, l, mag1, mag2, >> coords)
		    printf ("%s-%d %.2f %.2f %.2f %.2f\n",
			name, nstars, c, l, mag1, mag2, >> catalog)
		}
		fd3 = ""; delete (temp, verify-)

		if (verbose)
		    printf ("    Do photometry on %d objects in %s\n",
			nstars, im)
		phot (im, "", coords=coords, output="STDOUT", plotfile="",
		    datapars="mscdpars", centerpars="msccpars",
		    fitskypars="mscspars", photpars="mscppars",
		    interactive=no, radplots=no, verify=no, update=no,
		    verbose=no, graphics="stdgraph", display="stdimage",
		    icommands="", gcommands="",
		    scale=scale, apertures=apertures, >> phot)

		delete (coords, verify-)
	    }
	    fd2 = ""; delete (extlist, verify-)

	    if (!access (phot)) {
		delete (coords, verify-)
		delete (catalog, verify-)
		next
	    }

	    txdump (phot, "IFILTER", "yes", headers=no) | scan (filter)

	    if (verbose)
		printf ("    Compute aperture corrections\n")
	    mkapfile (phot, 3, apfile, smallap=1, largeap=0, magfile="",
		logfile=aplog, plotfile=applot, obsparams="",
		obscolumns="2 3 4 5", append=no, maglim=0.1, nparams=3,
		swings=1.2, pwings=0.1, pgauss=0.5, rgescale=0.9,
		xwings=0., interactive=no, verify=no, gcommands="",
		graphics="stdgraph")
	    if (nextn > 4) {
		i = nextn - 2
		printf ("%d-%d\n", 3, i) | scan (str)
	    } else if (nextn > 2) {
		i = nextn - 1
		printf ("%d-%d\n", 2, i) | scan (str)
	    } else
		printf ("%d-%d\n", 1, nextn) | scan (str)
	    match ("    "//in, aplog, stop-, print+, meta+) |
		sort ("STDIN", column=2, numeric+, ignore+, reverse-) |
		fields ("STDIN", 2, lines=str, quit-, print-) |
		average ("STDIN") | scan (seeing, eseeing)
	    seeing = 2 * seeing
	    eseeing = 2* eseeing
	    if (verbose) {
		fd2 = apfile
		while (fscan (fd2, name, str) != EOF) {
		    if (nscan() < 2)
			next
		    if (name == "#")
			next
		    printf ("      %s %s\n", name, str)
		}
		fd2 = ""
	    }

	    if (access (obsfile))
		delete (obsfile, verify-)
	    mkobsfile (phot, filter, obsfile, imsets=imsets,
		obsparams="", obscolumns="2 3 4 5", minmagerr=0.001,
		shifts="", apercors=apfile, aperture=1, tolerance=5.,
		allfilters=no, verify=no, verbose=no)
	    delete ("f"//obsfile//".dat", verify-)

	    rename (obsfile, temp, field="all")
	    fd2 = temp
	    fd3 = catalog
	    while (fscan (fd2,name,filter,otime,airmass,c,l,mag,emag) != EOF) {
		if (nscan() < 8)
		    next
		if (fscan (fd3, name, c, l, mag1, mag2) == EOF)
		    break
		if (mag < imagmin || mag > imagmax)
		    next
	    printf ("%-12s %-12s %6.1f %6.1f %6.3f %6.3f %6.3f %6.3f %6.3f\n",
		    name, in, c, l, airmass, mag, emag, mag1, mag2,
		    >> obsfile)
	    }
	    fd2 = ""; delete (temp, verify-)
	    fd3 = ""

	    if (verbose)
		printf ("    Fit photometric zero point\n")
	    if (plotfile == "")
		fitparams (obsfile, "", photcf, fitpar,
		    weighting="uniform", addscatter=yes,
		    tolerance=3.0000000000000E-5, maxiter=15, nreject=3,
		    low_reject=2., high_reject=2., grow=0., interactive=no,
		    logfile=lfile, log_unmatche=no, log_fit=yes,
		    log_results=no, catdir="", graphics="stdvdm", cursor="")
	    else {
		printf ("h\nq\n", > temp)
		fitparams (obsfile, "", photcf, fitpar,
		    weighting="uniform", addscatter=yes,
		    tolerance=3.0000000000000E-5, maxiter=15, nreject=3,
		    low_reject=2., high_reject=2., grow=0., interactive=yes,
		    logfile=lfile, log_unmatche=no, log_fit=yes,
		    log_results=no, catdir="", graphics="stdvdm",
		    cursor=temp, < "dev$null", > "dev$null")
		delete (temp, verify-)
	    }

	    match ("PHOTFUNC", photcf, stop-, print+, meta+) |
		scan (name, name, photfunc)
	    match ("photzero", photcf, stop-, print+, meta+) |
		scan (name, name, name, mag1)
	    match ("photcor", lfile, stop-, print+, meta+, > temp)
		tail (temp, nl=1) | scan (name, mag, emag)
	    delete (temp, verify-)

	    if (verbose) {
		printf ("      SEEING   = %.3g (%.3g)\n", seeing, eseeing)
		printf ("      PHOTFUNC = %s\n", photfunc)
		printf ("      PHOTZERO = %g\n", mag1)
		printf ("      PHOTCOR  = %.3g (%.3g)\n", mag, emag)
	    }

	    if (update) {
		printf ("Seeing (arcsec) +-%.3g\n", eseeing) | scan (str)
		addkey (in//"[0]", "SEEING", seeing, str, type="c")
		addkey (in//"[0]", "PHOTFUNC", photfunc, "", type="c")
		addkey (in//"[0]", "PHOTZERO", mag1,
		    "Expected zero point magnitude", type="r")
		printf ("Zeropoint correction +-%.3g\n", emag) | scan (str)
		addkey (in//"[0]", "PHOTCOR", mag, str, type="r")
	    }

	    delete (catalog, verify-)
	    delete (phot, verify-)
	    delete (apfile, verify-)
	    delete (aplog, verify-)
	    #delete (applot, verify-)
	    delete (fitpar, verify-)
	    delete (lfile, verify-)
	}
	fd1 = ""; delete (inlist, verify-)
end
