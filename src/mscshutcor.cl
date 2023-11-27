# MSCSHUTCOR - Calculate the shutter correction for a mosaic detector given
# a sequence of overscan corrected flats of varying durations.  The
# shutter correction is the intercept on a plot of exposure duration
# versus exposure level.  Notion courtesy Phil Massey.
#
# This is a revision on the OBSUTIL.SHUTCOR task that works with mosaic
# MEF files.  Note that this version will also work with simple images.
# When there are multiple extensions the photometric measurements are
# averaged to obtain a mean value over all the extensions.  This takes
# care of different gains in the extensions provided the relative gains
# are the same in all exposures.

procedure mscshutcor (images)

string	images			{prompt="Overscan corrected images"}
string	extnames = ""		{prompt="Extension names"}
string	section	= ""		{prompt="Image section for statistics"}
string	center	= "mode"	{prompt="Central statistical measure",
				    enum="mean|midpt|mode"}
int	nclip = 3		{prompt="Number of clipping iterations"}
real	lsigma = 4		{prompt="Lower clipping sigma factor"}
real	usigma = 4		{prompt="Upper clipping sigma factor"}
string	exposure = "exptime"	{prompt="Header keyword for the exposure time"}
bool	verbose	= yes		{prompt="Verbose output?"}

string	*list

begin
	string	limages, img, im1, imglist, statlist, explist, tmplist
	int	nims
	real	exp, avg, shutcorr, shutcorr_err
	real	slope, slope_err, intercept, intercept_err
	int	nstat, nexp, junk
	struct	tmp

	cache sections

	limages = images

	imglist = mktemp ("tmp$tmp")
	statlist = mktemp ("tmp$tmp")
	explist = mktemp ("tmp$tmp")
	tmplist = mktemp ("tmp$tmp")

	sections (limages, option="fullname", > imglist)
	nims = 0
	list = imglist
	while (fscan (list, img, exp, tmp) != EOF) {
	    mscextensions (img//section, output="file", index="0-",
		extname=extnames, extver="", lindex=no, lname=yes,
		lver=no, ikparams="", > tmplist)
	    im1 = ""
	    head (tmplist, nlines=1) | scan (im1)
	    if (im1 == "") {
		delete (tmplist, ver-, >& "dev$null")
		next
	    }

	    tmp = ""
	    hselect (im1, exposure//",overscan", yes) | scan (exp, tmp)
	    if (tmp == "") {
		printf ("%s is not overscan corrected!\n", img)
		delete (imglist, ver-, >& "dev$null")
		delete (tmplist, ver-, >& "dev$null")
		return
	    }
	    if (exp <= 0) {
		printf ("%s has zero exposure time!\n", img)
		delete (imglist, ver-, >& "dev$null")
		delete (tmplist, ver-, >& "dev$null")
		return
	    }
	    tmp = ""
	    hselect (im1, "flatcor", yes) | scan (tmp)
	    if (tmp != "")
		printf ("%s is flat fielded\n", img)
	    imstatistics ("@"//tmplist, fields=center,
		lower=INDEF, upper=INDEF, nclip=nclip, lsigma=lsigma,
		usigma=usigma, binwidth=0.1, format-) |
		average | scan (avg)
	    printf ("%g	%g\n", exp, avg, >> statlist)
	    nims = nims + 1
	    delete (tmplist, ver-, >& "dev$null")
	}
	list = ""; delete (imglist, verify-)

	if (!access (statlist))
	    error (1, "No images selected\n")
	else if (nims < 4)
	    error (1, "Not enough images\n")

	polyfit (statlist, 1, weighting="uniform", verbose=verbose,
	    listdata-, > tmplist)
	delete (statlist, verify-)

	list = tmplist
	junk = fscan (list, intercept, slope)
	junk = fscan (list, intercept_err, slope_err)
	list = ""

	shutcorr = intercept / slope
	shutcorr_err = abs (shutcorr) *
	    sqrt ((intercept_err/intercept)**2 + (slope_err/slope)**2)

	if (verbose)
	    printf ("\n")

	printf ("Shutter correction = %.3f +/- %.3f seconds\n",
	    shutcorr, shutcorr_err)

	if (verbose) {
	    printf ("\nInformation about the %s versus %s fit:\n\n",
		center, exposure)
	    printf ("       intercept        slope     (and errors)\n")
	    printf ("!sed 's+^+    +' %s\n", osfn(tmplist)) | cl
	    printf ("\n")
	}

	delete (tmplist, ver-, >& "dev$null")
end
