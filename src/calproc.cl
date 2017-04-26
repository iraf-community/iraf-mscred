# CALPROC -- Process calibration Mosaic CCD data.

procedure calproc ()

file	images = ""	{prompt="List of Mosaic CCD images to process"}

bool	noproc = no	{prompt="List processing steps only?\n"}

bool	xtalkcor = yes	{prompt="Apply crosstalk correction?"}
bool	overscan = yes	{prompt="Apply overscan strip correction?"}
bool	trim = yes	{prompt="Trim the image?"}
bool	fixpix = yes	{prompt="Apply bad pixel mask correction?"}
bool	zerocor = yes	{prompt="Apply zero level correction?"}
bool	darkcor = yes	{prompt="Apply dark count correction?"}
bool	flatcor = yes	{prompt="Apply flat field correction?"}
bool	sflatcor = no	{prompt="Apply sky flat field correction?"}
bool	merge = no	{prompt="Merge amplifiers from same CCD?\n"}

string	xtalkfile = "mscdb$noao/CCDMosaThin1/xtalk.dat" {prompt="Crosstalk file"}
string	biassec = ""	{prompt="Overscan strip image section"}
string	trimsec = ""	{prompt="Trim data section"}
string	fixfile = ""	{prompt="List of bad pixel masks"}
string	zero = ""	{prompt="List of zero level calibration images"}
string	dark = ""	{prompt="List of dark count calibration images"}
string	flat = ""	{prompt="List of flat field images"}
string	sflat = ""	{prompt="List of sky flat field images"}
real	minreplace = 1.	{prompt="Minimum flat field value\n"}

bool	interactive = no	{prompt="Fit overscan interactively?"}
string	function = "minmax"	{prompt="Fitting function"}
int	order = 1	{prompt="Number of polynomial terms or spline pieces",
			 min=1}
string	sample = "*"	{prompt="Sample points to fit"}
int	naverage = 1	{prompt="Number of sample points to combine"}
int	niterate = 1	{prompt="Number of rejection iterations", min=0}
real	low_reject = 3.	{prompt="Low sigma rejection factor", min=0.}
real	high_reject = 3. {prompt="High sigma rejection factor", min=0.}
real	grow = 0.	{prompt="Rejection growing radius", min=0.}

struct	*fd, *fd2

begin
	bool	mef
	int	nimages
	file	input, outname, extlist, xtalktemp, mergetemp, mergeinput, cal1
	string	ccdtype, extname, tmpfname

	cache mscextensions

	# Create temporary filenames.
	outname = mktemp ("tmp") // ".fits"
	xtalktemp = mktemp ("tmp") // ".fits"
	mergetemp = mktemp ("tmp") // ".fits"
	extlist = mktemp ("tmp$iraf")
	cal1 = mktemp ("tmp$iraf")

	# Process the calibration images.
	fd = images
	while (fscan (fd, input, ccdtype) != EOF) {
	    # Expand the input image into image extensions.
	    mscextensions (input, output="file", index="0-", extname="",
		extver="", lindex=no, lname=yes, lver=no, ikparams="",
		> extlist)
	    if (mscextensions.nimages == 0) {
		delete (extlist, verify-)
		next
	    }
	    nimages = mscextensions.nimages
	    mef = mscextensions.imext

	    if (mef && xtalkcor) {
		xtalkcor (input, xtalktemp, "", xtalkfile=xtalkfile,
		    split-, fextn="fits", noproc=no)
		delete (extlist, verify-)
		mscextensions (xtalktemp, output="file", index="0-",
		    extname="", extver="", lindex=no, lname=yes, lver=no,
		    ikparams="", > extlist)
		if (mscextensions.nimages == nimages) {
		    sleep (1)	# Delay to help FITS kernel cache
		    imcopy (xtalktemp//"[0]", outname, verbose=no)
		    fd2 = extlist
		    while (fscan (fd2, extname) != EOF) {
			tmpfname = input // substr (extname,
			    stridx("[", extname), strlen (extname))
			hedit (extname, "TMPFNAME", tmpfname, add+, del-,
			    show-, verify-, update+)
		    }
		    fd2 = ""
		} else {
		    delete (extlist, verify-)
		    mscextensions (input, output="file", index="0-",
			extname="", extver="", lindex=no, lname=yes,
			lver=no, ikparams="", > extlist)
		}
	    }

	    # Process the input image extensions.
	    sleep (1)	# Delay to help FITS kernel cache
	    if (mef && !imaccess (outname//"[0]"))
		imcopy (input//"[0]", outname, verbose=no)
	    _ccdtool ("@"//extlist, outname//"[inherit]", "", "", calproc=cal1,
		ccdtype="", proctype=ccdtype, max_cache=0, nointerp="",
		noproc=noproc, onerror="original", overscan=overscan,
		trim=trim, fixpix=fixpix, zerocor=zerocor, darkcor=darkcor,
		flatcor=flatcor, sflatcor=sflatcor, illumcor=no,
		fringecor=no, readcor=no, scancor=no,
		saturation="INDEF", sgrow=0, bleed="INDEF", btrail=0, bgrow=0,
		readaxis="line", biassec=biassec, trimsec=trimsec,
		fixfile=fixfile, zero=zero, dark=dark, flat=flat,
		sflat=sflat, illum="", fringe="",
		minreplace=minreplace, scantype="shortscan", nscan=1,
		interactive=interactive, function=function, order=order,
		sample=sample, naverage=naverage, niterate=niterate,
		low_reject=low_reject, high_reject=high_reject, grow=grow)
		flpr

	    # It is an error if there are calibration images to process.
	    if (access (cal1)) {
		fd = ""
		delete (outname, verify=no)
		delete (extlist, verify=no)
		delete (cal1, verify=no)
		error (1, "Error processing " // input)
	    }

	    # If no processing occurred delete output.
	    # If xtalkcor was done then either delete intermediate file
	    # or rename to output.
	    if (mef) {
		if (imaccess (outname//"[1]")) {
		    if (imaccess (xtalktemp//"[0]"))
			imdelete (xtalktemp, verify=no)
		} else {
		    imdelete (outname, verify=no)
		    if (imaccess (xtalktemp//"[0]"))
			imrename (xtalktemp, outname, verbose-)
		}
	    }

	    # Merge amplifiers if desired.
	    if (merge && mef) {
		if (imaccess (outname//"[1]")) {
		    imrename (outname, mergetemp, verbose-)
		    mergeinput = mergetemp
		    flpr
		} else {
		    imdelete (outname, verify=no, >& "dev$null")
		    mergeinput = input
		}

		mergeamps (mergeinput, outname, outmasks="", imcmb="$I",
		    headers="", bpmasks="", rejmasks="", nrejmasks="",
		    expmasks="", sigmas="", ccdtype="", amps=yes,
		    subsets=no, delete=no, combine="average",
		    reject="none", project=no, outtype="real", outlimits="",
		    offsets="physical", masktype="none", maskvalue=0.,
		    blank=1., scale="none", zero="none", weight="none",
		    statsec="", lthreshold=INDEF, hthreshold=INDEF, nlow=1,
		    nhigh=1, nkeep=1, mclip=yes, lsigma=3., hsigma=3.,
		    rdnoise="0.", gain="1.", snoise="0.", sigscale=0.1,
		    pclip=-0.5, grow=0.)

		if (imaccess (outname//"[0]") && verbose)
		    printf ("%s: Merge amplifiers\n", input)

		if (mergeinput == mergetemp) {
		    if (imaccess (outname//"[0]"))
			imdelete (mergetemp, verify-)
		    else {
			if (defvar (outname))
			    imrename (mergetemp, "./"//outname, verbose=no)
			else
			    imrename (mergetemp, outname, verbose=no)
		    }
		}
	    }

	    # Check if the input was processed.
	    # If so make backup and rename the temporary output to the
	    # original image name.  If CCDMEAN is defined compute a
	    # global mean.

	    delete (extlist, verify=no)
	    mscextensions (outname, output="file", index="0-", extname="",
		extver="", lindex=no, lname=yes, lver=no, ikparams="",
		> extlist)

	    if (mscextensions.nimages > 0) {
		if (mscextensions.nimages > 1) {
		    hselect (outname//"[1]", "ccdmean", yes) | scan (x)
		    if (nscan() > 0) {
			hselect ("@"//extlist, "ccdmean", yes) |
			    average | scan (x)
			hedit ("@"//extlist, "ccdmean,ccdmeant", add-, del+,
			    verify-, show-, update+)
			hedit (outname//"[0]", "ccdmean", x, add+, del-,
			    verify-, show-, update+)
		    }
		    hedit ("@"//extlist, "tmpfname", add-, del+, verify-,
			show-, update+)
		}

		ccddelete (input)
		if (defvar (input))
		    imrename (outname, "./"//input, verbose=no)
		else
		    imrename (outname, input, verbose=no)
	    } else
		imdelete (outname, verify=no, >& "dev$null")
	    delete (extlist, verify=no)
	}
	fd = ""
end
