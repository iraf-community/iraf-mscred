# CCDPROC -- Process Mosaic CCD data.

procedure ccdproc (images)

string	images = ""	{prompt="List of Mosaic CCD images to process"}
string	output = ""	{prompt="List of output processed images"}
string	bpmasks = ""	{prompt="List of output bad pixel masks"}

string	ccdtype = "object"	{prompt="CCD image type to process"}
bool	noproc = no	{prompt="List processing steps only?\n"}

bool	xtalkcor = no	{prompt="Apply crosstalk correction?"}
bool	fixpix = yes	{prompt="Apply bad pixel mask correction?"}
bool	overscan = yes	{prompt="Apply overscan strip correction?"}
bool	trim = yes	{prompt="Trim the image?"}
bool	zerocor = yes	{prompt="Apply zero level correction?"}
bool	darkcor = yes	{prompt="Apply dark count correction?"}
bool	flatcor = yes	{prompt="Apply flat field correction?"}
bool	sflatcor = no	{prompt="Apply sky flat field correction?"}
bool	split = no	{prompt="Use split images during processing?"}
bool	merge = yes	{prompt="Merge amplifiers from same CCD?\n"}

string	xtalkfile = ""	{prompt="Crosstalk file"}
string	fixfile = ""	{prompt="List of bad pixel masks"}
string	saturation = "INDEF"	{prompt="Saturated pixel threshold"}
int	sgrow = 0	{prompt="Saturated pixel grow radius"}
string	bleed = "INDEF"	{prompt="Bleed pixel threshold"}
int	btrail = 20	{prompt="Bleed trail minimum length"}
int	bgrow = 0	{prompt="Bleed pixel grow radius"}
string	biassec = ""	{prompt="Overscan strip image section"}
string	trimsec = ""	{prompt="Trim data section"}
string	zero = ""	{prompt="List of zero level calibration images"}
string	dark = ""	{prompt="List of dark count calibration images"}
string	flat = ""	{prompt="List of flat field images"}
string	sflat = ""	{prompt="List of secondary flat field images"}
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
	bool	mef, splt
	int	nimages, len
	string	ims, fixf, extname, bpmname, tmproot, tmpfname, ext1
	file	input, outname, bpmask
	file	outtemp, xtalktemp, mergetemp, mergemask, mergeinput, cal1, cal2
	file	inlist, inbpmlist, zerolist, darklist, flatlist, sflatlist
	file	extlist, outlist, bpmlist, xtlist, mergelist, moutlist, templist
	file	out

	cache mscextensions, sections

	# Create temporary filenames.
	tmproot = mktemp ("tmp$ccdproc")
	templist = tmproot // "A"
	inlist = tmproot // "B"
	outlist = tmproot // "C"
	extlist = tmproot // "D"
	bpmlist = tmproot // "E"
	xtlist = tmproot // "F"
	mergelist = tmproot // "G"
	moutlist = tmproot // "H"
	inbpmlist = tmproot // "I"
	zerolist = tmproot // "J"
	darklist = tmproot // "K"
	flatlist = tmproot // "L"
	sflatlist = tmproot // "M"
	outtemp = tmproot // "N"
	xtalktemp = tmproot // "O"
	mergetemp = tmproot // "P"
	mergemask = tmproot // "Q"
	cal1 = tmproot // "R"
	cal2 = tmproot // "S"

	print (inlist, >> templist)
	print (outlist, >> templist)
	print (extlist, >> templist)
	print (bpmlist, >> templist)
	print (xtlist, >> templist)
	print (mergelist, >> templist)
	print (moutlist, >> templist)
	print (inbpmlist, >> templist)
	print (zerolist, >> templist)
	print (darklist, >> templist)
	print (flatlist, >> templist)
	print (sflatlist, >> templist)
	print (cal1, >> templist)
	print (cal2, >> templist)

	# Expand the input and output lists. Use workaround for
	# ccdlist using the ccdproc pset which confuses later scans.
	#sections (images, option="root", > cal1)
	tmpfname = _ccdlist.ccdproc; _ccdlist.ccdproc="setinstrument"
	ccdlist (images, ccdtype=ccdtype, extname="mef", names+, long-, > cal1)
	_ccdlist.ccdproc=tmpfname
	sections ("@"//cal1, option="root", > "dev$null")
	nimages = sections.nimages
	sections (output, option="root") |
	joinlines (cal1, "STDIN", output=cal2, delim=" ", missing="-",
	    maxchars=161, shortest=no, verbose=no)
	delete (cal1, verify=no, >& "dev$null")
	if (sections.nimages > 0 && sections.nimages != nimages) {
	    delete (cal2, verify=no, >& "dev$null")
	    error (1, "Input and output lists do not match")
	}
	sections (bpmasks, option="root") |
	joinlines (cal2, "STDIN", output=inlist, delim=" ", missing="-",
	    maxchars=161, shortest=no, verbose=no)
	delete (cal2, verify=no, >& "dev$null")
	if (sections.nimages > 0 && sections.nimages != nimages) {
	    delete (inlist, verify=no, >& "dev$null")
	    error (1, "Input and mask lists do not match")
	}

	# Expand the calibration images into image extensions.
	fixf = fixfile
	if (stridx("!",fixf)==0) {
	    mscextensions (fixf, output="file", index="0-", extname="",
	        extver="", lindex=no, lname=yes, lver=no, ikparams="",
		> inbpmlist)
	    fixf = "@"//inbpmlist
	}
	mscextensions (zero, output="file", index="0-", extname="",
	    extver="", lindex=no, lname=yes, lver=no, ikparams="",
	    > zerolist)
	mscextensions (dark, output="file", index="0-", extname="",
	    extver="", lindex=no, lname=yes, lver=no, ikparams="",
	    > darklist)
	mscextensions (flat, output="file", index="0-", extname="",
	    extver="", lindex=no, lname=yes, lver=no, ikparams="",
	    > flatlist)
	mscextensions (sflat, output="file", index="0-", extname="",
	    extver="", lindex=no, lname=yes, lver=no, ikparams="",
	    > sflatlist)

	# Process the data.
	fd = inlist
	while (fscan (fd, input, outname, bpmask) != EOF) {
	    if (outname == "-") {
		if (split)
		    outname = input
		else
		    outname = outtemp
	    }
	    if (bpmask == "-")
		bpmask = ""

	    # Strip FITS extension from output name.
	    len = strlen (outname)
	    if (len > 5)
		if (substr (outname, len-4, len) == ".fits")
		    outname = substr (outname, 1, len-5)

	    # Expand the input image into image extensions.
	    mscextensions (input, output="file", index="0-", extname="",
		extver="", lindex=no, lname=yes, lver=no, ikparams="",
		> extlist)
	    nimages = mscextensions.nimages
	    mef = mscextensions.imext
	    if (nimages == 0) {
		delete (extlist, verify-)
		next
	    }

	    # Expand and check output images.
	    splt = (mef && split)
	    if (splt) {
		ext1 = ""
		fd2 = extlist
		while (fscan (fd2, extname) != EOF) {
		    if (ext1 == "")
		        ext1 = "_" // substr (extname,
			    stridx("[",extname)+1,stridx("]",extname)-1)
		    tmpfname = outname // "_" // substr (extname,
			stridx("[",extname)+1,stridx("]",extname)-1)
		    if (imaccess (tmpfname) == YES)
			break
		    print (tmpfname, >> outlist)
		    if (merge) {
			tmpfname = mergetemp // "_" // substr (extname,
			    stridx("[",extname)+1,stridx("]",extname)-1)
			print (tmpfname, >> mergelist)
		    }
		}
		fd2 = ""
		if (imaccess (tmpfname) == YES) {
		    if (access (outlist))
			delete (outlist, verify-)
		    delete (extlist, verify-)
		    printf ("WARNING: Output image already exists (%s)\n",
			tmpfname)
		    next
		}
	    } else {
		ext1 = "[1]"
		if (imaccess (outname//"[0]") == YES) {
		    delete (extlist, verify-)
		    printf ("WARNING: Output image already exists (%s)\n",
			outname)
		    next
		}
		if (defvar (outname))
		    print ("./" // outname, > outlist)
		else
		    print (outname, > outlist)
		if (merge)
		    print (mergetemp, > mergelist)
	    }

	    # Set output mask names.
	    bpmname = ""
	    if (bpmask != "") {
		if (bpmask == input || bpmask == output ||
		    bpmask//".fits" == input || bpmask//".fits" == output)
		    bpmask = bpmask // "_bpm"
		if (mef) {
		    if (!access (bpmask))
			mkdir (bpmask)
		    fd2 = extlist
		    while (fscan (fd2, extname) != EOF) {
			tmpfname = bpmask // "/bpm_" // substr (extname,
			    stridx("[",extname)+1,stridx("]",extname)-1)
			print (tmpfname, >> bpmlist)
		    }
		    fd2 = ""
		    bpmname = "@"//bpmlist
		} else
		    bpmname = bpmask
	    }

	    # Do crosstalk correction.
	    if (mef && xtalkcor) {
		xtalkcor (input, xtalktemp, "", xtalkfile=xtalkfile,
		    split=splt, fextn="fits", noproc=noproc)
		if (splt) {
		    mscextensions (xtalktemp//"_*", output="file", index="0-",
			extname="", extver="", lindex=no, lname=yes, lver=no,
			ikparams="") |
		    joinlines (extlist, "STDIN", output=xtlist, delim=" ",
			missing="-", maxchars=161, shortest=no, verbose=no)
		} else {
		    mscextensions (xtalktemp, output="file", index="0-",
			extname="", extver="", lindex=no, lname=yes, lver=no,
			ikparams="") |
		    joinlines (extlist, "STDIN", output=xtlist, delim=" ",
			missing="-", maxchars=161, shortest=no, verbose=no)
		}
		delete (extlist, verify-)
		if (mscextensions.nimages == nimages) {
		    fd2 = xtlist
		    while (fscan (fd2, tmpfname, extname) != EOF) {
			hedit (extname, "TMPFNAME", tmpfname, add+, del-,
			    show-, verify-, update+)
			print (extname, >> extlist)
		    }
		    fd2 = ""; delete (xtlist, verify-)
		    if (splt)
			copy (extlist, xtlist, verbose-)
		    else {
			print (xtalktemp, > xtlist)
			sleep (1)	# Delay to help FITS kernel cache
			imcopy (xtalktemp//"[0]", outname, verbose=no)
		    }
		} else {
		    delete (xtlist, verify-)
		    mscextensions (input, output="file", index="0-",
			extname="", extver="", lindex=no, lname=yes,
			lver=no, ikparams="", > extlist)
		}
	    }

	    # Process the input image extensions.
	    if (splt) {
		out = "@" // outlist
	    } else {
		sleep (1)	# Delay to help FITS kernel cache
		if (mef && !imaccess (outname//"[0]"))
		    imcopy (input//"[0]", outname, verbose=no)
		out = outname // "[inherit]"
	    }
	    _ccdtool ("@"//extlist, out, "", bpmname, calproc=cal1, nointerp="",
		ccdtype=ccdtype, proctype="", max_cache=0, noproc=noproc,
		onerror="original", overscan=overscan, trim=trim,
		fixpix=fixpix, zerocor=zerocor, darkcor=darkcor,
		flatcor=flatcor, sflatcor=sflatcor, illumcor=no,
		fringecor=no, readcor=no, scancor=no, readaxis="line",
		saturation=saturation, sgrow=sgrow, bleed=bleed,
		btrail=btrail, bgrow=bgrow, biassec=biassec,
		trimsec=trimsec, fixfile=fixf, zero="@"//zerolist,
		dark="@"//darklist, flat="@"//flatlist,
		sflat="@"//sflatlist, minreplace=minreplace,
		interactive=interactive, function=function, order=order,
		sample=sample, naverage=naverage, niterate=niterate,
		low_reject=low_reject, high_reject=high_reject, grow=grow)
		flpr

	    # If calibration images need to be processed first process them.
	    if (access (cal1)) {
		# Convert list of extensions to parent image names.
		mscuniq (cal1, cal2)
		delete (cal1, verify=no)

		# Process the calibration images.
		calproc (images=cal2, noproc=noproc, xtalkcor=xtalkcor,
		    overscan=overscan, trim=trim, fixpix=fixpix,
		    zerocor=zerocor, darkcor=darkcor, flatcor=flatcor,
		    sflatcor=sflatcor, merge=merge, xtalkfile=xtalkfile,
		    fixfile=fixf, biassec=biassec, trimsec=trimsec,
		    zero="@"//zerolist, dark="@"//darklist,
		    flat="@"//flatlist, sflat="@"//sflatlist,
		    minreplace=minreplace, interactive=interactive,
		    function=function, order=order, sample=sample,
		    naverage=naverage, niterate=niterate,
		    low_reject=low_reject, high_reject=high_reject,
		    grow=grow)
		delete (cal2, verify=no)

		# Reset calibration lists in case of merging.
		if (merge) {
		    delete (zerolist, verify-)
		    mscextensions (zero, output="file", index="0-",
			extname="", extver="", lindex=no, lname=yes,
			lver=no, ikparams="", > zerolist)
		    delete (darklist, verify-)
		    mscextensions (dark, output="file", index="0-",
			extname="", extver="", lindex=no, lname=yes,
			lver=no, ikparams="", > darklist)
		    delete (flatlist, verify-)
		    mscextensions (flat, output="file", index="0-",
			extname="", extver="", lindex=no, lname=yes,
			lver=no, ikparams="", > flatlist)
		    delete (sflatlist, verify-)
		    mscextensions (sflat, output="file", index="0-",
			extname="", extver="", lindex=no, lname=yes,
			lver=no, ikparams="", > sflatlist)
		}

		# Now process the input image.
		_ccdtool ("@"//extlist, out, "", bpmname,
		    calproc=cal1, ccdtype=ccdtype, proctype="", nointerp="",
		    max_cache=0, noproc=noproc, onerror="original",
		    overscan=overscan, trim=trim, fixpix=fixpix,
		    zerocor=zerocor, darkcor=darkcor, flatcor=flatcor,
		    sflatcor=sflatcor, illumcor=no, fringecor=no,
		    readcor=no, scancor=no, saturation=saturation,
		    sgrow=sgrow, bleed=bleed, btrail=btrail, bgrow=bgrow,
		    readaxis="line", biassec=biassec, trimsec=trimsec,
		    fixfile=fixf, zero="@"//zerolist,
		    dark="@"//darklist, flat="@"//flatlist,
		    sflat="@"//sflatlist, minreplace=minreplace,
		    interactive=interactive, function=function,
		    order=order, sample=sample, naverage=naverage,
		    niterate=niterate, low_reject=low_reject,
		    high_reject=high_reject, grow=grow)
		    flpr

		# It is an error if there are calibration images to process.
		if (access (cal1)) {
		    fd = ""
		    imdelete ("@"//outlist, verify=no)
		    delete ("@"//templist, verify=no, >& "dev$null")
		    delete (templist, verify=no)
		    error (1, "Error processing " // input)
		}
	    }

	    # If no processing occurred delete output.
	    # If xtalkcor was done then either delete intermediate file
	    # or rename to output.
	    if (mef) {
		if (imaccess (outname//ext1)) {
		    if (access (xtlist))
			imdelete ("@"//xtlist, verify=no)
		} else {
		    imdelete ("@"//outlist, verify=no, >& "dev$null")
		    if (access (xtlist))
			imrename ("@"//xtlist, "@"//outlist, verbose-)
		}
	    }

	    # Merge amplifiers if desired.
	    if (merge && mef) {
		if (imaccess (outname//ext1)) {
		    imrename ("@"//outlist, "@"//mergelist, verbose-)
		    mergeinput = "@" // mergelist
		    flpr
		} else {
		    imdelete ("@"//outlist, verify=no, >& "dev$null")
		    if (splt)
			mergeinput = "@" // extlist
		    else
			mergeinput = input
		}

		if (bpmask == "")
		    mergeamps (mergeinput, outname, "", headers="", bpmasks="",
			rejmasks="", nrejmasks="", expmasks="", sigmas="",
			outnames=moutlist, imcmb="$I",
			ccdtype="", amps=yes, subsets=no,
			delete=no, combine="average", reject="none",
			project=no, outtype="real", outlimits="",
			offsets="physical", masktype="none", maskvalue=0.,
			blank=1., scale="none", zero="none", weight="none",
			statsec="", lthreshold=INDEF, hthreshold=INDEF,
			nlow=1, nhigh=1, nkeep=1, mclip=yes, lsigma=3.,
			hsigma=3., rdnoise="0.", gain="1.", snoise="0.",
			sigscale=0.1, pclip=-0.5, grow=0., verbose=no)
		else
		    mergeamps (mergeinput, outname, bpmask//"/", headers="",
			bpmasks=bpmask//"/"//mergemask, imcmb="$I",
			rejmasks="", nrejmasks="", expmasks="", sigmas="",
			outnames=moutlist, ccdtype="", amps=yes,
			subsets=no, delete=no, combine="average",
			reject="none", project=no, outtype="real",
			outlimits="", offsets="physical", masktype="none",
			maskvalue=0., blank=1., scale="none", zero="none",
			weight="none", statsec="", lthreshold=INDEF,
			hthreshold=INDEF, nlow=1, nhigh=1, nkeep=1,
			mclip=yes, lsigma=3., hsigma=3., rdnoise="0.",
			gain="1.", snoise="0.", sigscale=0.1, pclip=-0.5,
			grow=0., verbose=no)

		if (access (moutlist) && verbose) {
		    if (splt)
			printf ("%s: Merge amplifiers to ", input)
		    else
			printf ("%s: Merge amplifiers\n", input)
		}

		if (mergeinput == "@" // mergelist) {
		    if (access (moutlist)) {
			imdelete (mergeinput, verify-)
			if (bpmname != "")
			    imdelete (bpmname, verify-)
			delete (outlist, verify-)
			rename (moutlist, outlist, field="all")
		    } else
			imrename ("@"//mergelist, "@"//outlist, verbose=no)
		}
		if (access (mergelist))
		    delete (mergelist, verify-)
		if (access (moutlist))
		    delete (moutlist, verify-)
	    }

	    # Check if the input was processed.
	    # If so make backup and rename the temporary output to the
	    # original image name.  If CCDMEAN is defined compute a
	    # global mean.

	    delete (extlist, verify=no)
	    mscextensions ("@"//outlist, output="file", index="0-", extname="",
		extver="", lindex=no, lname=yes, lver=no, ikparams="",
		> extlist)

	    if (mscextensions.nimages > 0) {
		if (mscextensions.nimages > 1) {
		    type (extlist) | scan (tmpfname)
		    hselect (tmpfname, "ccdmean", yes) | scan (x)
		    if (nscan() > 0) {
			hselect ("@"//extlist, "ccdmean", yes) |
			    average | scan (x)
			hedit ("@"//extlist, "ccdmean,ccdmeant", add-, del+,
			    verify-, show-, update+)
			if (splt) {
			    hedit ("@"//extlist, "ccdmean", x, add+, del-,
				verify-, show-, update+)
			} else {
			    hedit (outname//"[0]", "ccdmean", x, add+, del-,
				verify-, show-, update+)
			}
		    }
		    hedit ("@"//extlist, "tmpfname", add-, del+, verify-,
			show-, update+)
		}

		if (!splt && outname == outtemp) {
		    ccddelete (input)
		    if (defvar (input))
			imrename (outname, "./"//input, verbose=no)
		    else
			imrename (outname, input, verbose=no)
		}
	    } else
		imdelete ("@"//outlist, verify=no, >& "dev$null")
	    delete (extlist, verify=no)
	    if (access (outlist))
		delete (outlist, verify=no)
	    if (access (bpmlist))
		delete (bpmlist, verify=no)
	    if (access (xtlist))
		delete (xtlist, verify=no)
	    if (access (mergelist))
		delete (mergelist, verify=no)
	}

	# Delete all temporary files.
	fd = ""
	delete ("@"//templist, verify=no, >& "dev$null")
	delete (templist, verify=no)
end
