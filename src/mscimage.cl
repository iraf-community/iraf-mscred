procedure mscimage

begin

	file	in, out, ref, pl, image, trimsec, outsec, plsec
	file	inlists, extlist, pllist, coord, db, wcsref, outtemp, pltemp
	int	nc, nl, ncref, nlref
	int	cmin, cmax, lmin, lmax, nimage, nimages, nxblk, nyblk
	real	x, y, rval, xmin, xmax, ymin, ymax, crpix1, crpix2
	string	extname, str

	cache	mscextensions, mscgmask

	# Temporary files.
	inlists = mktemp ("tmp$iraf")
	extlist = mktemp ("tmp$iraf")
	pllist = mktemp ("tmp$iraf")
	coord = mktemp ("tmp$iraf")
	db = mktemp ("tmp$iraf")

	# Temporary images.
	outtemp = mktemp ("tmp")
	wcsref = mktemp ("tmp")
	pltemp = mktemp ("tmp")

        # Expand input MEF lists.
	joinlists (input, output, output=inlists, delim=" ", short+,
	    type="image")

	# Process each input MEF file. 
	fd_in = inlists
	while (fscan (fd_in, in, out) != EOF) {

	    # Check for an existing output image.
	    if (imaccess (out)) {
		printf ("Warning: Image already exists (%s)\n", out)
		next
	    }

	    # Set output pl file rootname.
	    if (pixmask) {
		pl = out
		nc = strlen (pl)
		if (nc > 5 && substr (pl, nc-4, nc) == ".fits")
		    pl = substr (pl, 1, nc-5)
		else if (nc > 4 && substr (out, nc-3, nc) == ".imh")
		    pl = substr (pl, 1, nc-4)
		pl = pl // "_bpm"

		if (format == "image" && imaccess (pl)) {
		    printf ("Warning: Mask already exists (%s)\n", pl)
		    next
		}
	    } else
		pl = ""

	    # Expand extensions and check for data.
	    mscextensions (in, output="file", index="0-", extname="",
		extver="", lindex=no, lname=yes, lver=no,
		ikparams="", > extlist)
	    nimages = mscextensions.nimages
	    nimage = 0
	    if (nimages < 1) {
		printf ("WARNING: No input image data found in `%s'.\n", in)
		delete (extlist, verify=no)
		next
	    }

	    # Set WCS image or create template.
	    if (!imaccess(wcsref)) {
		ref = reference
		if (wcssource == "match")
		    wcsref = ref
		else
		    mscwtemplate ("@"//extlist, wcsref, wcssource=wcssource,
			reference=ref, ra=ra, dec=dec, scale=scale,
			rotation=rotation, projection="",verbose=verbose)
	    }

	    # Create output mosaic.
	    fd_ext = extlist
	    while (fscan (fd_ext, image) != EOF) {
		nimage = nimage + 1

		# Set output MEF format.
		if (nimages > 1) {
		    hselect (image, "extname", yes) | scan (extname)
		    if (nscan() == 0)
			extname = "im"+nimage
		    printf ("%s[%s,append]\n", outtemp, extname) | scan (outsec)
		    printf ("%s%s\n", pl, extname) | scan (plsec)
		} else {
		    extname = ""
		    outsec = outtemp
		    plsec = pl
		}

		# Check for existing pixel mask before we actually do anything.
		if (pixmask && imaccess (plsec)) {
		    delete (coord, verify=no)
		    delete (db, verify=no)
		    printf ("Warning: Mask already exists (%s)\n", plsec)
		    next
		}
		if (verbose)
		    printf ("Resampling %s ...\n", image)

		# Trim data.
		hselect (image, "naxis1,naxis2", yes) | scan (nc, nl)
		cmin = 1+ntrim; cmax = nc-ntrim
		lmin = 1+ntrim; lmax = nl-ntrim
		printf ("[%d:%d,%d:%d]\n", cmin, cmax, lmin, lmax) |
		    scan (trimsec)

		# Determine grid points and mapping into the output.
		if (wcssource == "match") {
		    hselect (ref, "naxis1,naxis2", yes) | scan (ncref, nlref)
		    xmin = (ncref - 1.) / (nx - 1.)
		    ymin = (nlref - 1.) / (ny - 1.)
		    for (ymax=1; ymax<=nlref+1; ymax=ymax+ymin)
			for (xmax=1; xmax<=ncref+1; xmax=xmax+xmin)
			    print (xmax, ymax, xmax, ymax, >> coord)
		    mscctran (coord, db, ref, "logical", "world",
			columns="3 4", units="", formats="%.4H %.3h",
			min_sigdigit=10, verbose=no)
		    delete (coord, verify-)
		    wcsctran (db, coord, image//trimsec, inwcs="world",
		        outwcs="logical", columns="3 4", units="hours native",
			formats="", min_sigdigit=10, verbose=no)
		    delete (db, verify-)
		} else {
		    nc = cmax - cmin + 1
		    nl = lmax - lmin + 1
		    xmin = (nc - 1.) / (nx - 1.)
		    ymin = (nl - 1.) / (ny - 1.)
		    for (ymax=1; ymax<=nl+1; ymax=ymax+ymin)
			for (xmax=1; xmax<=nc+1; xmax=xmax+xmin)
			    print (xmax, ymax, xmax, ymax, >> coord)
		    mscctran (coord, db, image//trimsec, "logical", "world",
			columns="1 2", units="", formats="%.4H %.3h",
			min_sigdigit=10, verbose=no)
		    delete (coord, verify-)
		    wcsctran (db, coord, wcsref, inwcs="world",
		        outwcs="logical", columns="1 2", units="hours native",
			formats="", min_sigdigit=10, verbose=no)
		    delete (db, verify-)
		}

		# Determine the output limits.
		xmax = 0.; xmin = 1.; ymax = 0.; ymin = 1.
		fd_coord = coord
		while (fscan (fd_coord, x, y) != EOF) {
		    if (nscan() < 2)
			next
		    if (xmax < xmin) {
			xmin = x; xmax = x; ymin = y; ymax = y
		    } else {
			xmin = min (x, xmin); xmax = max (x, xmax)
			ymin = min (y, ymin); ymax = max (y, ymax)
		    }
		}
		fd_coord = ""
		if (xmax <= xmin || ymax <= ymin)
		    error (1, "No overlap for matching reference")
		cmin = nint (xmin - 1.5)
		cmax = nint (xmax + 1.5)
		lmin = nint (ymin - 1.5)
		lmax = nint (ymax + 1.5)

		# Compute transformation.
		geomap (coord, db, cmin, cmax, lmin, lmax, transforms="",
		    results="", fitgeometry=fitgeometry, function="chebyshev",
		    xxorder=xxorder, xyorder=xyorder, xxterms=xxterms,
		    yxorder=yxorder, yyorder=yyorder, yxterms=yxterms,
		    reject=INDEF, calctype="double", verbose=no,
		    interactive=interactive, graphics="stdgraph", cursor="")

		# Match reference image size.
		if (wcssource == "match") {
		    cmin = 1; lmin = 1
		    cmax = ncref; lmax = nlref
		}

		# Transform extension into output.
		if (nxblock == INDEF)
		    nxblk = cmax - cmin + 3
		else
		    nxblk = nxblock
		if (nyblock == INDEF)
		    nyblk = lmax - lmin + 3
		else
		    nyblk = nyblock
		geotran (image//trimsec, outsec, db, coord,
		    geometry="geometric", xin=INDEF, yin=INDEF,
		    xshift=INDEF, yshift=INDEF, xout=INDEF, yout=INDEF,
		    xmag=INDEF, ymag=INDEF, xrotation=INDEF,
		    yrotation=INDEF, xmin=cmin, xmax=cmax, ymin=lmin,
		    ymax=lmax, xsample=10., ysample=10., xscale=1.,
		    yscale=1., ncols=INDEF, nlines=INDEF,
		    interpolant=interpolant, boundary="constant",
		    constant=constant, fluxconserve=fluxconserve,
		    nxblock=nxblk, nyblock=nyblk, verbose=no)

		# Set WCS.
		wcscopy (outsec, wcsref, verbose-)
		xmin = 0.; ymin = 0.
		hselect (outsec, "crpix1,crpix2", yes) | scan (xmin, ymin)
		xmin = xmin - cmin + 1
		ymin = ymin - lmin + 1
		if (nimage == 1) {
		    crpix1 = xmin
		    crpix2 = ymin
		} else {
		    crpix1 = max (crpix1, xmin)
		    crpix2 = max (crpix2, ymin)
		}
		hedit (outsec, "crpix1", xmin, add+, verify-, show-, update+)
		hedit (outsec, "crpix2", ymin, add+, verify-, show-, update+)

		# Set output mask.
		if (pixmask) {
		    printf ("%s%s\n", pl, extname) | scan (plsec)
		    mscgmask (image//trimsec, pltemp//".pl", "BPM", mval=10000)
		    geotran (pltemp, plsec//".fits", db, coord,
			geometry="geometric", xin=INDEF, yin=INDEF,
			xshift=INDEF, yshift=INDEF, xout=INDEF,
			yout=INDEF, xmag=INDEF, ymag=INDEF,
			xrotation=INDEF, yrotation=INDEF, xmin=cmin,
			xmax=cmax, ymin=lmin, ymax=lmax, xsample=10.,
			ysample=10., interpolant=minterpolant,
			boundary="constant", constant=20000.,
			fluxconserve=no, nxblock=nxblk,
			nyblock=nyblk, verbose=no)
		    imdelete (pltemp, verify-)

		    # Convert values to mask with 1=bad pixel, 2=out of bounds.
#		    imexpr ("abs(a) < 1 ? 0 : int (abs(a) / 10010 + 1)",
#			plsec//".pl", plsec//".fits", dims="auto",
#			intype="auto", outtype="int", refim="auto", 
#			rangecheck=no, verbose=no)
		    mscpmask (plsec//".fits", plsec//".pl")
		    imdelete (plsec//".fits", verify-)

		    # Set WCS of mask and enter mask name in output image.
		    hedit (outsec, "BPM", plsec//".pl", add+, show-,
			verify-, update+)
		    wcscopy (plsec, outsec, verbose-)
		    print (plsec, >> pllist)
		} else
		    hedit (outsec, "BPM", del+, add-, addonly-, show-,
			verify-, update+)

		delete (coord, verify=no)
		delete (db, verify=no)
	    }
	    fd_ext = ""; delete (extlist, verify=no)

	    # Create the final output.
	    if (nimages > 1 && format == "image") {
		# Stack multiple pieces into a single image.
		if (verbose)
		    printf ("Creating image %s ...\n", out)

		mscextensions (outtemp, output="file", index="", extname="",
		    extver="", lindex=no, lname=yes, lver=no,
		    ikparams="", > extlist)

		# Make masks.
		if (pixmask) {
		    combine ("@"//pllist, pltemp//".pl", headers="", bpmasks=pl,
			rejmasks="", nrejmasks="", expmasks="", sigmas="",
			imcmb="", ccdtype="", amps=no, subsets=no, delete=no,
			combine="average", reject="none", project=no,
			outtype="real", outlimits="", offsets="wcs",
			masktype="none", maskvalue="0", blank=0.,
			scale="none", zero="none", weight="none",
			statsec="", lthreshold=INDEF, hthreshold=0.99,
			nlow=1, nhigh=1, nkeep=1, mclip=yes, lsigma=3.,
			hsigma=3., rdnoise="0.", gain="1.", snoise="0.",
			sigscale=0.1, pclip=-0.5, grow=0., > "dev$null")
		    imdelete (pltemp, verify-)

		    combine ("@"//extlist, out, headers="", bpmasks="",
			rejmasks="", nrejmasks="", expmasks="", sigmas="",
			imcmb="", ccdtype="", amps=no, subsets=no, delete=no,
			combine="average", reject="none", project=no,
			outtype="real", outlimits="", offsets="wcs",
			masktype="badvalue", maskvalue="2", blank=0.,
			scale="none", zero="none", weight="none",
			statsec="", lthreshold=INDEF, hthreshold=INDEF,
			nlow=1, nhigh=1, nkeep=1, mclip=yes, lsigma=3.,
			hsigma=3., rdnoise="0.", gain="1.", snoise="0.",
			sigscale=0.1, pclip=-0.5, grow=0., > "dev$null")

		    hedit (out, "BPM", pl, add+, verify-, show-, update+)
		    hedit (pl, "IMCMB???,PROCID??",
			add-, addonly-, del+, update+, verify-, show-)
		} else {
		    combine ("@"//extlist, out, headers="", bpmasks="",
			rejmasks="", nrejmasks="", expmasks="", sigmas="",
			imcmb="", ccdtype="", amps=no, subsets=no, delete=no,
			combine="average", reject="none", project=no,
			outtype="real", outlimits="", offsets="wcs",
			masktype="none", maskvalue="2", blank=0.,
			scale="none", zero="none", weight="none",
			statsec="", lthreshold=INDEF, hthreshold=INDEF,
			nlow=1, nhigh=1, nkeep=1, mclip=yes, lsigma=3.,
			hsigma=3., rdnoise="0.", gain="1.", snoise="0.",
			sigscale=0.1, pclip=-0.5, grow=0., > "dev$null")
		}

		# Fix up header.
		hselect ("@"//extlist, "gain", yes) | average (data_value=0.) |
		    scan (rval)
		hedit (out, "gain", rval, add+, del-, update+, verify-, show-)
		hselect ("@"//extlist, "rdnoise", yes) |
		    average (data_value=0.) | scan (rval)
		hedit (out, "rdnoise", rval, add+, del-, update+, verify-,
		    show-)
		hedit (out, "IMCMB???,PROCID??",
		    add-, addonly-, del+, update+, verify-, show-)
		hedit (out,
		"NEXTEND,DETSEC,CCDSEC,AMPSEC,IMAGEID,DATASEC,TRIMSEC,BIASSEC",
		    add-, addonly-, del+, update+, verify-, show-)

		imdelete (outtemp, verify-)
		if (access (pllist)) {
		    imdelete ("@"//pllist, verify-)
		    delete (pllist, verify-)
		}
		delete (extlist, verify=no)

	    } else if (nimages > 1) {
		# Set MEF output.
		imrename (outtemp, out, verbose-)
		mscextensions (out, output="file", index="", extname="",
		    extver="", lindex=no, lname=yes, lver=no,
		    ikparams="", > extlist)
		fd_ext = extlist
		while (fscan (fd_ext, image) != EOF) {
		    hselect (image, "naxis1,naxis2,crpix1,crpix2", yes) |
			scan (nc, nl, xmin, ymin)
		    cmin = nint (crpix1 - xmin + 1)
		    lmin = nint (crpix2 - ymin + 1)
		    cmax = nc + cmin - 1
		    lmax = nl + lmin - 1
		    printf ("[%d:%d,%d:%d]\n", cmin, cmax, lmin, lmax) |
			scan (str)
		    hedit (image, "DETSEC", str, add+, verify-, show-, update+)
		    hedit (image, "DTM1_1", 1., add+, verify-, show-, update+)
		    hedit (image, "DTM2_2", 1., add+, verify-, show-, update+)
		    cmin = cmin - 1
		    lmin = lmin - 1
		    hedit (image, "DTV1", cmin, add+, verify-, show-, update+)
		    hedit (image, "DTV2", lmin, add+, verify-, show-, update+)
		    hedit (image,
			"CCDSUM,CCDSEC,AMPSEC,ATM1_1,ATM2_2,ATV1,ATV2",
			del+, add-, addonly-, verify-, show-, update+)
		}
		fd_ext = ""; delete (extlist, verify-)
	    } else {
		# If just a single input image produce a single output.
		imrename (outsec, out, verbose-)
	    }
	    
	    if (access (pllist))
		delete (pllist, verify-)
	}
	fd_in = ""; delete (inlists, verify=no)

	# Delete all remaining temporary files.
	if (wcssource != "match" && imaccess(wcsref))
	    imdelete (wcsref, verify-)
end
