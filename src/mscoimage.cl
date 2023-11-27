procedure mscimage

begin
	file	in, out, pl, ref, image
	file	inlist, outlist, extlist, wcs, coord, db, pltemp, outsec
	int	nc, nl, cmin, cmax, lmin, lmax
	real	rval, xmin, xmax, ymin, ymax

	cache	sections, mscextensions, mscgmask

	inlist = mktemp ("tmp$iraf")
	outlist = mktemp ("tmp$iraf")
	extlist = mktemp ("tmp$iraf")
	coord = mktemp ("tmp$iraf")
	db = mktemp ("tmp$iraf")
	pltemp = mktemp ("tmp")

	# Expand lists.
	sections (input, option="fullname", > inlist)
	sections (output, option="fullname", > outlist)
	ref = reference
	if (verbose && ref != "")
	    printf ("Using `%s' for the WCS reference\n", ref)
	fd_in = inlist
	fd_out = outlist
	while (fscan (fd_in, in) != EOF) {
	    if (fscan (fd_out, out) == EOF)
		break

	    pl = out
	    nc = strlen (pl)
	    if (nc > 5 && substr (pl, nc-4, nc) == ".fits")
		pl = substr (pl, 1, nc-5)
	    else if (nc > 4 && substr (out, nc-3, nc) == ".imh")
		pl = substr (pl, 1, nc-4)
	    pl = pl // "_bpm.fits"
		
	    if (ref == "") {
		if (verbose)
		    printf ("Using `%s' for the WCS reference\n", in)
		ref = out
	    }
	    if (imaccess (out)) {
		printf ("Warning: Image already exists (%s)\n", out)
		next
	    }

	    # Expand extensions.
	    mscextensions (in, output="file", index="0-", extname="",
		extver="", lindex=no, lname=yes, lver=no,
		ikparams="", > extlist)
	    if (mscextensions.nimages < 1) {
		printf ("WARNING: No input image data found in `%s'.\n", in)
		delete (extlist, verify=no)
		next
	    }

	    # Create template output image.
	    if (verbose)
		printf ("Creating empty output mosaic %s ...\n", out)
	    msctemplate ("@"//extlist, out, reference=ref, blank=blank,
	       border=0, projection="", pixtype="real")
	    if (pixmask) {
		msctemplate ("@"//extlist, pl, reference=ref, blank=10000.,
		    border=0, projection="", pixtype="short")
		hedit (out, "bpm", pl, add+, del-, update+, verify-, show-)
	    } else
		hedit (out, "bpm", add-, del+, update+, verify-, show-)
	    hselect ("@"//extlist, "gain", yes) | average (data_value=0.) |\
		scan (rval)
	    hedit (out, "gain", rval, add+, del-, update+, verify-, show-)
	    hselect ("@"//extlist, "rdnoise", yes) | average (data_value=0.) |\
		scan (rval)
	    hedit (out, "rdnoise", rval, add+, del-, update+, verify-,
		show-)
	    hedit (out,
		"nextend,detsec,ccdsec,ampsec,imageid,datasec,trimsec,biassec",
		add-, del+, update+, verify-, show-)

	    # Create output mosaic.
	    fd_ext = extlist
	    while (fscan (fd_ext, image) != EOF) {
		if (verbose)
		    printf ("Mapping %s to %s ...\n", image, out)

		# Trim data.
		hselect (image, "naxis1,naxis2", yes) | scan (nc, nl)
		cmin = 1+ntrim; cmax = nc-ntrim
		lmin = 1+ntrim; lmax = nl-ntrim
		printf ("%s[%d:%d,%d:%d]\n", image, cmin, cmax, lmin, lmax) |
		    scan (image)

		# Determine grid points and mapping of extn into the mosaic.
		nc = cmax - cmin + 1
		nl = lmax - lmin + 1
		xmin = (nc - 1.) / (nx - 1.)
		ymin = (nl - 1.) / (ny - 1.)
		for (ymax=1; ymax<=nl+1; ymax=ymax+ymin)
		    for (xmax=1; xmax<=nc+1; xmax=xmax+xmin)
			print (xmax, ymax, xmax, ymax, >> coord)
		mscctran (coord, db, image, "logical", "world",
		    columns="1 2", units="", formats="%.3H %.2h",
		    min_sigdigit=9, verbose=no)
		delete (coord, verify-)
		wcsctran (db, coord, out, inwcs="world", outwcs="logical",
		    columns="1 2", units="hours native", formats="",
		    min_sigdigit=9, verbose=no)
		delete (db, verify-)

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

		hselect (out, "naxis1,naxis2", yes) | scan (nc, nl)
		cmin = max (1, nint (xmin - 1.5))
		cmax = min (nc, nint (xmax + 1.5))
		lmin = max (1, nint (ymin - 1.5))
		lmax = min (nl, nint (ymax + 1.5))

		# Compute transformation for extension.
		geomap (coord, db, cmin, cmax, lmin, lmax, transforms="",
		    results="", fitgeometry=fitgeometry, function="polynomial",
		    xxorder=xxorder, xyorder=xyorder, xxterms=xxterms,
		    yxorder=yxorder, yyorder=yyorder, yxterms=yxterms,
		    reject=INDEF, calctype="double", verbose=no,
		    interactive=interactive, graphics="stdgraph", cursor="")

		# Transform extension into output mosaic.
#		cmin = cmin + ntrim
#		cmax = cmax - ntrim
#		lmin = lmin + ntrim
#		lmax = lmax - ntrim
		printf ("%s[%d:%d,%d:%d]\n", out, cmin, cmax, lmin, lmax) |
		    scan (outsec)
		geotran (image, outsec, db, coord, geometry="geometric",
		    xin=INDEF, yin=INDEF, xshift=INDEF, yshift=INDEF,
		    xout=INDEF, yout=INDEF, xmag=INDEF, ymag=INDEF,
		    xrotation=INDEF, yrotation=INDEF, xmin=cmin, xmax=cmax,
		    ymin=lmin, ymax=lmax, xsample=10., ysample=10.,
		    xscale=1., yscale=1., ncols=INDEF, nlines=INDEF,
		    interpolant=interpolant, boundary=boundary,
		    constant=constant, fluxconserve=fluxconserve,
		    nxblock=nxblock, nyblock=nyblock, verbose=no)

		if (pixmask) {
		    mscgmask (image, pltemp, "BPM", mval=10000)
		    printf ("%s[%d:%d,%d:%d]\n",
			pl, cmin, cmax, lmin, lmax) |
			scan (outsec)
		    geotran (pltemp, outsec, db, coord,
			geometry="geometric", xin=INDEF, yin=INDEF,
			xshift=INDEF, yshift=INDEF, xout=INDEF,
			yout=INDEF, xmag=INDEF, ymag=INDEF,
			xrotation=INDEF, yrotation=INDEF, xmin=cmin,
			xmax=cmax, ymin=lmin, ymax=lmax, xsample=10.,
			ysample=10., interpolant=minterpolant,
			boundary="constant", constant=10000.,
			fluxconserve=no, nxblock=nxblock,
			nyblock=nyblock, verbose=no)
		    imdelete (pltemp, verify-)
		}

		delete (coord, verify=no)
		delete (db, verify=no)
	    }
	    fd_ext = ""; delete (extlist, verify=no)

	    if (pixmask) {
		pltemp = pl
		pl = out
		nc = strlen (pl)
		if (nc > 5 && substr (pl, nc-4, nc) == ".fits")
		    pl = substr (pl, 1, nc-5)
		else if (nc > 4 && substr (out, nc-3, nc) == ".imh")
		    pl = substr (pl, 1, nc-4)
		pl = pl // "_bpm.pl"
		#imcopy (pltemp, pl, verbose-)
		imfunc (pltemp, pl, function="abs", verbose-)
		imdelete (pltemp, verify-)
		hedit (out, "bpm", pl, add+, del-, update+, verify-, show-)
	    }
	}
	fd_out = ""; delete (outlist, verify=no)
	fd_in = ""; delete (inlist, verify=no)
end
