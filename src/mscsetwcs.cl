# MSCSETWCS -- Set the Mosaic WCS from a database and RA/Dec header keywords.
#
# If no database is specified (a value of "") then only the CRVAL are updated.

procedure mscsetwcs (images, database)

string	images			{prompt="Mosaic images"}
file	database = ""		{prompt="WCS database"}
string	ra = "ra"		{prompt="Right ascension keyword (hours)"}
string	dec = "dec"		{prompt="Declination keyword (degrees)"}
string	equinox = "equinox"	{prompt="Epoch keyword (years)"}
real	ra_offset = 0.		{prompt="RA offset (arcsec)"}
real	dec_offset = 0.		{prompt="Dec offset (arcsec)"}

struct	*extlist

begin
	file	db, inlist, image, logf
	string	ims, extname, str
	real	raval, decval, eqval
	bool	verbose
	struct	wcsastrm

	cache mscextensions

	ims = images
	db = database
	if (logfile == "") {
	    logf = "dev$null"
	    verbose = no
	} else {
	    logf = logfile
	    verbose = yes
	}

	raval = 0.
	decval = 0.

	inlist = mktemp ("tmp$iraf")
	mscextensions (ims, output="file", index="0-", extname="",
	    extver="", lindex=no, lname=yes, lver=no, ikparams="", > inlist)

	extlist = inlist
	while (fscan (extlist, image) != EOF) {
	    if (db != "") {
		match ("WCSASTRM", db, stop-) | scan (str, str, str, wcsastrm)
		if (nscan() > 3)
		    hedit (image, "WCSASTRM", wcsastrm, add+,
			del-, verify-, show-, update+)

		extname = ""
		hselect (image, "extname", yes) | scan (extname)
		if (extname == "")
		    match ("begin", db, stop-) | scan (extname, extname)
		ccsetwcs (image, db, extname, xref=INDEF, yref=INDEF,
		    xmag=INDEF, ymag=INDEF, xrotation=INDEF, yrotation=INDEF,
		    lngref=INDEF, latref=INDEF, lngunits="", latunits="",
		    transpose=no, projection="tan", coosystem="j2000",
		    update=yes, verbose=verbose) |
			match ("hours", "STDIN", stop+) |
			match ("Updating", "STDIN", stop+, >> logf)
		hedit (image, "WCSSOL", add-, del+, verify-, show-, update+)
	    }

	    hselect (image, ra//","//dec//","//equinox, yes) |
		translit ("STDIN", '"', delete+, collapse-) |
		scan (raval, decval, eqval)
	    if (nscan() < 3)
		eqval = 2000.
	    if (nscan() >= 2) {
		if (eqval != 2000.) {
		    printf ("%g %g\n", raval, decval) |
		    precess ("STDIN", eqval, 2000.) |
		    scan (raval, decval)
		}
		decval = decval + dec_offset / 3600.
		raval = raval * 15. + ra_offset / 3600. /
		    cos (decval/57.29577851) 
		hedit (image, "crval1", raval, add+, del-, update+,
		    show-, verify-)
		hedit (image, "crval2", decval, add+, del-, update+,
		    show-, verify-)
		if (verbose)
	    printf ("    Reference point: %.2H %.1h 2000.0 (hours degrees)\n",
			raval, decval, >> logf)
	    }
	}
	extlist = ""; delete (inlist, verify=no)
end
