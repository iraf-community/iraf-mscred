procedure dssfinder (image)

string	image			{prompt="DSS image name"}
string	objects		= ""	{prompt="List of program object X,Y coords\n"}

bool	update		= no	{prompt="Update image header WCS following fit?"}

bool	interactive	= yes	{prompt="Enter interactive image cursor loop?"}
bool	autocenter	= no	{prompt="Center at the catalog coords when entering task?"}
bool	autodisplay	= yes	{prompt="Redisplay after all-source keystroke command?\n"}

real	pangle		= 0.	{prompt="Position angle (CCW positive)\n"}

int	boxsize		= 9	{prompt="Centering box full width",min=1}
int	subsample	= 1	{prompt="Sampling factor, for display marking only"}
int	frame		= 1	{prompt="Display frame number\n"}

begin
	string	north = "top"
	string	east = "left"
	string	marker	= "circle"
	string	omarker	= "plus"
	string	goodcolor = "blue"
	string	badcolor  = "red"
	string	objcolor  = "green"
	string	date_obs = ""
	real	edge = 200.
#	real	pangle = 0.
	real	scale = 1.7
	real	equinox = 2000.
	real	xref = INDEF
	real	yref = INDEF
	bool	opaxis = no
	real	pi = 3.14159265358979
	bool	firsttime = yes
	bool	newobjects = yes

	string	table, database, logfile, cdecsign
	real	ra, dec, rah, ram, ras, decd, decm, decs
	real	cra, cdec, crah, cram, cras, cdecd, cdecm, cdecs
	real	del_ra, del_dec
#	real	amdx1, amdx2, amdy1, amdy2
#	real	pangle

	string	ra_ref, dec_ref
	real	eq_ref

	string	limage, tmp1, tmp2, buf1, buf2, lrewrite
	bool	lautocenter, lautodisplay, lreselect
	real	naxis1, naxis2, width
	int	junk

	cache ("tinfo", "tvmark_", "imgets")

	tmp1 = mktemp ("tmp$tmp")
	tmp2 = mktemp ("tmp$tmp")

	limage = image
	table = limage // ".tab"
	database = limage // ".db"
	logfile = limage // ".log"

	lautocenter = autocenter
	lautodisplay = autodisplay

	imgets (limage, "naxis1", >& "dev$null"); naxis1 = int (imgets.value)
	imgets (limage, "naxis2", >& "dev$null"); naxis2 = int (imgets.value)

	if (naxis1 == 0 || naxis2 == 0)
	    error (1, "Problem reading image header")

# would have to invert the phenomenological solution to retrieve correctly
#	imgets (limage, "amdx1", >& "dev$null"); amdx1 = real (imgets.value)
#	imgets (limage, "amdx2", >& "dev$null"); amdx2 = real (imgets.value)
#	imgets (limage, "amdy1", >& "dev$null"); amdy1 = real (imgets.value)
#	imgets (limage, "amdy2", >& "dev$null"); amdy2 = real (imgets.value)
#	# average the two sine terms from the rotation matrix using the
#	# small angle approximation for sine, and letting cos(pangle) ~ 1
#	pangle = -90. * ((amdx2/amdy1) - (amdy2/amdx1)) / pi
#	pangle = 90. * ((amdx2/amdy1) - (amdy2/amdx1)) / pi
#printf ("pangle = %.4f degrees\n", pangle)

	imgets (limage, "objctra", >& "dev$null")
	print (imgets.value) | scan (rah, ram, ras)
	ra = rah + (ram + ras/60.) / 60.

	imgets (limage, "objctdec", >& "dev$null")
	print (imgets.value) | scan (decd, decm, decs)
	dec = abs(decd) + (decm + decs/60.) / 60.
	if (decd < 0)
	    dec = -dec

	hselect (limage, "pltrah,pltram,pltras", yes) |
	    scan (crah, cram, cras)
	cra = crah + (cram + cras/60.) / 60.

	hselect (limage, "pltdecd,pltdecm,pltdecs", yes) |
	    scan (cdecd, cdecm, cdecs)
	hselect (limage, "pltdecsn", yes) | scan (cdecsign)
	cdec = cdecd + (cdecm + cdecs/60.) / 60.
	if (cdecsign == "-")
	    cdec = -cdec

	ra_ref = cra
	dec_ref = cdec
	eq_ref = equinox

	del_ra = 15. * (ra - cra) * cos (dec*pi/180.)
	del_dec = dec - cdec

	width = scale * (max (naxis1, naxis2) + edge) / 3600.

	if (access (table) || access (table // ".tab")) {
	    printf ("Output table %s exists, ", table)
	    if (_qpars.reopen) {
		firsttime = no
		if (lautocenter)
		    lautocenter = _qpars.recenter

	    } else {
		printf ("  ...in that case, ")
		if (_qpars.replace) {
		    tdelete (table, ver-, >& "dev$null")
		} else {
		    printf ("\nChoose another table name and try again.\n")
		    return
		}
	    }
	}

        if (access (logfile)) {
            printf ("Log file %s exists", logfile)
            lrewrite = _qpars.rewrite
            if (lrewrite == "replace") {
                delete (logfile, ver-, >& "dev$null")
            } else if (lrewrite != "append") {
                printf ("\nChoose another filename and try again.\n")
                return
            }
        }

        if (logfile != "" && ! access (logfile))
            printf ("", > logfile)

	if (firsttime) {
	    if (interactive)
		print ("\nSearching the Guide Star Catalog index...")

	    gscfind (ra, dec, equinox, width, > tmp1)

	    if (interactive) {
		print ("\nReading the Guide Star Catalog regions:")
		type (tmp1)
	    }

	    cdrfits ("@" // tmp1, "1", "aka", template="", long_header=no,
		short_header=no, datatype="", blank=0., scale=yes, xdimtogf=no,
		oldirafname=yes, offset=0, > tmp2)

	    delete (tmp1, ver-, >& "dev$null")

	    if (interactive) {
		print ("\nExtracting overlapping sources from regions:")
		type (tmp2)
	    }

	    tfield ("@" // tmp2, table, image=limage, catpars="", ra=ra,
		dec=dec, epoch=equinox, date_obs=date_obs, width=width,
		xref=xref, yref=yref, opaxis=opaxis, del_ra=del_ra,
		del_dec=del_dec, north=north, east=east, pangle=pangle,
		scale=scale, edge=edge)

	    tdelete ("@" // tmp2, ver-, >& "dev$null")
	    delete (tmp2, ver-, >& "dev$null")
	}

	tinfo (table, ttout-)
	if (tinfo.nrows <= 0) {
	    beep
	    print ("\nNo Guide Stars selected for this field!")
	    print ("Check the input parameters and images...")
	    return
	}

	if (firsttime)
	    tsort (table, "plate_id,region,gsc_id", ascend+, casesens+)

	# Provide reasonable defaults for the mark sizes, the extra
	# contour is for bolding (mostly for subsampling in saoimage).
	# This can be overridden within TPEAK by `:eparam tvmark_'.
	tvmark_.radii = (boxsize/2) // "," // (boxsize/2 + 1)
	tvmark_.lengths = (boxsize - 1) // "," // (boxsize + 1)

	if (interactive) {
	    printf ("\nInteractive centering using TPEAK:\n")
	    printf ( "  The size of the markers matches the centering box ")
		printf ("(%d pixels).\n", boxsize)
	    printf ("  Change the size with the command `:eparam tvmark'.\n")
	}

	lreselect = tpeak.reselect

	if (! firsttime) {
	    if (objects != "") {
		printf ("\nRead/reread objects '%s' into table '%s' ",
		    objects, table)
		newobjects = _qpars.go_ahead
	    }

	    if (lreselect) {
		printf ("\nSelect a new catalog subset ")
		lreselect = _qpars.go_ahead
	    }
	}

	if (newobjects) {
	    tpeak (limage, table, database, objects=objects,
		ra_ref=ra_ref, dec_ref=dec_ref, eq_ref=eq_ref,
		autocenter=lautocenter, autodisplay=lautodisplay,
		reselect=lreselect, interactive=interactive, boxsize=boxsize,
		subsample=subsample, rotate=0., xscale=100., yscale=100.,
		xshift=0, yshift=0, frame=frame, marker=marker,
		omarker=omarker, goodcolor=goodcolor,
		badcolor=badcolor, objcolor=objcolor, imcur="")

	} else {
	    tpeak (limage, table, database, objects="", autocenter=lautocenter,
		ra_ref=ra_ref, dec_ref=dec_ref, eq_ref=eq_ref,
		autodisplay=lautodisplay, reselect=lreselect,
		interactive=interactive, boxsize=boxsize, subsample=subsample,
		rotate=0., xscale=100., yscale=100., xshift=0, yshift=0,
		frame=frame, marker=marker, omarker=omarker,
		goodcolor=goodcolor, badcolor=badcolor, objcolor=objcolor,
		imcur="")
	}

	if (logfile != "")
	    logfile (table, logfile, append+)
end
