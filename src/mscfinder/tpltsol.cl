procedure tpltsol (image, table, database)

string	image			{prompt="Input image name"}
string	table			{prompt="Input table name"}
string	database		{prompt="Output database name"}
string	results = ""		{prompt="Results summary file\n"}

bool	append = no		{prompt="Append to database and results file?"}
bool	imupdate = no		{prompt="Update the image WCS?"}
bool	tabupdate = no		{prompt="Update input table?"}
bool	refitcat = no		{prompt="Recompute XYs for uncentered sources?"}
bool	verbose = yes		{prompt="Print verbose progress messages?\n"}

bool	dssheader = yes		{prompt="Read plate center from DSS header?"}
string	ra_ref = ""		{prompt="Plate center RA (hours)"}
string	dec_ref = ""		{prompt="Plate center Dec (degrees)"}
real	eq_ref = INDEF		{prompt="Plate center coordinate equinox\n"}

string	inpixsys = "logical"	{prompt="Input pixel system",
				    enum="|logical|physical|"}
string	outpixsys = "logical"	{prompt="Output pixel system",
				    enum="|logical|physical|"}
string	insystem = "j2000"	{prompt="Input celestial coordinate system\n"}

string	projection = "tan"	{prompt="Sky projection geometry"}
string	fitgeometry = "general" {prompt="Fitting geometry",
			enum="|shift|xyscale|rotate|rscale|rxyscale|general"}
string	function = "polynomial" {prompt="Surface type\n",
				    enum="|chebyshev|legendre|polynomial"}

int	xxorder = 2		{prompt="Order of xi fit in x", min=2}
int	xyorder = 2		{prompt="Order of xi fit in y", min=2}
string	xxterms = "half"	{prompt="Xi fit cross terms type\n",
				    enum="|none|half|full|"}

int	yxorder = 2		{prompt="Order of eta fit in x", min=2}
int	yyorder = 2		{prompt="Order of eta fit in y", min=2}
string	yxterms = "half"	{prompt="Eta fit cross terms type?\n",
				    enum="|none|half|full|"}

real	reject = INDEF		{prompt="Rejection limit in sigma units\n"}

bool	interactive = yes	{prompt="Fit the transformation interactively?"}
string	graphics = "stdgraph"	{prompt="Default graphics device"}
gcur	cursor = ""		{prompt="Graphics cursor"}

pset	catpars	= ""		{prompt="Catalog description pset\n"}

string	*list

begin
	string	a1=""#"
	string	a2, a3, a4, a5, a6, a7, a8
	string	limage, ltable, ldatabase, lresults, decsign, solution, geom
	string	tmp1, tmp2, tmp3, tmpresults, buf, lrewrite
	real	rah, ram, ras, decd, decm, decs, ra, dec, pltequinox
	real	xpred, ypred, xmin, xmax, ymin, ymax
	int	rowno, index, nobj

	cache ("tinfo", "tabpar")

	tmp1 = mktemp ("tmp$tmp")
	tmp2 = mktemp ("tmp$tmp")
	tmp3 = mktemp ("tmp$tmp")
	tmpresults = mktemp ("tmp$tmp")

	limage = image
	ltable = table
	ldatabase = database
	lresults = results

	if (!append) {
	    if (access(ldatabase)) {
		printf ("Output database %s exists", ldatabase)
		lrewrite = _qpars.rewrite
		if (lrewrite == "replace") {
		    delete (ldatabase, ver-, >& "dev$null")
		} else if (lrewrite != "append") {
		    printf ("\nChoose another filename and try again.\n")
		    return
		}
	    }

	    if (access (lresults)) {
		printf ("Results file %s exists", lresults)
		lrewrite = _qpars.rewrite
		if (lrewrite == "replace") {
		    delete (lresults, ver-, >& "dev$null")
		} else if (lrewrite != "append") {
		    printf ("\nChoose another filename and try again.\n")
		    return
		}
	    }
        }

        if (lresults == "" && ! tabupdate)
	    tmpresults = ""

	if (dssheader) {
	    hselect (limage, "PLTRAH,PLTRAM,PLTRAS", yes) | scan (rah, ram, ras)
	    ra = rah + ((ram + ras/60.0) / 60.0)

	    hselect (limage, "PLTDECSN,PLTDECD,PLTDECM,PLTDECS", yes) |
		scan (decsign, decd, decm, decs)
	    dec = decd + ((decm + decs/60.0) / 60.0)
	    if (decsign == "-")
		dec = -dec

	    hselect (limage, "EQUINOX", yes) | scan (pltequinox)

	} else {
	    ra = real (ra_ref)
	    dec = real (dec_ref)
	    pltequinox = eq_ref

	}

	tcopy (ltable, tmp2, verbose-)
	tcalc (tmp2, "rowno", "rownum", datatype="int", colunits="",
	    colfmt="%5d")

	buf =	catpars.sub_col // " == 1 && " //
		catpars.cen_col // " == 1 && " //
		catpars.obj_col // " == 0"

	tselect (tmp2, tmp1, buf)
	tdelete (tmp2, ver-, >& "dev$null")

	tinfo (tmp1, ttout-)
	nobj = tinfo.nrows
#	This is the FINDER logic which is too restrictive.
#	if (nobj <= 0) {
#	    printf ("No centered sources in input table\n")
#	    return
#	} else if (nobj < 4 && fitgeometry != "shift") {
#	    printf ("Too few sources, computing only shift\n")
#	    geom = "shift"
#	} else if (nobj < 6 && fitgeometry == "general") {
#	    printf ("Too few sources, computing only xyscale \n")
#	    geom = "xyscale"
#	} else if (nobj <= 8 && fitgeometry == "general") {
#	    printf ("Too few sources, computing only rxyscale\n")
#	    geom = "rxyscale"
#	} else
#	    geom = fitgeometry

	if (nobj < 3) {
	    printf ("A minimum of 3 centered sources is required\n")
	    return
	} else if (nobj < 6)
	    geom = "rxyscale"
	else
	    geom = fitgeometry

	tcalc (tmp1, "RA_HRS", "RA_DEG / 15.0d0", datatype="double",
	    colunits="", colfmt="%13.4h")

	tchcol (tmp1, "X_CENTER", "", "%10.3f", "", verbose=no)
	tchcol (tmp1, "Y_CENTER", "", "%10.3f", "", verbose=no)
	tchcol (tmp1, "DEC_DEG", "", "%13.3h", "", verbose=no)

	#outpixsys = inpixsys
	if (inpixsys != outpixsys) {
	    tprint (tmp1, columns="x_center,y_center,ra_hrs,dec_deg",
		prparam-, prdata+, pwidth=80, plength=0, showrow-, showhdr-,
		rows="-", option="plain", sp_col="", lgroup=0, > tmp3)
	    xmin = 1
	    ymin = 1
	    hselect (limage, "NAXIS1,NAXIS2", yes) | scan (xmax, ymax)
	    wcsctran (tmp3, tmp2, limage, inpixsys, outpixsys, columns="1 2",
		units="", formats="", min_sigdigit=9, verbose=no)
	    hselect (limage, "NAXIS1,NAXIS2", yes) | scan (xmax, ymax)
	    print (xmin, ymin) | wcsctran ("STDIN", "STDOUT", limage, inpixsys,
		outpixsys, columns="1,2", units="", formats="", min_sigdigit=9,
		verbose=no) | scan (xpred, ypred)
	    print (xmax, ymax) | wcsctran ("STDIN", "STDOUT", limage, inpixsys,
		outpixsys, columns="1,2", units="", formats="", min_sigdigit=9,
		verbose=no) | scan (xmax, ymax)
	    xmin = min (xpred, xmax)
	    xmax = max (xpred, xmax)
	    ymin = min (ypred, ymax)
	    ymax = max (ypred, ymax)
	    delete (tmp3, ver-, >& "dev$null")
	} else {
	    tprint (tmp1, columns="x_center,y_center,ra_hrs,dec_deg",
		prparam-, prdata+, pwidth=80, plength=0, showrow-, showhdr-,
		rows="-", option="plain", sp_col="", lgroup=0, > tmp2)
	    xmin = 1
	    ymin = 1
	    hselect (limage, "NAXIS1,NAXIS2", yes) | scan (xmax, ymax)
	}

	solution = ""
	hselect (image, "extname", yes) | scan (solution)
	if (solution == "")
	    solution = limage
	ccmap (tmp2, ldatabase, solutions=solution, images=limage,
	    results=tmpresults, xcolumn=1, ycolumn=2, lngcolumn=3,
	    latcolumn=4, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
	    lngunits="hours", latunits="degrees", insystem=insystem,
	    refpoint="user", lngref=ra, latref=dec,
	    refsystem="equinox"//pltequinox,
	    lngrefunits="hours", latrefunits="degrees",
	    projection=projection, fitgeometry=geom,
	    function=function, xxorder=xxorder, xyorder=xyorder,
	    xxterms=xxterms, yxorder=yxorder, yyorder=yyorder,
	    yxterms=yxterms, reject=reject, update=imupdate,
	    pixsystem=outpixsys, verbose=verbose, interactive=interactive,
	    graphics=graphics, cursor=cursor)

	delete (tmp2, ver-, >& "dev$null")

	if (tabupdate) {
	    printf (" \ntransferring deletions to the table...")

	    index = 0
	    list = tmpresults
	    while (fscan (list, a1, a2, a3, a4, a5, a6, a7, a8) != EOF) {
		if (nscan() != 8 || substr(a1,1,1) == "#")
		    next

		index += 1

		if (a5=="INDEF" && a6=="INDEF" && a7=="INDEF" && a8=="INDEF") {
		    tabpar (tmp1, "rowno", index)
		    partab (0, ltable, catpars.cen_col, int(tabpar.value))
		}
	    }

	    list = ""

	    tdelete (tmp1, ver-, >& "dev$null")

	    printf ("done\n")

	    if (refitcat) {
		tcopy (ltable, tmp1, verbose-)
		tcalc (tmp1, "rowno", "rownum", datatype="int", colunits="",
		    colfmt="%5d")

		buf =	catpars.cen_col // " == 0 && " //
			catpars.obj_col // " == 0"
		tselect (tmp1, tmp2, buf)
		tdelete (tmp1, ver-, >& "dev$null")

		tinfo (tmp2, ttout-)
		if (tinfo.nrows >= 1) {
		    printf ("calculating coords for uncentered sources...")

		    tcalc (tmp2, "RA_HRS", "RA_DEG / 15.0d0", datatype="double",
			colunits="", colfmt="%13.4h")

#		    tchcol (tmp2, "X_CENTER", "", "%8.2f", "", verbose=no)
#		    tchcol (tmp2, "Y_CENTER", "", "%8.2f", "", verbose=no)
		    tchcol (tmp2, "DEC_DEG", "", "%13.3h", "", verbose=no)

		    tprint (tmp2, columns="rowno,RA_HRS,DEC_DEG",
			prparam-, prdata+, pwidth=80, plength=0, showrow-,
			showhdr-, rows="-", option="plain", sp_col="",
			lgroup=0, > tmp1)

		    tdelete (tmp2, ver-, >& "dev$null")

		    if (inpixsys != outpixsys) {
			cctran (tmp1, tmp3, ldatabase, solution,
			    geometry="geometric", forward=no,
			    lngunits="hours", latunits="degrees",
			    xcolumn=2, ycolumn=3, lngformat="%10.3f",
			    latformat="%10.3f", min_sigdigit=9)
			wcsctran (tmp3, tmp2, limage, outpixsys, inpixsys,
			    columns="2 3", units="", formats="",
			    min_sigdigit=9, verbose=no)
			delete (tmp3, ver-, >& "dev$null")
		    } else {
			cctran (tmp1, tmp2, ldatabase, solution,
			    geometry="geometric", forward=no,
			    lngunits="hours", latunits="degrees",
			    xcolumn=2, ycolumn=3, lngformat="%10.3f",
			    latformat="%10.3f", min_sigdigit=9)
		    }


		    delete (tmp1, ver-, >& "dev$null")

		    printf ("done\n")

		printf ("transferring new predicted catalog coords to table...")

		    list = tmp2
		    while (fscan (list, rowno, xpred, ypred) != EOF) {
			partab (xpred, ltable, "X_CENTER", rowno)
			partab (ypred, ltable, "Y_CENTER", rowno)
		    }

		    list = ""
		    delete (tmp2, ver-, >& "dev$null")

		    printf ("done\n")

		} else {
		    tdelete (tmp1, ver-, >& "dev$null")

		}

	    } else {
		tcopy (ltable, tmp1, verbose-)
		tcalc (tmp1, "rowno", "rownum", datatype="int", colunits="",
		    colfmt="%5d")

		buf =	catpars.obj_col // " == 1"
		tselect (tmp1, tmp2, buf)
		tdelete (tmp1, ver-, >& "dev$null")

		tinfo (tmp2, ttout-)
		if (tinfo.nrows >= 1) {
		    printf ("calculating object coordinates...")

		    tchcol (tmp2, "X_CENTER", "", "%10.3f", "", verbose=no)
		    tchcol (tmp2, "Y_CENTER", "", "%10.3f", "", verbose=no)

		    tprint (tmp2, columns="rowno,x_center,y_center",
			prparam-, prdata+, pwidth=80, plength=0, showrow-,
			showhdr-, rows="-", option="plain", sp_col="",
			lgroup=0, > tmp1)

		    if (inpixsys != outpixsys) {
			tprint (tmp2, columns="rowno,x_center,y_center",
			    prparam-, prdata+, pwidth=80, plength=0, showrow-,
			    showhdr-, rows="-", option="plain", sp_col="",
			    lgroup=0, > tmp3)
			wcsctran (tmp3, tmp2, limage, inpixsys, outpixsys,
			    columns="2 3", units="", formats="",
			    min_sigdigit=9, verbose=no)
			delete (tmp3, ver-, >& "dev$null")
		    } else {
			tprint (tmp2, columns="rowno,x_center,y_center",
			    prparam-, prdata+, pwidth=80, plength=0, showrow-,
			    showhdr-, rows="-", option="plain", sp_col="",
			    lgroup=0, > tmp1)
		    }

		    tdelete (tmp2, ver-, >& "dev$null")

		    cctran (tmp1, tmp2, ldatabase, limage, geometry="geometric",
			forward+, xcolumn=2, ycolumn=3, lngformat="%13.4h",
			latformat="%13.3h", min_sigdigits=7)

		    delete (tmp1, ver-, >& "dev$null")

		    printf ("done\n")

		    printf ("transferring object coordinates to table...")

		    list = tmp2
		    while (fscan (list, rowno, ra, dec) != EOF) {
			ra = ra * 15.
			partab (ra, ltable, "RA_DEG", rowno)
			partab (dec, ltable, "DEC_DEG", rowno)
		    }

		    list = ""
		    delete (tmp2, ver-, >& "dev$null")

		    printf ("done\n")

		} else {
		    tdelete (tmp2, ver-, >& "dev$null")
		}

	    }

	} else {
	    tdelete (tmp1, ver-, >& "dev$null")

	}

	if (lresults != "")
	    rename (tmpresults, lresults, field="all")
	else
	    delete (tmpresults, ver-, >& "dev$null")
end
