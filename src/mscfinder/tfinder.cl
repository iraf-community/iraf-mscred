procedure tfinder (image, table)

string	image			{prompt="Image name for field information"}
string	table			{prompt="Output table name"}
string	objects = ""		{prompt="List of program object X,Y coords"}
string	logfile = ""		{prompt="Logfile for abridged table listing"}
pset	catpars	= ""		{prompt="Catalog description pset\n"}


real	ra = INDEF		{prompt="RA  of the reference point (hours)",
				    min=0., max=24.}
real	dec = INDEF		{prompt="Dec of the reference point (degrees)",
				    min=-90., max=90.}
real	epoch = INDEF		{prompt="Coordinate epoch"}
string	date_obs = ""		{prompt="Date of the observation (DD/MM/YY)"}

real	xref = INDEF		{prompt="X coordinate of the reference point"}
real	yref = INDEF		{prompt="Y coordinate of the reference point\n"}


bool	opaxis = no	{prompt="Is the reference point on the optical axis?"}
real	del_ra = 0.	{prompt="RA offset of field center from the OA (degrees)"}
real	del_dec = 0.	{prompt="Dec offset of field center from the OA (degrees)\n"}


real	scale			{prompt="Plate or image scale (\"/user)"}
real	edge	= 0.		{prompt="Edge buffer width (user units)"}
int	boxsize	= 9		{prompt="Centering box fullwidth",min=1}
int	frame	= 1		{prompt="Display frame number\n"}


string	north	= "top"		{prompt="Direction of North in the field",
				   enum="top|left|bottom|right"}
string	east	= "left"	{prompt="Direction of East in the field",
				   enum="top|left|bottom|right"}
real	pangle	= 0.		{prompt="Position angle (CCW positive)\n"}


string	sort = "plate_id,region,gsc_id" {prompt="Columns for sorting Output"}
bool	verbose	= yes		{prompt="Print a running commentary?\n"}


string	marker	= "circle"	{prompt="Marker type",
				   enum="point|circle|rectangle|plus|cross"}
string	omarker	= "plus"	{prompt="Overlay marker type",
				   enum="point|circle|rectangle|plus|cross"}
string	goodcolor = "blue"	{prompt="Color of good marker",
				    enum="black|white|red|green|blue|yellow"}
string	badcolor  = "red"	{prompt="Color of bad marker",
				   enum="black|white|red|green|blue|yellow"}
string	objcolor  = "green"	{prompt="Color of program object marker\n",
				   enum="black|white|red|green|blue|yellow"}

bool	replace	= yes		{prompt="replace?", mode="q"	}

string	*list

begin
	string	sp_col = ""

	string	limage, ltable, tmp1, tmp2, buf1, buf2
	real	lscale, naxis1, naxis2, ira, idec, iepoch, iwidth
	int	junk

	cache ("tinfo", "tvmark_", "imgets")

	tmp1 = mktemp ("tmp$tmp")
	tmp2 = mktemp ("tmp$tmp")

	limage = image
	ltable = table

	if (access (ltable) || access (ltable // ".tab")) {
	    printf ("Output table %s already exists, ", ltable)
	    if (replace) {
		tdelete (ltable, ver-, >& "dev$null")
	    } else {
		printf ("\nChoose another table name and try again.\n")
		return
	    }
	}

	if (access (logfile)) {
	    printf ("Log file %s already exists, ", logfile)
	    if (replace) {
		delete (logfile, ver-, >& "dev$null")
	    } else {
		printf ("\nChoose another log file name and try again.\n")
		return
	    }
	}

	lscale = scale		# query the user if no default was supplied

	imgets (limage, "naxis1", >& "dev$null"); naxis1 = int (imgets.value)
	imgets (limage, "naxis2", >& "dev$null"); naxis2 = int (imgets.value)

	if (naxis1 == 0 || naxis2 == 0)
	    error (1, "Problem reading image header")

	if (ra == INDEF) {
	    imgets (limage, "ra", >& "dev$null")
	    ira = real (imgets.value)
	    if (ira == 0)
		error (1, "No RA in image header or parameter file.")
	} else
	    ira = ra

	if (dec == INDEF) {
	    imgets (limage, "dec", >& "dev$null")
	    idec = real (imgets.value)
	    if (idec == 0)
		error (1, "No declination in image header or parameter file.")
	} else
	    idec = dec

	if (epoch == INDEF) {
	    imgets (limage, "epoch", >& "dev$null")
	    iepoch = real (imgets.value)
	    if (iepoch == 0)
		iepoch = INDEF
	} else
	    iepoch = epoch

	iwidth = lscale * (max (naxis1, naxis2) + edge) / 3600.

	if (verbose)
	    print ("\nSearching the Guide Star Catalog index...")

	gscfind (ira, idec, iepoch, iwidth, > tmp1)

	if (verbose) {
	    print ("\nReading the Guide Star Catalog regions:")
	    type (tmp1)
	}

	cdrfits ("@" // tmp1, "1", "aka", template="", long_header=no,
	    short_header=no, datatype="", blank=0., scale=yes, xdimtogf=no,
	    oldirafname=yes, offset=0, > tmp2)

	delete (tmp1, ver-, >& "dev$null")

	if (verbose) {
	    print ("\nExtracting overlapping sources from regions:")
	    type (tmp2)
	}

	tfield ("@" // tmp2, ltable, image=limage, catpars=catpars, ra=ira,
	    dec=idec, epoch=iepoch, date_obs=date_obs, width=iwidth,
	    xref=xref, yref=yref, opaxis=opaxis, del_ra=del_ra,
	    del_dec=del_dec, north=north, east=east, pangle=pangle,
	    scale=lscale, edge=edge)

	tdelete ("@" // tmp2, ver-, >& "dev$null")
	delete (tmp2, ver-, >& "dev$null")

	tinfo (ltable, ttout-)
	if (tinfo.nrows <= 0) {
	    beep
	    print ("\nNo Guide Stars selected for this field!")
	    print ("Check the input parameters and images...")
	    return
	}

	# Provide reasonable defaults for the mark sizes, the extra
	# contour is for bolding (mostly for subsampling in saoimage).
	# This can be overridden within TPEAK by `:eparam tvmark_'.
	tvmark_.radii = (boxsize/2) // "," // (boxsize/2 + 1)
	tvmark_.lengths = (boxsize - 1) // "," // (boxsize + 1)

	if (verbose) {
	    print ("\nInteractive centering using TPEAK:")
	    print ("  The size of the markers matches the centering box (",
		boxsize, "pixels).")
	    print ("  Change the size with the command `:eparam tvmark'.")
	}

	tpeak (limage, ltable, objects=objects, boxsize=boxsize, frame=frame,
	    marker=marker, omarker=omarker, goodcolor=goodcolor,
	    badcolor=badcolor, objcolor=objcolor, imcur="")

	files (sort, sort-, > tmp1)

	list = tmp1
	if (fscan (list, sp_col) == 1)
	    tsort (ltable, sort)	# adjust ascend & casesens externally

	list = ""; delete (tmp1, ver-, >& "dev$null")

	if (logfile != "") {
	    tselect (ltable, tmp1, "CEN_FLAG == 1")
	    tinfo (tmp1, ttout-)

	    if (tinfo.nrows <= 0) {
		print ("No sources were centered!", > logfile)

	    } else {
		print ("List of successfully centered sources:\n", > logfile)
		tprint (tmp1, prparam=no, prdata=yes,
		    pwidth=80, plength=0, showrow=yes, showhdr=yes, lgroup=0,
		    columns="REGION,GSC_ID,X_CENTER,Y_CENTER,MAG_BAND,MAG,CLASS,PLATE_ID",
		    rows="-", option="plain", align=yes, sp_col=sp_col,
		    >> logfile)
	    }

	    tdelete (tmp1, ver-, >& "dev$null")

	    tselect (ltable, tmp1, "CEN_FLAG != 1")
	    tinfo (tmp1, ttout-)

	    if (tinfo.nrows <= 0) {
		print ("\nAll sources were centered!", >> logfile)

	    } else {
		print ("\n\nList of UNcentered sources:\n", >> logfile)
		tprint (tmp1, prparam=no, prdata=yes,
		    pwidth=80, plength=0, showrow=yes, showhdr=yes, lgroup=0,
		    columns="REGION,GSC_ID,X_PRED,Y_PRED,MAG_BAND,MAG,CLASS,PLATE_ID",
		    rows="-", option="plain", align=yes, sp_col=sp_col, >> logfile)
	    }

	    tdelete (tmp1, ver-, >& "dev$null")
	}
end
