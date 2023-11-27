# PIPEPARS -- Set up pipeline reduction parameters.
#
# This tasks sets all the parameters used for pipeline processing.

procedure caldb (input)

file	input			{prompt="Mosaic exposure"}
file	caldb = "caldb$"	{prompt="Calibration database"}
string	date = "!DATE-OBS"	{prompt="Date"}

struct	*fd

begin
	string	dateid			#{prompt="Date identification string"}
	string	instrument		#{prompt="Instrument"}
	string	telescope		#{prompt="Telescope"}
	string	filter			#{prompt="Filter"}
	file	parfile			#{prompt="Parameter file"}
	file	wcs			#{prompt="WCS database"}
	file	xtalkfile		#{prompt="Crosstalk file"}
	file	bpm			#{prompt="Instrument bad pixel mask"}
	file	zero			#{prompt="Zero level calibration"}
	file	dark			#{prompt="Dark count calibration"}
	file	flat			#{prompt="Flat field calibration"}
	file	pupil			#{prompt="Pupil template calibration"}
	file	phot			#{prompt="Photometry calibration"}
	file	procrecipe		#{prompt="Processing recipe"}
	file	stackrecipe		#{prompt="Stack recipe\n"}

	string	in, db, dt, im, men, dir, val
	file	file1, file2
	file	temp
	int	year, month, day
	struct	str


	temp = mktemp ("tmp$iraf")

	# Set task parameters.
	in = input
	db = caldb
	dt = date

	# Trim any ".fits extension.
	day = strlen (in)
	if (substr (in, day-4, day) == ".fits")
	    in = substr (in, 1, day-5)

	# Use first extension for header keywords.
	im = in // "[1]"

	# Set date.
	if (substr (dt, 1, 1) == "!")
	    hselect (im, substr(dt,2,1000), yes) | scan (dt)
	if (stridx (dt, "/") > 0) {
	    if (fscanf (dt, "%2d/%2d/%2d", day, month, year) != 3)
		error (1, "Syntax error in date ("//dt//")")
	    year = 1900 + year
	} else {
	    if (fscanf (dt, "%4d-%2d-%2d", year, month, day) != 3)
		error (1, "Syntax error in date ("//dt//")")
	}
	printf ("%04d%02d%02d\n", year, month, day) | scan (val)
	dateid = val

	# Get instrument.
	men = db // "instruments.men"
	hselect (im, "detector", yes) | scan (str)
	match (str, men, stop-) | scan (val)
	instrument = val
	db = db // instrument // "/"

	# Get telescope.
	men = db // "telescopes.men"
	hselect (im, "telescope", yes) | scan (str)
	match (str, men, stop-) | scan (val)
	telescope = val
	db = db // telescope // "/"

	# Get filter.
	men = db // "filters.men"
	hselect (im, "filter", yes) | scan (str)
	match (str, men, stop-) | scan (val)
	filter = val
	db = db // filter // "/"

	# Set parameters.
	parfile = ""
	dir = db // "pars" // "/"
	file1 = dir // dateid
	files (dir//"[0-9]*", sort+, > temp)
	fd = temp
	while (fscan (fd, file2) != EOF) {
	    if (parfile == "")
		parfile = file2
	    if (file1 < file2)
		break
	}
	fd = ""; delete (temp, verify-)
	if (parfile == "")
	    error (1, "No parameter file found")
	cl (< parfile)

	# Set WCS database.
	wcs = ""
	dir = db // "wcs" // "/"
	file1 = dir // dateid
	files (dir//"[0-9]*", sort+, > temp)
	fd = temp
	while (fscan (fd, file2) != EOF) {
	    if (wcs == "")
		wcs = file2
	    if (file1 < file2)
		break
	}
	fd = ""; delete (temp, verify-)
	mscsetwcs.database = wcs

	# Set cross talk.
	xtalkfile = ""
	if (ccdproc.xtalkcor) {
	    dir = db // "xtalk" // "/"
	    file1 = dir // dateid
	    files (dir//"[0-9]*", sort+, > temp)
	    fd = temp
	    while (fscan (fd, file2) != EOF) {
		if (xtalkfile == "")
		    xtalkfile = file2
		if (file1 < file2)
		    break
	    }
	    fd = ""; delete (temp, verify-)
	    if (xtalkfile == "")
		error (1, "No cross-talk file found")
	    ccdproc.xtalkfile = xtalkfile
	}

	# Set BPM.
	bpm = ""
	if (ccdproc.fixpix) {
	    dir = db // "bpm" // "/"
	    file1 = dir // dateid
	    files (dir//"[0-9]*", sort+, > temp)
	    fd = temp
	    while (fscan (fd, file2) != EOF) {
		if (bpm == "")
		    bpm = file2
		if (file1 < file2)
		    break
	    }
	    fd = ""; delete (temp, verify-)
	    if (bpm == "")
		error (1, "No bad pixel file found")
	    ccdproc.fixfile = bpm
	}

	# Set zero level calibration.
	zero = ""
	if (ccdproc.zerocor) {
	    dir = db // "zero" // "/"
	    file1 = dir // dateid // ".fits"
	    files (dir//"[0-9]*.fits", sort+, > temp)
	    fd = temp
	    while (fscan (fd, file2) != EOF) {
		if (zero == "")
		    zero = file2
		if (file1 < file2)
		    break
	    }
	    fd = ""; delete (temp, verify-)
	    if (zero == "")
		error (1, "No zero level calibration found")
	    ccdproc.zero = zero
	}

	# Set dark count calibration.
	dark = ""
	if (ccdproc.darkcor) {
	    dir = db // "dark" // "/"
	    file1 = dir // dateid // ".fits"
	    files (dir//"[0-9]*.fits", sort+, > temp)
	    fd = temp
	    while (fscan (fd, file2) != EOF) {
		if (dark == "")
		    dark = file2
		if (file1 < file2)
		    break
	    }
	    fd = ""; delete (temp, verify-)
	    if (dark == "")
		error (1, "No dark count calibration found")
	    ccdproc.dark = dark
	}

	# Set flat field calibration.
	flat = ""
	if (ccdproc.flatcor) {
	    dir = db // "flat" // "/"
	    file1 = dir // dateid // ".fits"
	    files (dir//"[0-9]*.fits", sort+, > temp)
	    fd = temp
	    while (fscan (fd, file2) != EOF) {
		if (flat == "")
		    flat = file2
		if (file1 < file2)
		    break
	    }
	    fd = ""; delete (temp, verify-)
	    if (flat == "")
		error (1, "No flat field calibration found")
	    ccdproc.flat = flat
	}

	# Set pupil template.
	pupil = ""
	dir = db // "pupil" // "/"
	if (access (dir)) {
	    file1 = dir // dateid // ".fits"
	    files (dir//"[0-9]*.fits", sort+, > temp)
	    fd = temp
	    while (fscan (fd, file2) != EOF) {
		if (pupil == "")
		    pupil = file2
		if (file1 < file2)
		    break
	    }
	    fd = ""; delete (temp, verify-)
	    if (pupil == "")
		error (1, "No pupil template found")
	    rmpupil.template = pupil
	}

	# Set photometry configuration file.
	phot = ""
	dir = db // "photcal" // "/"
	if (access (dir)) {
	    file1 = dir // dateid
	    files (dir//"[0-9]*", sort+, > temp)
	    fd = temp
	    while (fscan (fd, file2) != EOF) {
		if (phot == "")
		    phot = file2
		if (file1 < file2)
		    break
	    }
	    fd = ""; delete (temp, verify-)
	    if (phot == "")
		error (1, "No photometry configuration found")
	    mscqphot.photconf = phot
	}

	# Set processing recipe.
	procrecipe = ""
	dir = db // "recipes" // "/"
	file1 = dir // dateid // ".cl"
	files (dir//"[0-9]*.cl", sort+, > temp)
	fd = temp
	while (fscan (fd, file2) != EOF) {
	    if (procrecipe == "")
		procrecipe = file2
	    if (file1 < file2)
		break
	}
	fd = ""; delete (temp, verify-)
	pipeline.procrecipe = procrecipe

	# Set stack recipe.
	stackrecipe = ""
	dir = db // "stackrecipes" // "/"
	file1 = dir // dateid // ".cl"
	files (dir//"[0-9]*.cl", sort+, > temp)
	fd = temp
	while (fscan (fd, file2) != EOF) {
	    if (stackrecipe == "")
		stackrecipe = file2
	    if (file1 < file2)
		break
	}
	fd = ""; delete (temp, verify-)
	pipeline.stackrecipe = stackrecipe

	if (logfile != "") {
	    printf ("Calibration database = %s -> %s\n", caldb, osfn(caldb),
		>> logfile)
	    printf ("Target date = %s\n", dateid, >> logfile) 
	    printf ("\n", >> logfile)

	    if (parfile != "")
		printf ("Parameter file = %s\n", parfile, >> logfile) 
	    if (wcs != "")
		printf ("Astrometry file = %s\n", wcs, >> logfile) 
	    if (xtalkfile != "")
		printf ("Crosstalk file = %s\n", xtalkfile, >> logfile) 
	    if (bpm != "")
		printf ("Instrument bad pixel mask = %s\n", bpm, >> logfile) 
	    if (zero != "")
		printf ("Zero level calibration = %s\n", zero, >> logfile) 
	    if (dark != "")
		printf ("Dark count calibration = %s\n", dark, >> logfile) 
	    if (flat != "")
		printf ("Flat field calibration = %s\n", flat, >> logfile) 
	    if (pupil != "")
		printf ("Pupil template calibration = %s\n", pupil, >> logfile) 
	    if (phot != "")
		printf ("Photometry calibration = %s\n", phot, >> logfile) 
	    if (procrecipe != "")
		printf ("Processing recipe = %s\n", procrecipe, >> logfile) 
	    if (stackrecipe != "")
		printf ("Stack recipe = %s\n", stackrecipe, >> logfile) 
	}
end
