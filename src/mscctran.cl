# MSCCTRAN -- Coordinate transformations using plate solution but using
# reference coordinate from the image.  The plate solution is specified by
# the header keyword WCSSOL giving the database filename and the record
# name.  The plate solution is assumed to be in the physical coordinate
# system of the image.  Input and output pixel coordinates are in logical
# coordinates which are converted to physical coordinates for transformation
# by the plate solution.  Input and output celestial coordinates are ra in
# hours on axis 1 and dec in degrees on axis 2.

procedure mscctran (input, output, image, forward)

file	input		{prompt="Input coordinate file"}
file	output		{prompt="Output coordinate file"}
file	image		{prompt="Input image"}
bool	forward = yes	{prompt="Logical to world?"}

int	xcolumn = 1	{prompt="Input column for ra/longitude/x coordinates"}
int	ycolumn = 2	{prompt="Input column for dec/latitude/y coordinates"}
string	lngformat = ""	{prompt="Output format of ra/longitude/x coordinates"}
string	latformat = ""	{prompt="Output format of dec/latitude/x coordinates"}
int	min_sigdigit = 7 {prompt="Minimum precision of the output coordinates"}
bool	wcssol = yes	{prompt="Use WCS plate solution?"}

struct	*fd

begin
	file	in, out, im, database, db
	bool	fwd
	struct	wcsstruct
	string	cols, fmts, sol, key, value, wcs
	real	crval1, crval2
	int	n

	in = input
	out = output
	im = image
	fwd = forward

	db = mktemp ("tmp$iraf")

	# Columns and formats for WCSCTRAN
	cols = xcolumn // " " // ycolumn
	fmts = lngformat // " " // latformat

	# Set the plate solution if requested.
	database = ""
	if (wcssol) {
	    hselect (im, "wcssol", yes) | scan (wcsstruct)
	    if (fscan (wcsstruct, database, sol) == 2) {
		if (access (database) == NO) {
		    printf ("Warning: WCS database `%s' not found.", database)
		    printf ("  Using header WCS.\n")
		    database = ""
		}
	    } else
		database = ""
	}

	# Coordinate transform without a plate solution.
	if (database == "") {
	    if (fwd) {
		wcsctran (in, "STDOUT", im, "logical", "world",
		    columns=cols, units="", formats="%.4H %.3h",
		    min_sigdigit=min_sigdigit, verbose=no) |
		wcsctran ("STDIN", out, im, "world", "world",
		    columns=cols, units="native native", formats=fmts,
		    min_sigdigit=min_sigdigit, verbose=no)
	    } else {
		wcsctran (in, out, im, "world", "logical",
		    columns=cols, units="hours native", formats=fmts,
		    min_sigdigit=min_sigdigit, verbose=no)
	    }
	    return
	}
	    
	# Coordinate transform with a plate solution.
	fd = database
	while (fscan (fd, key, value) != EOF) {
	    if (key != "begin")
		next
	    if (value != sol)
		next
	    printf ("%s\t%s\n", key, value, > db)
	    break
	}
	if (access (db) == NO)
	    error (1, "plate solution not found")

	# Copy and modify the plate solution.
	hselect (im, "crval1,crval2", yes) | scan (crval1, crval2)
	crval1 = crval1 / 15.
	while (fscan (fd, key, value) != EOF) {
	    if (key == "begin")
		break
	    else if (key == "lngref")
		printf ("%s\t%g\n", key, crval1, >> db)
	    else if (key == "latref")
		printf ("%s\t%g\n", key, crval2, >> db)
	    else
		printf ("%s\t%s\n", key, value, >> db)
	}
	fd = ""

	# Transform the coordinates.
	wcs = "physical"
	if (fwd) {
	    wcsctran (in, "STDOUT", im, "logical", wcs,
		columns=cols, units="", formats="",
		min_sigdigit=min_sigdigit, verbose=no) |
	    cctran ("STDIN", out, db, sol, geometry="geometric",
		forward=yes, xcolumn=xcolumn, ycolumn=ycolumn,
		lngunits="", latunits="", lngformat=lngformat,
		latformat=latformat, min_sigdigit=min_sigdigit)
	} else {
	    cctran (in, "STDOUT", db, sol, geometry="geometric",
		forward=no, xcolumn=xcolumn, ycolumn=ycolumn,
		lngunits="", latunits="", lngformat="", latformat="",
		min_sigdigit=min_sigdigit) |
	    wcsctran ("STDIN", out, im, wcs, "logical",
		columns=cols, units="", formats=fmts,
		min_sigdigit=min_sigdigit, verbose=no)
	}
	delete (db, verify-)
end
