# MSCTPEAK -- Run TPEAK on a Mosaic image given a list of celestial coordinates.
# The input is a list of WCS calibrated images and a list of ra(hours),
# dec(degrees), and optional integer id.

procedure msctpeak (images, coordinates, database)

string	images		    {prompt="List of WCS calibrated Mosaic images"}
file	coordinates	    {prompt="List of ra(hr), dec(deg), optional id"}
file	database	    {prompt="Database for astrometric fit"}
string	extname = ""	    {prompt="Extensions"}
real	epoch = 1950.	    {prompt="Coordinate epoch"}
bool	update = yes	    {prompt="Update image header WCS following fit?"}
bool	autocenter = no     {prompt="Center catalog coords when entering task?"}
int	boxsize = 9	    {prompt="Centering box fullwidth\n", min=1}

string	projection = "tan"  {prompt="Sky projection geometry"}
string	fitgeometry = "general" {prompt="Fitting geometry",
			enum="|shift|xyscale|rotate|rscale|rxyscale|general"}
string	function = "polynomial" {prompt="Surface type",
				    enum="|chebyshev|legendre|polynomial"}
int	xxorder = 3	    {prompt="Order of xi fit in x", min=2}
int	xyorder = 3	    {prompt="Order of xi fit in y", min=2}
string	xxterms = "half"    {prompt="Xi fit cross terms type\n",
				    enum="|none|half|full|"}
int	yxorder = 3	    {prompt="Order of eta fit in x", min=2}
int	yyorder = 3	    {prompt="Order of eta fit in y", min=2}
string	yxterms = "half"    {prompt="Eta fit cross terms type?\n",
				    enum="|none|half|full|"}
real	reject = INDEF	    {prompt="Rejection limit in sigma units\n"}

bool	interactive = yes   {prompt="Enter interactive image cursor loop?"}
int	frame = 1	    {prompt="Display frame number", min=1, max=4}
string	marker = "circle"   {prompt="Marker type",
				enum="point|circle|rectangle|plus|cross"}
string	omarker = "plus"   {prompt="Overlay marker type",
				enum="point|circle|rectangle|plus|cross"}
string	goodcolor = "blue"   {prompt="Color of good marker",
				enum="black|white|red|green|blue|yellow"}
string	badcolor = "red"   {prompt="Color of bad marker",
				enum="black|white|red|green|blue|yellow"}

struct	*fdimages
struct	*fdcoords

begin
	file	image, coords, db, temp1, temp2, temp3, temp4
	int	id, nc, nl
	real	xpos, ypos, ra, dec, eq_ref
	string	ra_ref, dec_ref

	cache mscextensions

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")
	temp3 = mktemp ("tmp$iraf")
	temp4 = mktemp ("tmp$iraf")

	# Query parameters.
	image = images
	coords = coordinates
	db = database

	# Expand input image list.
	mscextensions (image, output="file", index="0-", extname=extname,
	    extver="", lindex=no, lname=yes, lver=no, ikparams="", > temp1)
	if (mscextensions.nimages == 0) {
	    delete (temp1, verify-)
	    printf ("WARNING: No images found\n")
	    return
	}

	# Expand the input coordinates.
	id = 0
	fdcoords = coords
	while (fscan (fdcoords, ra, dec, id) != EOF) {
	    if (nscan() < 2)
		next
	    else if (nscan() == 2)
		id = id + 1
	    print (ra, dec, ra, dec, id, >> temp2)
	    #xpos = ra * 15.
	    #print (xpos, dec, ra, dec, id, >> temp2)
	}
	fdcoords = ""

	# Initialize psets.
	catpars.cat_epoch = epoch
	catpars.ra_col = "RA_DEG"
	catpars.dec_col = "DEC_DEG"
	catpars.region_col = "REGION"
	catpars.xpred_col = "X_PRED"
	catpars.ypred_col = "Y_PRED"
	catpars.xcen_col = "X_CENTER"
	catpars.ycen_col = "Y_CENTER"
	catpars.cerr_col = "CEN_ERR"
	catpars.datatype = "real"
	catpars.format = "%8.2f"
	catpars.units = "pixels"
	catpars.sub_col = "SUB_FLAG"
	catpars.cen_col = "CEN_FLAG"
	catpars.obj_col = "OBJ_FLAG"
	catpars.id_col = "ID"
	selectpars.explicit = ""
	selectpars.disjunction = no
	selectpars.column1 = ""
	selectpars.column2 = ""
	selectpars.column3 = ""
	selectpars.column4 = ""
	tpltsol.append = yes
	tpltsol.verbose = no
	tpltsol.inpixsys = "logical"
	tpltsol.outpixsys = "physical"
	tpltsol.projection = projection
	tpltsol.fitgeometry = fitgeometry
	tpltsol.function = function
	tpltsol.xxorder = xxorder
	tpltsol.xyorder = xyorder
	tpltsol.xxterms = xxterms
	tpltsol.yxorder = yxorder
	tpltsol.yyorder = yyorder
	tpltsol.yxterms = yxterms
	tpltsol.reject = reject
	#tvmark_.radii = 7

	# Do each image.
	fdimages = temp1
	while (fscan (fdimages, image) != EOF) {

	    # Set image pixel coordinates based on WCS.
	    mscctran (temp2, temp4, image, "world", "logical", columns="1 2",
		units="hours native", formats="", min_sigdigit=9, verbose=no)

	    hselect (image, "naxis1,naxis2", yes) | scan (nc, nl)
	    fdcoords = temp4
	    while (fscan (fdcoords, xpos, ypos, ra, dec, id) != EOF) {
		if (xpos < -9 || xpos > nc+10 || ypos < -9 || ypos > nl+10)
		    next
		print (ra, dec, ra, dec, id, xpos, ypos, >> temp3)
	    }
	    fdcoords = ""
	    delete (temp4, verify-)
	    if (access (temp3) == NO) {
		printf ("Warning: No objects for `%s'\n", image)
		next
	    }

	    mscctran (temp3, temp4, image, "world", "logical", columns="1 2",
		units="hours native", formats="", min_sigdigit=9, verbose=no)
	    delete (temp3, verify-)

	    # Make database for TPEAK
	    mktpeaktab (temp4, temp3//".fits", centered-)
	    delete (temp4, verify-)

	    # Run TPEAK.
	    ra_ref = "INDEF"
	    dec_ref = "INDEF"
	    eq_ref = INDEF
	    hselect (image, "telra,teldec,telepoch", yes) |
		translit ("STDIN", '"', delete+, collapse-) |
		scan (ra_ref, dec_ref, eq_ref)
	    if (nscan() != 3)
		hselect (image, "ra,dec,equinox", yes) |
		    translit ("STDIN", '"', delete+, collapse-) |
		    scan (ra_ref, dec_ref, eq_ref)
	    tpeak (image, temp3//".fits", db, objects="",
		ra_ref=ra_ref, dec_ref=dec_ref, eq_ref=eq_ref,
		update=update, interactive=interactive,
		autocenter=autocenter, autodisplay=no, boxsize=boxsize,
		xscale=100., yscale=100., xshift=0, yshift=0, reselect=yes,
		subsample=1, frame=frame, marker=marker, omarker=omarker,
		goodcolor=goodcolor, badcolor=badcolor)

	    tdelete (temp3//".fits", verify-)
	}
	fdimages = ""
	delete (temp1, verify-)
	delete (temp2, verify-)
end
