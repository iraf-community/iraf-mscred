# TFIELD -- Task to extract sources within a specified field (RA, Dec)
# from a list of tables.  The predicted X&Y coordinates for the sources are
# added to the output table along with columns for the centered X&Y's and a 
# preselect flag and a postselect flag (i.e., centering was ok).  The
# centering is to be done by a subsequent task or by a measuring engine
# (e.g., the Grant machine at NOAO/Tucson).


include	<error.h>
include	<imhdr.h>
include	<ctotok.h>
include	<math.h>
include	<tbset.h>


define	SZ_TOKEN	2

define	TF_LEN		32

# parameters for the various coordinate transformations
define	SCALE		Memr[$1]		# arcseconds / user units

define	CAT_EPOCH	Memr[$1+1]
define	PLT_EPOCH	Memr[$1+2]

define	RA_TAN		Memr[$1+3]		# sky coordinates
define	DEC_TAN		Memr[$1+4]

define	XI_CEN		Memr[$1+5]		# standard coordinates
define	ETA_CEN		Memr[$1+6]

define	X_CEN		Memr[$1+7]		# plate coordinates
define	Y_CEN		Memr[$1+8]

define	POS_ANGLE	Memr[$1+9]
define	TRANSPOSE	Memi[$1+10]	

define	X_MIN		Memr[$1+11]		# plate edges
define	X_MAX		Memr[$1+12]
define	Y_MIN		Memr[$1+13]
define	Y_MAX		Memr[$1+14]


# table column pointers
define	RA_COL		Memi[$1+15]		# Right Ascension
define	DEC_COL		Memi[$1+16]		# Declination

define	NUMCOL		9			# number of new columns
define	COLPTR		Memi[$1+17]		# more obvious than REGION_COL

define	REGION_COL	Memi[$1+17]		# GSC region number
define	XPRED_COL	Memi[$1+18]		# predicted X coord
define	YPRED_COL	Memi[$1+19]		# predicted Y coord
define	XCEN_COL	Memi[$1+20]		# centered X coord
define	YCEN_COL	Memi[$1+21]		# centered Y coord
define	CERR_COL	Memi[$1+22]		# centering error
define	SUBSET_COL	Memi[$1+23]		# subset selection flag
define	CENTER_COL	Memi[$1+24]		# good centering flag
define	OBJECT_COL	Memi[$1+25]		# program object flag

define	IROWNUM		Memi[$1+26]		# input table row number
define	OROWNUM		Memi[$1+27]		# output table row number

define	REGION		Memi[$1+28]		# current GSC region number

define	REGION_KEY	"REGION"		# should agree with CDRFITS


# for user prompting, if needed
define	DIR_TYPES	"|top|left|bottom|right"
define	DIR_PROMPT	"top|left|bottom|right"
define	DIR_HORIZ	"left|right           "
define	DIR_VERT	"top|bottom           "

define	DIR_LEN		6

define	TOP		1
define	LEFT		2
define	BOTTOM		3
define	RIGHT		4


procedure t_tfield ()

pointer	sp, itp, otp, input, output, tf, tftmp
bool	firsttime, errorseen
int	tblist, row
real	ra, dec, xi, eta, x, y

int	clpopnu(), clgfil(), tbpsta(), tbhgti()
pointer	tf_open(), tf_init(), tbtopn()

errchk	tf_open, tf_init

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	# get the query parameters
	tblist = clpopnu ("input")
	call clgstr ("output", Memc[output], SZ_FNAME)
	call tbtext (Memc[output], Memc[output], SZ_FNAME)

	tf = NULL
	otp = NULL
	firsttime = true
	errorseen = false

	iferr {
	    # get the parameters and initialize the task structure
	    tftmp = tf_init ()
	    tf = tftmp			# so tf=NULL if error in tf_init()

	    while (clgfil (tblist, Memc[input], SZ_FNAME) != EOF) {
		itp = tbtopn (Memc[input], READ_ONLY, 0)
		iferr (REGION(tf) = tbhgti (itp, REGION_KEY)) {
		    call eprintf ("Warning: REGION keyword not found\n")
		    call flush (STDERR)
		    REGION(tf) = INDEFI
		}

		# open output table, append columns
		if (firsttime) {
		    otp = tf_open (itp, Memc[output], tf)
		    firsttime = false
		}

		do row = 1, tbpsta (itp, TBL_NROWS) {
		    IROWNUM(tf) = row	# the do loop wants a simple variable

		    call tf_geteq (itp, ra, dec, tf)
		    call eq_to_std (ra, dec, xi, eta, tf)
		    call std_to_plt (xi, eta, x, y, tf)

		    if (x >= X_MIN(tf) && x <= X_MAX(tf) &&
			y >= Y_MIN(tf) && y <= Y_MAX(tf)) {

			call tf_copyrow (itp, otp, x, y, tf)

		    }
		}

		call tbtclo (itp)
	    }

	} then {
	    errorseen = true
	    call erract (EA_WARN)
	}

	if (tf != NULL)
	    call mfree (tf, TY_STRUCT)
	if (otp != NULL)
	    call tbtclo (otp)

	call clpcls (tblist)
	call sfree (sp)

	if (errorseen)
	    call error (1, "TFIELD")
end


# TF_INIT -- Initialize the plate / catalog information structure from
# the supplied parameters / header keywords.  Various computations are
# needed to initialize the coordinate transforms.

# This whole routine should be recoded to use double precision,
# not just the naughty bits.

pointer procedure tf_init ()

pointer	tf, sp, buf, date, im
real	ra, dec, ra1, dec1, ra_cen, dec_cen, del_ra, del_dec
real	xref, yref, xref1, yref1
real	plt_epoch, plt_epoch1, width, edge, pos_angle
double	obs_epoch, ut, dra, ddec
long	xsize, ysize
int	north, east, day, month, year
char	tokstr[SZ_TOKEN]

real	clgetr(), imgetr()
double	imgetd()
int	clgwrd(), strmatch(), btoi(), nscan()
bool	clgetb()
pointer	immap()

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)
	call salloc (date, SZ_FNAME, TY_CHAR)

	call malloc (tf, TF_LEN, TY_STRUCT)

	SCALE(tf) = DEGTORAD(clgetr ("scale") / 3600.)
	edge = clgetr ("edge")

	north = clgwrd ("north", Memc[buf], DIR_LEN, DIR_TYPES)

	# limit the east enumeration to orthogonal choices
# does this work???
	if (north == TOP || north == BOTTOM) {
	    call clprintf ("east.p_min", DIR_HORIZ)
	    east = clgwrd ("east", Memc[buf], DIR_LEN, DIR_TYPES)
	} else {
	    call clprintf ("east.p_min", DIR_VERT)
	    east = clgwrd ("east", Memc[buf], DIR_LEN, DIR_TYPES)
	}

	# restore the original prompt
	call clprintf ("east.p_min", DIR_PROMPT)

	# this relies on DIR_TYPES being cyclic clockwise
	# could just enumerate these in the switch below
	TRANSPOSE(tf) = btoi (mod (east-north+4, 4) == 3)

	# should really catch the impossible combinations...
	switch (north) {
	case TOP:
	    if (east == LEFT)
		pos_angle =   0.
	    else
		pos_angle = -90.

	case LEFT:
	    if (east == TOP)
		pos_angle = 180.
	    else
		pos_angle = -90.

	case BOTTOM:
	    if (east == LEFT)
		pos_angle =  90.
	    else
		pos_angle = 180.

	case RIGHT:
	    if (east == TOP)
		pos_angle =  90.
	    else
		pos_angle =   0.
	}

	if (TRANSPOSE(tf) == YES)
	    POS_ANGLE(tf) =   DEGTORAD(clgetr ("pangle") - pos_angle)
	else
	    POS_ANGLE(tf) = - DEGTORAD(clgetr ("pangle") - pos_angle)

	# read the coord info from the image and/or the parameters

	call clgstr ("image", Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {

	    iferr (im = immap (Memc[buf], READ_ONLY, 0)) {
		call sfree (sp)
		call erract (EA_ERROR)
	    }

	    if (IM_NDIM(im) != 2) {
		call imunmap (im)
		call sfree (sp)
		call error (1, "Image is not two dimensional.")
	    }

	    xsize = IM_LEN(im,1)
	    ysize = IM_LEN(im,2)

	    X_MIN(tf) = 1 - edge
	    X_MAX(tf) = xsize + edge
	    Y_MIN(tf) = 1 - edge
	    Y_MAX(tf) = ysize + edge

	    X_CEN(tf) = (xsize + 1) / 2.
	    Y_CEN(tf) = (ysize + 1) / 2.

	    iferr (ra = imgetr (im, "RA"))
		ra = INDEFR
	    iferr (dec = imgetr (im, "DEC"))
		dec = INDEFR
	    iferr (plt_epoch = imgetr (im, "EPOCH"))
		plt_epoch = INDEFR
	    iferr (ut = imgetd (im, "UT"))
		ut = 7.d0	# supply a default UT (local midnight)
	    iferr (call imgstr (im, "DATE-OBS", Memc[date], SZ_FNAME))
		Memc[date] = EOS

# call eprintf ("ra = %g   dec = %g   epoch = %g\n date = %s   ut = %g\n")
# call pargr (ra)
# call pargr (dec)
# call pargr (plt_epoch)
# call pargstr (Memc[date])
# call pargd (ut)
# call flush (STDERR)

	    call imunmap (im)

	} else {

# this is bogus, fix it later

	    width = DEGTORAD(clgetr ("width")) / SCALE(tf)

	    X_MIN(tf) = - width / 2.
	    X_MAX(tf) =   width / 2.
	    Y_MIN(tf) = - width / 2.
	    Y_MAX(tf) =   width / 2.

	    X_CEN(tf) = 0.
	    Y_CEN(tf) = 0.

	    # supply a default UT (local midnight) for the obs. epoch calc.
	    ut = 7.d0
	}

	ra1 = clgetr ("ra")
	if (! IS_INDEFR(ra1))
	    ra = ra1

	dec1 = clgetr ("dec")
	if (! IS_INDEFR(dec1))
	    dec = dec1

	if (IS_INDEFR(ra) || IS_INDEFR(dec)) {
	    call sfree (sp)
	    call error (1, "Field's RA or Dec undefined.")
	}

	plt_epoch1 = clgetr ("epoch")
	if (! IS_INDEFR(plt_epoch1))
	    plt_epoch = plt_epoch1

# call eprintf ("ra = %g   dec = %g   epoch = %g\n")
# call pargr (ra)
# call pargr (dec)
# call pargr (plt_epoch)
# call flush (STDERR)

	call clgstr ("date_obs", Memc[buf], SZ_FNAME)
	if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0)
	    call strcpy (Memc[buf], Memc[date], SZ_FNAME)

	call sscan (Memc[date])
	    call gargi (day)
	    call gargtok (token1, tokstr, SZ_TOKEN)
	    call gargi (month)
	    call gargtok (token2, tokstr, SZ_TOKEN)
	    call gargi (year)

	if (nscan() == 5 &&
	    (token1 == TOK_OPERATOR || token1 == TOK_PUNCTUATION) &&
	    (token2 == TOK_OPERATOR || token2 == TOK_PUNCTUATION)) {

	    if (day < 1 || day > 31 || month < 1 || month > 12) {
		call eprintf ("DATE-OBS = >%s<\n")
		    call pargstr (Memc[date])
		call flush (STDERR)
		call sfree (sp)
		call error (1, "DATE-OBS is impossible (reversed DD/MM?)")
	    }

	    call f_date_to_epoch (year, month, day, ut, obs_epoch)

#call eprintf ("observation epoch = %g\n")
#call pargd (obs_epoch)
#call flush (STDERR)

	    call f_precess (double(ra), double(dec), double(plt_epoch),
		dra, ddec, obs_epoch)

	    ra = real (dra)
	    dec = real (ddec)
	    PLT_EPOCH(tf) = real (obs_epoch)

# call eprintf ("ra = %g   dec = %g   epoch = %g   %d-%d-%d\n")
# call pargr (ra)
# call pargr (dec)
# call pargr (PLT_EPOCH(tf))
# call pargi (day)
# call pargi (month)
# call pargi (year)
# call flush (STDERR)

	} else {

	    PLT_EPOCH(tf) = plt_epoch
	}

# call eprintf ("ra = %g   dec = %g   epoch = %g\n")
# call pargr (ra)
# call pargr (dec)
# call pargr (PLT_EPOCH(tf))
# call flush (STDERR)

	dec = DEGTORAD(dec)
	ra = DEGTORAD(ra * 15.)

	del_ra	= DEGTORAD(clgetr("del_ra")) / cos(dec)
	del_dec	= DEGTORAD(clgetr("del_dec"))

	if (clgetb ("opaxis")) {
	    RA_TAN(tf) = ra
	    DEC_TAN(tf) = dec

	    ra_cen  =  ra + del_ra
	    dec_cen = dec + del_dec

	} else {
	    xref = clgetr ("xref")
	    if (IS_INDEFR(xref))
		xref = X_CEN(tf)

	    yref = clgetr ("yref")
	    if (IS_INDEFR(yref))
		yref = Y_CEN(tf)

	    if (TRANSPOSE(tf) == YES) {
		xref1 = + cos (POS_ANGLE(tf)) * (xref - X_CEN(tf)) +
			  sin (POS_ANGLE(tf)) * (yref - Y_CEN(tf))

		yref1 = - sin (POS_ANGLE(tf)) * (xref - X_CEN(tf)) +
			  cos (POS_ANGLE(tf)) * (yref - Y_CEN(tf))

		xref = yref1
		yref = xref1

	    } else {
		xref1 = + cos (POS_ANGLE(tf)) * (xref - X_CEN(tf)) -
			  sin (POS_ANGLE(tf)) * (yref - Y_CEN(tf))

		yref1 = + sin (POS_ANGLE(tf)) * (xref - X_CEN(tf)) +
			  cos (POS_ANGLE(tf)) * (yref - Y_CEN(tf))

		xref = xref1
		yref = yref1
	    }

	    # RA is lefthanded
	    ra_cen  =  ra + (SCALE(tf) * xref) / cos(dec)
	    dec_cen = dec - (SCALE(tf) * yref)

	    RA_TAN(tf)  =  ra_cen - del_ra
	    DEC_TAN(tf) = dec_cen - del_dec
	}

	call eq_to_std (ra_cen, dec_cen, XI_CEN(tf), ETA_CEN(tf), tf)

	OROWNUM(tf) = 0

	call sfree (sp)
	return (tf)
end


# TF_OPEN -- Check input table compatibility, open the output table
# and append the X, Y, and flag columns.

pointer procedure tf_open (itp, output, tf)

pointer	itp				#I input table descriptor
char	output[ARB]			#I output table name
pointer	tf				#I task structure pointer

char	colname[SZ_COLNAME, NUMCOL]
char	colunits[SZ_COLUNITS, NUMCOL]
char	colfmt[SZ_COLFMT, NUMCOL]
int	type[NUMCOL], len[NUMCOL]

pointer	otp, sp, pp, buf
int	i

pointer	clopset(), tbtopn()
int	strdic()
real	clgpsetr()

errchk	tbtopn, tbtcre

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# open the catalog pset for the column information
	call clgstr ("catpars", Memc[buf], SZ_FNAME)
	pp = clopset (Memc[buf])

	CAT_EPOCH(tf) = clgpsetr (pp, "cat_epoch")

	call clgpset (pp, "region_col", colname[1,1], SZ_LINE)
	call clgpset (pp, "xpred_col", colname[1,2], SZ_LINE)
	call clgpset (pp, "ypred_col", colname[1,3], SZ_LINE)
	call clgpset (pp, "xcen_col", colname[1,4], SZ_LINE)
	call clgpset (pp, "ycen_col", colname[1,5], SZ_LINE)
	call clgpset (pp, "cerr_col", colname[1,6], SZ_LINE)
	call clgpset (pp, "sub_col", colname[1,7], SZ_LINE)
	call clgpset (pp, "cen_col", colname[1,8], SZ_LINE)
	call clgpset (pp, "obj_col", colname[1,9], SZ_LINE)

	call clgpset (pp, "units", colunits[1,2], SZ_COLUNITS)
	call clgpset (pp, "format", colfmt[1,2], SZ_COLFMT)

	do i = 3, 6 {
	    call strcpy (colunits[1,2], colunits[1,i], SZ_COLUNITS)
	    call strcpy (colfmt[1,2], colfmt[1,i], SZ_COLFMT)
	}

	call strcpy ("index", colunits[1,1], SZ_COLUNITS)
	call strcpy ("%5d", colfmt[1,1], SZ_COLFMT)

	call strcpy ("flag", colunits[1,7], SZ_COLUNITS)
	call strcpy ("flag", colunits[1,8], SZ_COLUNITS)
	call strcpy ("flag", colunits[1,9], SZ_COLUNITS)
	call strcpy ("%1d", colfmt[1,7], SZ_COLFMT)
	call strcpy ("%1d", colfmt[1,8], SZ_COLFMT)
	call strcpy ("%1d", colfmt[1,9], SZ_COLFMT)

	call clgpset (pp, "datatype", Memc[buf], SZ_LINE)
	switch (strdic (Memc[buf], Memc[buf], SZ_LINE, "|int|real|double")) {
	case 1:
	    call amovki (TY_INT, type[2], 5)
	case 2:
	    call amovki (TY_REAL, type[2], 5)
	case 3:
	    call amovki (TY_DOUBLE, type[2], 5)
	default:
	    call sfree (sp)
	    call error (1, "unknown switch case in tf_open")
	}

	type[1] = TY_INT
	type[7] = TY_INT
	type[8] = TY_INT
	type[9] = TY_INT

	# apparently `lendata' isn't used by tbcdef
	do i = 1, NUMCOL
	    len[i] = 0

	otp = tbtopn (output, NEW_COPY, itp)
	call tbcdef (otp, COLPTR(tf), colname, colunits, colfmt,
	    type, len, NUMCOL)
	call tbtcre (otp)

	call clgpset (pp, "ra_col", Memc[buf], SZ_LINE)
	call tbcfnd (otp, Memc[buf], RA_COL(tf), 1)
	call clgpset (pp, "dec_col", Memc[buf], SZ_LINE)
	call tbcfnd (otp, Memc[buf], DEC_COL(tf), 1)

	call clcpset (pp)
	call sfree (sp)

	return (otp)
end


# TF_GETEQ -- Get the RA and DEC from the input table.  Precess from
# the catalog to the plate epoch and convert to radians.

procedure tf_geteq (tp, ra, dec, tf)

pointer	tp			#I table descriptor
real	ra, dec			#O RA and Dec in radians
pointer	tf			#I task structure descriptor

double	ra_deg, dec_deg, ra_hrs, cat_epoch, plt_epoch
bool	undefined

begin
	call tbrgtd (tp, RA_COL(tf), ra_deg, undefined, 1, IROWNUM(tf))
	if (undefined)
	    call error (1, "undefined right ascension in table")

	call tbrgtd (tp, DEC_COL(tf), dec_deg, undefined, 1, IROWNUM(tf))
	if (undefined)
	    call error (1, "undefined declination in table")

	ra_hrs = ra_deg / 15.d0

	if (IS_INDEFR(CAT_EPOCH(tf)))
	    cat_epoch = INDEFD
	else
	    cat_epoch = double (CAT_EPOCH(tf))

	if (IS_INDEFR(PLT_EPOCH(tf)))
	    plt_epoch = INDEFD
	else
	    plt_epoch = double (PLT_EPOCH(tf))

	call f_precess (ra_hrs, dec_deg, cat_epoch,
	    ra_hrs, dec_deg, plt_epoch)

	ra = DEGTORAD(real (ra_hrs) * 15.)
	dec = DEGTORAD(real (dec_deg))
end


# Should do the calculations with the vector operators...

# EQ_TO_STD -- Convert from equitorial coordinates to standard
# (tangential) coordinates.

procedure eq_to_std (ra, dec, xi, eta, tf)

real	ra, dec			#I equitorial coords of source (radians)
real	xi, eta			#O standard coords of source
pointer	tf			#I task structure descriptor

real	denom

begin
	denom = sin (dec) * sin (DEC_TAN(tf)) +
		cos (dec) * cos (DEC_TAN(tf)) * cos (ra - RA_TAN(tf))

	xi    = cos (dec) * sin (ra - RA_TAN(tf))
	xi    = xi / (SCALE(tf) * denom)

	eta   = sin (dec) * cos (DEC_TAN(tf)) -
		cos (dec) * sin (DEC_TAN(tf)) * cos (ra - RA_TAN(tf))
	eta   = eta / (SCALE(tf) * denom)
end


# STD_TO_PLT -- Convert from standard coordinates to plate (measured)
# coordinates.

procedure std_to_plt (xi, eta, x, y, tf)

real	xi, eta			#I standard coords of source
real	x, y			#O plate coords of source
pointer	tf			#I task structure descriptor

real	xx, yy

begin
	# note that x = - xi, i.e., (xi,eta) coords are lefthanded

	xx =  - cos (POS_ANGLE(tf)) * ( xi - XI_CEN(tf)) +
		sin (POS_ANGLE(tf)) * (eta - ETA_CEN(tf)) + X_CEN(tf)

	yy =  + sin (POS_ANGLE(tf)) * ( xi - XI_CEN(tf)) +
		cos (POS_ANGLE(tf)) * (eta - ETA_CEN(tf)) + Y_CEN(tf)

	if (TRANSPOSE(tf) == YES) {
	    x = yy
	    y = xx
	} else {
	    x = xx
	    y = yy
	}
end


# TF_COPYROW -- Copy (append) a row from an input catalog table to the
# output catalog table.  Fill in the predicted X,Y coordinates, the
# centered X,Y coordinates are set to the same values.  Initialize the
# SUBSET flag to YES, the centering flag to NO, and the OBJECT flag (of
# course) to NO.  Set the REGION field.

procedure tf_copyrow (itp, otp, x, y, tf)

pointer	itp, otp			#I input and output table desc.
real	x, y				#I predicted X and Y coordinates
pointer	tf				#I task structure descriptor

begin
	OROWNUM(tf) = OROWNUM(tf) + 1
	call tbrcpy (itp, otp, IROWNUM(tf), OROWNUM(tf))

	call tbrpti (otp, REGION_COL(tf), REGION(tf), 1, OROWNUM(tf))

	call tbrptr (otp, XPRED_COL(tf), x, 1, OROWNUM(tf))
	call tbrptr (otp, YPRED_COL(tf), y, 1, OROWNUM(tf))

	call tbrptr (otp, XCEN_COL(tf), x, 1, OROWNUM(tf))
	call tbrptr (otp, YCEN_COL(tf), y, 1, OROWNUM(tf))

	call tbrptr (otp, CERR_COL(tf), INDEFR, 1, OROWNUM(tf))

	call tbrpti (otp, SUBSET_COL(tf), YES, 1, OROWNUM(tf))
	call tbrpti (otp, CENTER_COL(tf), NO, 1, OROWNUM(tf))
	call tbrpti (otp, OBJECT_COL(tf), NO, 1, OROWNUM(tf))
end
