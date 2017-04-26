# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<tbset.h>
include <math.h>

define	NUM_COLS	13		# Number of table columns
define	DIR_SLEN	5		# Size of directory name
define	NUM_DIR		24		# Number of dec zone directories
define	DEC_ZONES	(NUM_DIR/2)	# Number of dec zones (per hemisphere)
define	ZONE_SIZE	(90.0/DEC_ZONES)# Width of dec zones
define	HRSTODEG	($1*15.d0)	# Convert hours to degrees (double)
define	NORTH		1
define	SOUTH		-1
define	LAST_NORTH	5259

define	CAT_EPOCH	2000.d0

procedure t_gscfind ()

#  GSCFIND -- Search the Guide Star Catalog index table for fields
#  in the specified range of coordinates and magnitudes.  Build a
#  list containing the pathnames of the files on the CD-ROM.

pointer	sp, indtab
pointer	tp			# Index table pointer
pointer	cdv[NUM_COLS]		# Column descriptors
double	ra, dec			# Coordinates of field center in degrees
double	epoch			# Input coordinate epoch
real	width			# Width of field in degrees
bool	verbose
double	cat_ra, cat_dec
double	ra1, ra2, dec1, dec2	# Coordinate limits
int	nrgn			# Number of regions found
pointer	north, south

int	prtrgn()
bool	clgetb()
pointer	tbtopn()
double	clgetd()
real	clgetr()

begin
	call smark (sp)
	call salloc (indtab, SZ_FNAME, TY_CHAR)
	call salloc (north, SZ_FNAME, TY_CHAR)
	call salloc (south, SZ_FNAME, TY_CHAR)

	# open the index table
	call clgstr ("index", Memc[indtab], SZ_FNAME)
	tp = tbtopn (Memc[indtab], READ_ONLY, 0)	# Open the index table
	call gcolds (tp, cdv)				# Column descriptors

	# get the rest of the parameters
	ra	= clgetd ("ra")
	dec	= clgetd ("dec")
	epoch	= clgetd ("epoch")
	width	= clgetr ("width")

	call clgstr ("north", Memc[north], SZ_FNAME)
	call clgstr ("south", Memc[south], SZ_FNAME)
	verbose	= clgetb ("verbose")

	call f_precess (ra, dec, epoch, cat_ra, cat_dec, CAT_EPOCH)
	cat_ra = HRSTODEG (cat_ra)

	call crdlim (cat_ra, cat_dec, width, ra1, ra2, dec1, dec2, verbose)

	nrgn = prtrgn (tp, cdv, ra1, ra2, dec1, dec2,
	    Memc[north], Memc[south], verbose)

	call clputi ("nregions", nrgn)

	call sfree (sp)
end


procedure gcolds (tp, cdv)

#  GCOLDS -- Find the columns in the index table

pointer	tp			# Index table descriptor
pointer	cdv[NUM_COLS]		# Column pointers

pointer	sp, cnstr, errmsg
int	col
char	colpar[8,NUM_COLS]

begin
	call smark (sp)
	call salloc (cnstr, SZ_COLNAME+1, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	call strcpy ("region",   colpar[1,1],  8) 
	call strcpy ("rahlow",   colpar[1,2],  8)
	call strcpy ("ramlow",   colpar[1,3],  8)
	call strcpy ("raslow",   colpar[1,4],  8)
	call strcpy ("rahhi",    colpar[1,5],  8)
	call strcpy ("ramhi",    colpar[1,6],  8)
	call strcpy ("rashi",    colpar[1,7],  8)
	call strcpy ("decsilow", colpar[1,8],  8)
	call strcpy ("decdlow",  colpar[1,9],  8)
	call strcpy ("decmlow",  colpar[1,10], 8)
	call strcpy ("decsihi",  colpar[1,11], 8)
	call strcpy ("decdhi",   colpar[1,12], 8)
	call strcpy ("decmhi",   colpar[1,13], 8)

	do col = 1, NUM_COLS {
	    # For each defined column
	    # Get the column name
	    call clgstr (colpar[1,col], Memc[cnstr], SZ_COLNAME)
	    call tbcfnd (tp, Memc[cnstr], cdv[col], 1)

	    if (cdv[col] <= 0) {
		call sprintf (Memc[errmsg], SZ_LINE, "Could not find column %s")
		    call pargstr (Memc[cnstr])
		call error (0, Memc[errmsg])
	    }
	}

	call sfree (sp)
end


# CRDLIM -- find the coordinates of the corners of a field from the
# plate center and size.

procedure crdlim (ra, dec, width, ra1, ra2, dec1, dec2, verbose)

double	ra, dec			#I Coordinates of region center in degrees
real	width			#I Size of region in degrees
double	ra1, ra2, dec1, dec2	#O Coordinates of region corners
bool	verbose			#I print the coordinates on the STDERR

double	cosdec

begin
	dec1 = dec - width / 2.0
	if (dec1 <= -90.0) {
	    # South pole
	    dec1 = -90.0
	    dec2 = dec + width / 2.0
	    ra1  = 0.0
	    ra2  = 360.0
	    return
	}

	dec2 = dec + width / 2.0
	if (dec2 >= +90.0) {
	    # North pole
	    dec2 = +90.0
	    dec1 = dec - width / 2.0
	    ra1  = 0.0
	    ra2  = 360.0
	    return
	}

	if (dec > 0.0)
	    # North
	    cosdec = cos (DEGTORAD (dec2))
	else
	    # South
	    cosdec = cos (DEGTORAD (dec1))

	ra1 = ra - (0.5 * width / cosdec)

	if (ra1 < 0)
	    ra1 = ra1 + 360.0

	ra2 = ra + (0.5 * width / cosdec)
	if (ra2 > 360.0)
	    ra2 = ra2 - 360.0
	
	if (verbose) {
	    call eprintf (
		"%00.0h %00.0h %00.0h --> %00.0h %00.0h %00.0h %00.0h\n\n")

		call pargd (ra/15.d0)
		call pargd (dec)
		call pargr (width)
		call pargd (ra1/15.d0)
		call pargd (ra2/15.d0)
		call pargd (dec1)
		call pargd (dec2)

	    call flush (STDERR)
	}
end


# PRTRGN -- Search the index table to find the region identifiers
# whose coordinate limits overlap the specified field.  Writes to
# STDOUT commands for the cdrom command to extract the regions files.

int procedure prtrgn (tp, cdv, ra1, ra2, dec1, dec2, north, south, verbose)

pointer	tp			#I Index table descriptor
pointer	cdv[NUM_COLS]		#I Column descriptors
double	ra1, ra2		#I Right ascension limits in hours
double	dec1, dec2		#I Declination limits in degrees
char	north[ARB]		#I Drive (including node) for Northern CD
char	south[ARB]		#I Drive (including node) for Southern CD
bool	verbose			#I print the coordinates on the STDERR

int	numrows
int	row
double	ralow, rahi
double	declow, dechi
int	regnum
bool	null
int	zone
char	zdir[DIR_SLEN,NUM_DIR]
int	nrgn
int	hemsph

int	tbpsta(), fzone()
double	rdira(), rdidec()

begin
	call initzd (zdir)	# Initialize the directory name for each zone

	numrows = tbpsta (tp, TBL_NROWS)

	hemsph = 0
	nrgn   = 0

	do row = 1, numrows {

	    # Declination range of the GS region
	    dechi = rdidec (tp, row, cdv[11], cdv[12], cdv[13])

	    # Note:  southern dechi and declow are reversed
	    if (dechi > 0 && dechi < dec1)			# North
		next
	    else if (dechi < 0 && dechi > dec2)			# South
		next

	    # Limit of GS region closer to equator
	    declow = rdidec (tp, row, cdv[8],  cdv[9],  cdv[10])

	    if (declow > 0 && declow > dec2) {			# North
		    next

	    } else if (declow < 0 && declow < dec1) {		# South
		    next

	    # Lower limit of region is ON equator
	    } else if (dechi > 0 && (dechi < dec1 || declow > dec2)) {	# North
		    next

	    } else if (dechi < 0 && (dechi > dec2 || declow < dec1)) {	# South
		    next

	    }

	    # Right ascension range of the GS region
	    if (ra1 < ra2) {				# 0 R.A. not in region
		ralow = rdira (tp, row, cdv[2], cdv[3], cdv[4])
		if (ralow > ra2)
		    next

		rahi  = rdira (tp, row, cdv[5], cdv[6], cdv[7])
		if (ralow > rahi)
		    rahi = rahi + 360.0
		if (rahi < ra1)
		    next

	    } else {					# 0 R.A. in region
		ralow = rdira (tp, row, cdv[2], cdv[3], cdv[4])
		rahi  = rdira (tp, row, cdv[5], cdv[6], cdv[7])
		if (ralow > rahi)
		    rahi = rahi + 360.0
		if ((ralow > ra2) && (rahi < ra1))
		    next

	    }

	    call tbrgti (tp, cdv[1], regnum, null, 1, row)	# Region number
	    zone = fzone (declow, dechi)	# Zone number => directory name

	    if (regnum <= LAST_NORTH) {
		hemsph = NORTH		# Read the northern disk (Volume 1)

		call printf ("%s/gsc/%s/%04d.gsc\n")
		    call pargstr (north)
		    call pargstr (zdir[1,zone])
		    call pargi (regnum)

	    } else if (regnum > LAST_NORTH) {
		hemsph = SOUTH		# Read the southern disk (Volume 2)

		call printf ("%s/gsc/%s/%04d.gsc\n")
		    call pargstr (south)
		    call pargstr (zdir[1,zone])
		    call pargi (regnum)

	    }

	    nrgn = nrgn + 1

	    if (verbose) {
		call eprintf (
		    "%5d %6d %00.0h %00.0h %00.0h %00.0h %d %d %d\n")

		    call pargi (nrgn)
		    call pargi (row)
		    call pargd (ralow/15.d0)
		    call pargd (rahi/15.d0)
		    call pargd (declow)
		    call pargd (dechi)
		    call pargi (hemsph)
		    call pargi (regnum)
		    call pargi (zone)

		call flush (STDERR)
	    }
	}

	return (nrgn)
end


double procedure rdira (tp, row, hcol, mcol, scol)

#  RDIRA -- Returns R.A. in degrees from the G.S. index table

pointer	tp			# Index table descriptor
int	row			# Table row number
pointer	hcol			# Column descriptor for hours
pointer	mcol			# Column descriptor for minutes
pointer	scol			# Column descriptor for seconds

int	hrs			# Hours of RA
int	min			# Minutes of RA
real	sec			# Seconds of RA
bool	null			# Null column?
double	ra

begin
	call tbrgti (tp, hcol, hrs, null, 1, row)	# Hours of R.A.
	if (null)
	    return (INDEFD)

	call tbrgti (tp, mcol, min, null, 1, row)	# Minutes of R.A.
	if (null)
	    return (INDEFD)

	call tbrgtr (tp, scol, sec, null, 1, row)	# Seconds of R.A.
	if (null)
	    return (INDEFD)

	ra = double (hrs) + double (min) / 6.d1 + double (sec) / 3.6d3
	return (HRSTODEG(ra))
end


double procedure rdidec (tp, row, sgncol, dcol, mcol)

#  RDIDEC -- Returns the declination in decimal degrees from the G.S.
#  index table.  This is converted from degrees, minutes, and seconds
#  columns.

pointer	tp			# Index table descriptor
int	row			# Table row number
pointer	sgncol			# Column descriptor for sign
pointer	dcol			# Column descriptor for degrees
pointer	mcol			# Column descriptor for minutes

char	sign[4]			# Sign of Dec
int	deg			# Degrees of Dec
real	min			# Minutes of Dec
bool	null			# Null column?
double	dec
char	minus
data	minus /'-'/

int	stridx()

begin
	call tbrgtt (tp, sgncol, sign, null, 4, 1, row)	# Declination sign
	if (null)
	    return (INDEFD)

	call tbrgti (tp, dcol, deg, null, 1, row)	# Degrees of Dec.
	if (null)
	    return (INDEFD)

	call tbrgtr (tp, mcol, min, null, 1, row)	# Minutes of Dec.
	if (null)
	    return (INDEFD)

	dec = double (deg) + double (min) / 6.d1

	if (stridx (minus, sign) != 0)
	    dec = -dec

	return (dec)
end


int procedure fzone (declow, dechi)

#  FZONE -- Find the zone number from the range of declinations in the
#  region.

double	declow, dechi		# Limits of declination of field

double	dec
int	zone

begin
	dec = (declow + dechi) / 2.d0

	zone = int (dec / ZONE_SIZE) + 1

	if (dec < 0)
	    zone = (DEC_ZONES + 2) - zone
	
	return (zone)
end


procedure initzd (zdir)

char	zdir[DIR_SLEN,NUM_DIR]

begin
	call strcpy ("n0000", zdir[1,1],  DIR_SLEN)
	call strcpy ("n0730", zdir[1,2],  DIR_SLEN)
	call strcpy ("n1500", zdir[1,3],  DIR_SLEN)
	call strcpy ("n2230", zdir[1,4],  DIR_SLEN)
	call strcpy ("n3000", zdir[1,5],  DIR_SLEN)
	call strcpy ("n3730", zdir[1,6],  DIR_SLEN)
	call strcpy ("n4500", zdir[1,7],  DIR_SLEN)
	call strcpy ("n5230", zdir[1,8],  DIR_SLEN)
	call strcpy ("n6000", zdir[1,9],  DIR_SLEN)
	call strcpy ("n6730", zdir[1,10], DIR_SLEN)
	call strcpy ("n7500", zdir[1,11], DIR_SLEN)
	call strcpy ("n8230", zdir[1,12], DIR_SLEN)

	call strcpy ("s0000", zdir[1,13], DIR_SLEN)
	call strcpy ("s0730", zdir[1,14], DIR_SLEN)
	call strcpy ("s1500", zdir[1,15], DIR_SLEN)
	call strcpy ("s2230", zdir[1,16], DIR_SLEN)
	call strcpy ("s3000", zdir[1,17], DIR_SLEN)
	call strcpy ("s3730", zdir[1,18], DIR_SLEN)
	call strcpy ("s4500", zdir[1,19], DIR_SLEN)
	call strcpy ("s5230", zdir[1,20], DIR_SLEN)
	call strcpy ("s6000", zdir[1,21], DIR_SLEN)
	call strcpy ("s6730", zdir[1,22], DIR_SLEN)
	call strcpy ("s7500", zdir[1,23], DIR_SLEN)
	call strcpy ("s8230", zdir[1,24], DIR_SLEN)
end
