# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <fio.h>
include	<imhdr.h>
include	<mach.h>
include	<math.h>

# List of catalogs recognized by this task.
define	CATALOGS	"|NOAO:USNO-A2|CADC:USNO-A2|"
define	NOAOUSNO	1		# NOAO:USNO-A2
define	CADCUSNO	2		# CADC:USNO-A2

define  SZ_BUF          32768
define	NMAGS		5		# Maximum number of magnitudes

# T_GETCATALOG -- Given a list of images or a position and radius return an
# output text file with the results of the query.  If possible the output
# should have RA, DEC, and a magnitude for the objects in the first three
# columns.  Other information may be included.  It is up to the user of the
# output to know about the fields.  Currently the task should return
# J2000 coordinates and internally precess them if necessary.
# 
# If a list of images is given then the WCS defines the position and radius.
# The position is the midpoint of the extremes of the corners of the images
# in the list.  The radius is the minimum radius from the midpoint that
# includes all the corners of all the images.  The "rmin" parameter can be
# used to force a bigger radius. The feature to use a list of images with
# common tangent point, as opposed to a single image, is appropriate for a
# list of extensions from a mosaic.

procedure t_getcatalog ()

int	images		#I List of images with WCS
double	rafield		#I RA of field (hours)
double	decfield	#I DEC of field (degrees)
double	radius		#I Field radius (arcmin)
double	magmin		#I Minimum magnitude
double	magmax		#I Minimum magnitude
pointer	catalog		#I Catalog
pointer	output		#I Output file
double	rmin		#I Minimum radius

int	i, j, out, nobjs, catid
double	r, ratan, dectan, x, y, xmin, xmax, ymin, ymax
pointer	sp, image
pointer	im, mw, wcs, ct
pointer	ra, dec, mags[NMAGS]

int	strdic(), open()
int	imtopenp(), imtlen(), imtgetim()
pointer	immap(), msc_openim, msc_sctran()
double	clgetd(), slDSEP()
errchk	immap, msc_openim, open, mscagetcat, noaousno, cadcusno

begin
	call smark (sp)
	call salloc (catalog, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Get parameters.
	images = imtopenp ("images")
	if (imtlen (images) == 0) {
	    rafield = clgetd ("ra")
	    decfield = clgetd ("dec")
	    radius = clgetd ("radius")
	}
	magmin = clgetd ("magmin")
	magmax = clgetd ("magmax")
	call clgstr ("catalog", Memc[catalog], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	rmin = clgetd ("rmin")

	# Get region if images are given.
	# Multiple images must have the same tangent point.
	if (imtlen (images) > 0) {
	    ratan = INDEFD; dectan = INDEFD
	    xmin = MAX_DOUBLE; xmax=-MAX_DOUBLE
	    ymin = MAX_DOUBLE; ymax=-MAX_DOUBLE
	    while (imtgetim (images, Memc[image], SZ_FNAME) != EOF) {
		im = immap (Memc[image], READ_ONLY, 0)
		mw = msc_openim (im, wcs)

		ct = msc_sctran (wcs, 2, "astrometry", "world", 3)
		call msc_c2trand (wcs, 2, 0D0, 0D0, x, y)
		if (IS_INDEFD(ratan)) {
		    ratan = x
		    dectan = y
		} else {
		    r = sqrt ((x-ratan)**2+(y-dectan)**2)
		    r = r / 3600.
		    if (r > 1)
			call error (1,
			    "Images don't have the same tangent point")
		}

		ct = msc_sctran (wcs, 1, "logical", "astrometry", 3)
		call msc_c2trand (wcs, 1, 1D0, 1D0, x, y)
		xmin = min (x, xmin); xmax = max (x, xmax)
		ymin = min (y, ymin); ymax = max (y, ymax)
		call msc_c2trand (wcs, 1, double(IM_LEN(im,1)), 1D0, x, y)
		xmin = min (x, xmin); xmax = max (x, xmax)
		ymin = min (y, ymin); ymax = max (y, ymax)
		call msc_c2trand (wcs, 1, double(IM_LEN(im,1)),
		    double(IM_LEN(im,2)), x, y)
		xmin = min (x, xmin); xmax = max (x, xmax)
		ymin = min (y, ymin); ymax = max (y, ymax)
		call msc_c2trand (wcs, 1, 1D0, double(IM_LEN(im,2)), x, y)
		xmin = min (x, xmin); xmax = max (x, xmax)
		ymin = min (y, ymin); ymax = max (y, ymax)

		x = (xmax + xmin) / 2
		y = (ymax + ymin) / 2
		call msc_c2trand (wcs, 2, x, y, rafield, decfield)
		radius = 0
		r = sqrt ((xmin-x)**2+(ymin-y)**2)
		radius = max (radius, r)
		r = sqrt ((xmax-x)**2+(ymin-y)**2)
		radius = max (radius, r)
		r = sqrt ((xmin-x)**2+(ymax-y)**2)
		radius = max (radius, r)
		r = sqrt ((xmax-x)**2+(ymax-y)**2)
		radius = max (radius, r)

		call msc_close (wcs)
		call imunmap (im)
	    }
	    rafield = rafield / 15.
	    radius = radius / 60.
	    radius = max (rmin, radius)
	    call clputd ("ra", rafield)
	    call clputd ("dec", decfield)
	    call clputd ("radius", radius)
	}
	
	# Get catalog data and format the output.  This is catalog dependent.
	# Each entry in the switch has some knowledge of how to query the
	# catalog and what it returns.  In future there may be epoch
	# parameters to the task to allow precession of coordinates from
	# whatever the catalog returns.  Currently the routines should
	# return J2000 coordinates.

	call aclri (mags, NMAGS)
	catid = strdic (Memc[catalog], Memc[catalog], SZ_FNAME, CATALOGS)
	switch (catid) {
	case NOAOUSNO:
	    call noaousno (rafield, decfield, radius, ra, dec,
		mags, nobjs)
	case CADCUSNO:
	    call cadcusno (rafield, decfield, radius, ra, dec,
		mags, nobjs)
	default:
	    call mscagetcat (Memc[catalog], rafield, decfield, radius,
		ra, dec, mags, nobjs)
	}

	rafield = DEGTORAD (15D0 * rafield)
	decfield = DEGTORAD (decfield)
	radius = DEGTORAD (radius / 60D0)

	out = open (Memc[output], NEW_FILE, TEXT_FILE)
	do i = 0, nobjs-1 {
	    if (Memd[mags[1]+i] < magmin || Memd[mags[1]+i] > magmax)
		next
	    x = DEGTORAD (15D0 * Memd[ra+i])
	    y = DEGTORAD (Memd[dec+i])
	    r = slDSEP (rafield, decfield, x, y)
	    if (r > radius)
		next
	    call fprintf (out, "%13.3h %13.2h")
		call pargd (Memd[ra+i])
		call pargd (Memd[dec+i])
	    do j = 1, NMAGS {
		if (mags[j] == NULL)
		    break
		call fprintf (out, " %6.3f")
		    call pargd (Memd[mags[j]+i])
	    }
	    call fprintf (out, "\n")
	}

	call close (out)
	call mfree (ra, TY_DOUBLE)
	call mfree (dec, TY_DOUBLE)
	do j = 1, NMAGS
	    call mfree (mags[j], TY_DOUBLE)

	# Finish up.
	call sfree (sp)
end


# MSCAGETCAT -- Get catalog through the ASTCAT package.

procedure mscagetcat (catalog, rafield, decfield, radius, ra, dec,
	mags, nobjs)

char	catalog[ARB]		#I Catalog name
double	rafield			#I Field RA (hours)
double	decfield		#I Field DEC (degrees)
double	radius			#I Field radius (arc min)
pointer	ra			#O Double pointer to RA (hours)
pointer	dec			#O Double pointer to DEC (degrees)
pointer	mags[NMAGS]		#O Double pointers to magnitudes
int	nobjs			#O Number of objects returned

int     i, nmags, fd
double	raval, decval, magval[NMAGS]
pointer sp, temp, cmd
int     access(), open(), fscan(), nscan()
errchk	open, delete, malloc, realloc

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (cmd, 2*SZ_LINE, TY_CHAR)

	# Get catalog data into a temporary file.
	call mktemp ("temp", Memc[temp], SZ_FNAME)
	call sprintf (Memc[cmd], SZ_LINE, "mscagetcat %s %s %g %g %g %g")
	    call pargstr (Memc[temp])
	    call pargstr (catalog)
	    call pargd (rafield)
	    call pargd (decfield)
	    call pargd (2*radius)
	    call pargd (2*radius)
	call clcmdw (Memc[cmd])
	if (access (Memc[temp], 0, 0) == NO) {
	    call sprintf (Memc[cmd], SZ_LINE, "Catalog not found (%s)")
		call pargstr (catalog)
	    call error (1, Memc[cmd])
	}

	# Read the catalog fields.
	fd = open (Memc[temp], READ_ONLY, TEXT_FILE)
	nobjs = 0; nmags = 0
	while (fscan (fd) != EOF) {
	    call gargd (raval)
	    call gargd (decval)
	    do i = 1, NMAGS
		call gargd (magval[i])
	    i = nscan()
	    if (nmags == 0)
	        nmags = i - 2
	    if (i < 2 + nmags)
		next

	    if (nobjs == 0) {
		call malloc (ra, 100, TY_DOUBLE)
		call malloc (dec, 100, TY_DOUBLE)
		do i = 1, nmags
		    call malloc (mags[i], 100, TY_DOUBLE)
	    } else if (mod (nobjs, 100) == 0) {
		call realloc (ra, nobjs+100, TY_DOUBLE)
		call realloc (dec, nobjs+100, TY_DOUBLE)
		do i = 1, nmags
		    call realloc (mags[i], nobjs+100, TY_DOUBLE)
	    }

	    Memd[ra+nobjs] = raval
	    Memd[dec+nobjs] = decval
	    do i = 1, nmags
		Memd[mags[i]+nobjs] = magval[i]
	    nobjs = nobjs + 1
        }
        call close (fd)
	call delete (Memc[temp])

	call sfree (sp)
end


# NOAOUSNO -- USNO-A2 catalog from NOAO server.

procedure noaousno (rafield, decfield, radius, ra, dec, mags, nobjs)

double	rafield			#I Field RA (hours)
double	decfield		#I Field DEC (degrees)
double	radius			#I Field radius (arc min)
pointer	ra			#O Double pointer to RA (hours)
pointer	dec			#O Double pointer to DEC (degrees)
pointer	mags[NMAGS]		#O Double pointer to magnitudes

int     i, fd, nchars, nobjs
double	raval, decval, mag1val, mag2val
pointer sp, url, buf
int     ndopen(), read(), stropen(), fscan(), nscan()
errchk	ndopen, malloc, realloc

begin
	call smark (sp)
	call salloc (url, SZ_LINE, TY_CHAR)

	# Initialize.
	ra = NULL
	dec = NULL
	mags[1] = NULL
	mags[2] = NULL
	nobjs = 0

        # Connect to HTTP server and return on error.
        fd = ndopen ("inet:80:www.noao.edu:text", READ_WRITE)

        # Send the get-url request to the server.
        call sprintf (Memc[url], SZ_BUF,
	    "/cgi-bin/usno/usnoextract?search=yes&ra=%0h&dec=%h&width=%0.1f")
            call pargd (rafield)
            call pargd (decfield)
            call pargd (radius)
        call fprintf (fd, "GET %s HTTP/1.0\n\n")
            call pargstr (Memc[url])
        call flush (fd)

	# Read the returned text into a string buffer.
	nchars = 0
	repeat {
	    if (nchars == 0)
		call malloc (buf, SZ_BUF, TY_CHAR)
	    else
		call realloc (buf, nchars+SZ_BUF, TY_CHAR)
            call fseti (fd, F_CANCEL, OK)
            i = read (fd, Memc[buf+nchars], SZ_BUF)
            if (i <= 0)
		break
	    nchars = nchars + i
	}
	Memc[buf+nchars] = EOS
        call close (fd)

	# Parse the text.
	fd = stropen (Memc[buf], nchars, READ_ONLY)
	while (fscan (fd) != EOF) {
	    call gargd (raval)
	    call gargd (decval)
	    call gargd (mag1val)
	    call gargd (mag2val)
	    if (nscan() != 4)
		next

	    if (nobjs == 0) {
		call malloc (ra, 100, TY_DOUBLE)
		call malloc (dec, 100, TY_DOUBLE)
		call malloc (mags[1], 100, TY_DOUBLE)
		call malloc (mags[2], 100, TY_DOUBLE)
	    } else if (mod (nobjs, 100) == 0) {
		call realloc (ra, nobjs+100, TY_DOUBLE)
		call realloc (dec, nobjs+100, TY_DOUBLE)
		call realloc (mags[1], nobjs+100, TY_DOUBLE)
		call realloc (mags[2], nobjs+100, TY_DOUBLE)
	    }

	    Memd[ra+nobjs] = raval
	    Memd[dec+nobjs] = decval
	    Memd[mags[1]+nobjs] = mag1val
	    Memd[mags[2]+nobjs] = mag2val
	    nobjs = nobjs + 1
        }

        call close (fd)
	call mfree (buf, TY_CHAR)
	call sfree (sp)
end


# CADCUSNO -- USNO-A2 catalog from CADC server.

procedure cadcusno (rafield, decfield, radius, ra, dec, mags, nobjs)

double	rafield			#I Field RA (hours)
double	decfield		#I Field DEC (degrees)
double	radius			#I Field radius (arc min)
pointer	ra			#O Double pointer to RA (hours)
pointer	dec			#O Double pointer to DEC (degrees)
pointer	mags[NMAGS]		#O Double pointer to magnitudes

int     i, fd, nchars, nobjs
double	raval, decval, mag1val, mag2val
pointer sp, url, id, buf
int     ndopen(), read(), stropen(), fscan(), nscan()
errchk	ndopen, malloc, realloc

begin
	call smark (sp)
	call salloc (url, SZ_LINE, TY_CHAR)
	call salloc (id, SZ_LINE, TY_CHAR)

	# Initialize.
	ra = NULL
	dec = NULL
	mags[1] = NULL
	mags[2] = NULL
	nobjs = 0

        # Connect to HTTP server and return on error.
        fd = ndopen ("inet:80:cadcwww.dao.nrc.ca:text", READ_WRITE)

        # Send the get-url request to the server.
        call sprintf (Memc[url], SZ_BUF,
"/cadcbin/getusno2?ra=%0h&dec=%h&radius=%0.1f&m=0,21&sort_select=Alpha&epoch_in=2000.0&nout=1000000")
            call pargd (rafield)
            call pargd (decfield)
            call pargd (radius)
        call fprintf (fd, "GET %s HTTP/1.0\n\n")
            call pargstr (Memc[url])
        call flush (fd)

	# Read the returned text into a string buffer.
	nchars = 0
	repeat {
	    if (nchars == 0)
		call malloc (buf, SZ_BUF, TY_CHAR)
	    else
		call realloc (buf, nchars+SZ_BUF, TY_CHAR)
            call fseti (fd, F_CANCEL, OK)
            i = read (fd, Memc[buf+nchars], SZ_BUF)
            if (i <= 0)
		break
	    nchars = nchars + i
	}
	Memc[buf+nchars] = EOS
        call close (fd)

	# Parse the text.
	fd = stropen (Memc[buf], nchars, READ_ONLY)
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[id], SZ_LINE)
	    call gargd (raval)
	    call gargd (decval)
	    call gargd (mag1val)
	    call gargd (mag2val)
	    if (nscan() != 5)
		next

	    if (nobjs == 0) {
		call malloc (ra, 100, TY_DOUBLE)
		call malloc (dec, 100, TY_DOUBLE)
		call malloc (mags[1], 100, TY_DOUBLE)
		call malloc (mags[2], 100, TY_DOUBLE)
	    } else if (mod (nobjs, 100) == 0) {
		call realloc (ra, nobjs+100, TY_DOUBLE)
		call realloc (dec, nobjs+100, TY_DOUBLE)
		call realloc (mags[1], nobjs+100, TY_DOUBLE)
		call realloc (mags[2], nobjs+100, TY_DOUBLE)
	    }

	    Memd[ra+nobjs] = raval / 15.
	    Memd[dec+nobjs] = decval
	    Memd[mags[1]+nobjs] = mag2val
	    Memd[mags[2]+nobjs] = mag1val
	    nobjs = nobjs + 1
        }

        call close (fd)
	call mfree (buf, TY_CHAR)
	call sfree (sp)
end
