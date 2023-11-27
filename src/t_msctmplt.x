include <error.h>
include <mach.h>
include <math.h>
include <imhdr.h>
include <imset.h>

# T_MSCTEMPLATE -- Make an empty template image into which the input images
# can be transformed based on their WCS.  The image will be just large enough
# to include all the input images (based only on the image corners).  The
# output image WCS is either the first input WCS or that of a reference
# image.   The WCS reference coordinate value is shifted by an integer number
# of pixels to preserve the same logical coordinate reference pixel; i.e. the
# tangent point is tied to an image pixel and not to a point on the sky.
# When a reference image is given the output image WCS is such that an
# integer shift in the two image axes is all that is needed to align the
# images.

procedure t_msctemplate ()

int	input		# List of images
pointer output		# Output image
pointer ref		# Reference image
real	blank		# Blank value
int	border		# Border width
pointer proj		# WCS projection
int	pixtype		# Pixel type
int	bufsize		# I/O buffer size in Mb

short	blanks
int	i, nimages, refim, xshift, yshift, axes[2]
double	r[2], wcsref[2,3], lterm[2,3], r2, r2min, temp[2,2]
double	x, y, wx, wy, xmin, xmax, ymin, ymax
double	wxmin[4], wxmax[4], wymin[4], wymax[4]
pointer sp, image, attrib, out, im, mw, wcs, mwref, tmp

bool	strne(), streq()
real	clgetr()
int	clgeti(), clgwrd(), imtopenp(), imtgetim(), imtrgetim()
pointer	nowhite(), imaccess(), msc_openim(), msc_sctran()
pointer immap(), mw_openim(), mw_open(), impl2r(), impl2s()
errchk	immap
errchk	mw_openim, msc_open, msc_openim, mw_saveim, mw_gwtermd, mw_swtermd
errchk	msc_sctran, msc_c2trand

data	axes/1,2/

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (ref, SZ_FNAME, TY_CHAR)
	call salloc (proj, SZ_FNAME, TY_CHAR)
	call salloc (attrib, SZ_FNAME, TY_CHAR)

	# Get task parameters.
	input = imtopenp ("input")
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("reference", Memc[ref], SZ_FNAME)
	blank = clgetr ("blank")
	border = clgeti ("border")
	call clgstr ("projection", Memc[proj], SZ_FNAME)
	pixtype = clgwrd ("pixtype", Memc[attrib], SZ_FNAME, "|short|real|")
	#bufsize = max (1024., 1E6 * clgetr ("im_bufsize"))
	bufsize = 65000.

	switch (pixtype) {
	case 1:
	    pixtype = TY_SHORT
	    blanks = blank
	case 2:
	    pixtype = TY_REAL
	}

	# Check the output image is specified and does not exist.
	# Check if a reference image is specified.  It is an error if it
	# is specified and does not exist unless it is the same as the
	# output image.

	out = NULL
	mwref = NULL
	if (nowhite (Memc[output], Memc[output], SZ_FNAME) == 0)
	    call error (1, "No output image specified")
	if (imaccess (Memc[output], 0) == YES)
	    call error (1, "Output image already exists")
	if (nowhite (Memc[ref], Memc[ref], SZ_FNAME) > 0) {
	    if (strne (Memc[ref], Memc[output])) {
		im = immap (Memc[ref], READ_ONLY, 0)
		mwref = mw_openim (im)
		call imunmap (im)
	    }
	}

	# Determine the world coordinate limits.  Set the reference image
	# to the image closest to it's tangent point if no reference image is
	# specified.  It is a warning if an input images cannot be accessed.
	# It is a fatal error if the output image can't be created as a copy
	# of the first accessible input image.

	wxmin[1] = MAX_DOUBLE; wxmax[1] = -MAX_DOUBLE;
	wymin[2] = MAX_DOUBLE; wymax[2] = -MAX_DOUBLE
	nimages = 0
	r2min = MAX_DOUBLE
	while (imtgetim (input, Memc[image], SZ_FNAME) != EOF) {
	    iferr {
		im = NULL; mw = NULL; wcs = NULL

		tmp = immap (Memc[image], READ_ONLY, 0); im = tmp
		tmp = msc_openim (im, wcs); mw = tmp
		tmp = msc_sctran (wcs, 1, "logical", "astrometry", 3)
		tmp = msc_sctran (wcs, 2, "astrometry", "world", 3)
		tmp = msc_sctran (wcs, 3, "astrometry", "logical", 3)

		# Find closest image to it's own tangent point.
		call msc_c2trand (wcs, 3, 0D0, 0D0, xmin, ymin)
		x = max (1D0, min (double(IM_LEN(im,1)), xmin))
		y = max (1D0, min (double(IM_LEN(im,2)), ymin))
		r2 = (x - xmin)**2 + (y - ymin)**2
		if (r2 < r2min) {
		    refim = nimages + 1
		    r2min = r2
		}

		# Find limits.  Use astrometry coordinates to avoid problems
		# at poles and prime meridian.
		xmin = 1; xmax = IM_LEN(im,1)
		ymin = 1; ymax = IM_LEN(im,2)
		call msc_c2trand (wcs, 1, xmin, ymin, x, y)
		call msc_c2trand (wcs, 2, x, y, wx, wy)
		if (x < wxmin[1]) {
		    wxmin[1] = x; wxmin[2] = y; wxmin[3] = wx; wxmin[4] = wy
		}
		if (x > wxmax[1]) {
		    wxmax[1] = x; wxmax[2] = y; wxmax[3] = wx; wxmax[4] = wy
		}
		if (y < wymin[2]) {
		    wymin[1] = x; wymin[2] = y; wymin[3] = wx; wymin[4] = wy
		}
		if (y > wymax[2]) {
		    wymax[1] = x; wymax[2] = y; wymax[3] = wx; wymax[4] = wy
		}
		call msc_c2trand (wcs, 1, xmax, ymin, x, y)
		call msc_c2trand (wcs, 2, x, y, wx, wy)
		if (x < wxmin[1]) {
		    wxmin[1] = x; wxmin[2] = y; wxmin[3] = wx; wxmin[4] = wy
		}
		if (x > wxmax[1]) {
		    wxmax[1] = x; wxmax[2] = y; wxmax[3] = wx; wxmax[4] = wy
		}
		if (y < wymin[2]) {
		    wymin[1] = x; wymin[2] = y; wymin[3] = wx; wymin[4] = wy
		}
		if (y > wymax[2]) {
		    wymax[1] = x; wymax[2] = y; wymax[3] = wx; wymax[4] = wy
		}
		call msc_c2trand (wcs, 1, xmin, ymax, x, y)
		call msc_c2trand (wcs, 2, x, y, wx, wy)
		if (x < wxmin[1]) {
		    wxmin[1] = x; wxmin[2] = y; wxmin[3] = wx; wxmin[4] = wy
		}
		if (x > wxmax[1]) {
		    wxmax[1] = x; wxmax[2] = y; wxmax[3] = wx; wxmax[4] = wy
		}
		if (y < wymin[2]) {
		    wymin[1] = x; wymin[2] = y; wymin[3] = wx; wymin[4] = wy
		}
		if (y > wymax[2]) {
		    wymax[1] = x; wymax[2] = y; wymax[3] = wx; wymax[4] = wy
		}
		call msc_c2trand (wcs, 1, xmax, ymax, x, y)
		call msc_c2trand (wcs, 2, x, y, wx, wy)
		if (x < wxmin[1]) {
		    wxmin[1] = x; wxmin[2] = y; wxmin[3] = wx; wxmin[4] = wy
		}
		if (x > wxmax[1]) {
		    wxmax[1] = x; wxmax[2] = y; wxmax[3] = wx; wxmax[4] = wy
		}
		if (y < wymin[2]) {
		    wymin[1] = x; wymin[2] = y; wymin[3] = wx; wymin[4] = wy
		}
		if (y > wymax[2]) {
		    wymax[1] = x; wymax[2] = y; wymax[3] = wx; wymax[4] = wy
		}

		nimages = nimages + 1
	    } then
		call erract (EA_WARN)

	    if (mw != NULL)
		call msc_close (wcs)
	    if (im != NULL)
		call imunmap (im)
	}

	# Create the empty output image using reference WCS.
	# The size of the output image is that just enclosing all the input
	# images.  Shift the reference point by an integer number of logical
	# pixels.

	if (nimages > 0) {
	    iferr {
		im = NULL; mw = NULL; wcs = NULL; out = NULL

		i = imtrgetim (input, refim, Memc[image], SZ_FNAME)
		tmp = immap (Memc[image], READ_ONLY, 0); im = tmp
		tmp = immap (Memc[output], NEW_COPY, im); out = tmp
		call imseti (out, IM_BUFSIZE, bufsize)
		if (mwref == NULL)
		    mwref = mw_openim (im)

		mw = mw_open (NULL, 2)
		call mw_gsystem (mwref, Memc[attrib], SZ_FNAME)
		call mw_newsystem (mw, Memc[attrib], 2)
		if (nowhite (Memc[proj], Memc[attrib], SZ_FNAME) == 0)
		    call mw_gwattrs (mwref, 1, "wtype", Memc[attrib], SZ_FNAME)
		if (streq (Memc[attrib], "tnx") ||
		    streq (Memc[attrib], "zpx") ||
		    streq (Memc[attrib], "zpn"))
		    call strcpy ("tan", Memc[attrib], SZ_LINE)
		call mw_swtype (mw, axes, 2, Memc[attrib], "")
		call mw_gwattrs (mwref, 1, "axtype", Memc[attrib], SZ_FNAME)
		call mw_swattrs (mw, 1, "axtype", Memc[attrib])
		call mw_gwattrs (mwref, 2, "axtype", Memc[attrib], SZ_FNAME)
		call mw_swattrs (mw, 2, "axtype", Memc[attrib])
		call mw_gwtermd (mwref, r, wcsref[1,3], wcsref, 2)
		call mw_gltermd (mwref, lterm, lterm[1,3], 2)
		call mwvmuld (lterm, r, temp, 2)
		call aaddd (temp, lterm[1,3], r, 2)
		call mwinvertd (lterm, temp, 2)
		call amovd (wcsref, lterm, 4)
		call mwmmuld (lterm, temp, wcsref, 2)
		call mw_swtermd (mw, r, wcsref[1,3], wcsref, 2)
		call mw_close (mwref)

		# Find pixel limints in new WCS using world coordinates.
		call msc_open (mw, wcs)
		tmp = msc_sctran (wcs, 1, "world", "logical", 3)
		xmin = MAX_DOUBLE; xmax = -MAX_DOUBLE
		ymin = MAX_DOUBLE; ymax = -MAX_DOUBLE
		call msc_c2trand (wcs, 1, wxmin[3], wxmin[4], x, y)
		xmin = min (x, xmin); xmax = max (x, xmax)
		ymin = min (y, ymin); ymax = max (y, ymax)
		call msc_c2trand (wcs, 1, wxmax[3], wxmax[4], x, y)
		xmin = min (x, xmin); xmax = max (x, xmax)
		ymin = min (y, ymin); ymax = max (y, ymax)
		call msc_c2trand (wcs, 1, wymin[3], wymin[4], x, y)
		xmin = min (x, xmin); xmax = max (x, xmax)
		ymin = min (y, ymin); ymax = max (y, ymax)
		call msc_c2trand (wcs, 1, wymax[3], wymax[4], x, y)
		xmin = min (x, xmin); xmax = max (x, xmax)
		ymin = min (y, ymin); ymax = max (y, ymax)

		xshift = nint (1.5 - xmin + border)
		yshift = nint (1.5 - ymin + border)
		r[1] = r[1] + xshift
		r[2] = r[2] + yshift
		call mw_swtermd (mw, r, wcsref[1,3], wcsref, 2)

		IM_PIXTYPE(out) = pixtype
		IM_NDIM(out) = 2
		IM_LEN(out,1) = nint (xmax + xshift + 0.5 + border)
		IM_LEN(out,2) = nint (ymax + yshift + 0.5 + border)
		iferr (call imdelf (out, "wcssol"))
		    ;
		call mw_saveim (mw, out)

		switch (IM_PIXTYPE(out)) {
		case TY_SHORT:
		    if (blanks == 0)
			do i = 1, IM_LEN(out,2)
			    call aclrs (Mems[impl2s(out,i)], IM_LEN(out,1))
		    else
			do i = 1, IM_LEN(out,2)
			    call amovks (blanks, Mems[impl2s(out,i)],
				IM_LEN(out,1))
		case TY_REAL:
		    if (blank == 0.)
			do i = 1, IM_LEN(out,2)
			    call aclrr (Memr[impl2r(out,i)], IM_LEN(out,1))
		    else
			do i = 1, IM_LEN(out,2)
			    call amovkr (blank, Memr[impl2r(out,i)],
				IM_LEN(out,1))
		}
	    } then
		call erract (EA_WARN)

	    if (wcs != NULL)
	       call msc_close (wcs)
	    else if (mw != NULL)
		call mw_close (mw)
	    if (mwref != NULL)
		call mw_close (mwref)
	    if (out != NULL)
		call imunmap (out)
	    if (im != NULL)
		call imunmap (im)
	}

	call imtclose (input)
	call sfree (sp)
end


# T_MSCWTEMPLATE -- Make WCS template.
# The output image WCS is either the first input WCS or that of a reference
# image.   The WCS reference coordinate value is shifted by an integer number
# of pixels to preserve the same logical coordinate reference pixel; i.e. the
# tangent point is tied to an image pixel and not to a point on the sky.
# When a reference image is given the output image WCS is such that an
# integer shift in the two image axes is all that is needed to align the
# images.

procedure t_mscwtemplate ()

int	input		# List of images
pointer output		# Output image
int	wcssource	# WCS source
pointer ref		# Reference image
double	ra		# RA
double	dec		# DEC
double	scale		# Scale
double	rot		# Rotation
pointer proj		# WCS projection
bool	verbose		# Verbose?

bool	needref
int	i, nimages, refim, axes[2]
double	x, y, xref, yref, r2, r2min, r[2], wcsref[2,3], lterm[2,3]
pointer sp, image, attrib, out, im, mw, wcs, mwref, tmp

bool	clgetb(), strne(), streq()
int	clgwrd(), imtopenp(), imtgetim(), imtrgetim(), nowhite(), imaccess()
double	clgetd()
pointer immap(), impl1s(), mw_openim(), mw_open(), msc_openim(), msc_sctran()
errchk	immap, impl1s
errchk	mw_openim, mw_open, msc_openim, mw_saveim, msc_sctran, msc_c2trand

data	axes/1,2/

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (ref, SZ_FNAME, TY_CHAR)
	call salloc (proj, SZ_FNAME, TY_CHAR)
	call salloc (attrib, SZ_FNAME, TY_CHAR)

	# Get task parameters.
	input = imtopenp ("input")

	call clgstr ("output", Memc[output], SZ_FNAME)
	if (nowhite (Memc[output], Memc[output], SZ_FNAME) == 0)
	    call error (1, "No output image specified")
	if (imaccess (Memc[output], 0) == YES)
	    call error (1, "Output image already exists")

	verbose = clgetb ("verbose")

	wcssource = clgwrd ("wcssource", Memc[attrib], SZ_FNAME,
	    "|image|parameters|")
	switch (wcssource) {
	case 1:
	    call clgstr ("reference", Memc[ref], SZ_FNAME)
	    call clgstr ("projection", Memc[proj], SZ_FNAME)
	    i = nowhite (Memc[proj], Memc[proj], SZ_FNAME)

	    needref = true
	case 2:
	    ra = clgetd ("ra")
	    dec = clgetd ("dec")
	    scale = clgetd ("scale")
	    rot = clgetd ("rotation")
	    call clgstr ("projection", Memc[proj], SZ_FNAME)

	    if (!IS_INDEFD(ra))
		ra = ra * 15
	    if (!IS_INDEFD(scale))
		scale = scale / 3600.
	    if (!IS_INDEFD(rot))
		rot = DEGTORAD(rot)
	    i = nowhite (Memc[proj], Memc[proj], SZ_FNAME)

	    needref = (IS_INDEFD(ra) || IS_INDEFD(dec) || IS_INDEFD(scale))
	    needref = (needref || IS_INDEFD(rot) || Memc[proj] == EOS)
	    if (needref)
		call clgstr ("reference", Memc[ref], SZ_FNAME)

	default:
	    call error (1, "Unrecognized WCS source")
	}


	# Get reference WCS image if needed.
	if (needref) {

	    # Check if a reference WCS image is specified.  It is an
	    # error if it is specified and does not exist unless it is the
	    # same as the output image.

	    mwref = NULL
	    if (nowhite (Memc[ref], Memc[ref], SZ_FNAME) > 0) {
		if (strne (Memc[ref], Memc[output])) {
		    if (verbose) {
			call printf ("WCS reference image is %s\n")
			    call pargstr (Memc[ref])
		    }
		    im = immap (Memc[ref], READ_ONLY, 0)
		    mwref = mw_openim (im)
		    call imunmap (im)
		}
	    }

	    # If there is no WCS reference specified set it to the image
	    # closest to it's tangent point.  It is a warning if an input
	    # image cannot be accessed.

	    if (mwref == NULL) {
		nimages = 0
		r2min = MAX_DOUBLE
		while (imtgetim (input, Memc[image], SZ_FNAME) != EOF) {
		    iferr {
			im = NULL; mw = NULL; wcs = NULL

			tmp = immap (Memc[image], READ_ONLY, 0); im = tmp
			tmp = msc_openim (im, wcs); mw = tmp
			tmp = msc_sctran (wcs, 1, "astrometry", "logical", 3)

			# Find closest image to it's own tangent point.
			call msc_c2trand (wcs, 1, 0D0, 0D0, xref, yref)
			x = max (1D0, min (double(IM_LEN(im,1)), xref))
			y = max (1D0, min (double(IM_LEN(im,2)), yref))
			r2 = (x - xref)**2 + (y - yref)**2
			if (r2 < r2min) {
			    refim = nimages + 1
			    r2min = r2
			}
			nimages = nimages + 1
		    } then
			call erract (EA_WARN)

		    if (mw != NULL)
			call msc_close (wcs)
		    if (im != NULL)
			call imunmap (im)
		}

		if (nimages == 0)
		    call error (1, "No input images found")

		i = imtrgetim (input, refim, Memc[image], SZ_FNAME)
		if (verbose) {
		    call printf ("WCS reference image is %s\n")
			call pargstr (Memc[image])
		}
		im = immap (Memc[image], READ_ONLY, 0)
		mwref = mw_openim (im)
		call imunmap (im)
	    }
	}

	# Set WCS from parameters.
	if (wcssource == 2) {
	    if (needref) {
		call mw_gwtermd (mwref, r, wcsref[1,3], wcsref, 2)
		call mw_gltermd (mwref, lterm, lterm[1,3], 2)
		call mwvmuld (lterm, r, r, 2)
		call aaddd (r, lterm[1,3], r, 2)
		call mwinvertd (lterm, lterm, 2)
		call mwmmuld (wcsref, lterm, wcsref, 2)
		call mw_gwattrs (mwref, 2, "axtype", Memc[attrib], SZ_FNAME)
		if (streq (Memc[attrib], "RA")) {
		    x = wcsref[1,3]
		    wcsref[1,3] = wcsref[2,3]
		    wcsref[2,3] = x
		    x = wcsref[1,1]
		    wcsref[1,1] = wcsref[2,2]
		    wcsref[2,2] = x
		    x = wcsref[1,2]
		    wcsref[1,2] = wcsref[2,1]
		    wcsref[2,1] = x
		}
		if (IS_INDEFD(ra))
		    ra = wcsref[1,3]
		if (IS_INDEFD(dec))
		    dec = wcsref[2,3]
		if (IS_INDEFD(scale))
		    scale = sqrt (abs(wcsref[1,2] * wcsref[2,1]) +
			abs (wcsref[1,1] * wcsref[2,2]))
		if (IS_INDEFD(rot))
		    rot = atan2 (-wcsref[2,1], wcsref[2,2])
		if (Memc[proj] == EOS)
		    call mw_gwattrs (mwref, 1, "wtype", Memc[proj], SZ_FNAME)

		call mw_close (mwref)
	    }

	    if (verbose) {
		call printf ("Output WCS parameters:\n")
	    call printf ("    RA=%.2H, DEC=%.1h")
		call pargd (ra)
		call pargd (dec)
	    call printf (", SCALE=%.3g arcsec/pixel, ROTATION=%.4g degrees\n")
		call pargd (scale*3600.)
		call pargd (RADTODEG(rot))
	    }

	    wcsref[1,1] = -scale * cos (rot)
	    wcsref[1,2] = -scale * sin (rot)
	    wcsref[2,1] = -scale * sin (rot)
	    wcsref[2,2] = scale * cos (rot)
	    wcsref[1,3] = ra
	    wcsref[2,3] = dec
	    r[1] = 0.
	    r[2] = 0.

	    mwref = mw_open (NULL, 2)
	    call mw_newsystem (mwref, "image", 2)
	    call mw_swtype (mwref, axes, 2, Memc[proj], "")
	    call mw_swattrs (mwref, 1, "axtype", "ra")
	    call mw_swattrs (mwref, 2, "axtype", "dec")
	    call mw_swtermd (mwref, r, wcsref[1,3], wcsref, 2)
	}


	# Create WCS template image.  This is an 2D image with 1 pixel.
	# The only change to the reference WCS is to set the projection
	# type.  The tnx, zpx, and zpn projections are converted to tan.

	iferr {
	    mw = NULL; out = NULL

	    # Set WCS.

	    mw = mw_open (NULL, 2)
	    call mw_gsystem (mwref, Memc[attrib], SZ_FNAME)
	    call mw_newsystem (mw, Memc[attrib], 2)
	    if (nowhite (Memc[proj], Memc[attrib], SZ_FNAME) == 0)
		call mw_gwattrs (mwref, 1, "wtype", Memc[attrib], SZ_FNAME)
	    if (streq (Memc[attrib], "tnx") ||
	        streq (Memc[attrib], "zpx") ||
	        streq (Memc[attrib], "zpn"))
		call strcpy ("tan", Memc[attrib], SZ_LINE)
	    call mw_swtype (mw, axes, 2, Memc[attrib], "")
	    call mw_gwattrs (mwref, 1, "axtype", Memc[attrib], SZ_FNAME)
	    call mw_swattrs (mw, 1, "axtype", Memc[attrib])
	    call mw_gwattrs (mwref, 2, "axtype", Memc[attrib], SZ_FNAME)
	    call mw_swattrs (mw, 2, "axtype", Memc[attrib])
	    call mw_gwtermd (mwref, r, wcsref[1,3], wcsref, 2)
	    call mw_gltermd (mwref, lterm, lterm[1,3], 2)
	    call mwvmuld (lterm, r, r, 2)
	    call aaddd (r, lterm[1,3], r, 2)
	    call mwinvertd (lterm, lterm, 2)
	    call mwmmuld (wcsref, lterm, wcsref, 2)
	    call mw_swtermd (mw, r, wcsref[1,3], wcsref, 2)

	    # Create WCS template image.
	    tmp = immap (Memc[output], NEW_IMAGE, 0); out = tmp
	    IM_NDIM(out) = 2
	    IM_LEN(out,1) = 1
	    IM_LEN(out,2) = 1
	    IM_PIXTYPE(out) = TY_INT
	    Mems[impl1s(out)] = 0
	    call mw_saveim (mw, out)
	    call imunmap (out)
	    call mw_close (mw)
	    call mw_close (mwref)
	} then {
	    call erract (EA_WARN)
	    if (mw != NULL)
		call mw_close (mw)
	    if (mwref != NULL)
		call mw_close (mwref)
	    if (out != NULL) {
		call imunmap (out)
		iferr (call delete (Memc[output]))
		    ;
	    }
	}

	call imtclose (input)
	call sfree (sp)
end
