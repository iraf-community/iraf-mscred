include	<error.h>
include	<imhdr.h>
include	<mach.h>
include <math/iminterp.h>


define	OUTTYPE	"|area|multiply|divide|"
define	AREA	1
define	MUL	2
define	DIV	3


# T_PIXAREA -- Compute areas of pixels in astrometry coordinates.

procedure t_pixarea ()

int	inlist		# List of input images
int	outlist		# List of output images
int	outtype		# Output type
double	norm		# Normalization	

int	c, l, nc, nl, nc1, nl1, nstep
real	cr, lr
double	crpix1, crpix2, r2, minr2, area
pointer	sp, input, output
pointer	in, out, wcs, areas, x, y, x1, x2, y1, y2, inbuf, outbuf, ptr, msi

int	imtopenp(), imtlen(), imtgetim(), clgwrd()
real	msieval()
double	clgetd(), pixarea()
pointer	immap(), msc_openim(), msc_sctran(), imgl2r(), impl2r()
errchk	immap, msc_openim, msc_sctran, imgl2r, impl2r


begin
	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_LINE, TY_CHAR)

	# Get parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")

	if (imtlen (inlist) != imtlen (outlist))
	    call error (1, "Input and output lists do not match")

	outtype = clgwrd ("outtype", Memc[input], SZ_LINE, OUTTYPE)
	norm = clgetd ("norm")
	nstep = 50

	# Compute average tangent point area for normalization if necessary.
	if (IS_INDEFD(norm)) {
	    call malloc (x, 4, TY_DOUBLE)
	    call malloc (y, 4, TY_DOUBLE)
	    norm = 0.
	    minr2 = MAX_DOUBLE
	    while (imtgetim (inlist, Memc[input], SZ_LINE) != EOF) {
		iferr {
		    in = NULL
		    wcs = NULL

		    ptr = immap (Memc[input], READ_ONLY, 0); in = ptr
		    ptr = msc_openim (in, wcs)
		    ptr = msc_sctran (wcs, 1, "logical", "astrometry", 3)
		    ptr = msc_sctran (wcs, 2, "astrometry", "logical", 3)

		    call msc_c2trand (wcs, 2, 0D0, 0D0, crpix1, crpix2)

		    cr = max (1D0, min (double(IM_LEN(in,1)), crpix1))
		    lr = max (1D0, min (double(IM_LEN(in,2)), crpix2))
		    r2 = (cr - crpix1)**2 + (lr - crpix2)**2
		    if (r2 < minr2) {
			call msc_c2trand (wcs, 1, crpix1-0.5D0, crpix2-0.5D0,
			    Memd[x], Memd[y])
			call msc_c2trand (wcs, 1, crpix1+0.5D0, crpix2-0.5D0,
			    Memd[x+1], Memd[y+1])
			call msc_c2trand (wcs, 1, crpix1-0.5D0, crpix2+0.5D0,
			    Memd[x+2], Memd[y+2])
			call msc_c2trand (wcs, 1, crpix1+0.5D0, crpix2+0.5D0,
			    Memd[x+3], Memd[y+3])

			norm = pixarea (Memd[x], Memd[x+2], Memd[y], Memd[y+2])
			minr2 = r2
		    }
		} then
		    call erract (EA_WARN)

		if (wcs != NULL)
		    call msc_close(wcs)
		if (in != NULL)
		    call imunmap (in)
	    }
	    call mfree (x, TY_DOUBLE)
	    call mfree (y, TY_DOUBLE)

	    if (norm <= 0.) {
		call sfree (sp)
		call error (1, "No input images")
	    }

	    call imtrew (inlist)
	}

	# Loop through input and output images making correction.
	while (imtgetim (inlist, Memc[input], SZ_LINE) != EOF) {
	    if (imtgetim (outlist, Memc[output], SZ_LINE) == EOF)
		Memc[output] = EOS
	    iferr {
		in = NULL
		out = NULL
		wcs = NULL
		areas = NULL
		x = NULL
		y = NULL

		# Open images and WCS.
	        ptr = immap (Memc[input], READ_ONLY, 0); in = ptr
		if (Memc[output] != EOS) {
		    ptr = immap (Memc[output], NEW_COPY, in); out = ptr
		    IM_PIXTYPE(out) = TY_REAL
		}
		ptr = msc_openim (in, wcs)
		ptr = msc_sctran (wcs, 1, "logical", "astrometry", 3)

		# Compute area on a lower resolution grid and fit surface.
		nc = real (IM_LEN(in,1) - 1) / nstep + 1.999
		nl = real (IM_LEN(in,2) - 1) / nstep + 1.999
		nc1 = nc + 1
		nl1 = nl + 1
		call malloc (x, 2*nc1, TY_DOUBLE)
		call malloc (y, 2*nc1, TY_DOUBLE)
		call malloc (areas, nc*nl, TY_REAL)
		outbuf = areas
		area = nstep * nstep

		do l = 0, nl {
		    x2 = x + mod (l,2) * nc1
		    y2 = y + mod (l,2) * nc1
		    do c = 0, nc {
			call msc_c2trand (wcs, 1,
			    double(c*nstep+0.5), double(l*nstep+0.5),
			    Memd[x2], Memd[y2])
			x2 = x2 + 1
			y2 = y2 + 1
		    }
		    x1 = x + mod (l-1,2) * nc1
		    y1 = y + mod (l-1,2) * nc1
		    x2 = x + mod (l,2) * nc1
		    y2 = y + mod (l,2) * nc1
		    if (l > 0) {
			do c = 1, nc {
			    Memr[outbuf] = pixarea (Memd[x1], Memd[x2],
				Memd[y1], Memd[y2]) / area
			    x1 = x1 + 1
			    x2 = x2 + 1
			    y1 = y1 + 1
			    y2 = y2 + 1
			    outbuf = outbuf + 1 
			}
		    }
		}

		call msiinit (msi, II_LINEAR)
		call msifit (msi, Memr[areas], nc, nl, nc)
		call mfree (areas, TY_REAL)
		call mfree (x, TY_DOUBLE)
		call mfree (y, TY_DOUBLE)

		# Now evaluate on full image.
		nc = IM_LEN(in,1)
		nl = IM_LEN(in,2)

		# Compute pixel areas and range.
		do l = 1, nl {
		    lr = real(l-1) / nstep + 1
		    if (outtype != AREA)
			inbuf = imgl2r (in, l)
		    if (out != NULL)
			outbuf = impl2r (out, l)
		    do c = 1, nc {
			cr = real(c-1) / nstep + 1
			area = msieval (msi, cr, lr)

			area = area / norm
			if (out != NULL) {
			    switch (outtype) {
			    case AREA:
				Memr[outbuf] = area
			    case MUL:
				Memr[outbuf] = Memr[inbuf] * area
				inbuf = inbuf + 1
			    case DIV:
				Memr[outbuf] = Memr[inbuf] / area
				inbuf = inbuf + 1
			    }
			    outbuf = outbuf + 1
			}
		    }
		}
		if (out != NULL)
		    call imaddd (out, "NORMAREA", norm)

		call msifree (msi)
		call msc_close (wcs)
		if (out != NULL)
		    call imunmap (out)
		call imunmap (in)
	    } then {
		call mfree (areas, TY_REAL)
		call mfree (x, TY_DOUBLE)
		call mfree (y, TY_DOUBLE)
		call msifree (msi)
		if (wcs != NULL)
		    call msc_close(wcs)
		if (out != NULL)
		    call imunmap (out)
		if (in != NULL)
		    call imunmap (in)
		call erract (EA_WARN)
	    }
	}

	call sfree (sp)
end


# PIXAREA -- Compute area of pixel given by corner coordinates.
#
#  x2[1],y2[1]    x2[2],y2[2]
#	+--------------+
#	|              |
#	|              |
#	|              |
#	|              |
#	+--------------+
#  x1[1],y1[1]    x1[2],y1[2]

double procedure pixarea (x1, x2, y1, y2)

double	x1[2], x2[2], y1[2], y2[2]	#I Pixel corners
double	area				#O Area

begin
	area = x1[1] * y1[2]
	area = area + x1[2] * y2[2]
	area = area + x2[2] * y2[1]
	area = area + x2[1] * y1[1]
	area = area - y1[1] * x1[2]
	area = area - y1[2] * x2[2]
	area = area - y2[2] * x2[1]
	area = area - y2[1] * x1[1]
	area = abs (area / 2)
	return (area)
end
