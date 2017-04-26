include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	"starfocus.h"
include "../mosgeom.h"
include "../mosim.h"


# STF_MEASURE -- PSF measuring routine.
# This is a stand-alone routine that can be called to return the FWHM.
# It is a greatly abbreviated version of starfocus.

procedure stf_measure (im, xc, yc, level, radius, nit, sbuffer, swidth,
	saturation, gp, logfd, bkg, peak, dfwhm, efwhm)

pointer	im		#I Image pointer
real	xc		#I Initial X center
real	yc		#I Initial Y center
real	level		#I Measurement level
real	radius		#I Profile radius
int	nit		#I Number of iterations on radius
real	sbuffer		#I Sky buffer (pixels)
real	swidth		#I Sky width (pixels)
real	saturation	#I Saturation
pointer	gp		#I Graphics output if not NULL
int	logfd		#I Log output if not NULL
real	bkg		#O Background used
real	peak		#O Peak used
real	dfwhm		#O Direct FWHM
real	efwhm		#O Enclosed flux FWHM

int	i
bool	ignore_sat
pointer	sp, sf, sfd, sfds, mg

real	stf_r2i()
errchk	stf_find, stf_bkgd, stf_profile, stf_fwhm, stf_radius, stf_organize

begin
	call smark (sp)
	call salloc (sf, SF, TY_STRUCT)
	call salloc (sfd, SFD, TY_STRUCT)
	call salloc (sfds, 1, TY_POINTER)
	call aclri (Memi[sf], SF)
	call aclri (Memi[sfd], SFD)
	Memi[sfds] = sfd

	# Initialize parameters.
	SF_TASK(sf) = PSFMEASURE
	call strcpy ("FWHM", SF_RTYPE(sf), SF_SZRTYPE)
	SF_SCALE(sf) = 1.
	SF_LEVEL(sf) = level
	SF_RADIUS(sf) = radius
	SF_SBUF(sf) = sbuffer
	SF_SWIDTH(sf) = swidth
	SF_SAT(sf) = saturation
	SF_NIT(sf) = nit
	SF_OVRPLT(sf) = NO
	mg = MI_CMG(im)
	SF_NCOLS(sf) = NX(mg)
	SF_NLINES(sf) = NY(mg)
	SF_XF(sf) = (NX(mg) + 1) / 2.
	SF_YF(sf) = (NY(mg) + 1) / 2.
	ignore_sat = false

#	call imstats (im, IM_IMAGENAME, SFD_IMAGE(sfd), SF_SZFNAME)
	call strcpy (Memc[MI_RNAME(im)], SFD_IMAGE(sfd), SF_SZFNAME)
	SFD_ID(sfd) = 1
	SFD_X(sfd) = xc
	SFD_Y(sfd) = yc
	SFD_F(sfd) = INDEF
	SFD_STATUS(sfd) = 0
	SFD_SFS(sfd) = NULL
	SFD_SFF(sfd) = NULL
	SFD_SFI(sfd) = NULL

	if (SF_LEVEL(sf) > 1.)
	    SF_LEVEL(sf) = SF_LEVEL(sf) / 100.
	SF_LEVEL(sf) = max (0.05, min (0.95, SF_LEVEL(sf)))

	# Evaluate PSF data.
	iferr {
	    do i = 1, SF_NIT(sf) {
		if (i == 1)
		    SFD_RADIUS(sfd) = SF_RADIUS(sf)
		else
		    SFD_RADIUS(sfd) = SFD_R(sfd) *
			sqrt (log (200.) / log (1/(1-SF_LEVEL(sf))))
		SFD_NPMAX(sfd) = stf_r2i (SFD_RADIUS(sfd)) + 1
		SFD_NP(sfd) = SFD_NPMAX(sfd)
		call stf_find (sf, sfd, im)
		call stf_bkgd (sf, sfd)
		if (SFD_NSAT(sfd) > 0 && i == 1) {
		    if (ignore_sat)
			call error (0,
			"Saturated pixels found - ignoring object")
		    else
			call eprintf (
			    "WARNING: Saturated pixels found.\n")
		}
		call stf_profile (sf, sfd)
		call stf_fwhm (sf, sfd)
		call stf_radius (sf, sfd, SF_LEVEL(sf), SFD_R(sfd))
	    }
	    if (SF_RTYPE(sf) == 'F')
		SFD_R(sfd) = 2 * SFD_R(sfd) *
		    sqrt (log (2.) / log (1/(1-SF_LEVEL(sf))))

	    # Set output results.
	    bkg = SFD_BKGD(sfd)
	    peak = SFD_PEAK(sfd)
	    efwhm = SFD_R(sfd)
	    dfwhm = SFD_DFWHM(sfd)
	} then
	    call erract (EA_WARN)

	# Finish up
	call asifree (SFD_ASI1(sfd))
	call asifree (SFD_ASI2(sfd))
	# Free data buffer
	if (SFD_DATA(sfd) != NULL)
	    call mfree (SFD_DATA(sfd), TY_REAL)
	call stf_free (sf)
	call sfree (sp)
end


# STF_FREE -- Free the starfocus data structures.

procedure stf_free (sf)

pointer	sf			#I Starfocus structure
int	i

begin

	do i = 1, SF_NSTARS(sf)
	    call mfree (SF_SFS(sf,i), TY_STRUCT)
	do i = 1, SF_NFOCUS(sf)
	    call mfree (SF_SFF(sf,i), TY_STRUCT)
	do i = 1, SF_NIMAGES(sf)
	    call mfree (SF_SFI(sf,i), TY_STRUCT)
	call mfree (SF_STARS(sf), TY_POINTER)
	call mfree (SF_FOCUS(sf), TY_POINTER)
	call mfree (SF_IMAGES(sf), TY_POINTER)
	SF_NSTARS(sf) = 0
	SF_NFOCUS(sf) = 0
	SF_NIMAGES(sf) = 0
end


include	<imhdr.h>
include	<mach.h>
include	<math.h>
include	<math/iminterp.h>
include	"starfocus.h"


# STF_FIND    -- Find the object and return the data raster and object center.
# STF_BKGD    -- Compute the background.
# STF_PROFILE -- Compute enclosed flux profile, derivative, and moments.
# STF_NORM    -- Renormalized enclosed flux profile
# STF_I2R     -- Radius from sample index.
# STF_R2I     -- Sample index from radius.
# STF_R2N     -- Number of subsamples from radius.
# STF_FWHM    -- Measure FWHM vs level.
# STF_RADIUS  -- Measure the radius at the specified level.


# STF_FIND -- Find the object and return the data raster and object center.
# Centering uses centroid of marginal distributions of data above the mean.

procedure stf_find (sf, sfd, im)

pointer	sf			#I Starfocus pointer
pointer	sfd			#I Object pointer
pointer	im			#I Image pointer

long	lseed
int	i, j, k, x1, x2, y1, y2, nx, ny, npts
real	radius, buffer, width, xc, yc, xlast, ylast, r1, r2
real	mean, sum, sum1, sum2, sum3, asumr(), urand()
pointer	mg, buff, data, ptr

pointer	migs2r()
errchk	migs2r

begin
	radius = SFD_RADIUS(sfd)
	buffer = SF_SBUF(sf)
	width = SF_SWIDTH(sf)
	mg = MI_CMG(im)

	xc = SFD_X(sfd)
	yc = SFD_Y(sfd)
	r1 = radius + buffer + width
	#r2 = max (SF_RMIN, r1)
	#r2 = r1
	r2 = radius

	# Iterate on the center finding.
	do k = 1, 3 {

	    # Extract region around current center.
	    xlast = xc
	    ylast = yc

	    x1 = max (1, nint (xc - r2))
	    x2 = min (NX(mg), nint (xc + r2))
	    nx = x2 - x1 + 1
	    y1 = max (1, nint (yc - r2))
	    y2 = min (NY(mg), nint (yc + r2))
	    ny = y2 - y1 + 1
	    npts = nx * ny

	    buff = migs2r (im, x1, x2, y1, y2)

	    # Find center of gravity of marginal distributions above mean.
	    npts = nx * ny
	    sum = asumr (Memr[buff], npts)
	    mean = sum / nx
	    sum1 = 0.
	    sum2 = 0.

	    do i = x1, x2 {
		ptr = buff + i - x1
		sum3 = 0.
		do j = y1, y2 {
		    sum3 = sum3 + Memr[ptr]
		    ptr = ptr + nx
		}
		sum3 = sum3 - mean
		if (sum3 > 0.) {
		    sum1 = sum1 + i * sum3
		    sum2 = sum2 + sum3
		}
	    }
	    xc = sum1 / sum2
	    if (xlast - xc > 0.2 * nx)
		xc = xlast - 0.2 * nx
	    if (xc - xlast > 0.2 * nx)
		xc = xlast + 0.2 * nx

	    ptr = buff
	    mean = sum / ny
	    sum1 = 0.
	    sum2 = 0.
	    do j = y1, y2 {
		sum3 = 0.
		do i = x1, x2 {
		    sum3 = sum3 + Memr[ptr]
		    ptr = ptr + 1
		}
		sum3 = sum3 - mean
		if (sum3 > 0.) {
		    sum1 = sum1 + j * sum3
		    sum2 = sum2 + sum3
		}
	    }
	    yc = sum1 / sum2
	    if (ylast - yc > 0.2 * ny)
		yc = ylast - 0.2 * ny
	    if (yc - ylast > 0.2 * ny)
		yc = ylast + 0.2 * ny

	    if (nint(xc) == nint(xlast) && nint(yc) == nint(ylast))
		break
	}

	# Get a new centered raster if necessary.
	if (nint(xc) != nint(xlast) || nint(yc) != nint(ylast) || r2 < r1) {
	    x1 = max (1, nint (xc - r1))
	    x2 = min (NX(mg), nint (xc + r1))
	    nx = x2 - x1 + 1
	    y1 = max (1, nint (yc - r1))
	    y2 = min (NY(mg), nint (yc + r1))
	    ny = y2 - y1 + 1
	    npts = nx * ny
	    buff = migs2r (im, x1, x2, y1, y2)
	}


	# We make a copy of the data in the mosim buffer since we will be
	# modifying its content. This should realy be done internal to 
	# mosim!

	call malloc (data, npts, TY_REAL)

	# Add a dither for integer data.  The random numbers are always
	# the same to provide reproducibility.

	i = IM_PIXTYPE(im)
	if (i == TY_SHORT || i == TY_INT || i == TY_LONG) {
	    lseed = 1
	    do i = 0, npts-1
		Memr[data+i] = Memr[buff+i] + urand(lseed) - 0.5
	} else {
	    call amovr (Memr[buff], Memr[data], npts)
	}

	SFD_DATA(sfd) = data
	SFD_X1(sfd) = x1
	SFD_X2(sfd) = x2
	SFD_Y1(sfd) = y1
	SFD_Y2(sfd) = y2
	SFD_X(sfd) = xc
	SFD_Y(sfd) = yc
end


# STF_BKGD -- Compute the background.
# A mode is estimated from the minimum slope in the sorted background pixels
# with a bin width of 5%.

procedure stf_bkgd (sf, sfd)

pointer	sf			#I Parameter structure
pointer	sfd			#I Star structure

int	i, j, x1, x2, y1, y2, xc, yc, nx, ny, npts, ns, nsat
real	sat, bkgd, miso
real	r, r1, r2, r3, dx, dy, dz
pointer	sp, data, bdata, ptr

begin
	data = SFD_DATA(sfd)
	x1 = SFD_X1(sfd)
	x2 = SFD_X2(sfd)
	y1 = SFD_Y1(sfd)
	y2 = SFD_Y2(sfd)
	xc = SFD_X(sfd)
	yc = SFD_Y(sfd)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	ns = 0
	nsat = 0
	r1 = SFD_RADIUS(sfd) ** 2
	r2 = (SFD_RADIUS(sfd) + SF_SBUF(sf)) ** 2
	r3 = (SFD_RADIUS(sfd) + SF_SBUF(sf) + SF_SWIDTH(sf)) ** 2
	sat = SF_SAT(sf)
	if (IS_INDEF(sat))
	    sat = MAX_REAL

	call smark (sp)
	call salloc (bdata, npts, TY_REAL)

	ptr = data
	do j = y1, y2 {
	    dy = (yc - j) ** 2
	    do i = x1, x2 {
		dx = (xc - i) ** 2
		r = dx + dy
		if (r <= r1) {
		    if (Memr[ptr] >= sat)
			nsat = nsat + 1
		} else if (r >= r2 && r <= r3) {
		    Memr[bdata+ns] = Memr[ptr]
		    ns = ns + 1
		}
		ptr = ptr + 1
	    }
	}

	call asrtr (Memr[bdata], Memr[bdata], ns)
	r = Memr[bdata+ns-1] - Memr[bdata]
	bkgd = Memr[bdata] + r / 2
	miso = r / 2

	j = 1 + 0.50 * ns
	do i = 0, ns - j {
	    dz = Memr[bdata+i+j-1] - Memr[bdata+i]
	    if (dz < r) {
		r = dz
		bkgd = Memr[bdata+i] + dz / 2
		miso = dz / 2
	    }
	}

	SFD_BKGD1(sfd) = bkgd
	SFD_BKGD(sfd) = bkgd
	SFD_MISO(sfd) = miso
	SFD_NSAT(sfd) = nsat

	call sfree (sp)
end


# STF_PROFILE -- Compute enclosed flux profile, derivative, direct FWHM, and
# profile moments..
# 1.  The flux profile is normalized at the maximum value.
# 2.  The radial profile is computed from the numerical derivative of the
#     enclose flux profile.

procedure stf_profile (sf, sfd)

pointer	sf			#I Parameter structure
pointer	sfd			#I Star structure

int	np
real	radius, xc, yc

int	i, j, k, l, m, ns, nx, ny, x1, x2, y1, y2
real	bkgd, miso, sigma, peak
real	r, r1, r2, r3, dx, dy, dx1, dx2, dy1, dy2, dz, xx, yy, xy, ds, da
real	fwhm, hm, hmr1, hmr2, hmi1, hmi2, fwhmr, fwhmi
pointer	sp, data, profile, ptr, asi, msi, gs
int	stf_r2n()
real	asieval(), msieval(), gseval(), stf_i2r(), stf_r2i()
errchk	asiinit, asifit, msiinit, msifit, gsrestore

real	gsdata[24]
data	gsdata/ 1., 4., 4., 1., 0., 0.6726812, 1., 2., 1.630641, 0.088787,
		0.00389378, -0.001457133, 0.3932125, -0.1267456, -0.004864541,
		0.00249941, 0.03078612, 0.02731274, -4.875850E-4, 2.307464E-4,
		-0.002134843, 0.007603908, -0.002552385, -8.010564E-4/

begin
	data = SFD_DATA(sfd)
	x1 = SFD_X1(sfd)
	x2 = SFD_X2(sfd)
	y1 = SFD_Y1(sfd)
	y2 = SFD_Y2(sfd)
	xc = SFD_X(sfd)
	yc = SFD_Y(sfd)
	bkgd = SFD_BKGD(sfd)
	miso = SFD_MISO(sfd)
	radius = SFD_RADIUS(sfd)
	np = SFD_NP(sfd)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

	# Use an image interpolator fit to the data.
	call msiinit (msi, II_BISPLINE3)
	call msifit (msi, Memr[data], nx, ny, nx)

	# Compute the direct FWHM, enclosed flux profile and moments.
	call smark (sp)
	call salloc (profile, np, TY_REAL)
	call aclrr (Memr[profile], np)

	# Find the peak value for the direct FWHM calculation.  First find
	# the highest pixel.  Then subsample the pixel using the image
	# interpolation function to find the highest subpixel and the
	# integral over the pixel defined by the interpolation function.
	# Use the ratio of the interpolator integral to the pixel value
	# as a correction to the peak subpixel value of the interpolator.
	# The peak value is critical to the direct FWHM measurement.

	k = x1
	l = y1
	ptr = data
	peak = Memr[ptr]
	do j = y1, y2 {
	    do i = x1, x2 {
		if (Memr[ptr] > peak) {
		    peak = Memr[ptr]
		    k = i
		    l = j
		}
		ptr = ptr + 1
	    }
	}
	r1 = 0.
	hm = peak
	do j = -5, 4 {
	    do i = -5, 4 {
		r = msieval (msi, k+(i+0.5)/10.-x1+1, l+(j+0.5)/10.-y1+1)
		if (r > hm)
		    hm = r
		r1 = r1 + r
	    }
	}
	hm = 0.5 * (hm * peak / (r1 / 100.) - bkgd)
	hmi1 = -MAX_REAL
	hmi2 = MAX_REAL
	fwhmr = 0.
	fwhmi = 0.

	xx = 0.
	yy = 0.
	xy = 0.
	ptr = data
	do j = y1, y2 {
	    dy = j - yc
	    do i = x1, x2 {
		dx = i - xc

		# Set the subpixel sampling which may be a function of radius.
		r = sqrt (dx * dx + dy * dy)
		ns = stf_r2n (r)
		ds = 1. / ns
		da = ds * ds
		dz = 0.5 + 0.5 * ds

		# Sum the interpolator values over the subpixels and compute
		# an offset to give the correct total for the pixel.

		r2 = 0.
		dy1 = dy - dz
		do l = 1, ns {
		    dy1 = dy1 + ds
		    dy2 = dy1 * dy1
		    dx1 = dx - dz
		    do k = 1, ns {
			dx1 = dx1 + ds
			dx2 = dx1 * dx1
			r1 = msieval (msi, dx1+xc-x1+1, dy1+yc-y1+1)
			r2 = r2 + r1
		    }
		}

		r1 = Memr[ptr] - bkgd
		ptr = ptr + 1
		r2 = r1 - r2 * da

		# Accumulate the enclosed flux over the sub pixels.
		dy1 = dy - dz
		do l = 1, ns {
		    dy1 = dy1 + ds
		    dy2 = dy1 * dy1
		    dx1 = dx - dz
		    do k = 1, ns {
			dx1 = dx1 + ds
			dx2 = dx1 * dx1
			r = max (0., sqrt (dx2 + dy2) - ds / 2)
			if (r < radius) {
			    r1 = da * (msieval (msi, dx1+xc-x1+1, dy1+yc-y1+1) +
				r2)

			    # Use approximation for fractions of a subpixel.
			    for (m=stf_r2i(r)+1; m<=np; m=m+1) {
				r3 = (stf_i2r (real(m)) - r) / ds
				if (r3 >= 1.)
				    break
				Memr[profile+m-1] = Memr[profile+m-1] + r3 * r1
			    }

			    # The subpixel is completely within these radii.
			    for (; m<=np; m=m+1)
				Memr[profile+m-1] = Memr[profile+m-1] + r1

			    # Accumulate points near the half-max for a
			    # weighted average estimate of the FWHM.  Save
			    # the nearest points above and below the half-max
			    # in case there are no points within the
			    # accumulation window.

			    r = r + ds / 2
			    r3 = r1 / da - hm
			    if (r3 < 0.) {
				if (r3 > hmi1) {
				    hmi1 = r3
				    hmr1 = r
				}
			    } else {
				if (r3 < hmi2) {
				    hmi2 = r3
				    hmr2 = r
				}
			    }
			    r3 = max (0., 0.2 - abs (r3 / hm)) / ns
			    if (r3 > 0.) {
				fwhmi = fwhmi + r3
				fwhmr = fwhmr + r3 * r
			    }

			    # Accumulate the moments above an isophote.
			    if (r1 > miso) {
				xx = xx + dx2 * r1
				yy = yy + dy2 * r1
				xy = xy + dx1 * dy1 * r1
			    }
			}
		    }
		}
	    }
	}

	call msifree (msi)

	# Compute the direct FWHM.
	if (fwhmi > 0.)
	    fwhm = 2 * fwhmr / fwhmi
	else {
	    if (hmi1 == hmi2)
		fwhm = hmr1 + hmr2
	    else
		fwhm = 2 * (hmr1 * hmi2 - hmr2 * hmi1) / (hmi2 - hmi1)
	}
	SFD_PEAK(sfd) = 2 * hm
	SFD_DFWHM(sfd) = fwhm

	# Compute the ellipticity and position angle from the moments.
	r = (xx + yy)
	if (r > 0.) {
	    r1 = (xx - yy) / r
	    r2 = 2 * xy / r
	    SFD_E(sfd) = sqrt (r1**2 + r2**2)
	    SFD_PA(sfd) = RADTODEG (atan2 (r2, r1) / 2.)
	} else {
	    SFD_E(sfd) = 0.
	    SFD_PA(sfd) = 0.
	}

	# The magnitude and profile normalization is from the max enclosed flux.
	call alimr (Memr[profile], np, r, SFD_M(sfd))
	if (SFD_M(sfd) <= 0.)
	    call error (1, "Invalid flux profile")
	call adivkr (Memr[profile], SFD_M(sfd), Memr[profile], np)

	# Fit interpolator to the enclosed flux profile.
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[profile], np)
	SFD_ASI1(sfd) = asi

	# Estimate a gaussian sigma (actually sqrt(2)*sigma) and if it is
	# it is small subtract the gaussian so that the image interpolator
	# can more accurately estimate subpixel values.

	#call stf_radius (sf, sfd, SF_LEVEL(sf), r)
	#sigma = r / sqrt (log (1/(1-SF_LEVEL(sf))))
	call stf_radius (sf, sfd, 0.8, r)
	sigma = 2 * r * sqrt (log(2.) / log (1/(1-0.8)))
	if (sigma < 5.) {
	    if (sigma <= 2.) {
		call gsrestore (gs, gsdata)
		dx = xc - nint (xc)
		dy = yc - nint (yc)
		r = sqrt (dx * dx + dy * dy)
		dx = 1.
		ds = abs (sigma - gseval (gs, r, dx))
		for (da = 1.; da <= 2.; da = da + .01) {
		    dz = abs (sigma - gseval (gs, r, da))
		    if (dz < ds) {
			ds = dz
			dx = da
		    }
		}
		sigma = dx
	    }

	    sigma = sigma / (2 * sqrt (log(2.)))
	    sigma = sigma * sigma

	    # Compute the peak that gives the correct central pixel value.
	    i = nint (xc)
	    j = nint (yc)
	    dx = i - xc
	    dy = j - yc
	    r = sqrt (dx * dx + dy * dy)
	    ns = stf_r2n (r)
	    ds = 1. / ns
	    da = ds * ds
	    dz = 0.5 + 0.5 * ds

	    r1 = 0.
	    dy1 = dy - dz
	    do l = 1, ns {
		dy1 = dy1 + ds
		dy2 = dy1 * dy1
		dx1 = dx - dz
		do k = 1, ns {
		    dx1 = dx1 + ds
		    dx2 = dx1 * dx1
		    r2 = (dx2 + dy2) / sigma
		    if (r2 < 25.)
			r1 = r1 + exp (-r2)
		}
	    }
	    ptr = data + (j - y1) * nx + (i - x1)
	    peak = (Memr[ptr] - bkgd) / (r1 * da)

	    # Subtract the gaussian from the data.
	    ptr = data
	    do j = y1, y2 {
		dy = j - yc
		do i = x1, x2 {
		    dx = i - xc
		    r = sqrt (dx * dx + dy * dy)
		    ns = stf_r2n (r)
		    ds = 1. / ns
		    da = ds * ds
		    dz = 0.5 + 0.5 * ds

		    r1 = 0.
		    dy1 = dy - dz
		    do l = 1, ns {
			dy1 = dy1 + ds
			dy2 = dy1 * dy1
			dx1 = dx - dz
			do k = 1, ns {
			    dx1 = dx1 + ds
			    dx2 = dx1 * dx1
			    r2 = (dx2 + dy2) / sigma
			    if (r2 < 25.)
				r1 = r1 + peak * exp (-r2)
			}
		    }
		    Memr[ptr] = Memr[ptr] - r1 * da
		    ptr = ptr + 1
		}
	    }

	    # Fit the image interpolator to the residual data.
	    call msiinit (msi, II_BISPLINE3)
	    call msifit (msi, Memr[data], nx, ny, nx)

	    # Recompute the direct FWHM, enclosed flux profile and moments
	    # using the gaussian plus image interpolator fit to the residuals.

	    call aclrr (Memr[profile], np)

	    hm = 0.5 * (max (hm, peak+msieval (msi, xc-x1+1, yc-y1+1)) - bkgd)
	    hmi1 = -MAX_REAL
	    hmi2 = MAX_REAL
	    fwhmr = 0.
	    fwhmi = 0.

	    xx = 0.
	    yy = 0.
	    xy = 0.
	    ptr = data
	    do j = y1, y2 {
		dy = j - yc
		do i = x1, x2 {
		    dx = i - xc
		    r = sqrt (dx * dx + dy * dy)
		    ns = stf_r2n (r)
		    ds = 1. / ns
		    da = ds * ds
		    dz = 0.5 + 0.5 * ds

		    # Compute interpolator correction.
		    r2 = 0.
		    dy1 = dy - dz
		    do l = 1, ns {
			dy1 = dy1 + ds
			dx1 = dx - dz
			do k = 1, ns {
			    dx1 = dx1 + ds
			    r1 = msieval (msi, dx1+xc-x1+1, dy1+yc-y1+1)
			    r2 = r2 + r1
			}
		    }

		    r1 = Memr[ptr] - bkgd
		    ptr = ptr + 1
		    r2 = r1 - r2 * da

		    # Accumulate the enclosed flux and moments.
		    dy1 = dy - dz
		    do l = 1, ns {
			dy1 = dy1 + ds
			dy2 = dy1 * dy1
			dx1 = dx - dz
			do k = 1, ns {
			    dx1 = dx1 + ds
			    dx2 = dx1 * dx1
			    r3 = (dx2 + dy2) / sigma
			    if (r3 < 25.)
				r3 = peak * exp (-r3)
			    else
				r3 = 0.
			    r = max (0., sqrt (dx2 + dy2) - ds / 2)
			    if (r < radius) {
				r1 = msieval (msi, dx1+xc-x1+1, dy1+yc-y1+1)
				r1 = da * (r1 + r2 + r3)

				for (m=stf_r2i(r)+1; m<=np; m=m+1) {
				    r3 = (stf_i2r (real(m)) - r) / ds
				    if (r3 >= 1.)
					break
				    Memr[profile+m-1] = Memr[profile+m-1] +
					r3 * r1
				}
				for (; m<=np; m=m+1)
				    Memr[profile+m-1] = Memr[profile+m-1] + r1

				# Accumulate points near the half-max for a
				# weighted average estimate of the FWHM.
				# Save the nearest points above and below the
				# half-max in case there are no points within
				# the accumulation window.

				r = r + ds / 2
				r3 = r1 / da - hm
				if (r3 < 0.) {
				    if (r3 > hmi1) {
					hmi1 = r3
					hmr1 = r
				    }
				} else {
				    if (r3 < hmi2) {
					hmi2 = r3
					hmr2 = r
				    }
				}
				r3 = max (0., 0.2 - abs (r3 / hm)) / ns
				if (r3 > 0.) {
				    fwhmi = fwhmi + r3
				    fwhmr = fwhmr + r3 * r
				}

				if (r1 > miso) {
				    xx = xx + dx2 * r1
				    yy = yy + dy2 * r1
				    xy = xy + dx1 * dy1 * r1
				}
			    }
			}
		    }
		}
	    }

	    call msifree (msi)

	    # Recompute the direct FWHM.
	    if (fwhmi > 0.)
		fwhm = 2 * fwhmr / fwhmi
	    else {
		if (hmi1 == hmi2)
		    fwhm = hmr1 + hmr2
		else
		    fwhm = 2 * (hmr1 * hmi2 - hmr2 * hmi1) / (hmi2 - hmi1)
	    }
	    SFD_PEAK(sfd) = 2 * hm
	    SFD_DFWHM(sfd) = fwhm

	    # Recompute the moments, magnitude, normalized flux, and interp.
	    r = (xx + yy)
	    if (r > 0.) {
		r1 = (xx - yy) / r
		r2 = 2 * xy / r
		SFD_E(sfd) = sqrt (r1**2 + r2**2)
		SFD_PA(sfd) = RADTODEG (atan2 (r2, r1) / 2.)
	    } else {
		SFD_E(sfd) = 0.
		SFD_PA(sfd) = 0.
	    }

	    call alimr (Memr[profile], np, r, SFD_M(sfd))
	    if (SFD_M(sfd) <= 0.)
		call error (1, "Invalid flux profile")
	    call adivkr (Memr[profile], SFD_M(sfd), Memr[profile], np)

	    call asifit (asi, Memr[profile], np)
	    SFD_ASI1(sfd) = asi
	}

	# Compute derivative of enclosed flux profile with a one pixel
	# width and fit an image interpolator.

	do i = 1, np {
	    r = stf_i2r (real(i))
	    r1 = stf_r2i (r - 0.5)
	    if (r1 < 1.) {
		j = i
		next
	    }
	    r2 = stf_r2i (r + 0.5)
	    if (r2 > np) {
		k = i
		break
	    }

	    dy = asieval (asi, r2) - asieval (asi, r1)
	    Memr[profile+i-1] = dy / r
	}
	do i = 1, j
	    Memr[profile+i-1] = Memr[profile+j]
	do i = k, np
	    Memr[profile+i-1] = Memr[profile+k-2]

	call alimr (Memr[profile], np, SFD_YP1(sfd), SFD_YP2(sfd))
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[profile], np)
	SFD_ASI2(sfd) = asi
	SF_XP1(sf) = j+1
	SF_XP2(sf) = k-1

	call sfree (sp)
end


# STF_NORM -- Renormalize the enclosed flux profile.

procedure stf_norm (sf, sfd, x, y)

pointer	sf			#I Parameter structure
pointer	sfd			#I Star structure
real	x			#I Radius
real	y			#I Flux

int	npmax, np
pointer	asi

int	i, j, k
real	r, r1, r2, dy
pointer	sp, profile
real	asieval(), stf_i2r(), stf_r2i()
errchk	asifit

begin
	npmax = SFD_NPMAX(sfd)
	np = SFD_NP(sfd)
	asi = SFD_ASI1(sfd)

	call smark (sp)
	call salloc (profile, npmax, TY_REAL)

	# Renormalize the enclosed flux profile.
	if (IS_INDEF(x) || x <= 0.) {
	    dy = SFD_BKGD(sfd) - SFD_BKGD1(sfd)
	    SFD_BKGD(sfd) = SFD_BKGD(sfd) - dy
	    do i = 1, npmax
		Memr[profile+i-1] = asieval (asi, real(i)) +
		    dy * stf_i2r(real(i)) ** 2
	    call alimr (Memr[profile], np, r1, r2)
	    call adivkr (Memr[profile], r2, Memr[profile], npmax)
	} else if (IS_INDEF(y)) {
	    r = max (1., min (real(np), stf_r2i (x)))
	    r2 = asieval (asi, r)
	    if (r2 <= 0.)
		return
	    do i = 1, npmax
		Memr[profile+i-1] = asieval (asi, real(i))
	    call adivkr (Memr[profile], r2, Memr[profile], npmax)
	} else {
	    r = max (1., min (real(np), stf_r2i (x)))
	    r1 = asieval (asi, r)
	    dy = (y - r1) / x ** 2
	    SFD_BKGD(sfd) = SFD_BKGD(sfd) - dy
	    do i = 1, npmax
		Memr[profile+i-1] = asieval (asi, real(i)) +
		    dy * stf_i2r(real(i)) ** 2
	}

	call asifit (asi, Memr[profile], npmax)
	SFD_ASI1(sfd) = asi

	# Recompute derivative of enclosed flux profile with a one pixel
	# width and fit an image interpolator.

	do i = 1, npmax {
	    r = stf_i2r (real(i))
	    r1 = stf_r2i (r - 0.5)
	    if (r1 < 1.) {
		j = i
		next
	    }
	    r2 = stf_r2i (r + 0.5)
	    if (r2 > np) {
		k = i
		break
	    }

	    dy = asieval (asi, r2) - asieval (asi, r1)
	    Memr[profile+i-1] = dy / r
	}
	do i = 1, j
	    Memr[profile+i-1] = Memr[profile+j]
	do i = k, npmax
	    Memr[profile+i-1] = Memr[profile+k-2]

	call alimr (Memr[profile], np, SFD_YP1(sfd), SFD_YP2(sfd))
	asi = SFD_ASI2(sfd)
	call asifit (asi, Memr[profile], np)
	SFD_ASI2(sfd) = asi
	SF_XP1(sf) = min (j+1, np)
	SF_XP2(sf) = min (k-1, np)

	call sfree (sp)
end


# STF_I2R -- Compute radius from sample index.

real procedure stf_i2r (i)

real	i			#I Index
real	r			#O Radius

begin
	if (i < 20)
	    r = 0.05 * i
	else if (i < 30)
	    r = 0.1 * i - 1
	else if (i < 40)
	    r = 0.2 * i - 4
	else if (i < 50)
	    r = 0.5 * i - 16
	else
	    r = i - 41
	return (r)
end


# STF_R2I -- Compute sample index from radius.

real procedure stf_r2i (r)

real	r			#I Radius
real	i			#O Index

begin
	if (r < 1)
	    i = 20 * r
	else if (r < 2)
	    i = 10 * (r + 1)
	else if (r < 4)
	    i = 5 * (r + 4)
	else if (r < 9)
	    i = 2 * (r + 16)
	else
	    i = r + 41
	return (i)
end


# STF_R2N -- Compute number of subsamples from radius.

int procedure stf_r2n (r)

real	r			#I Radius
int	n			#O Number of subsamples

begin
	if (r < 1)
	    n = 20
	else if (r < 2)
	    n = 10
	else if (r < 4)
	    n = 5
	else if (r < 9)
	    n = 2
	else
	    n = 1
	return (n)
end


# STF_FWHM -- Measure FWHM vs level.

procedure stf_fwhm (sf, sfd)

pointer	sf			#I Main data structure
pointer	sfd			#I Star data structure

int	i
real	level, r

begin
	do i = 1, 19 {
	    level = i * 0.05
	    call stf_radius (sf, sfd, level, r)
	    SFD_FWHM(sfd,i) = 2 * r * sqrt (log (2.) / log (1/(1-level)))
	}
end


# STF_RADIUS -- Measure the radius at the specified level.

procedure stf_radius (sf, sfd, level, r)

pointer	sf			#I Main data structure
pointer	sfd			#I Star data structure
real	level			#I Level to measure
real	r			#O Radius

int	np
pointer	asi
real	f, fmax, rmax, asieval(), stf_i2r()

begin
	np = SFD_NP(sfd)
	asi = SFD_ASI1(sfd)

	for (r=1; r <= np && asieval (asi, r) < level; r = r + 0.01)
	    ;
	if (r > np) {
	    fmax = 0.
	    rmax = 0.
	    for (r=1; r <= np; r = r + 0.01) {
		f = asieval (asi, r)
		if (f > fmax) {
		    fmax = f
		    rmax = r
		}
	    }
	    r = rmax
	}
	r = stf_i2r (r) * SF_SCALE(sf)
end
