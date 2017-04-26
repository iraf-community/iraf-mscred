include	<error.h>
include	<math.h>
include	<mach.h>
include	<imhdr.h>
include	<time.h>
include	<math/curfit.h>

define	TYPES	"|data|fit|difference|ratio|mask|"
define	DATA	1
define	FIT	2
define	DIFF	3
define	RATIO	4
define	MASK	5


# T_PUPILFIT -- Fit a pupil image and output the fit, the difference, or ratio.

procedure t_pupilfit ()

int	inlist			# Input list of images
int	outlist			# Output list of images.
int	mlist			# Input list of masks
int	type			# Output type
real	ixc, iyc		# Center of pupil
real	irin1, irin2		# Inner ring
real	irout1, irout2		# Outer ring
bool	lmedian			# Line median subtraction
int	tfunc1			# Azimuthal function type (inner)
int	torder1			# Azimuthal function order (inner)
int	tfunc2			# Azimuthal function type (outer)
int	torder2			# Azimuthal function order (outer)
int	rfunc			# Radial function type
int	rorder			# Radial function order
int	sfunc			# Radial scale function type
int	sorder			# Radial scale function order
real	abin			# Radial scale azimuthal binning
real	astep			# Radial scale azimuthal step
int	niterate		# Number of rejection iterations
real	lreject, hreject	# Rejection factors
real	datamin, datamax	# Data limits to accept
bool	verbose			# Verbose?

int	i, j, k, nc, nl, nring, nin, nout, ncv, nsum, nbkg
real	rref[2], wref[2], cd[2,2], xc, yc, rin1, rin2, rout1, rout2
real	x, y, z, y2, a, b, zmax, bkgval, bkgavg
real	r, rmin, rmin2, rmax, rmax2, rin3, rout3
real	dt, tstep, t, tmin, tmax, t1, t2
double	sum1, sum2, meansky, ringsum, maxring
pointer	sp, input, output, mask, str, time
pointer	data, cvs, bufin, bufout, bufpm, ptr, bkg, bkgdata
pointer	rring, rin, rout, rp
pointer	tring, tin, tout, tp
pointer	zring, zin, zout, zp
pointer	wring, win, wout, wp
pointer	in, out, pm, mw, ct, cvin, cvout, cvr, cvt

bool	clgetb(), streq(), im_pmlne2()
int	clgeti(), clgwrd(), imtopenp(), imtlen(), imtgetim()
real	clgetr(), cveval(), asokr()
long	clktime()
pointer	immap(), yt_pmmap(), imgl2r(), imgl2s(), impl2r()
pointer	mw_openim(), mw_sctran()
errchk	immap, yt_pmmap, mw_openim, malloc, pupfit

define	output_	10

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (mask, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	# Get task parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	mlist = imtopenp ("masks")
	if (imtlen (inlist) < 1 ||
	    (imtlen (outlist) > 0 && imtlen (outlist) != imtlen (inlist)))
	    call error (1, "Error in input and output lists")
	if (imtlen (mlist) > 1 && imtlen (mlist) != imtlen (inlist))
	    call error (1, "Error in mask list")
	type = clgwrd ("type", Memc[str], SZ_LINE, TYPES)
	
	ixc = clgetr ("xc")
	iyc = clgetr ("yc")
	irin2 = clgetr ("rin")
	irin1 = clgetr ("drin")
	irout2 = clgetr ("rout")
	irout1 = clgetr ("drout")

	lmedian = clgetb ("lmedian")
	tfunc1 = clgwrd ("funcin", Memc[str], SZ_LINE, CV_FUNCTIONS)
	torder1 = clgeti ("orderin")
	tfunc2 = clgwrd ("funcout", Memc[str], SZ_LINE, CV_FUNCTIONS)
	torder2 = clgeti ("orderout")
	rfunc = clgwrd ("rfunction", Memc[str], SZ_LINE, CV_FUNCTIONS)
	rorder = clgeti ("rorder")
	sfunc = clgwrd ("sfunction", Memc[str], SZ_LINE, CV_FUNCTIONS)
	sorder = clgeti ("sorder")
	abin = clgetr ("abin")
	astep = clgetr ("astep")
	niterate = clgeti ("niterate")
	lreject = clgetr ("lreject")
	hreject = clgetr ("hreject")
	datamin = clgetr ("datamin")
	datamax = clgetr ("datamax")
	verbose = clgetb ("verbose")

	if (IS_INDEFR(datamin))
	    datamin = -MAX_REAL
	if (IS_INDEFR(datamax))
	    datamax = MAX_REAL

	# Set ring boundaries from input parameters.
	irin2 = irin2 + irin1 / 2.
	irin1 = max (0., irin2 - irin1)
	irout2 = irout2 + irout1 / 2.
	irout1 = max (0., irout2 - irout1)
	if (irin2 > irout1)
	    call error (2, "Inner ring extends into or beyond the outer ring")

	# Loop through images.
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		Memc[output] = EOS
	    if (imtlen (mlist) > 0)
		i = imtgetim (mlist, Memc[mask], SZ_FNAME)
	    else
		Memc[mask] = EOS

	    in = NULL; out = NULL; pm = NULL; mw = NULL
	    cvin = NULL; cvout = NULL; cvs = NULL
	    data = NULL; bufpm = NULL; bkg = NULL; bkgdata = NULL

	    iferr {
		# Map the input image.
		if (streq (Memc[input], Memc[output])) {
		    ptr = immap (Memc[input], READ_WRITE, 0); in = ptr
		    out = in
		} else {
		    ptr = immap (Memc[input], READ_ONLY, 0); in = ptr
		    if (Memc[output] != EOS) {
			if (type == MASK) {
			    iferr (call imgstr (in, "extname", Memc[str],
			        SZ_LINE))
				call strcpy ("bpm", Memc[str], SZ_LINE)
			    call xt_maskname (Memc[output], Memc[str],
			        NEW_IMAGE, Memc[output], SZ_FNAME)
			}
			ptr = immap (Memc[output], NEW_COPY, in); out = ptr
		    }
		}
		if (Memc[mask] != EOS) {
		    ptr = yt_pmmap (Memc[mask], in, Memc[str], SZ_LINE)
		    pm = ptr
		}

		nc = IM_LEN(in,1)
		nl = IM_LEN(in,2)

		# Convert to logical coordinates.
		ptr = mw_openim (in); mw = ptr
		ct = mw_sctran (mw, "physical", "logical", 03)
		call mw_gwtermr (mw, rref, wref, cd, 2)
		rref[1] = rref[1] + ixc
		rref[2] = rref[2] + iyc
		call mw_c2tranr (ct, rref[1], rref[2], xc, yc)
		call mw_c2tranr (ct, rref[1]+irin1, rref[2], x, y)
		rin1 = sqrt ((x-xc)**2 + (y-yc)**2)
		call mw_c2tranr (ct, rref[1]+irin2, rref[2], x, y)
		rin2 = sqrt ((x-xc)**2 + (y-yc)**2)
		call mw_c2tranr (ct, rref[1]+irout1, rref[2], x, y)
		rout1 = sqrt ((x-xc)**2 + (y-yc)**2)
		call mw_c2tranr (ct, rref[1]+irout2, rref[2], x, y)
		rout2 = sqrt ((x-xc)**2 + (y-yc)**2)
		call mw_close (mw)
		mw = NULL

		rin3 = (rin1 + rin2) / 2. 
		rout3 = (rout1 + rout2) / 2. 
		rmin = min (rin1, rin2, rout1, rout2)
		rmax = max (rin1, rin2, rout1, rout2)
		rmin2 = rmin ** 2
		rmax2 = rmax ** 2

		# Determine number of points in each ring and the total number.
		nin = 0
		nout = 0
		nring = 0
		do i = max (1,nint(yc-rmax)), min (nl,nint(yc+rmax)) {
		    y = (i - yc)
		    y2 = y ** 2
		    do j = max (1,nint(xc-rmax)), min (nc,nint(xc+rmax)) { 
			x = (j - xc)
			r = x**2 + y2
			if (r >= rmax2)
			    next
			r = sqrt (r)
			if (r >= rout1)
			    nout = nout + 1
			else if (r >= rin1 && r <= rin2)
			    nin = nin + 1
			nring = nring + 1
		    }
		}

		# If no points then do quick output.
		if (nring == 0 || type == MASK)
		    goto output_
		if (nin < 2 || nout < 2) {
		    call eprintf (
		"WARNING: Background rings do not contain sufficient points\n")
		    nring = 0
		    goto output_
		}

		if (verbose) {
		    call printf ("  Fitting pupil image in %s ...\n")
			call pargstr (Memc[input])
		    call flush (STDOUT)
		}

		# Allocate memory and set pointers for each ring.
		call malloc (data, 5*nring, TY_REAL)
		if (lmedian) {
		    call malloc (bkg, nl, TY_REAL)
		    call malloc (bkgdata, nc, TY_REAL)
		    call amovkr (1., Memr[bkg], nl)
		}
		rring = data
		tring = data + nring
		zring = data + 2 * nring
		wring = data + 3 * nring
		rin = rring + (nring - nin - nout)
		tin = tring + (nring - nin - nout)
		zin = zring + (nring - nin - nout)
		win = wring + (nring - nin - nout)
		rout = rin + nin
		tout = tin + nin
		zout = zin + nin
		wout = win + nin
		call amovkr (1., Memr[wring], nring)

		# Get data.
		nin = 0
		nout = 0
		nring = 0
		tmin = 360.
		tmax = -360.
		sum1 = 0.
		sum2 = 0.
		bkgavg = 0.
		do i = max (1,nint(yc-rmax)), min (nl,nint(yc+rmax)) {
		    y = (i - yc)
		    y2 = y ** 2
		    bufin = imgl2r (in, i)
		    if (pm != NULL) {
			if (im_pmlne2 (pm, i))
			    bufpm = imgl2s (pm, i)
			else
			    bufpm = NULL
		    }
		    if (lmedian) {
			nbkg = 0
			do j = 1, nc {
			    if (bufpm != NULL)
				if (Mems[bufpm+j-1] != 0)
				    next
			    z = Memr[bufin+j-1]
			    if (z < datamin || z > datamax)
				next
			    Memr[bkgdata+nbkg] = z
			    nbkg = nbkg + 1
			}
			if (nbkg > 1)
			    bkgval = asokr (Memr[bkgdata], nbkg, nbkg / 2)
			Memr[bkg+i-1] = bkgval
		    } else
			bkgval = 1
		    do j = max (1,nint(xc-rmax)), min (nc,nint(xc+rmax)) { 
			x = (j - xc)
			r = x**2 + y2
			if (r >= rmax2)
			    next
			if (bufpm != NULL)
			    if (Mems[bufpm+j-1] != 0)
				next
			z = Memr[bufin+j-1]
			if (z < datamin || z > datamax)
			    next
			z = z / bkgval
			sum1 = sum1 + z
			sum2 = sum2 + z * z
			bkgavg = bkgavg + bkgval
			r = sqrt (r)
			t = RADTODEG (atan2 (y, x))
			tmin = min (t, tmin)
			tmax = max (t, tmax)
			if (r >= rout1) {
			    Memr[rout+nout] = r
			    Memr[tout+nout] = t
			    Memr[zout+nout] = z
			    nout = nout + 1
			} else if (r >= rin1 && r <= rin2) {
			    Memr[rin+nin] = r
			    Memr[tin+nin] = t
			    Memr[zin+nin] = z
			    nin = nin + 1
			} else {
			    Memr[rring+nring] = r
			    Memr[tring+nring] = t
			    Memr[zring+nring] = z
			    nring = nring + 1
			}
		    }
		}
		if (pm != NULL)
		    call imunmap (pm)

		nsum = nin + nout + nring

		# Adjust background to average.
		if (lmedian) {
		    bkgavg = bkgavg / nsum
		    call amulkr (Memr[zring], bkgavg, Memr[zring], nsum)
		    sum1 = sum1 * bkgavg
		    sum2 = sum2 * bkgavg * bkgavg
		}

		# Sigma clip.
		sum1 = sum1 / nsum
		sum2 = sqrt ((sum2 - nsum * sum1 * sum1) / (nsum - 1))

		a = sum1 - lreject * sum2
		b = sum1 + hreject * sum2

		rp = rring
		tp = tring
		zp = zring
		j = 0
		do i = 1, nring {
		    z = Memr[zp]
		    if (z >= a && z <= b) {
			Memr[rring+j] = Memr[rp]
			Memr[tring+j] = Memr[tp]
			Memr[zring+j] = z
			j = j + 1
		    }
		    rp = rp + 1
		    tp = tp + 1
		    zp = zp + 1
		}
		nring = j

		rp = rin
		tp = tin
		zp = zin
		j = 0
		do i = 1, nin {
		    z = Memr[zp]
		    if (z >= a && z <= b) {
			Memr[rin+j] = Memr[rp]
			Memr[tin+j] = Memr[tp]
			Memr[zin+j] = z
			j = j + 1
		    }
		    rp = rp + 1
		    tp = tp + 1
		    zp = zp + 1
		}
		nin = j

		rp = rout
		tp = tout
		zp = zout
		j = 0
		do i = 1, nout {
		    z = Memr[zp]
		    if (z >= a && z <= b) {
			Memr[rout+j] = Memr[rp]
			Memr[tout+j] = Memr[tp]
			Memr[zout+j] = z
			j = j + 1
		    }
		    rp = rp + 1
		    tp = tp + 1
		    zp = zp + 1
		}
		nout = j

		# Adjust for masked pixels.
		if (rin - rring > nring) {
		    call amovr (Memr[rin], Memr[rring+nring], nin)
		    call amovr (Memr[tin], Memr[tring+nring], nin)
		    call amovr (Memr[zin], Memr[zring+nring], nin)
		    rin = rring + nring
		    tin = tring + nring
		    zin = zring + nring
		}
		if (rout - rin > nin) {
		    call amovr (Memr[rout], Memr[rin+nin], nout)
		    call amovr (Memr[tout], Memr[tin+nin], nout)
		    call amovr (Memr[zout], Memr[zin+nin], nout)
		    rout = rin + nin
		    tout = tin + nin
		    zout = zin + nin
		}

		nring = nring + nin + nout

		# Fit background rings.
		call pupfit (cvin, tfunc1, torder1, tmin, tmax, niterate,
		    lreject, hreject, Memr[tin], Memr[zin], Memr[win], nin)
		call amovkr (1., Memr[win], nin)
		call pupfit (cvout, tfunc2, torder2, tmin, tmax, niterate,
		    lreject, hreject, Memr[tout], Memr[zout], Memr[wout], nout)
		call amovkr (1., Memr[wout], nout)

		# Remove background from data.
		meansky = 0.
		nsum = 0.
		rp = rring
		tp = tring
		zp = zring
		do i = 1, nring {
		    r = Memr[rp]
		    t = Memr[tp]
		    z = Memr[zp]
		    a = (r - rin3) / (rout3 - rin3)
		    b = 1 - a
		    y = a * cveval (cvout, t) + b * cveval (cvin, t)
		    meansky = meansky + y
		    nsum = nsum + 1
		    if (type == RATIO)
			z = z / y - 1
		    else
			z = z - y
		    Memr[zp] = z
		    rp = rp + 1
		    tp = tp + 1
		    zp = zp + 1
		}
		meansky = meansky / nsum
		if (meansky == 0.) {
		    call eprintf (
		"Warning: mean sky is 0. so amplitudes are not meaningful\n")
		    meansky = 1.
		}

		if (type == DATA)
		    goto output_

		# Fit radial profile.
		call xt_sort3 (Memr[tring], Memr[rring], Memr[zring], nring)
		call pupfit (cvr, rfunc, rorder, 0., rmax,
		    niterate, lreject, hreject, Memr[rring], Memr[zring],
		    Memr[wring], nring)
		x = (rout2 - rin1) / 1000.
		zmax = cveval (cvr, rin1)
		for (r=rin1; r<=rout2; r=r+x)
		    zmax = max (zmax, cveval (cvr, r))
		zmax = zmax

		rp = rring
		zp = zring
		wp = wring
		do i = 1, nring {
		    z = cveval (cvr, memr[rp])
		    x = (z / zmax) ** 2
		    if (x > 0.00001) {
			Memr[zp] = Memr[zp] / z
			Memr[wp] = x
		    } else {
			Memr[zp] = 0.
			Memr[wp] = 0.
		    }
		    rp = rp + 1
		    zp = zp + 1
		    wp = wp + 1
		}

		# Fit differential radial profile.
		dt = max (abin, RADTODEG(3./rout1))
		tstep = max (astep, dt)
		ncv = int ((tmax - tmin) / tstep + 1)
		call calloc (cvs, ncv, TY_POINTER)

		tin = tring
		tout = tring + nring - 1
		tp = tin
		do k = 0, ncv - 1 {
		    t1 = tmin + (k + 0.5) * tstep - dt / 2.
		    t2 = t1 + dt
		    if (t2 > tmax + dt / 2) {
			t2 = tmax + dt / 2
			t1 = t2 - dt
		    }
		    while (tp<tout && Memr[tp] < t1)
			tp = tp + 1
		    while (tin < tout && Memr[tin] < t2)
			tin = tin + 1
		    if (tin > tring && Memr[tin] > t2)
			tin = tin - 1
		    while (tin - tp + 1 < 2*sorder) {
			tp = max (tring, tp - 1)
			tin = min (tout, tin + 1)
		    }
		    i = tp - tring
		    rp = rring + i
		    zp = zring + i
		    wp = wring + i

		    i = tin - tp + 1
		    call pupfit (Memi[cvs+k], sfunc, sorder, 0., rmax,
			niterate, lreject, hreject, Memr[rp], Memr[zp],
			Memr[wp], i)
		}


		# Create output.
output_
		if (data != NULL)
		    call mfree (data, TY_REAL)
		maxring = -MAX_DOUBLE
		ringsum = 0.
		nsum = 0.
		if (out == in) {
		    if (nring != 0) {
			do i = max (1,nint(yc-rmax)), min (nl,nint(yc+rmax)) {
			    y = (i - yc)
			    y2 = y ** 2
			    bufout = impl2r (out, i)
			    switch (type) {
			    case DATA:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					Memr[bufout] = 0.
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					a = (r - rin3) / (rout3 - rin3)
					b = 1 - a
					z = a * cveval (cvout, t) +
					    b * cveval (cvin, t)
					z = Memr[bufin] - z
					Memr[bufout] = z
					maxring = max (double(z), maxring)
					if (r > rmin)
					    ringsum = ringsum + z
				    }
				    bufin = bufin + 1
				    bufout = bufout + 1
				}
			    case FIT:
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					Memr[bufout] = 0.
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL)
					    z = cveval (cvr, r) *
						cveval (cvt, r)
					else
					    z = 0.
					Memr[bufout] = z
					maxring = max (double(z), maxring)
					if (r > rmin)
					    ringsum = ringsum + z
				    }
				    bufout = bufout + 1
				}
			    case MASK:
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r > rmax2 || r < rmin2)
					Memr[bufout] = 1.
				    else {
					r = sqrt (r)
					if (r > rout1 || r < rin2)
					    Memr[bufout] = 1.
					else
					    Memr[bufout] = 0.
				    }
				    bufout = bufout + 1
				}
			    case DIFF:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					Memr[bufout] = Memr[bufin]
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL)
					    z = cveval (cvr, r) *
						cveval (cvt, r)
					else
					    z = 0.
					Memr[bufout] = Memr[bufin] - z
					maxring = max (double(z), maxring)
				    }
				    bufin = bufin + 1
				    bufout = bufout + 1
				}
			    case RATIO:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					Memr[bufout] = Memr[bufin]
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL)
					    z = max (0., cveval (cvr, r) *
						cveval (cvt, r))
					else
					    z = 0.
					Memr[bufout] = Memr[bufin] / (z + 1)
					maxring = max (double(z), maxring)
				    }
				    bufin = bufin + 1
				    bufout = bufout + 1
				}
			    }
			}
		    } else if (type == DATA || type == FIT) {
			do i = 1, nl
			    call aclrr (Memr[impl2r(out,i)], nc)
		    } else if (type == MASK) {
			do i = 1, nl
			    call amovkr (1., Memr[impl2r(out,i)], nc)
		    }
		} else if (out != NULL) {
		    if (nring == 0) {
			do i = 1, nl {
			    bufout = impl2r (out, i)
			    switch (type) {
			    case DATA, FIT:
				call aclrr (Memr[bufout], nc)
			    case DIFF, RATIO:
				bufin = imgl2r (in, i)
				call amovr (Memr[bufin], Memr[bufout], nc)
			    case MASK:
				call amovkr (1., Memr[bufout], nc)
			    }
			}
		    } else {
			do i = 1, nl {
			    y = (i - yc)
			    y2 = y ** 2
			    bufout = impl2r (out, i)
			    switch (type) {
			    case DATA:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					Memr[bufout] = 0.
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					a = (r - rin3) / (rout3 - rin3)
					b = 1 - a
					z = a * cveval (cvout, t) +
					    b * cveval (cvin, t)
					z = Memr[bufin] - z
					Memr[bufout] = z
					maxring = max (double(z), maxring)
					if (r > rmin)
					    ringsum = ringsum + z
				    }
				    bufin = bufin + 1
				    bufout = bufout + 1
				}
			    case FIT:
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					Memr[bufout] = 0.
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL)
					    z = cveval (cvr, r) *
						cveval (cvt, r)
					else
					    z = 0.
					Memr[bufout] = z
					maxring = max (double(z), maxring)
					if (r > rmin)
					    ringsum = ringsum + z
				    }
				    bufout = bufout + 1
				}
			    case MASK:
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r > rmax2 || r < rmin2)
					Memr[bufout] = 1.
				    else {
					r = sqrt (r)
					if (r > rout1 || r < rin2)
					    Memr[bufout] = 1.
					else
					    Memr[bufout] = 0.
				    }
				    bufout = bufout + 1
				}
			    case DIFF:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					Memr[bufout] = Memr[bufin]
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL)
					    z = cveval (cvr, r) *
						cveval (cvt, r)
					else
					    z = 0.
					Memr[bufout] = Memr[bufin] - z
					maxring = max (double(z), maxring)
				    }
				    bufin = bufin + 1
				    bufout = bufout + 1
				}
			    case RATIO:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					Memr[bufout] = Memr[bufin]
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL)
					    z = max (0., cveval (cvr, r) *
						cveval (cvt, r))
					else
					    z = 0.
					Memr[bufout] = Memr[bufin] / (z + 1)
					maxring = max (double(z), maxring)
				    }
				    bufin = bufin + 1
				    bufout = bufout + 1
				}
			    }
			}
		    }
		} else {
		    if (nring != 0) {
			do i = 1, nl {
			    y = (i - yc)
			    y2 = y ** 2
			    switch (type) {
			    case DATA:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					;
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					a = (r - rin3) / (rout3 - rin3)
					b = 1 - a
					z = a * cveval (cvout, t) +
					    b * cveval (cvin, t)
					z = Memr[bufin] - z
					maxring = max (double(z), maxring)
					if (r > rmin)
					    ringsum = ringsum + z
				    }
				    bufin = bufin + 1
				}
			    case FIT:
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					;
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL) {
					    z = cveval (cvr, r) *
						cveval (cvt, r)
					    maxring = max (double(z), maxring)
					    if (r > rmin)
						ringsum = ringsum + z
					}
				    }
				}
			    case MASK:
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r > rmax2 || r < rmin2)
					Memr[bufout] = 1.
				    else {
					r = sqrt (r)
					if (r > rout1 || r < rin2)
					    Memr[bufout] = 1.
					else
					    Memr[bufout] = 0.
				    }
				    bufout = bufout + 1
				}
			    case DIFF:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					;
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL) {
					    z = cveval (cvr, r) *
						cveval (cvt, r)
					    maxring = max (double(z), maxring)
					}
				    }
				    bufin = bufin + 1
				}
			    case RATIO:
				bufin = imgl2r (in, i)
				do j = 1, nc { 
				    x = (j - xc)
				    r = x**2 + y2
				    if (r >= rmax2)
					;
				    else {
					r = sqrt (r)
					t = RADTODEG (atan2 (y, x))
					k = max (0, min (ncv-1,
					    nint ((t-tmin)/tstep-0.5)))
					cvt = Memi[cvs+k]
					if (cvt != NULL) {
					    z = max (0., cveval (cvr, r) *
						cveval (cvt, r))
					    maxring = max (double(z), maxring)
					}
				    }
				    bufin = bufin + 1
				}
			    }
			}
		    }
		}
		if (nring != 0) {
		    switch (type) {
		    case DIFF, RATIO:
			if (out != NULL) {
			    call cnvdate (clktime(0), Memc[time], SZ_DATE)
			    call sprintf (Memc[str], SZ_LINE,
				"%s maximum amplitude = %.4g")
				call pargstr (Memc[time])
				call pargd (maxring / meansky)
			    call imastr (out, "pupilcor", Memc[str])
			} else if (!verbose) {
			    call printf ("%g\n")
				call pargd (maxring / meansky)
			}
			if (verbose) {
			    call printf ("    maximum amplitude = %.4g\n")
				call pargd (maxring / meansky)
			    call flush (STDOUT)
			}
		    case DATA, FIT:
			if (out != NULL) {
			    call imaddd (out, "pupilsky", meansky)
			    call imaddd (out, "pupilsum", ringsum)
			    call imaddd (out, "pupilmax", maxring)
			    call sprintf (Memc[str], SZ_LINE,
				"%.1f %.1f %.1f %.1f %.1f %.1f \"%s\"")
				call pargr (clgetr ("xc"))
				call pargr (clgetr ("yc"))
				call pargr (clgetr ("rin"))
				call pargr (clgetr ("drin"))
				call pargr (clgetr ("rout"))
				call pargr (clgetr ("drout"))
				call pargstr (Memc[mask])
			    call imastr (out, "pupilpar", Memc[str])
			} else if (!verbose) {
			    call printf ("%g %g %g\n")
				call pargd (meansky)
				call pargd (ringsum)
				call pargd (maxring)
			}
			if (verbose) {
			    call printf ("    mean background = %.4g\n")
				call pargd (meansky)
			    call printf ("    pupil sum = %.4g\n")
				call pargd (ringsum)
			    call printf ("    maximum pupil value = %.4g\n")
				call pargd (maxring)
			    call printf ("    maximum amplitude = %.4g\n")
				call pargd (maxring / meansky)
			    call flush (STDOUT)
			}
		    }
		}
	    } then {
		if (data != NULL)
		    call mfree (data, TY_REAL)
		if (mw != NULL)
		    call mw_close (mw)
		if (pm != NULL)
		    call imunmap (pm)
		call erract (EA_WARN)
	    }

	    if (out != in && out != NULL)
		call imunmap (out)
	    if (in != NULL)
		call imunmap (in)

	    if (bkg != NULL)
		call mfree (bkg, TY_REAL)
	    if (bkgdata != NULL)
		call mfree (bkgdata, TY_REAL)
	    if (cvin != NULL)
		call cvfree (cvin)
	    if (cvout != NULL)
		call cvfree (cvout)
	    if (cvr != NULL)
		call cvfree (cvr)
	    if (cvs != NULL) {
		do k = 0, ncv-1
		    if (Memi[cvs+k] != NULL)
			call cvfree (Memi[cvs+k])
		call mfree (cvs, TY_POINTER)
	    }
	}

	call imtclose (inlist)
	call imtclose (outlist)
	call sfree (sp)
end


# PUPFIT -- Fit 1D functions to pupil using CURFIT with iterative rejection.

procedure pupfit (cv, function, order, xmin, xmax, niterate, lreject, hreject,
	x, y, w, n)

pointer	cv			#O CV pointer
int	function		#I Fitting function type
int	order			#I Fitting function order
real	xmin, xmax		#I Fitting range
int	niterate		#I Number of rejection iteration
real	lreject, hreject	#I Rejection sigma factors
real	x[ARB]			#I X values
real	y[ARB]			#I Y values
real	w[ARB]			#U Weights (set to zero for rejected points) 
int	n			#I Number of data points

int	i, j, ier, nrms, nreject
real	rms, low, high, residual
pointer	sp, str, resid

errchk	cvinit, cvfit, cvrject, cvsolve

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	if (niterate > 0)
	    call salloc (resid, n, TY_REAL)

	call cvinit (cv, function, order, xmin, xmax)

	call cvfit (cv, x, y, w, n, WTS_USER, ier)
	if (ier == NO_DEG_FREEDOM)
	    call error (1, "No degrees of freedom in fit")

	do j = 1, niterate {
	    call cvvector (cv, x, Memr[resid], n)
	    call asubr (y, Memr[resid], Memr[resid], n)

	    rms = 0.
	    nrms = 0
	    do i = 1, n {
		if ((w[i] != 0.)) {
		    rms = rms + Memr[resid+i-1] ** 2
		    nrms = nrms + 1
		}
	    }

	    if (nrms < 5)
		break

	    rms = sqrt (rms / nrms)
	    low = -lreject * rms
	    high = hreject * rms
	    nreject = 0
	    do i = 1, n {
		if (w[i] == 0.)
		    next

		residual = Memr[resid+i-1]
		if ((residual > high) || (residual < low)) {
		    call cvrject (cv, x[i], y[i], w[i])
		    w[i] = 0.
		    nreject = nreject + 1
		}
	    }

	    if (nreject == 0)
		break

	    call cvsolve (cv, ier)
	}

	call sfree (sp)
end
