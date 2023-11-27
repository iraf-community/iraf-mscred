include	<fset.h>
include	<imhdr.h>
include	<imset.h>
include <gset.h>
include	<pkg/gtools.h>
include	<math/curfit.h>


# T_MSCIMATCH -- Match intensity scales in a set of images.

procedure t_mscimatch ()

int	input		# List of images
int	bpm		# List of bad pixel masks
pointer	measured	# File of measurements
pointer	coords		# File of coordinates
bool	doscale		# Determine scale?
bool	dozero		# Determine zero?
int	box1		# Measurement box 1
int	box2		# Measurement box 2
double	lower		# Lower data threshold
double	upper		# Upper data threshold
int	niterate	# Number of interations
double	sigma		# Sigma clipping factor
bool	interactive	# Interactive fits?
bool	verbose		# Verbose output?

int	i, j, ncoord, nreg, nimages, nimages2, nbpm, fd, fdin, fdout
double	x, y, skyval
pointer	sp, fname, bpmname, sky, data, wts, a, b, ra, dec
pointer	im, mw, pm, ic

bool	clgetb()
int	clgeti(), open(), fscan(), nscan(), access(), imaccess(), nowhite()
int	imtopenp(), imtgetim(), imtrgetim(), imtlen()
double	clgetd(), imgetd()
pointer	immap(), mw_openim(), yt_pmmap()

begin
	call smark (sp)
	call salloc (measured, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (bpmname, SZ_FNAME, TY_CHAR)

	# Get query parameters.
	input = imtopenp ("input")
	bpm = imtopenp ("bpm")
	call clgstr ("measured", Memc[measured], SZ_FNAME)
	call clgstr ("coords", Memc[coords], SZ_FNAME)
	doscale = clgetb ("scale")
	dozero = clgetb ("zero")
	box1 = clgeti ("box1")
	box2 = clgeti ("box2")
	lower = clgetd ("lower")
	upper = clgetd ("upper")
	niterate = clgeti ("niterate")
	sigma = clgetd ("sigma")
	interactive = clgetb ("interactive")
	verbose = clgetb ("verbose")

	if (!doscale && !dozero)
	    call error (1, "Must have at least one of scale and zero set")

	if (box1 < 1 || box1 >= box2)
	    call error (1, "Invalid box sizes")

	# Count and verify input images.
	# We do this now since the measuring step can be slow.
	nimages = 0
	while (imtgetim (input, Memc[fname], SZ_FNAME) != EOF) {
	    if (imaccess (Memc[fname], READ_ONLY) != YES) {
		call sprintf (Memc[coords], SZ_LINE,
		    "Can't access image `(%s)'")
		    call pargstr (Memc[fname])
		call error (2, Memc[coords])
	    }
	    nimages = nimages + 1
	}
	if (nimages < 2)
	    call error (1, "At least two images are required")
	nimages2 = nimages * nimages
	call imtrew (input)

	# Check bpm.
	nbpm = imtlen (bpm)
	if (nbpm > 1 && nbpm != nimages)
	    call error (2, "Mask list not appropriate for image list")

	# See if there is a measured data file.
	fdin = NULL
	fdout = NULL
	if (nowhite (Memc[measured], Memc[measured], SZ_FNAME) > 0) {
	    if (access (Memc[measured], READ_ONLY, 0) == YES)
		fdin = open (Memc[measured], READ_ONLY, TEXT_FILE)
	    else
		fdout = open (Memc[measured], NEW_FILE, TEXT_FILE)
	}

	if (verbose) {
	    call fseti (STDOUT, F_FLUSHNL, YES)
	    call printf ("MSCIMATCH:\n")
	}

	# If there is no measured data file read the coordinates and measure
	# the regions.  Possibly save the measurements in a file.

	if (fdin == NULL) {
	    if (verbose) {
		call printf ("  Reading region coordinates from %s\n")
		    call pargstr (Memc[coords])
	    }

	    # Read coordinates.
	    ncoord = 0
	    fd = open (Memc[coords], READ_ONLY, TEXT_FILE)
	    while (fscan (fd) != EOF) {
		call gargd (x)
		call gargd (y)
		if (nscan() < 2)
		    next
		if (ncoord == 0) {
		    call malloc (ra, 100, TY_DOUBLE)
		    call malloc (dec, 100, TY_DOUBLE)
		} else if (mod (ncoord, 100) == 0) {
		    call realloc (ra, ncoord+100, TY_DOUBLE)
		    call realloc (dec, ncoord+100, TY_DOUBLE)
		}
		Memd[ra+ncoord] = x
		Memd[dec+ncoord] = y
		ncoord = ncoord + 1
	    }
	    call close (fd)
	    if (ncoord < 1)
		call error (3, "Insufficient number of coordinates")
	    nreg = 2 * ncoord
	} else {
	    if (verbose) {
		call printf ("  Reading measurements from %s\n")
		    call pargstr (Memc[measured])
	    }

	    i = fscan (fdin)
	    call gargi (i)
	    call gargi (ncoord)
	    if (nscan() != 2)
		call error (1, "Syntax error in measurement file")
	    if (i != nimages)
		call error (1,
		    "Number of images does not match measurement file")
	    nreg = 2 * ncoord

	    call malloc (ra, ncoord, TY_DOUBLE)
	    call malloc (dec, ncoord, TY_DOUBLE)
	}

	if (verbose) {
	    call printf ("    %d coordinates read\n")
		call pargi (ncoord)
	}

	# Allocate and initialize memory.
	call salloc (sky, nimages, TY_DOUBLE)
	call salloc (data, nreg*nimages, TY_DOUBLE)
	call salloc (wts, nimages2*nreg, TY_DOUBLE)
	call salloc (a, nimages2*2, TY_DOUBLE)
	call salloc (b, nimages2*2, TY_DOUBLE)

	call amovkd (0D0, Memd[sky], nimages)
	call amovkd (INDEFD, Memd[data], nreg*nimages)
	call amovkd (1D0, Memd[wts], nimages2*nreg)
	call amovkd (1D0, Memd[a], nimages2)
	call amovkd (0D0, Memd[b], nimages2)
	call amovkd (0D0, Memd[a+nimages2], nimages2)
	call amovkd (0D0, Memd[b+nimages2], nimages2)

	if (fdin == NULL) {
	    # Measure regions.
	    if (fdout != NULL) {
		if (verbose) {
		    call printf ("  Writing measurements to %s\n")
			call pargstr (Memc[measured])
		}
		call fprintf (fdout, "%d %d\n")
		    call pargi (nimages)
		    call pargi (ncoord)
	    }
	    do i = 1, nimages {
		# Open image.
		if (imtrgetim (input, i, Memc[fname], SZ_FNAME) == EOF)
		    break
		im = immap (Memc[fname], READ_ONLY, 0)

		if (!dozero) {
		    iferr (skyval = imgetd (im, "skymean"))
			skyval = 0.
		    Memd[sky+i-1] = skyval
		}

		if (verbose) {
		    call printf ("  Measuring regions in %s ...\n")
			call pargstr (Memc[fname])
		}

		# Open mask.
		pm = NULL
		if (nbpm > 0) {
		    if (imtrgetim (bpm, min (i, nbpm), Memc[bpmname],
			SZ_FNAME) == EOF)
			break
		    pm = yt_pmmap (Memc[bpmname], im, Memc[bpmname], SZ_FNAME)
		}
		if (pm != NULL && verbose) {
		    call printf ("  Using bad pixel mask %s ...\n")
			call pargstr (Memc[bpmname])
		}

		# Open WCS.
		mw = mw_openim (im)

		# Measure the regions.
		call imat_measure (im, pm, mw, Memd[ra], Memd[dec], ncoord,
		    Memd[data+(i-1)*nreg], nreg, box1, box2, lower, upper,
		    Memd[sky+i-1], i, fdout, verbose)

		call mw_close (mw)
		if (pm != NULL)
		    call imunmap (pm)
		call imunmap (im)
	    }
	    if (fdout != NULL)
		call close (fdout)
	} else {
	    while (fscan (fdin) != EOF) {
		call gargi (i)
		call gargi (j)
		call gargwrd (Memc[fname], SZ_FNAME)
		if (nscan() != 3)
		    next
		call gargd (Memd[ra+j-1])
		call gargd (Memd[dec+j-1])
		call gargd (x)
		call gargd (y)
		call gargd (skyval)
		call gargd (x)
		call gargd (y)
		if (nscan() != 10)
		    next

		Memd[sky+i-1] = skyval
		Memd[data+(i-1)*nreg+2*(j-1)] = x
		Memd[data+(i-1)*nreg+2*(j-1)+1] = y
	    }
	    call close (fdin)
	}

	if (verbose)
	    call printf ("  Determining scale factors ...\n")

	if (interactive) {
	    call imat_icinit (ic, input)
	    call imat_fit (NULL, Memd[data], Memd[wts], Memd[a], Memd[b],
		nimages, nreg, niterate, sigma, doscale, dozero)
	    call imat_fit (ic, Memd[data], Memd[wts], Memd[a], Memd[b],
		nimages, nreg, niterate, sigma, doscale, dozero)
	    call imat_icfree (ic)
	} else
	    call imat_fit (NULL, Memd[data], Memd[wts], Memd[a], Memd[b],
		nimages, nreg, niterate, sigma, doscale, dozero)

	# Correct for fixed zero.
	if (!dozero) {
	    skyval = Memd[sky] * Memd[a]
	    do i = 1, nimages
		Memd[b+(i-1)*nimages] = skyval -
		    Memd[sky+i-1] * Memd[a+(i-1)*nimages]
	}

	# Output zero values are relative to it's own scale.
	do i = 1, nimages
	    Memd[b+(i-1)*nimages] = Memd[b+(i-1)*nimages] /
		Memd[a+(i-1)*nimages]

	# Print final scale factors.
	do i = 1, nimages {
	    if (imtrgetim (input, i, Memc[fname], SZ_FNAME) == EOF)
		break
	    call printf ("%20s: %6.4f (%.4f) %8.2f (%.2f)\n")
		call pargstr (Memc[fname])
		call pargd (Memd[a+(i-1)*nimages])
		call pargd (Memd[a+(i-1)*nimages+nimages2])
		call pargd (Memd[b+(i-1)*nimages])
		call pargd (Memd[b+(i-1)*nimages+nimages2])
	}

	# Update images.
	if (clgetb ("accept")) {
	    do i = 1, nimages {
		if (imtrgetim (input, i, Memc[fname], SZ_FNAME) == EOF)
		    break
		im = immap (Memc[fname], READ_WRITE, 0)
		call imaddd (im, "mscscale", Memd[a+(i-1)*nimages])
		call imaddd (im, "msczero", Memd[b+(i-1)*nimages])
		call imunmap (im)
	    }
	}

	call imtclose (input)

	call mfree (ra, TY_DOUBLE)
	call mfree (dec, TY_DOUBLE)
	call sfree (sp)
end


procedure t_rand ()

int	nimages		# Number of images
int	ncoord		# Number of coordinates
int	ntrials		# Number of trials
real	noise		# Noise factor
long	seed		# Seed
bool	verbose		# Verbose output?

int	i, j, k, nreg, nimages2, nsum
double	flux1, flux2, sum1, sum2, sum3, sum4
pointer	sp, data, wts, a, b

bool	clgetb()
int	clgeti()
long	clgetl
real	clgetr(), urand()

begin
	call smark (sp)

	# Get query parameters.
	nimages = clgeti ("nimages")
	ncoord = clgeti ("ncoord")
	ntrials = clgeti ("ntrials")
	noise = clgetr ("noise")
	seed = clgetl ("seed")
	verbose = clgetb ("verbose")

	if (nimages < 2)
	    call error (1, "At least two images are required")
	nimages2 = nimages * nimages

	if (verbose) {
	    call fseti (STDOUT, F_FLUSHNL, YES)
	    call printf ("MSCIMATCH:\n")
	}

	if (ncoord < 1)
	    call error (3, "Insufficient number of coordinates")
	nreg = 2 * ncoord

	# Allocate and initialize memory.
	call salloc (data, nreg*nimages, TY_DOUBLE)
	call salloc (wts, nimages2*nreg, TY_DOUBLE)
	call salloc (a, nimages2*2, TY_DOUBLE)
	call salloc (b, nimages2*2, TY_DOUBLE)

	call amovkd (INDEFD, Memd[data], nreg*nimages)
	call amovkd (1D0, Memd[wts], nimages2*nreg)
	call amovkd (1D0, Memd[a], nimages2)
	call amovkd (0D0, Memd[b], nimages2)
	call amovkd (0D0, Memd[a+nimages2], nimages2)
	call amovkd (0D0, Memd[b+nimages2], nimages2)

	# Set data.
	sum1 = 0.
	sum2 = 0.
	sum3 = 0.
	sum4 = 0.
	nsum = 0
	do k = 1, ntrials {
	    do i = 1, ncoord {
		flux1 = 100
		flux2 = flux1 + 1000 * urand (seed)
		do j = 1, nimages {
		    Memd[data+(j-1)*nreg+2*(i-1)] = (flux1 + sqrt (flux1) *
			noise * (2 * urand (seed) - 1.) - 10*(j-1)) / j
		    Memd[data+(j-1)*nreg+2*(i-1)+1] = (flux2 + sqrt (flux2) *
			noise * (2 * urand (seed) - 1.) - 10*(j-1)) / j
		}
	    }

	    if (verbose)
		call printf ("  Determining scale factors ...\n")
	    call imat_fit (NULL, Memd[data], Memd[wts], Memd[a], Memd[b],
		nimages, nreg, 4, double(4.), true, true)

	    do i = 2, nimages {
		sum1 = sum1 + (Memd[a+(i-1)*nimages]/i-1)**2
		sum2 = sum2 + Memd[a+(i-1)*nimages+nimages2]/i
		sum3 = sum3 + (Memd[b+(i-1)*nimages]-10*(i-1))**2
		sum4 = sum4 + Memd[b+(i-1)*nimages+nimages2]
		nsum = nsum + 1
	    }

	    # Print final scale factors.
	    if (k == 1) {
		do i = 1, nimages {
		    call printf ("%d: %6.4f (%.4f) %8.2f (%.2f)\n")
			call pargi (i)
			call pargd (Memd[a+(i-1)*nimages])
			call pargd (Memd[a+(i-1)*nimages+nimages2])
			call pargd (Memd[b+(i-1)*nimages])
			call pargd (Memd[b+(i-1)*nimages+nimages2])
		}
	    }
	}

	call printf ("%8.4f %8.4f %8.4f %8.4f\n")
	    call pargd (sqrt (sum1 / nsum))
	    call pargd (sum2 / nsum)
	    call pargd (sqrt (sum3 / nsum))
	    call pargd (sum4 / nsum)

	call sfree (sp)
end


# IMAT_MEASURE -- Measure the fluxes within boxes and thresholds.

procedure imat_measure (im, pm, mw, ra, dec, ncoords, data, nreg, box1, box2,
	lower, upper, sky, nimage, fd, verbose)

pointer	im		#I IMIO pointer
pointer	pm		#I Mask IMIO pointer
pointer	mw		#I MWCS pointer
double	ra[ncoords]	#I RA coordinates
double	dec[ncoords]	#I DEC coordinates
int	ncoords		#I Number of coordinates
double	data[nreg]	#U Photon counts
int	nreg		#I Number of regions
int	box1		#I Measurement box 1
int	box2		#I Measurement box 2
double	lower		#I Lower data threshold
double	upper		#I Upper data threshold
double	sky		#I Sky to subtract
int	nimage		#I Image number
int	fd		#I Output file pointer
bool	verbose		#I Verbose?

int	i, j, k, n, nthresh
int	nc, nl, nbox1, nbox2, hbox1, hbox2, xc, yc, x1, x2, y1, y2
bool	lcheck, ucheck
double	val, sum1, sum2
pointer	sp, x, y, index, imname, err, ct

bool	im_pmsne2()
pointer	buf, mw_sctran(), imgs2d()

int	imat_comp()
extern	imat_comp

define	skip_	10

begin
	call smark (sp)
	call salloc (x, ncoords, TY_DOUBLE)
	call salloc (y, ncoords, TY_DOUBLE)
	call salloc (index, ncoords, TY_INT)

	# Convert world coordinates to image coordinates and sort by line
	# to optimize image I/O.

	ct = mw_sctran (mw, "world", "logical", 3)
	do i = 1, ncoords {
	    call mw_c2trand (ct, 15*ra[i], dec[i], Memd[x+i-1], Memd[y+i-1])
	    Memi[index+i-1] = i
	}
	call mw_ctfree (ct)
	call gqsort (Memi[index], ncoords, imat_comp, y)

	# Measure.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	nbox1 = box1 * box1
	nbox2 = box2 * box2 - nbox1
	hbox1 = box1 / 2
	hbox2 = box2 / 2
	lcheck = (!IS_INDEFD(lower))
	ucheck = (!IS_INDEFD(upper))
	n = 0
	nthresh = 0
	do i = 1, ncoords {
	    j = Memi[index+i-1]
	    xc = nint (Memd[x+j-1])
	    yc = nint (Memd[y+j-1])
	    x1 = xc - hbox2
	    x2 = xc + hbox2
	    y1 = yc - hbox2
	    y2 = yc + hbox2
	    if (x1 < 1 || x2 > nc || y1 < 1 || y2 > nl)
		goto skip_

	    if (pm != NULL)
		if (im_pmsne2 (pm, x1, x2, y1, y2))
		    goto skip_

	    buf = imgs2d (im, x1, x2, y1, y2)
	    sum1 = 0.
	    sum2 = 0.
	    do j = -hbox2, hbox2 {
		do k = -hbox2, hbox2 {
		    val = Memd[buf]
		    buf = buf + 1
		    if (lcheck) {
			if (val < lower) {
			    nthresh = nthresh + 1
			    goto skip_
			}
		    }
		    if (ucheck) {
			if (val > upper) {
			    nthresh = nthresh + 1
			    goto skip_
			}
		    }
		    if (j < -hbox1 || j > hbox1 || k < -hbox1 || k > hbox1)
			sum2 = sum2 + val
		    else
			sum1 = sum1 + val
		}
	    }

	    data[2*i-1] = sum1 / nbox1 - sky
	    data[2*i] = sum2 / nbox2 - sky
	    n = n + 1

skip_	    next
	}

	if (n < 1) {
	    call salloc (imname, SZ_FNAME, TY_CHAR)
	    call salloc (err, SZ_LINE, TY_CHAR)
	    call imstats (im, IM_IMAGENAME, Memc[imname], SZ_FNAME)
	    if (nthresh > 0) {
		call sprintf (Memc[err], SZ_LINE,
		    "No data found `(%s)': check lower and upper limits")
		    call pargstr (Memc[imname])
	    } else {
		call sprintf (Memc[err], SZ_LINE, "No data found `(%s)'")
		    call pargstr (Memc[imname])
	    }
	    call error (4, Memc[err])
	}

	if (verbose) {
	    call printf ("    %d good regions measured\n")
		call pargi (n)
	}

	if (fd != NULL) {
	    call salloc (imname, SZ_FNAME, TY_CHAR)
	    call imstats (im, IM_IMAGENAME, Memc[imname], SZ_FNAME)
	    do i = 1, ncoords {
		if (IS_INDEFD(data[2*i]))
		    next
		j = Memi[index+i-1]
		xc = nint (Memd[x+j-1])
		yc = nint (Memd[y+j-1])
		call fprintf (fd,
		    "%2d %4d %-15s %11.2h %11.1h %4d %4d %.6g %8.6g %8.6g\n")
		    call pargi (nimage)
		    call pargi (i)
		    call pargstr (Memc[imname])
		    call pargd (ra[j])
		    call pargd (dec[j])
		    call pargi (xc)
		    call pargi (yc)
		    call pargd (sky)
		    call pargd (data[2*i-1])
		    call pargd (data[2*i])
	    }
	}

	call sfree (sp)
end


int procedure imat_comp (arg, i, j)

pointer	arg		# Data to compare
int	i, j		# Indices to compare

begin
	if (Memd[arg+i-1] < Memd[arg+j-1])
	    return (-1)
	else if (Memd[arg+i-1] > Memd[arg+j-1])
	    return (1)
	else
	    return (0)
end


# IMAT_FIT -- Determine the scale factors from a set of measurements.
# The measurements are the number of photons in a set of region each
# of which is measured for all images.  If a region was not measured in
# a particular region (such as being off the image) then a value of INDEF
# given.  The relationship between the measurements determined is
#
#	data[i,k] = a[k,j] * data[i,j] + b[k,j]
#
# where j= 1 to nimages, k = 1 to nimages, i = 1 to nreg. 

procedure imat_fit (ic, data, wts, a, b, nimages, nreg, niterate, sigma,
	doscale, dozero)

pointer	ic				#I IC pointer
double	data[nreg,nimages]		#I Photon counts
double	wts[nimages,nimages,nreg]	#U Weights for pairs of measurments
double	a[nimages,nimages,2]		#U Scale factors
double	b[nimages,nimages,2]		#U Zero factors
int	nimages				#I Number of images
int	nreg				#I Number of regions per image
int	niterate			#I Number of iterations
double	sigma				#I Sigma clipping factor
bool	doscale				#I Determine scale?
bool	dozero				#I Determine zero?

int	i, nrej, ndel
pointer	sp, x, y, w

begin
	call smark (sp)
	call salloc (x, nreg, TY_DOUBLE)
	call salloc (y, nreg, TY_DOUBLE)
	call salloc (w, nreg, TY_DOUBLE)

	call imat_fit1 (ic, data, wts, a, b, Memd[x], Memd[y], Memd[w],
	    nimages, nreg, niterate, sigma, doscale, dozero, ndel)
	if (ndel > 0)
	    call imat_fit1 (NULL, data, wts, a, b, Memd[x], Memd[y],
		Memd[w], nimages, nreg, niterate, sigma, doscale, dozero, ndel)
	call imat_scale (a, b, nimages)

	do i = 1, niterate {
	    call imat_rej (NULL, data, wts, a, b, nimages, nreg, sigma, nrej)
	    if (nrej == 0)
		break
	    call imat_fit1 (NULL, data, wts, a, b, Memd[x], Memd[y],
		Memd[w], nimages, nreg, niterate, sigma, doscale, dozero, ndel)
	    call imat_scale (a, b, nimages)
	}

	call sfree (sp)
end


# IMAT_FIT1 -- Fit scale and zero independently for each pair of images.
# This may include iterative rejection based on residual / sqrt (fit).

procedure imat_fit1 (ic, data, wts, a, b, x, y, w, nimages, nreg,
	niterate, sigma, doscale, dozero, ndel)

pointer	ic				#I IC pointer
double	data[nreg,nimages]		#I Photon counts
double	wts[nimages,nimages,nreg]	#U Weights for pairs of measurments
double	a[nimages,nimages,2]		#U Scale factors
double	b[nimages,nimages,2]		#U Zero factors
double	x[nreg], y[nreg], w[nreg]	#I Working arrays for fitting
int	nimages				#I Number of images
int	nreg				#I Number of regions per image
int	niterate			#I Number of iterations
double	sigma				#I Sigma clipping factor
bool	doscale				#I Determine scale?
bool	dozero				#I Determine zero?
int	ndel				#O Number deleted by the user

int	i, j, k, n
double	xmin, xmax, chisqr, coeff[2]
pointer	cv

bool	dos, doz
common	/imatcom/dos, doz
extern	imat_fnc

begin
	ndel = 0
	do k = 1, nimages {
	    do j = 1, nimages {
		if (j == k)
		    next
		n = 0
		do i = 1, nreg {
		    if (IS_INDEFD(data[i,j]) || IS_INDEFD(data[i,k]))
			next
		    n = n + 1
		    x[n] = data[i,j]
		    y[n] = data[i,k]
		    w[n] = wts[j,k,i]
		    if (n == 1) {
			xmin = x[n]
			xmax = x[n]
		    } else {
			xmin = min (xmin, x[n])
			xmax = max (xmax, x[n])
		    }
		}
		if (n < 2)
		    next

		dos = doscale
		doz = dozero
		if (doscale && dozero)
		    call dcvinit (cv, USERFNC, 2, xmin, xmax)
		else
		    call dcvinit (cv, USERFNC, 1, xmin, xmax)
		call dcvuserfnc (cv, imat_fnc)
		if (ic != NULL && k == j+1) {
		    call imat_iclabels (ic, j, k)
		    call imat_fit2 (ic, cv, x, y, w, n, niterate, sigma)
		    n = 0
		    do i = 1, nreg {
			if (IS_INDEFD(data[i,j]) || IS_INDEFD(data[i,k]))
			    next
			n = n + 1
			if (w[n] == 0D0) {
			    wts[j,k,i] = 0D0
			    wts[k,j,i] = 0D0
			    ndel = ndel + 2
			}
		    }
		} else
		    call imat_fit2 (NULL, cv, x, y, w, n, niterate, sigma)
		if (doscale && dozero) {
		    call dcvcoeff (cv, coeff, i)
		    b[k,j,1] = coeff[1]
		    a[k,j,1] = coeff[2]
		    call dcvvector (cv, x, x, n)
		    call dcverrors (cv, y, w, x, n, chisqr, coeff)
		    b[k,j,2] = coeff[1]
		    a[k,j,2] = coeff[2]
		} else if (dozero) {
		    call dcvcoeff (cv, coeff, i)
		    b[k,j,1] = coeff[1]
		    a[k,j,1] = 1
		    call dcvvector (cv, x, x, n)
		    call dcverrors (cv, y, w, x, n, chisqr, coeff)
		    b[k,j,2] = coeff[1]
		    a[k,j,2] = 0
		} else {
		    call dcvcoeff (cv, coeff, i)
		    b[k,j,1] = 0
		    a[k,j,1] = coeff[1]
		    call dcvvector (cv, x, x, n)
		    call dcverrors (cv, y, w, x, n, chisqr, coeff)
		    b[k,j,2] = 0
		    a[k,j,2] = coeff[1]
		}
		call dcvfree (cv)
	    }
	}
end


# IMAT_FNC -- CURFIT user function.

procedure imat_fnc (x, order, k1, k2, basis)

double	x		# array of data points
int	order		# order of polynomial, order = 1, constant
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions

bool	dos, doz
common	/imatcom/dos, doz

begin
	if (doz && dos) {
	    basis[1] = 1
	    basis[2] = x
	} else if (doz)
	    basis[1] = 1
	else
	    basis[1] = x
end


# IMAT_FIT2 -- Fit data from a single pair of images with rejection.

procedure imat_fit2 (ic, cv, x, y, w, n, niterate, sigma)

pointer	ic		#I IC pointer
pointer	cv		#I CV pointer (initialized to desired fit)
double	x[n]		#I X values
double	y[n]		#I Y values
double	w[n]		#U Weight values
int	n		#I Number of points
int	niterate	#I Number of iterations
double	sigma		#I Sigma factor

int	i, nit, nrms, nrej
double	rms, fit, dcveval()

begin
	call dcvfit (cv, x, y, w, n, WTS_USER, i)

	do nit = 1, niterate {
	    rms = 0.
	    nrms = 0
	    do i = 1, n {
		if (w[i] <= 0D0)
		    next
		fit = dcveval (cv, x[i])
		if (fit <= 0D0)
		    next
		rms = rms + ((y[i] - fit) / sqrt (fit)) ** 2
		nrms = nrms + 1
	    }
	    if (nrms == 0)
		break
	    rms = sigma * sqrt (rms / nrms)

	    nrej = 0
	    do i = 1, n {
		if (w[i] <= 0D0)
		    next
		fit = dcveval (cv, x[i])
		if (fit <= 0D0)
		    next
		if (abs ((y[i] - fit) / sqrt (fit)) > rms) {
		    w[i] = 0.
		    nrej = nrej + 1
		}
	    }
	    if (nrej == 0)
		break
	    call dcvfit (cv, x, y, w, n, WTS_USER, i)
	}

	call imat_ic1 (ic, x, y, w, n)
end


# IMAT_SCALE -- Independent scale factors for each pair of images are
# combined into a set of self-consistent scale factors using the relations
#
#    a[k,i] = a[k,j] * a[j,i]
#    b[k,i] = a[k,j] * b[j,i] + b[k,j]

procedure imat_scale (a, b, nimages)

double	a[nimages,nimages,2]	#U Scale factors
double	b[nimages,nimages,2]	#U Zero factors
int	nimages			#I Number of images

int	j, k, n 
double	val, sig, mean, err, stddev
pointer	sp, scale, zero, sstddev, zstddev

begin
	call smark (sp)
	call salloc (scale, nimages, TY_DOUBLE)
	call salloc (zero, nimages, TY_DOUBLE)
	call salloc (sstddev, nimages, TY_DOUBLE)
	call salloc (zstddev, nimages, TY_DOUBLE)

	do k = 1, nimages {

	    # a[k,1]
	    val = a[k,1,1]
	    sig = a[k,1,2]
	    mean = val
	    stddev = val**2
	    err = sig**2

	    # a[k,1] = 1 / a[1,k]
	    call imat_eprop ("/", 1D0, 0D0, a[1,k,1], a[1,k,2], val, sig)
	    mean = mean + val
	    stddev = stddev + val**2
	    err = err + sig**2

	    n = 2
	    do j = 2, nimages {
		if (j == k)
		    next

		# a[k,1] = a[k,j] * a[j,1]
		call imat_eprop ("*", a[k,j,1], a[k,j,2], a[j,1,1], a[j,1,2],
		    val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# a[k,1] = a[k,j] / a[1,j]
		call imat_eprop ("/", a[k,j,1], a[k,j,2], a[1,j,1], a[1,j,2],
		    val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# a[k,1] = a[j,1] / a[j,k]
		call imat_eprop ("/", a[j,1,1], a[j,1,2], a[j,k,1], a[j,k,2],
		    val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# a[k,1] = 1 / (a[j,k] * a[1,j])
		call imat_eprop ("*", a[j,k,1], a[j,k,2], a[1,j,1], a[1,j,2],
		    val, sig)
		call imat_eprop ("/", 1D0, 0D0, val, sig, val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		n = n + 4
	    }

	    # Factor of nimages is currently a fudge factor until
	    # I understand the systematics.
	    mean = mean / n
	    stddev = nimages * (stddev - n * mean * mean) / (n - 1)
	    err = nimages * err / n**2

	    Memd[scale+k-1] = mean
	    if (stddev < 0.)
		Memd[sstddev+k-1] = sqrt (err)
	    else
		Memd[sstddev+k-1] = sqrt (err + stddev)
	}

	do k = 1, nimages {

	    # b[k,1]
	    val = b[k,1,1]
	    sig = b[k,1,2]
	    mean = val
	    stddev = val**2
	    err = sig**2

	    # b[k,1] = -b[1,k] / a[1,k]
	    call imat_eprop ("/", -b[1,k,1], b[1,k,2], a[1,k,1], a[1,k,2],
		val, sig)
	    mean = mean + val
	    stddev = stddev + val**2
	    err = err + sig**2

	    # b[k,1] = -b[1,k] * a[k,1]
	    call imat_eprop ("*", -b[1,k,1], b[1,k,2], a[k,1,1], a[k,1,2],
		val, sig)
	    mean = mean + val
	    stddev = stddev + val**2
	    err = err + sig**2

	    n = 3
	    do j = 2, nimages {
		if (j == k)
		    next

		# b[k,1] = b[j,1] * a[k,j] + b[k,j]
		call imat_eprop ("*", b[j,1,1], b[j,1,2], a[k,j,1], a[k,j,2],
		    val, sig)
		call imat_eprop ("+", val, sig, b[k,j,1], b[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = (b[j,1] - b[j,k]) / a[j,k]
		call imat_eprop ("-", b[j,1,1], b[j,1,2], b[j,k,1], b[j,k,2],
		    val, sig)
		call imat_eprop ("/", val, sig, a[j,k,1], a[j,k,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = -b[1,j] / a[1,j] * a[k,j] + b[k,j]
		call imat_eprop ("/", -b[1,j,1], b[1,j,2], a[1,j,1], a[1,j,2],
		    val, sig)
		call imat_eprop ("*", val, sig, a[k,j,1], a[k,j,2], val, sig)
		call imat_eprop ("+", val, sig, b[k,j,1], b[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = (-b[1,j] / a[1,j] - b[j,k]) / a[j,k]
		call imat_eprop ("/", -b[1,j,1], b[1,j,2], a[1,j,1], a[1,j,2],
		    val, sig)
		call imat_eprop ("-", val, sig, b[j,k,1], b[j,k,2], val, sig)
		call imat_eprop ("/", val, sig, a[j,k,1], a[j,k,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = b[j,1] / a[j,k] + b[k,j]
		call imat_eprop ("/", b[j,1,1], b[j,1,2], a[j,k,1], a[j,k,2],
		    val, sig)
		call imat_eprop ("+", val, sig, b[k,j,1], b[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = (b[j,1] - b[j,k]) * a[k,j]
		call imat_eprop ("-", b[j,1,1], b[j,1,2], b[j,k,1], b[j,k,2],
		    val, sig)
		call imat_eprop ("*", val, sig, a[k,j,1], a[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = -b[1,j] / a[1,j] / a[j,k] + b[k,j]
		call imat_eprop ("/", -b[1,j,1], b[1,j,2], a[1,j,1], a[1,j,2],
		    val, sig)
		call imat_eprop ("/", val, sig, a[j,k,1], a[j,k,2], val, sig)
		call imat_eprop ("+", val, sig, b[k,j,1], b[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = (-b[1,j] / a[1,j] - b[j,k]) * a[k,j]
		call imat_eprop ("/", -b[1,j,1], b[1,j,2], a[1,j,1], a[1,j,2],
		    val, sig)
		call imat_eprop ("-", val, sig, b[j,k,1], b[j,k,2], val, sig)
		call imat_eprop ("*", val, sig, a[k,j,1], a[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = -b[1,j] * a[j,1] * a[k,j] + b[k,j]
		call imat_eprop ("*", -b[1,j,1], b[1,j,2], a[j,1,1], a[j,1,2],
		    val, sig)
		call imat_eprop ("*", val, sig, a[k,j,1], a[k,j,2], val, sig)
		call imat_eprop ("+", val, sig, b[k,j,1], b[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = (-b[1,j] * a[j,1] - b[j,k]) / a[j,k]
		call imat_eprop ("*", -b[1,j,1], b[1,j,2], a[j,1,1], a[j,1,2],
		    val, sig)
		call imat_eprop ("-", val, sig, b[j,k,1], b[j,k,2], val, sig)
		call imat_eprop ("/", val, sig, a[j,k,1], a[j,k,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = -b[1,j] * a[j,1] / a[j,k] + b[k,j]
		call imat_eprop ("*", -b[1,j,1], b[1,j,2], a[j,1,1], a[j,1,2],
		    val, sig)
		call imat_eprop ("/", val, sig, a[j,k,1], a[j,k,2], val, sig)
		call imat_eprop ("+", val, sig, b[k,j,1], b[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		# b[k,1] = (-b[1,j] * a[j,1] - b[j,k]) * a[k,j]
		call imat_eprop ("*", -b[1,j,1], b[1,j,2], a[j,1,1], a[j,1,2],
		    val, sig)
		call imat_eprop ("-", val, sig, b[j,k,1], b[j,k,2], val, sig)
		call imat_eprop ("*", val, sig, a[k,j,1], a[k,j,2], val, sig)
		mean = mean + val
		stddev = stddev + val**2
		err = err + sig**2

		n = n + 12
	    }

	    # Factor of nimages is currently a fudge factor until
	    # I understand the systematics.
	    mean = mean / n
	    stddev = nimages * (stddev - n * mean * mean) / (n - 1)
	    err = nimages * err / n**2

	    Memd[zero+k-1] = mean
	    if (stddev < 0.)
		Memd[zstddev+k-1] = sqrt (err)
	    else
		Memd[zstddev+k-1] = sqrt (err + stddev)
	}

	call imat_fix (Memd[scale], Memd[zero], Memd[sstddev], Memd[zstddev],
	    a, b, nimages)

	call sfree (sp)
end


# IMAT_EPROP -- Error propagation.

procedure imat_eprop (op, x, sx, y, sy, z, sz)

char	op
double	x, sx
double	y, sy
double	z, sz

double	ymin, val, sig

begin
	switch (op) {
	case '+':
	    val = x + y
	    sig = sqrt (sx**2 + sy**2)
	case '-':
	    val = x - y
	    sig = sqrt (sx**2 + sy**2)
	case '*':
	    val = x * y
	    sig = sqrt ((y * sx) ** 2 + (x * sy) ** 2)
	case '/':
	    ymin = max (1D-3, y)
	    val = x / ymin
	    sig = sqrt ((sx ** 2 + (x / ymin * sy) ** 2) / ymin ** 2)
	}
	z = val
	sz = sig
end


# IMAT_FIX -- Given a single set of scale factors relative to one image
# fix the scale factors for all pairs of images.

procedure imat_fix (scale, zero, sstddev, zstddev, a, b, nimages)

double	scale[nimages]		#I Scale factors
double	zero[nimages]		#I Zero factors
double	sstddev[nimages]	#I Standard deviation of scale factors
double	zstddev[nimages]	#I Standard deviation of zero factors
double	a[nimages,nimages,2]	#O Scale factors
double	b[nimages,nimages,2]	#O Zero factors
int	nimages			#I Number of images

int	j, k

begin
	do k = 1, nimages {
	    do j = 1, nimages {
		if (j == k)
		    next
		a[k,j,1] = scale[k] / scale[j]
		call imat_eprop ("/", scale[k], sstddev[k], scale[j],
		    sstddev[j], a[k,j,1], a[k,j,2])

		# b[k,j] = zero[k] - a[k,j] * zero[j]
		call imat_eprop ("*", zero[j], zstddev[j], a[k,j,1],
		    a[k,j,2], b[k,j,1], b[k,j,2])
		call imat_eprop ("-", zero[k], zstddev[k], b[k,j,1],
		    b[k,j,2], b[k,j,1], b[k,j,2])
	    }
	}
end


# IMAT_REJ -- Use the residuals from all pairs of images taken together
# to sigma clip.

procedure imat_rej (ic, data, wts, a, b, nimages, nreg, sigma, nrej)

pointer	ic				#I IC pointer
double	data[nreg,nimages]		#I Photon counts
double	wts[nimages,nimages,nreg]	#U Weights for pairs of measurments
double	a[nimages,nimages,2]		#I Scale factors
double	b[nimages,nimages,2]		#I Zero factors
int	nimages				#I Number of images
int	nreg				#I Number of measurements per image
double	sigma				#I Sigma clipping factor
int	nrej				#O Number of data points rejected

int	i, j, k, nrms
double	rms, resid

begin
    	call imat_rms (data, wts, a, b, nimages, nreg, rms, nrms)
	if (nrms == 0)
	    return

	rms = sigma * rms
	do k = 1, nimages {
	    do j = 1, nimages {
		if (j == k)
		    next
		do i = 1, nreg {
		    if (IS_INDEFD(data[i,j]) || IS_INDEFD(data[i,k]))
			next
		    if (wts[j,k,i] <= 0D0)
			next
		    if (data[i,k] <= 0D0)
			next
		    resid = (data[i,k] - a[k,j,1] * data[i,j] - b[k,j,1]) /
			sqrt (data[i,k])
		    if (abs (resid) > rms) {
			wts[j,k,i] = 0.
			nrej = nrej + 1
		    }
		}
	    }
	}

	call imat_ic2 (ic, data, wts, a, b, nimages, nreg)
end


# IMAT_RMS -- Compute the RMS over all pairs of images excluding previously
# rejected pairs of measurements.  The residuals are given by
#
#    residual = (data[i,k] - (a[k,j] * data[i,j] - b[k,j])) / sqrt (data[i,k])

procedure imat_rms (data, wts, a, b, nimages, nreg, rms, nrms)

double	data[nreg,nimages]		#I Photon counts
double	wts[nimages,nimages,nreg]	#I Weights for pairs of measurments
double	a[nimages,nimages,2]		#I Scale factors
double	b[nimages,nimages,2]		#I Zero factors
int	nimages				#I Number of images
int	nreg				#I Number of measurements per image
double	rms				#O RMS
int	nrms				#O Number of values in RMS

int	i, j, k
double	resid

begin
	rms = 0.
	nrms = 0
	do k = 1, nimages {
	    do j = 1, nimages {
		if (j == k)
		    next
		do i = 1, nreg {
		    if (IS_INDEFD(data[i,j]) || IS_INDEFD(data[i,k]))
			next
		    if (wts[j,k,i] <= 0D0)
			next
		    if (data[i,k] <= 0D0)
			next
		    resid = (data[i,k] - a[k,j,1] * data[i,j] - b[k,j,1]) /
			sqrt (data[i,k])
		    rms = rms + resid ** 2
		    nrms = nrms + 1
		}
	    }
	}
	if (nrms > 0)
	    rms = sqrt (rms / nrms)

	#call printf ("nrms = %d, rms = %g\n")
	#    call pargi (nrms)
	#    call pargd (rms)
end


# IMAT_ICINIT -- Initialize ICG_FIT.

procedure imat_icinit (ic, input)

pointer	ic		#O ICG_FIT pointer
int	input		#I List of input images

int	inlist
pointer	gp, gt
common	/imat_ic/ gp, gt, inlist

pointer	gopen(), gt_init()

begin
	gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	gt = gt_init ()
	call ic_open (ic)
	call ic_pstr (ic, "function", "chebyshev")
	call ic_puti (ic, "order", 2)
	call ic_puti (ic, "key", 1)
	inlist = input
end


# IMAT_ICFREE -- Free ICG_FIT.

procedure imat_icfree (ic)

pointer	ic		#O ICG_FIT pointer

int	inlist
pointer	gp, gt
common	/imat_ic/ gp, gt, inlist

begin
	if (ic == NULL)
	    return

	call ic_closed (ic)
	call gt_free (gt)
	call gclose (gp)
end


# IMAT_ICLABELS -- Set labels for ICG_FIT.

procedure imat_iclabels (ic, i, j)

pointer	ic		#I ICG_FIT pointer
int	i, j		#I Image indices

int	k, imtrgetim()
pointer	sp, label, imname1, imname2

int	inlist
pointer	gp, gt
common	/imat_ic/ gp, gt, inlist

begin
	if (ic == NULL)
	    return

	call smark (sp)
	call salloc (label, SZ_LINE, TY_CHAR)
	call salloc (imname1, SZ_FNAME, TY_CHAR)
	call salloc (imname2, SZ_FNAME, TY_CHAR)

	k = imtrgetim (inlist, i, Memc[imname1], SZ_LINE)
	k = imtrgetim (inlist, j, Memc[imname2], SZ_LINE)

	call sprintf (Memc[label], SZ_LINE, "Counts of %s vs %s")
	    call pargstr (Memc[imname2])
	    call pargstr (Memc[imname1])
	call gt_sets (gt, GTTITLE, Memc[label])
	call ic_pstr (ic, "xlabel", Memc[imname1])
	call ic_pstr (ic, "ylabel", Memc[imname2])
end

	
# IMAT_IC1 -- Routine to call ICG_FIT with a set of values.
# This can be called for a particular pair of images or for residuals from
# all pairs of images.

procedure imat_ic1 (ic, x, y, w, n)

pointer	ic		#I ICG_FIT pointer
double	x[n]		#I X values
double	y[n]		#I Y values
double	w[n]		#I Weight values
int	n		#I Number of points

int	i, j
pointer	sp, index, x1, y1, w1, cv

int	inlist
pointer	gp, gt
common	/imat_ic/ gp, gt, inlist

int	imat_comp()
extern	imat_comp

begin
	if (ic == NULL)
	    return

	call smark (sp)
	call salloc (index, n, TY_INT)
	call salloc (x1, n, TY_DOUBLE)
	call salloc (y1, n, TY_DOUBLE)
	call salloc (w1, n, TY_DOUBLE)

	do i = 1, n {
	    Memi[index+i-1] = i
	    Memd[x1+i-1] = x[i]
	}
	call gqsort (Memi[index], n, imat_comp, x1)
	do i = 0, n-1 {
	    j = Memi[index+i]
	    Memd[x1+i] = x[j]
	    Memd[y1+i] = y[j]
	    Memd[w1+i] = w[j]
	}
	call icg_fitd (ic, gp, "gcur", gt, cv, Memd[x1], Memd[y1], Memd[w1], n)

	do i = 0, n-1 {
	    j = Memi[index+i]
	    w[j] = Memd[w1+i]
	}

	call dcvfree (cv)
	call sfree (sp)
end


# IMAT_IC2 -- Routine to call ICG_FIT with residuals over all pairs of images.

procedure imat_ic2 (ic, data, wts, a, b, nimages, nreg)

pointer	ic				#I ICG_FIT pointer
double	data[nreg,nimages]		#I Photon counts
double	wts[nimages,nimages,nreg]	#I Weights for pairs of measurments
double	a[nimages,nimages,2]		#I Scale factors
double	b[nimages,nimages,2]		#I Zero factors
int	nimages				#I Number of images
int	nreg				#I Number of measurements per image

int	i, j, k, n
pointer	sp, x, y, w

begin
	if (ic == NULL)
	    return

	call smark (sp)
	call salloc (x, nimages*nimages*nreg, TY_DOUBLE)
	call salloc (y, nimages*nimages*nreg, TY_DOUBLE)
	call salloc (w, nimages*nimages*nreg, TY_DOUBLE)

	n = 0
	do k = 1, nimages {
	    do j = 1, nimages {
		if (j == k)
		    next
		do i = 1, nreg {
		    if (IS_INDEFD(data[i,j]) || IS_INDEFD(data[i,k]))
			next
		    if (data[i,k] <= 0D0)
			next
		    Memd[x+n] = data[i,j]
		    Memd[y+n] = (data[i,k] - a[k,j,1] * data[i,j] - b[k,j,1]) /
			sqrt (data[i,k])
		    Memd[w+n] = wts[j,k,i]
		    n = n + 1
		}
	    }
	}

	call imat_ic1 (ic, Memd[x], Memd[y], Memd[w], n)

	call sfree (sp)
end
