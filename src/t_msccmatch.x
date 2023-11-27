include	<error.h>
include	<fset.h>
include	<imhdr.h>
include	<imset.h>
include	<math.h>

define	SZ_CMD	4*SZ_LINE
define	CT_LW	1	# Logical to world
define	CT_WL	2	# World to logical
define	CT_WA	3	# World to astrometry

procedure t_msccmatch ()

int	input		# List of input Mosaic images
pointer	coords		# Input coordinate file or command
pointer outcoords	# Output coordinate files
bool	usebpm		# Use BPM?
int	nsearch		# Maximum number of coordinates for search
double	search		# Maximum search radius (may be zero)
double	rsearch		# Maximum rotation search (degrees)
int	nfit		# Minimum number of coordinates for fit
double	rms		# Maximum rms to accept
double	maxshift	# Maximum shift (arc sec)
pointer	fitgeom		# Fit geometry
int	interactive	# Interactive?
int	ifit		# Interactive fitting?
int	update		# Update?
int	verbose		# Verbose?
int	listcoords	# List coordinates?
int	accept		# Accept solution?
int	cbox		# Centering box (pixels)
double	csig		# Maximum centering uncertainty to accept (pixels)
double	cfrac		# Minimum fraction of accepted centers

int	i, n, nm, outlist, extlist, nims, stat, srch
double	scale, xshift, yshift, theta, xshft, yshft, t,  results[8]
real	reject
pointer	sp, sp1, mef, image, bpmname, keyval
pointer	xptr, yptr, mptr, mw, ct
pointer	im, wcs, crpix, bpm, imin, wcsin, xref, yref, mref, xin, yin, sort, tmp

bool	clgetb()
real	clgetr()
int	clgeti(), btoi (), cm_compared()
int	imtopenp(), imtlen(), imtgetim(), imtrgetim(), xt_extns()
int	clpopnu(), clgfil()
long	clktime()
double	clgetd(), msc_wcsstatd()
pointer	immap(), yt_pmmap(),  msc_openim(), msc_sctran(), wcs_trans()
errchk	open, immap, yt_pmmap, msc_openim
errchk	cm_pixel, cm_shift, cm_center, cm_geomap, wcs_adjust, cm_updcoords
errchk	cm_getcoords
extern	cm_compared()

begin
	call smark (sp)
	call salloc (mef, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (bpmname, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_LINE, TY_CHAR)
	call salloc (outcoords, SZ_FNAME, TY_CHAR)
	call salloc (fitgeom, SZ_FNAME, TY_CHAR)
	call salloc (keyval, SZ_FNAME, TY_CHAR)

	# Get task parameters.
	input = imtopenp ("input")
	call clgstr ("coords", Memc[coords], SZ_LINE)
	outlist = clpopnu ("outcoords")
	usebpm = clgetb ("usebpm")
	nsearch = clgeti ("nsearch")
	search = clgetd ("search")
	rsearch = clgetd ("rsearch")
	maxshift = clgetd ("maxshift")
	nfit = clgeti ("nfit")
	rms = clgetd ("rms")
	call clgstr ("fitgeometry", Memc[fitgeom], SZ_FNAME)
	interactive = btoi (clgetb ("interactive"))
	ifit = btoi (clgetb ("fit"))
	update = btoi (clgetb ("update"))
	verbose = btoi (clgetb ("verbose"))
	listcoords = btoi (clgetb ("listcoords"))
	reject = clgetr ("reject")
	accept = update
	cbox = clgeti ("cbox")
	csig = clgetd ("csig")
	cfrac = clgetd ("cfrac")

	if (verbose == YES) {
	    call fseti (STDOUT, F_FLUSHNL, YES)
	    call printf ("MSCCMATCH:\n")
	}

	# Initialize shifts.  If using a dither placing this outside the
	# image loop will track the shifts.
	xshift = 0.
	yshift = 0.
	theta = 0.

	# Loop on the input MEF files.
	while (imtgetim (input, Memc[mef], SZ_FNAME) != EOF) {
	    if (clgfil (outlist, Memc[outcoords], SZ_FNAME) == EOF)
		Memc[outcoords] = EOS

	    if (verbose == YES) {
		call printf ("  %s:\n")
		    call pargstr (Memc[mef])
	    }

	    iferr {
		bpm = NULL; wcs = NULL; im = NULL
		call smark (sp1)

		extlist = xt_extns (Memc[mef], "IMAGE", "0-", "", "",
		    YES, NO, NO, NO, "", NO, i)
		nims = imtlen (extlist)
		if (nims == 0)
		    call error (1, "No images found")

		# Read coordinates.
		call cm_getcoords (Memc[mef], Memc[coords], xptr, yptr,
		    n, mptr, nm)

		if (n == 0)
		    call error (1, "No input coordinates found")

		if (nfit <= 0)
		    nfit = max (1, n + nfit)
		if (n < nfit)
		    call error (1,
			"Too few input coordinates for minimum number to fit")

		call salloc (imin, n, TY_POINTER)
		call salloc (wcsin, n, TY_POINTER)
		call salloc (xref, n, TY_DOUBLE)
		call salloc (yref, n, TY_DOUBLE)
		call salloc (mref, n, TY_DOUBLE)
		call salloc (xin, n, TY_DOUBLE)
		call salloc (yin, n, TY_DOUBLE)
		call salloc (sort, n, TY_INT)

		call amovd (Memd[xptr], Memd[xref], n)
		call amovd (Memd[yptr], Memd[yref], n)
		call amovd (Memd[mptr], Memd[mref], n)
		call mfree (xptr, TY_DOUBLE)
		call mfree (yptr, TY_DOUBLE)
		call mfree (mptr, TY_DOUBLE)

		do i = 0, n-1
		    Memi[sort+i] = i

		call salloc (im, nims, TY_POINTER)
		call salloc (wcs, nims, TY_POINTER)
		call salloc (crpix, 2*nims, TY_DOUBLE)
		call salloc (bpm, nims, TY_POINTER)

		call amovki (NULL, Memi[im], nims)
		call amovki (NULL, Memi[wcs], nims)
		call amovki (NULL, Memi[bpm], nims)

		# Open data structures and set average scale.
		scale = 0.
		do i = 1, nims {
		    stat = imtrgetim (extlist, i, Memc[image], SZ_FNAME)
		    tmp = immap (Memc[image], READ_ONLY, 0)
		    Memi[im+i-1] = tmp
		    mw = msc_openim (Memi[im+i-1], tmp)
		    Memi[wcs+i-1] = tmp
		    ct = msc_sctran (Memi[wcs+i-1], CT_LW, "logical",
			"world", 3)
		    ct = msc_sctran (Memi[wcs+i-1], CT_WL, "world",
			"logical", 3)
		    ct = msc_sctran (Memi[wcs+i-1], CT_WA, "world",
			 "astrometry", 3)
		    scale = scale + msc_wcsstatd (Memi[wcs+i-1], "scale")
		    Memd[crpix+2*i-2] = msc_wcsstatd (Memi[wcs+i-1], "crpix1")
		    Memd[crpix+2*i-1] = msc_wcsstatd (Memi[wcs+i-1], "crpix2")
		    if (usebpm) {
			tmp = yt_pmmap ("BPM", Memi[im+i-1],
			    Memc[bpmname], SZ_FNAME)
			Memi[bpm+i-1] = tmp
		    }
		}
		scale = scale / nims

		# Search for approximate shift.
		if (nsearch > 0 && search > 0.) {
		    # Sort by mref if possible.
		    if (nm > 0)
			call gqsort (Memi[sort], n, cm_compared, mref)

		    srch = nint (search / scale)

		    call cm_pixel (Memi[im], Memi[wcs], Memd[crpix], Memi[bpm],
			nims, Memd[xref], Memd[yref], xshift, yshift,
			theta, srch, 0.5, 2, Memi[imin], Memi[wcsin], Memd[xin],
			Memd[yin], n, verbose)

		    iferr (call cm_shift (Memi[im], Memi[wcs], Memd[crpix],
			nims, Memi[imin], Memd[xin], Memd[yin], Memi[sort],
			n, nsearch, srch, rsearch, xshft, yshft, t,
			"", verbose)) {
			srch = 2 * srch
		        call cm_shift (Memi[im], Memi[wcs], Memd[crpix], nims,
			    Memi[imin], Memd[xin], Memd[yin], Memi[sort],
			    n, nsearch, srch, rsearch, xshft, yshft, t,
			    "", verbose)
		    }
		    xshift = xshift + xshft
		    yshift = yshift + yshft
		    theta = theta + t

		    while (abs (xshft) > 0.8 * srch ||
			abs (yshft) > 0.8 * srch) { 
			call cm_pixel (Memi[im], Memi[wcs], Memd[crpix],
			    Memi[bpm], nims, Memd[xref], Memd[yref],
			    xshift, yshift, theta, srch, 0.5, 2, Memi[imin],
			    Memi[wcsin], Memd[xin], Memd[yin], n,
			    verbose)

			call cm_shift (Memi[im], Memi[wcs], Memd[crpix], nims,
			    Memi[imin], Memd[xin], Memd[yin], Memi[sort],
			    n, nsearch, srch, rsearch, xshft, yshft, t,
			    "", verbose)
			xshift = xshift + xshft
			yshift = yshift + yshft
			theta = theta + t
		    }
		}

		# Convert to pixel coordinates.
		call cm_pixel (Memi[im], Memi[wcs], Memd[crpix], Memi[bpm],
		    nims, Memd[xref], Memd[yref], xshift, yshift, theta,
		    nint(maxshift/scale), 0.0, 0, Memi[imin], Memi[wcsin],
		    Memd[xin], Memd[yin], n, verbose)

		# Sort by yin for I/O efficiency.
		call gqsort (Memi[sort], n, cm_compared, yin)

		# Center on coordinates.
		call cm_center (Memi[im], Memi[wcs], nims, Memi[imin],
		    Memd[xin], Memd[yin], Memi[sort], n, nfit, cbox,
		    maxshift, csig, cfrac, verbose, listcoords)

		# Compute the WCS adjustment.
		call  cm_geomap (Memi[imin], Memi[wcsin], Memd[xref],
		    Memd[yref], Memd[xin], Memd[yin], n, nfit, rms,
		    Memc[fitgeom], interactive, ifit, results, verbose, reject)

		# Close data structures before update.
		do i = 1, nims {
		    if (Memi[bpm+i-1] != NULL) {
		       call imunmap (Memi[bpm+i-1])
		       Memi[bpm+i-1] = NULL
		    }
		    if (Memi[wcs+i-1] != NULL) {
			call msc_close (Memi[wcs+i-1])
			Memi[wcs+i-1] = NULL
		    }
		    if (Memi[im+i-1] != NULL) {
			call imunmap (Memi[im+i-1])
			Memi[im+i-1] = NULL
		    }
		}

		# Update WCS if desired.
		if (update == YES && interactive == YES)
		    accept = btoi (clgetb ("accept"))
		if (accept == YES) {
		    call cnvdate (clktime(0), Memc[image], SZ_FNAME)
		    call sprintf (Memc[keyval], SZ_FNAME,
			"%s %.2f/%.2f %.3f/%.3f %.3f/%.3f")
			call pargstr (Memc[image])
			call pargd (results[1])
			call pargd (results[2])
			call pargd (results[3])
			call pargd (results[4])
			call pargd (results[5])
			call pargd (results[6])
		    mw = wcs_trans (results[1], results[2], results[3],
			results[4], results[5], results[6])
		    do i = 1, nims {
			stat = imtrgetim (extlist, i, Memc[image], SZ_FNAME)
			call wcs_adjust (Memc[image], mw, NO, 200, 4, 4, 4, 4)
			tmp = immap (Memc[image], READ_WRITE, 0)
			call imastr (tmp, "MSCCMATCH", Memc[keyval])
			call imunmap (tmp)
		    }
		    call mw_close (mw)
		    if (verbose == YES)
			call printf ("    Coordinate system updated.\n")

		    if (Memc[outcoords] != EOS) {
			call cm_updcoords (Memc[coords], Memc[outcoords],
			    Memd[xin])
			if (verbose == YES)
			    call printf ("    Coordinate list updated.\n")
		    }
		}
	    } then {
		call erract (EA_WARN)
		call eprintf ("ERROR: MSCCMATCH failed for %s\n")
		    call pargstr (Memc[mef])
	    }

	    # Close data structures in case of an error.
	    do i = 1, nims {
		if (bpm != NULL) {
		    if (Memi[bpm+i-1] != NULL)
		       call imunmap (Memi[bpm+i-1])
		}
		if (wcs != NULL) {
		    if (Memi[wcs+i-1] != NULL)
			call msc_close (Memi[wcs+i-1])
		}
		if (im != NULL) {
		    if (Memi[im+i-1] != NULL)
			call imunmap (Memi[im+i-1])
		}
	    }

	    call imtclose (extlist)
	    call sfree(sp1)
	}
	    
	call clpcls (outlist)
	call imtclose (input)
	call sfree (sp)
end


# CM_PIXEL -- Convert to pixel coordinates.

procedure cm_pixel (im, wcs, crpix, bpm, nims, xref, yref, xshift, yshift,
	theta, bpmbox, bpmfrac, bpmflag, imin, wcsin, xin, yin, n, verbose)

pointer	im[nims]	#I IMIO pointers
pointer	wcs[nims]	#I WCS pointers
double	crpix[2,nims]	#I Tangent point in pixels
pointer	bpm[nims]	#I BPM pointers
int	nims		#I Number of images
double	xref[n]		#I X reference coordinate
double	yref[n]		#I Y reference coordinate
double	xshift		#I X shift in pixels
double	yshift		#I Y shift in pixels
double	theta		#I rotation in radians
int	bpmbox		#I BPM check box
real	bpmfrac		#I Maximum fraction to be rejected by mask
int	bpmflag		#I BPM flag value (pos to select neg to exclude)
pointer	imin[n]		#O IMIO pointer assigned to each coordinate
pointer	wcsin[n]	#O WCS pointer assigned to each coordinate
double	xin[n]		#O X pixel coordinate for reference coordinate
double	yin[n]		#O Y pixel coordinate for reference coordinate
int	n		#I Number of coordinates
int	verbose		#I Verbose?

bool	sat
int	i, j, bpmbx, nofb, nbpm, c1, c2, l1, l2, c, l
double	x, y, sint, cost
pointer	buf, imgs2s()

begin
	if (verbose == YES) {
	    call printf ("    %d input coordinates\n")
		call pargi (n)
	}

	sint = sin (theta)
	cost = cos (theta)
	bpmbx = bpmbox

	repeat {
	    nofb = 0
	    nbpm = 0
	    do j = 1, n {
		imin[j] = NULL
		wcsin[j] = NULL
		xin[j] = INDEFD
		yin[j] = INDEFD
		do i = 1, nims {
		    call msc_c2trand (wcs[i], CT_WL, xref[j], yref[j], x, y)
		    x = (x-crpix[1,i])*cost - (y-crpix[2,i])*sint + crpix[1,i]
		    y = (x-crpix[1,i])*sint + (y-crpix[2,i])*cost + crpix[2,i]
		    x = x + xshift
		    y = y + yshift
		    if (x>=1 && x<=IM_LEN(im[i],1) &&
			y>=1 && y<=IM_LEN(im[i],2))
			break
		}
		if (i > nims) {
		    nofb = nofb + 1
		    next
		}
		if (!IS_INDEFI(bpmbx) && bpm[i] != NULL) {
		    c1 = max (1, nint (x) - bpmbx)
		    c2 = min (IM_LEN(im[i],1), nint (x) + bpmbx)
		    l1 = max (1, nint (y) - bpmbx)
		    l2 = min (IM_LEN(im[i],2), nint (y) + bpmbx)
		    buf = imgs2s (bpm[i], c1, c2, l1, l2)
		    sat = false
		    do l = l1, l2 {
			do c = c1, c2 {
			   if (Mems[buf] != 0) {
			       if (bpmflag == 0 || Mems[buf] == bpmflag) {
				   sat = true
				   break
				}
			    }
			    buf = buf + 1
			}
			if (sat)
			    break
		    }
		    if (sat) {
			nbpm = nbpm + 1
			next
		    }
		}
		imin[j] = im[i]
		wcsin[j] = wcs[i]
		xin[j] = x
		yin[j] = y
	    }

	    bpmbx = INDEFI
	} until (n-nofb-nbpm >= bpmfrac*(n-nofb))

	if (verbose == YES) {
	    if (nofb > 0) {
		call printf ("    %d/%d coordinates out of bounds\n")
		    call pargi (nofb)
		    call pargi (n)
	    }
	    if (nbpm > 0) {
		call printf ("    %d/%d coordinates masked\n")
		    call pargi (nbpm)
		    call pargi (n-nofb)
	    }
	}

	if (n - nofb - nbpm == 0)
	    call error (1, "No coordinates")
end


define	CM_SHIFT_SAMP	50	# Number of sample points per axis for sorting
define	CM_SHIFT_THRESH	0.9	# Threshold for voting

# CM_SHIFT -- Find shift.

procedure cm_shift (im, wcs, crpix, nims, imin, xin, yin, sort, ncoords,
	nsearch, search, rsearch, xshift, yshift, theta, vote, verbose)

pointer	im[nims]	#I IMIO pointers
pointer	wcs[nims]	#I WCS pointers
double	crpix[2,nims]	#I Tangent point in pixels
int	nims		#I Number of images
pointer	imin[ncoords]	#I IMIO pointer assigned to each coordinate
double	xin[ncoords]	#I X target coordinate for reference coordinate
double	yin[ncoords]	#I Y target coordinate for reference coordinate
int	sort[ncoords]	#I Sort index
int	ncoords		#I Number of coordinates
int	nsearch		#I Number of brightest objects to use
int	search		#I Search radius (pixels)
double	rsearch		#I Rotation search radius (degrees)
double	xshift		#O X shift in pixels
double	yshift		#O Y shift in pixels
double	theta		#O Rotation in radians
char	vote[ARB]	#O Vote array output name
int	verbose		#I Verbose?

int	i, j, k, c, l
int	nc, nl, nt, npix, nsamp, nwork, c1, c2, l1, l2, nfound
real	maxval, val, sum
real	r, rmax, dsint, sint, cost, c0, l0, ck0, lk0, ck, lk
pointer	sp, votes, work, buf, ptr1, ptr2, imv

real	asokr()
pointer	immap(), imgs2r(), imps2r(), imps3r()

errchk	immap

begin
	if (verbose == YES) {
	    call printf ("    search using up to %d objects:\n")
		call pargi (nsearch)
	}

	# Determine maximum radius from tangent point.
	nfound = 0
	rmax = 0
	for (i=1; i<=ncoords && nfound<nsearch; i=i+1) {
	    j = sort[i] + 1
	    if (IS_INDEFD(xin[j]))
		next
	    do k = 1, nims
		if (im[k] == imin[j])
		    break
	    c1 = nint (xin[j]) - search
	    c2 = nint (xin[j]) + search
	    l1 = nint (yin[j]) - search
	    l2 = nint (yin[j]) + search
	    if (c1 < 1 || c2 > IM_LEN(im[k],1) ||
		l1 < 1 || l2 > IM_LEN(im[k],2))
		next
	    nfound = nfound + 1

	    r = sqrt ((xin[j]-crpix[1,k])**2 + (yin[j]-crpix[2,k])**2)
	    rmax = max (r, rmax)
	}

	# Set memory.
	dsint = max (10, search/10) / rmax
	nc = 2 * search + 1
	nl = 2 * search + 1
	nt = 2 * nint (min (real(0.8*search/rmax), real(DEGTORAD(rsearch))) /
	    dsint) + 1
	npix = nc * nl
	nsamp = max (1, nc / CM_SHIFT_SAMP)
	nwork = (nc / nsamp + 1) * (nl / nsamp + 1)

	call smark (sp)
	call salloc (votes, npix * nt, TY_REAL)
	call salloc (work, nwork, TY_REAL)

	# Accumulate data.
	call aclrr (Memr[votes], npix * nt)
	nfound = 0
	for (i=1; i<=ncoords && nfound<nsearch; i=i+1) {
	    j = sort[i] + 1
	    if (IS_INDEFD(xin[j]))
		next
	    do k = 1, nims
		if (im[k] == imin[j])
		    break
	    c1 = nint (xin[j]) - search
	    c2 = nint (xin[j]) + search
	    l1 = nint (yin[j]) - search
	    l2 = nint (yin[j]) + search
	    if (c1 < 1 || c2 > IM_LEN(im[k],1) ||
		l1 < 1 || l2 > IM_LEN(im[k],2))
		next
	    nfound = nfound + 1

	    # Get data.
	    buf = imgs2r (im[k], c1, c2, l1, l2)
	    c0 = crpix[1,k]
	    l0 = crpix[2,k]

	    # Subsample for finding threshold.
	    ptr2 = work
	    do l = l1, l2, nsamp {
		ptr1 = buf + (l - l1) * nc
		do c = c1, c2, nsamp {
		    Memr[ptr2] = Memr[ptr1]
		    ptr1 = ptr1 + 1
		    ptr2 = ptr2 + 1
		}
	    }
	    maxval = asokr (Memr[work], nwork, nint (CM_SHIFT_THRESH * nwork))

	    # Accumulate.  Optimize the calculation and accumulation.
	    ptr1 = votes - 1
	    do k = 1, nt {
		if (k == nt / 2 + 1) {
		    ptr2 = buf - 1
		    do l = l1, l2 {
			do c = c1, c2 {
			    ptr1 = ptr1 + 1
			    ptr2 = ptr2 + 1
			    val = Memr[ptr2]
			    if (val > maxval)
				Memr[ptr1] = Memr[ptr1] + 1
			}
		    }
		} else {
		    sint = (k - 1 - (nt - 1) / 2.) * dsint 
		    cost = sqrt (1. - sint * sint)
		    ck0 = (c1-c0) * cost - (l1-l0) * sint + c0
		    lk0 = (c1-c0) * sint + (l1-l0) * cost + l0
		    ck0 = ck0 - cost + sint
		    lk0 = lk0 - sint - cost
		    do l = l1, l2 {
			ck0 = ck0 - sint
			lk0 = lk0 + cost
			ck = ck0
			lk = lk0
			do c = c1, c2 {
			    ck = ck + cost
			    lk = lk + sint
			    ptr1 = ptr1 + 1
			    if (ck < c1 || ck > c2 || lk < l1 || lk > l2)
				next
			    ptr2 = buf + nint (lk - l1) * nc + nint (ck - c1)
			    val = Memr[ptr2]
			    if (val > maxval)
				Memr[ptr1] = Memr[ptr1] + 1
			}
		    }
		}
	    }
	}

	# Output vote array if desired.
	if (vote[1] != EOS) {
	    imv = immap (vote, NEW_IMAGE, 0)
	    IM_NDIM(imv) = 3
	    IM_LEN(imv,1) = nc
	    IM_LEN(imv,2) = nl
	    IM_LEN(imv,3) = nt
	    IM_PIXTYPE(imv) = TY_REAL
	    if (nt > 1)
		call amovr (Memr[votes], Memr[imps3r(imv,1,nc,1,nl,1,nt)],
		    npix*nt)
	    else {
		IM_NDIM(imv) = 2
		call amovr (Memr[votes], Memr[imps2r(imv,1,nc,1,nl)], npix)
	    }
	    call imunmap (imv)
	}

	# Find centroids above half the votes.
	maxval = nfound / 2
	ptr1 = votes
	xshift = 0
	yshift = 0
	theta = 0
	sum = 0
	do k = 1, nt {
	    sint = (k - 1 - (nt - 1) / 2.) * dsint 
	    do l = 1, nl {
		do c = 1, nc {
		    val = Memr[ptr1] - maxval
		    ptr1 = ptr1 + 1
		    if (val <= 0.)
			next
		    xshift = xshift + c * val
		    yshift = yshift + l * val
		    theta = theta + sint * val
		    sum = sum + val
		}
	    }
	}
	if (sum == 0)
	   call error (1, "Automatic search failed")

	xshift = xshift / sum - search
	yshift = yshift / sum - search
	theta = asin (theta / sum)

	if (verbose == YES) {
	    call printf ("    search found offsets of (%.0f, %.0f) pixels")
		call pargd (xshift)
		call pargd (yshift)
	    call printf (" and rotation %.2f degrees\n")
		call pargd (RADTODEG (theta))
	}

	call sfree (sp)
end


# CM_CENTER -- Find and center on objects near reference coordinates.

procedure cm_center (im, wcs, nims, imin, xin, yin, sort, n, nfit, cbox,
	maxshift, csig, cfrac, verbose, listcoords)

pointer	im[nims]	#I IMIO pointers
pointer	wcs[nims]	#I WCS pointers
int	nims		#I Number of images
pointer	imin[n]		#I IMIO pointer assigned to each coordinate
double	xin[n]		#I X target coordinate for reference coordinate
double	yin[n]		#I Y target coordinate for reference coordinate
int	sort[n]		#I Sort index
int	n		#I Number of coordinates
int	nfit		#I Minimum number to centroid
int	cbox		#I Centering box (pixels)
double	maxshift	#I Maximum shift (arcsec)
double	csig		#I Maximum centering uncertainty (arcsec)
double	cfrac		#I Minimum fraction of accepted centers
int	verbose		#I Verbose?
int	listcoords	#I List coordinates?

int	i, j, k, ncenter, nfail, fd, stat
double	x, y, scale, xshift, yshift, xerr, yerr
pointer	sp, image, cmd, temp1

int	open(), fscan(), nscan()
double	msc_wcsstatd()
errchk	open

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_CMD, TY_CHAR)
	call salloc (temp1, SZ_FNAME, TY_CHAR)

	call mktemp ("tmp$iraf", Memc[temp1], SZ_FNAME)

	# Center coordinates for each image in turn.
	ncenter = 0
	nfail = 0
	do i = 1, nims {
	    fd = NULL
	    do k = 1, n {
		j = sort[k] + 1
		if (imin[j] != im[i])
		    next
		if (fd == NULL)
		    fd = open (Memc[temp1], NEW_FILE, TEXT_FILE)
		call fprintf (fd, "%g %g\n")
		    call pargd (xin[j])
		    call pargd (yin[j])
		ncenter = ncenter + 1
	    }
	    if (fd == NULL)
		next
	    call close (fd)

	    # Set scale to convert maxshift to pixels.
	    call imstats (im[i], IM_IMAGENAME, Memc[image], SZ_FNAME)
	    scale = msc_wcsstatd (wcs[i], "scale")

	    # Execute centering command.
	    call sprintf (Memc[cmd], SZ_CMD,
		"msccntr %s %s cbox=%d maxshift=%g")
		call pargstr (Memc[image])
		call pargstr (Memc[temp1])
		call pargi (cbox)
		call pargd (maxshift/scale)
	    call clcmdw (Memc[cmd])

	    # Update with the centered coordinates.  Print results if needed.
	    fd = open (Memc[temp1], READ_ONLY, TEXT_FILE)
	    do k = 1, n {
		j = sort[k] + 1
		if (imin[j] != im[i])
		    next
		stat = fscan (fd)
		call gargd (x)
		call gargd (y)
		call gargd (xshift)
		call gargd (yshift)
		call gargd (xerr)
		call gargd (yerr)
		call gargi (stat)
		call gargwrd (Memc[cmd], SZ_CMD)
		if (nscan() < 8) {
		    imin[j] = NULL
		    next
		}
		if (verbose == YES && listcoords == YES) {
		    call printf (
			"      %s %8.2f %8.2f %6.2f %6.2f %4.2f %4.2f")
			call pargstr (Memc[image])
			call pargd (x)
			call pargd (y)
			call pargd (xshift)
			call pargd (yshift)
			call pargd (xerr)
			call pargd (yerr)
		    if (stat != 0) {
			call printf (" **%s**")
			    call pargstr (Memc[cmd])
		    }
		    call printf ("\n")
		}
		if (stat != 0 || IS_INDEFD(xerr) || IS_INDEFD(yerr)) {
		    imin[j] = NULL
		    xin[j] = INDEFD
		    yin[j] = INDEFD
		    nfail = nfail + 1
		} else if (sqrt (xerr**2 + yerr**2) > csig / scale) {
		    imin[j] = NULL
		    xin[j] = INDEFD
		    yin[j] = INDEFD
		    nfail = nfail + 1
		} else {
		    xin[j] = x
		    yin[j] = y
		}
	    }
	    call close (fd)

	    call delete (Memc[temp1])
	}

	if (verbose == YES) {
	    call printf ("    %d/%d not centroided\n")
		call pargi (nfail)
		call pargi (ncenter)
	}

	if (nfail > cfrac * ncenter) {
	    call sprintf (Memc[cmd], SZ_CMD,
		"Too many coordinates failed to centroid: %d/%d < %.2f")
		    call pargi (nfail)
		    call pargi (ncenter)
		    call pargd (cfrac)
	    call error (1, Memc[cmd])
	}

	call sfree (sp)
end


# CM_GEOMAP -- Compute shift, scale, and rotation.

procedure cm_geomap (imin, wcsin, xref, yref, xin, yin, n,
	nfit, rms, fitgeom, interactive, ifit, results, verbose, reject)

pointer	imin[n]		#I IMIO pointer assigned to each coordinate
pointer	wcsin[n]	#I WCS pointer assigned to each coordinate
double	xref[n]		#I X reference coordinate
double	yref[n]		#I Y reference coordinate
double	xin[n]		#I X target coordinate for reference coordinate
double	yin[n]		#I Y target coordinate for reference coordinate
int	n		#I Number of coordinates
int	nfit		#I Minimum number of coordinates for fit
double	rms		#I Maximum rms to accept
char	fitgeom[ARB]	#I Fit geometry
int	interactive	#I Interactive?
int	ifit		#I Interactive fit?
double	results[8]	#O Results
int	verbose		#I Verbose?
real	reject 		#I sigma rejection

int	i, nfound, fd
double	x1, y1, x2, y2, xavg, yavg
pointer	sp, temp1, temp2, cmd, graphics, cursor

int	open(), stropen(), strdic(), fscan(), nscan()
errchk	open

begin
	call smark (sp)
	call salloc (temp1, SZ_FNAME, TY_CHAR)
	call salloc (temp2, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (cursor, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_CMD, TY_CHAR)

	call mktemp ("tmp$iraf", Memc[temp1], SZ_FNAME)
	call mktemp ("tmp$iraf", Memc[temp2], SZ_FNAME)

	# Convert input and centered coordinates to astrometry coordinates.
	nfound = 0
	do i = 1, n
	    if (imin[i] != NULL)
		nfound = nfound + 1

	if (nfound < nfit) {
	    call sprintf (Memc[cmd], SZ_CMD,
		"Too few coordinates to fit: %d/%d")
		    call pargi (nfound)
		    call pargi (nfit)
	    call error (1, Memc[cmd])
	}

	if (verbose == YES)
	    call printf ("    Fit coordinates:\n")

	fd = open (Memc[temp1], NEW_FILE, TEXT_FILE)
	xavg = 0.; yavg = 0.
	do i = 1, n {
	    if (imin[i] == NULL)
		next
	    call msc_c2trand (wcsin[i], CT_WA, xref[i], yref[i], x1, y1)
	    call msc_c2trand (wcsin[i], CT_LW, xin[i], yin[i], x2, y2)
	    call msc_c2trand (wcsin[i], CT_WA, x2, y2, x2, y2)
	    call fprintf (fd, "%g %g %g %g\n")
		call pargd (x1)
		call pargd (y1)
		call pargd (x2)
		call pargd (y2)
	    xavg = xavg + x2 - x1
	    yavg = yavg + y2 - y1
	}
	call close (fd)

	call clgstr ("graphics", Memc[graphics], SZ_LINE)
	call clgstr ("cursor", Memc[cursor], SZ_LINE)
	fd = stropen (Memc[cmd], SZ_CMD, NEW_FILE)
	call fprintf (fd,
	    "geomap input=%s database=%s transforms='' results=''")
	    call pargstr (Memc[temp1])
	    call pargstr (Memc[temp2])
	call fprintf (fd, " xmin=INDEF xmax=INDEF ymin=INDEF ymax=INDEF")
	call fprintf (fd, " fitgeom=%s func=polynomial")
	    if (nfound > 1)
		call pargstr (fitgeom)
	    else
		call pargstr ("shift")
	call fprintf (fd, " xxo=2 xyo=2 xxt=half yxo=2 yyo=2 yxt=half")
	call fprintf (fd, " maxiter=%d reject=%g calc=double verb-")
	    if (reject <= 0. || IS_INDEF(reject)) {
		call pargi (0)
		call pargr (3.)	
	    } else {
		call pargi (4)
		call pargr (reject)	
	    }
	call fprintf (fd, " inter=%b graphics=%s cursor=\"%s\"")
	    call pargb ((ifit==YES&&(interactive==YES||Memc[cursor]!=EOS)))
	    call pargstr (Memc[graphics])
	    call pargstr (Memc[cursor])
	if (Memc[cursor] != EOS)
	    call fprintf (fd, " > dev$null")
	call close (fd)

	call clcmdw (Memc[cmd])

	fd = open (Memc[temp2], READ_ONLY, TEXT_FILE)
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[cmd], SZ_CMD)
	    if (nscan() != 1)
		next
	    i = strdic (Memc[cmd], Memc[cmd], SZ_CMD,
		"|xshift|yshift|xmag|ymag|xrotation|yrotation|xrms|yrms|")
	    if (i == 0)
		next
	    call gargd (results[i])
	    if (i == 5 || i == 6)
		if (results[i] > 180.)
		    results[i] = results[i] - 360.
	}
	call close (fd)

	if (verbose == YES || interactive == YES) {
	    call printf ("      input number of coordinates = %d\n")
		call pargi (nfound)
	    call printf ("      average shift = (%.2f, %.2f) arcsec\n")
		call pargd (xavg / nfound)
		call pargd (yavg / nfound)
	    call printf ("      tangent point shift = (%.2f, %.2f) arcsec\n")
		call pargd (results[1])
		call pargd (results[2])
	    call printf ("      fractional scale change = (%.3f, %.3f)\n")
		call pargd (results[3])
		call pargd (results[4])
	    call printf ("      axis rotation = (%.3f, %.3f) degrees\n")
		call pargd (results[5])
		call pargd (results[6])
	    call printf ("      rms = (%.3f, %.3f) arcsec\n")
		call pargd (results[7])
		call pargd (results[8])
	}

	call delete (Memc[temp1])
	call delete (Memc[temp2])

	if (interactive == NO && rms < max (results[7], results[8])) {
	    call sprintf (Memc[cmd], SZ_CMD,
		"RMS of fit is too large: %.3f > %.3f")
		call pargd (max (results[7], results[8]))
		call pargd (rms)
	    call error (1, Memc[cmd])
	}

	call sfree (sp)
end


# CM_UPDCOORDS -- Update coordinate file.

procedure cm_updcoords (incoords, outcoords, flags)

char	incoords[ARB]		#I Input coordinate filename
char	outcoords[ARB]		#I Output coordinate filename
double	flags[ARB]		#I Rejected points are given as INDEFD

int	fdin, fdout, n
double	x, y
pointer	sp, line, fname

bool	streq()
int	open(), getline(), nscan()
errchk	open

begin
	# Do nothing if no output coordinate file is specified.
	if (outcoords[1] == EOS)
	    return

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Open coordinate files.
	if (streq (incoords, outcoords)) {
	    call salloc (fname, SZ_FNAME, TY_CHAR)
	    call mktemp ("tmp$iraf", Memc[fname], SZ_FNAME)
	    fdout = open (Memc[fname], NEW_FILE, TEXT_FILE)
	} else {
	    fname = NULL
	    fdout = open (outcoords, NEW_FILE, TEXT_FILE)
	}
	fdin = open (incoords, READ_ONLY, TEXT_FILE)

	# Copy input file to output file with rejected coordinates removed.
	n = 0
	while (getline (fdin, Memc[line]) != EOF) {
	    call sscan (Memc[line])
	    call gargd (x)
	    call gargd (y)
	    if (nscan() < 2)
		call putline (fdout, Memc[line])
	    else {
		n = n + 1
		if (!IS_INDEFD(flags[n]))
		    call putline (fdout, Memc[line])
	    }
	}

	# Finish up.
	call close (fdout)
	call close (fdin)
	if (streq (incoords, outcoords)) {
	    call delete (incoords)
	    call rename (Memc[fname], outcoords)
	}
	call sfree (sp)
end


# CM_COMPARED --  Compare values in double array given by pointer.

int procedure cm_compared (arg, x1, x2)

pointer	arg		#I pointer to data
int	x1		#I comparison index
int	x2		#I comparison index

double	y1, y2

begin
	y1 = Memd[arg+x1]
	y2 = Memd[arg+x2]

	if (y1 == y2)
	    return (0)
	else if (IS_INDEFD(y1))
	    return (1)
	else if (IS_INDEF(y2))
	    return (-1)
	else if (y1 < y2)
	    return (-1)
	else
	    return (1)
end


# CM_GETCOORDS -- Get coordinates from a file or a command.

procedure cm_getcoords (image, coords, xptr, yptr, n, mptr, nm)

char	image[ARB]		#I Image to which coordinates apply
char	coords[ARB]		#I Coordinate specification
pointer	xptr			#O Pointer to x coordinates
pointer	yptr			#O Pointer to y coordinates
int	n			#O Number of coordinates
pointer	mptr			#O Pointer to magnitudes
int	nm			#O Number of magnitudes

int	i, j, fd
double	x, y, m
pointer	sp, fname, cmd

bool	strne()
int	strlen(), open(), fscan(), nscan()
errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Set the coordinate file.
	j = 0
	if (coords[1] == '!') {
	    call mktemp ("tmp", Memc[fname], SZ_FNAME)
	    for (i=2; coords[i] != EOS; i=i+1) {
		if (coords[i] == '$') {
		    i = i + 1
		    if (coords[i] == 'I')
			call strcpy (image, Memc[cmd+j], SZ_LINE-j)
		    else if (coords[i] == 'C')
			call strcpy (Memc[fname], Memc[cmd+j], SZ_LINE-j)
		    else
			call error (1,
			   "Syntax error in coordinate specification")
		    j = strlen (Memc[cmd])
		} else {
		    Memc[cmd+j] = coords[i]
		    j = j + 1
		}
	    }
	    call clcmdw (Memc[cmd])
	} else
	    call strcpy (coords, Memc[fname], SZ_FNAME)

	# Get coordinates.
	n = 0
	nm = 0
	fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	while (fscan (fd) != EOF) {
	    call gargd (x)
	    call gargd (y)
	    call gargd (m)
	    if (nscan() < 2)
		next
	    if (nscan() < 3)
		m = INDEFD
	    if (n == 0) {
		call malloc (xptr, 100, TY_DOUBLE)
		call malloc (yptr, 100, TY_DOUBLE)
		call malloc (mptr, 100, TY_DOUBLE)
	    } else if (mod (n, 100) == 0) {
		call realloc (xptr, n+100, TY_DOUBLE)
		call realloc (yptr, n+100, TY_DOUBLE)
		call realloc (mptr, n+100, TY_DOUBLE)
	    }
	    Memd[xptr+n] = x * 15.
	    Memd[yptr+n] = y
	    Memd[mptr+n] = m
	    n = n + 1
	    if (!IS_INDEFD(m))
		nm = nm + 1
	}
	call close (fd)

	if (strne (coords, Memc[fname]))
	    call delete (Memc[fname])
	call sfree (sp)
end
