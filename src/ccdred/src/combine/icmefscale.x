# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"src/icombine.h"


# MEFSCALES -- Compute scaling factors for MEF based on image statistics.
#
# MEF data requires all extensions from a single file have the same
# scaling factor.  If the scaling is to be done based on image statistics
# it measures the image statistics for each extension (using the standard
# icstat routine) and then combines the statistics for all extensions from
# a single MEF to give a single statistic.

procedure mefscales (images, iimage, nimages, nsubsets, scales, zeros, wts,
	nims)

pointer	images[nsubsets]	#I Extension image names grouped by amplifier
pointer	iimage[nsubsets]	#I List of image indices for each amplifer 
int	nimages[nsubsets]	#I Number of images in each amplifier
int	nsubsets		#I Number of amplifiers
real	scales[nims]		#U Scales for each MEF file
real	zeros[nims]		#U Zeros for each MEF file
real	wts[nims]		#U Weights for each MEF file
int	nims			#I Number of MEF files

int	i, j, k, l, fd
int	stype, ztype, wtype
bool	dos, doz, dow, domode, domedian, domean
pointer	sp, str, section, offsets, modes, medians, means
pointer	im, imname

int	strdic(), nowhite(), open(), fscan()
pointer	immap()
errchk	open, ic_statr

include	"src/icombine.com"

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (offsets, IM_MAXDIM, TY_INT)
	call aclri (Memi[offsets], IM_MAXDIM)

	# Check if anything needs to be calculated.
	call clgstr ("scale", Memc[str], SZ_FNAME)
	stype = strdic (Memc[str], Memc[str], SZ_FNAME, STYPES)
	call clgstr ("zero", Memc[str], SZ_FNAME)
	ztype = strdic (Memc[str], Memc[str], SZ_FNAME, ZTYPES)
	call clgstr ("weight", Memc[str], SZ_FNAME)
	wtype = strdic (Memc[str], Memc[str], SZ_FNAME, WTYPES)

	dos = ((stype==S_MODE)||(stype==S_MEDIAN)||(stype==S_MEAN))
	doz = ((ztype==S_MODE)||(ztype==S_MEDIAN)||(ztype==S_MEAN))
	dow = ((wtype==S_MODE)||(wtype==S_MEDIAN)||(wtype==S_MEAN))
	if (dos) {
	    dos = false
	    do i = 1, nims
		if (IS_INDEFR(scales[i])) {
		    dos = true
		    break
		}
	}
	if (doz) {
	    doz = false
	    do i = 1, nims
		if (IS_INDEFR(zeros[i])) {
		    doz = true
		    break
		}
	}
	if (dow) {
	    dow = false
	    do i = 1, nims
		if (IS_INDEFR(wts[i])) {
		    dow = true
		    break
		}
	}

	if (!(dos || doz || dow)) {
	    call sfree (sp)
	    return
	}

	# Compute the statistics.

	i = nims * nsubsets
	call salloc (modes, i, TY_REAL)
	call salloc (medians, i, TY_REAL)
	call salloc (means, i, TY_REAL)
	call amovkr (INDEFR, Memr[modes], i)
	call amovkr (INDEFR, Memr[medians], i)
	call amovkr (INDEFR, Memr[means], i)

	domode = ((stype==S_MODE)||(ztype==S_MODE)||(wtype==S_MODE))
	domedian = ((stype==S_MEDIAN)||(ztype==S_MEDIAN)||(wtype==S_MEDIAN))
	domean = ((stype==S_MEAN)||(ztype==S_MEAN)||(wtype==S_MEAN))

	# Compute the statistics for each extension.
	Memc[section] = EOS
	if (nowhite (Memc[statsec], Memc[statsec], ARB) != 0) {
	    if (Memc[statsec] == '@')
		fd = open (Memc[statsec+1], READ_ONLY, TEXT_FILE)
	    else
		call strcpy (Memc[statsec], Memc[section], SZ_FNAME)
	}
	do j = 1, nsubsets {
	    if (Memc[statsec] == '@') {
		if (j == 1)
		    call seek (fd, BOF)
		if (fscan (fd) != EOF)
		    call gargwrd (Memc[section], SZ_FNAME)
		if (Memc[section] != '[')
		    next
	    }
	    do k = 1, nimages[j] {
		i = Memi[iimage[j]+k-1]
		if (! (IS_INDEFR(scales[i]) || IS_INDEFR(zeros[i]) ||
		    IS_INDEFR(wts[i])))
		    next
		imname = images[j] + (k - 1) * SZ_FNAME
		im = immap (Memc[imname], READ_ONLY, 0)
		call ic_mopen (im, im, 1, Memi[offsets], 0)

		l = (j - 1) * nims + i - 1
		call ic_statr (im, im, Memc[section], Memi[offsets], 1,
		    1, domode, domedian, domean,
		    Memr[modes+l], Memr[medians+l], Memr[means+l])

		call ic_mclose (1)
		call imunmap (im)
	    }
	}
	if (Memc[statsec] == '@')
	    call close (fd)

	# Compute final statistics for each MEF image.
	if (dos) {
	    if (stype == S_MODE) 
		call mefscales1 (Memr[modes], scales, nims, nsubsets)
	    else if (stype == S_MEDIAN) 
		call mefscales1 (Memr[medians], scales, nims, nsubsets)
	    else if (stype == S_MEAN) 
		call mefscales1 (Memr[means], scales, nims, nsubsets)
	}
	if (doz) {
	    if (ztype == S_MODE) 
		call mefscales1 (Memr[modes], zeros, nims, nsubsets)
	    else if (ztype == S_MEDIAN) 
		call mefscales1 (Memr[medians], zeros, nims, nsubsets)
	    else if (ztype == S_MEAN) 
		call mefscales1 (Memr[means], zeros, nims, nsubsets)
	}
	if (dow) {
	    if (wtype == S_MODE) 
		call mefscales1 (Memr[modes], wts, nims, nsubsets)
	    else if (wtype == S_MEDIAN) 
		call mefscales1 (Memr[medians], wts, nims, nsubsets)
	    else if (wtype == S_MEAN) 
		call mefscales1 (Memr[means], wts, nims, nsubsets)
	}

	call sfree (sp)
end


# MEFSCALES1 -- Combine image statistics from extensions into composite values.
#
# For each input MEF file the statistics for all extensions in the file
# are combined by averaging the individual statistics.  If there are enough
# images deviant statistics are removed and uniform balance factors for
# each image are measured and used.

define	MINIMS		4		# Minimum number of images for clipping
define	SIGCLIP		2.		# Sigma clipping factor.
define	DEBUG		NO

procedure mefscales1 (stats, final, nims, nsubsets)

real	stats[nims,nsubsets]	#I Input statistics
real	final[nims]		#O Final averages
int	nims			#I Number of images
int	nsubsets		#I Number of subsets

int	i, j, n, aravr()
real	a, sig
pointer	sp, avgs, avgr, ratios

begin
	# Number of images to use.
	n = 0
	do i = 1, nims
	    if (IS_INDEFR(final[i]))
		n = n + 1

	# If only a few images just compute an average.
	if (n < MINIMS) {
	    do i = 1, nims {
		if (!IS_INDEFR(final[i]))
		    next

		a = 0.
		n = 0
		do j = 1, nsubsets {
		    if (IS_INDEFR(stats[i,j]))
			next
		    a = a + stats[i,j]
		    n = n + 1
		}
		if (n > 0)
		    final[i] = a / n
	    }
	    return
	}

	call smark (sp)
	call salloc (avgs, nims, TY_REAL)
	call salloc (avgr, nsubsets, TY_REAL)
	call salloc (ratios, max (nims, nsubsets), TY_REAL)

	if (DEBUG == YES) {
	    do i = 1, nims
		do j = 1, nsubsets {
		    call printf ("%d %d %g\n")
			call pargi (i)
			call pargi (j)
			call pargr (stats[i,j])
		}
	}

	# Average value for each image with no rejection.
	do i = 1, nims {
	    if (!IS_INDEFR(final[i]))
		next

	    a = 0.
	    n = 0
	    do j = 1, nsubsets {
		if (IS_INDEFR(stats[i,j]))
		    next
		a = a + stats[i,j]
		n = n + 1
	    }
	    if (n > 0)
		Memr[avgs+i-1] = a / n
	    else
		Memr[avgs+i-1] = INDEFR
	    if (DEBUG == YES) {
		call printf ("image %d: average = %g\n")
		    call pargi (i)
		    call pargr (Memr[avgs+i-1])
	    }
	}

	# Average balance factor for each subset with rejection.
	do j = 1, nsubsets {
	    n = 0
	    do i = 1, nims {
		if (!IS_INDEFR(final[i]) || IS_INDEFR(stats[i,j]) ||
		    IS_INDEFR(Memr[avgs+i-1]))
		    next
		Memr[ratios+i-1] = stats[i,j] / Memr[avgs+i-1]
		n = n + 1
		if (DEBUG == YES) {
		    call printf ("subset %d, image %d: ratio = %g\n")
			call pargi (j)
			call pargi (i)
			call pargr (Memr[ratios+i-1])
		}
	    }
	    i = aravr (Memr[ratios], n, Memr[avgr+j-1], sig, 2.)
	    if (DEBUG == YES) {
		call printf ("subset %d: n = %d, average = %g, sig = %g\n")
		    call pargi (j)
		    call pargi (i)
		    call pargr (Memr[avgr+j-1])
		    call pargr (sig)
	    }
	}

	# Average balance corrected value for each image with rejection.
	do i = 1, nims {
	    if (!IS_INDEFR(final[i]))
		next

	    n = 0
	    do j = 1, nsubsets {
		if (IS_INDEFR(stats[i,j]) || IS_INDEFR(Memr[avgr+j-1]))
		    next
		Memr[ratios+j-1] = stats[i,j] / Memr[avgr+j-1]
		n = n + 1
		if (DEBUG == YES) {
		    call printf ("image %d, subset %d: ratio = %g\n")
			call pargi (i)
			call pargi (j)
			call pargr (Memr[ratios+j-1])
		}
	    }
	    j = aravr (Memr[ratios], n, final[i], sig, 2.)
	    if (DEBUG == YES) {
		call printf ("image %d: n = %d, average = %g, sig = %g\n")
		    call pargi (i)
		    call pargi (j)
		    call pargr (final[i])
		    call pargr (sig)
	    }
	}

	call sfree (sp)
end
