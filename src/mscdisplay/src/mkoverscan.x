include	<imhdr.h>
include	<imset.h>
include	"mosgeom.h"

# MK_OVERSCAN -- Make the overscan vector.
#
#   1.	Average the overscan columns or lines.
#   2.	Fit a function with the ICFIT routines to smooth the overscan vector.

procedure mk_overscan (im, mg)

pointer	im			# IMIO pointer for image.
pointer	mg			# MOSGEOM pointer.

int	i, first, last, navg, npts
pointer	sp, imname, buf, x

real	asumr()
pointer	imgs2r()
errchk	imgs2r, fit_overscan

begin
	call smark (sp)
	call salloc (imname, SZ_LINE, TY_CHAR)
	call imstats (im, IM_IMAGENAME, Memc[imname], SZ_LINE)

	first = BX1(mg)
	last  = BX2(mg)
	navg  = last - first + 1
	npts  = NY(mg)

	# Set zero overscan vector
	if (navg < 1) {
	    call calloc (OVRSCN(mg), npts, TY_REAL)
	    call sfree (sp)
	    return
	}

	call salloc (buf, npts, TY_REAL)
	do i = 1, npts
	    Memr[buf+i-1] = asumr (Memr[imgs2r(im, first, last, i, i)], navg)
	if (navg > 1)
	    call adivkr (Memr[buf], real (navg), Memr[buf], npts)

	# Trim the overscan vector and set the pixel coordinate.
	# Use DATA not CCD coordinates (ala ccdproc) as x. Not sure this is
	# the right choice.
	npts = DY2(mg) - DY1(mg) + 1
	call malloc (OVRSCN(mg), npts, TY_REAL)
	call salloc (x, npts, TY_REAL)
	call trim_overscan (Memr[buf], npts, DY1(mg), Memr[x],
	    Memr[OVRSCN(mg)])

	call fit_overscan (Memc[imname], BX1(mg), BX2(mg), BY1(mg), BY2(mg),
	Memr[x], Memr[OVRSCN(mg)], npts)

	call sfree (sp)
end

# AVG_OVERSCAN -- Average (kclipped in X) the overscan vector.
#

procedure avg_overscan (im, mg, sample)

pointer	im			# IMIO pointer for image.
pointer	mg			# MOSGEOM pointer.
real	sample			# Fraction of overscan to sample

int	nlines, npts, interval, start, line, bx1, bx2, navg
pointer	 buf
real	sum

double	signorm()
real	linebiasr()
pointer	imgs2r()
errchk	imgs2r, linebiasr

include "lbias.com"

begin
	# Set parameters for linebias routine
	itmax  = 2
	ksigma = 3.0
	sigcor = real (signorm (double (ksigma)))

	bx1  = BX1(mg)
	bx2  = BX2(mg)
	navg = bx2 - bx1 + 1

	# Set zero bias value if no overscan strip.
	if (navg < 1) {
	    BIAS(mg) = 0.0
	    return
	}

	if (sample <= 0.0) {
	    interval = 1
	    start    = 1
	} else {
	    # Sample evenly (we deliberately avoid pixels at start and end)
	    nlines = NY(mg)
	    npts = max (5., nlines * sample)
	    interval = nlines / (npts + 1)
	    start = max (1, (nlines - interval * npts) / 2)
	}

	npts = 0
	sum  = 0.0
	do line = start, NY(mg), interval {
	    npts = npts + 1
	    buf = imgs2r (im, bx1, bx2, line, line)
	    if (navg == 1) {
		sum = Memr[buf]
	    } else {
		sum = sum + linebiasr (Memr[buf], navg)
	    }
	}

	BIAS(mg) = sum / npts
end
