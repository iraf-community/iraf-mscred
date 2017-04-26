# This version differs only in calling the zt_fp instead of xt_fp routines.
# The zt_fp routiens are a version that calls the msc routines to get data.
# Also there is a bug fix in sigm2_setup which is why there is a version here.

include	<error.h>
include	<imhdr.h>
include	<pmset.h>
include	"xtfixpix.h"
include	"mosgeom.h"

# Scaled image descriptor for 2-dim images

define	SI_LEN		19
define	SI_MAXDIM	2		# images of 2 dimensions supported
define	SI_NBUFS	3		# nbuffers used by SIGL2

define	SI_IM		Memi[$1]	# pointer to input image header
define	SI_FP		Memi[$1+1]	# pointer to fixpix structure
define	SI_GRID		Memi[$1+2+$2-1]	# pointer to array of X coords
define	SI_NPIX		Memi[$1+4+$2-1]	# number of X coords
define	SI_BAVG		Memi[$1+6+$2-1]	# X block averaging factor
define	SI_INTERP	Memi[$1+8+$2-1]	# interpolate X axis
define	SI_BUF		Memi[$1+10+$2-1]# line buffers
define	SI_BUFY		Memi[$1+13+$2-1]# Y values of buffers
define	SI_ORDER	Memi[$1+15]	# interpolator order
define	SI_TYBUF	Memi[$1+16]	# buffer type
define	SI_XOFF		Memi[$1+17]	# offset in input image to first X
define	SI_INIT		Memi[$1+18]	# YES until first i/o is done

define	OUTBUF		SI_BUF($1,3)

define	SI_TOL		(1E-5)		# close to a pixel
define	INTVAL		(abs ($1 - nint($1)) < ($2))
define	SWAPI		{tempi=$2;$2=$1;$1=tempi}
define	SWAPP		{tempp=$2;$2=$1;$1=tempp}
define	NOTSET		(-9999)


# SIGM2_SETUP -- Set up the spatial transformation for SIGL2[SR].  Compute
# the block averaging factors (1 if no block averaging is required) and
# the sampling grid points, i.e., pixel coordinates of the output pixels in
# the input image.

pointer procedure zigm2_setup (im, pm, px1,px2,nx,xblk, py1,py2,ny,yblk, order)

pointer	im			# the input image
pointer	pm			# pixel mask
real	px1, px2		# range in X to be sampled on an even grid
int	nx			# number of output pixels in X
int	xblk			# blocking factor in x
real	py1, py2		# range in Y to be sampled on an even grid
int	ny			# number of output pixels in Y
int	yblk			# blocking factor in y
int	order			# interpolator order (0=replicate, 1=linear)

int	npix, noldpix, nbavpix, i, j
int	npts[SI_MAXDIM]		# number of output points for axis
int	blksize[SI_MAXDIM]	# block averaging factor (npix per block)
real	tau[SI_MAXDIM]		# tau = p(i+1) - p(i) in fractional pixels
real	p1[SI_MAXDIM]		# starting pixel coords in each axis
real	p2[SI_MAXDIM]		# ending pixel coords in each axis
real	scalar, start
pointer	si, gp, xt_fpinit()

begin
	iferr (call calloc (si, SI_LEN, TY_STRUCT))
	    call erract (EA_FATAL)

	SI_IM(si) = im
	SI_FP(si) = xt_fpinit (pm, 1, INDEFI)
	SI_NPIX(si,1) = nx
	SI_NPIX(si,2) = ny
	SI_ORDER(si) = order
	SI_INIT(si) = YES

	p1[1] = px1			# X = index 1
	p2[1] = px2
	npts[1] = nx
	blksize[1] = xblk

	p1[2] = py1			# Y = index 2
	p2[2] = py2
	npts[2] = ny
	blksize[2] = yblk

	# Compute block averaging factors if not defined.
	# If there is only one pixel then the block average is the average
	# between the first and last point.

	do i = 1, SI_MAXDIM {
	    if ((blksize[i] >= 1) && !IS_INDEFI (blksize[i])) {
	        if (npts[i] == 1)
		    tau[i] = 0.
	        else
	            tau[i] = (p2[i] - p1[i]) / (npts[i] - 1)
	    } else {
		if (npts[i] == 1) {
		    tau[i] = 0.
		    blksize[i] = int (p2[i] - p1[i] + 1 + SI_TOL)
	        } else {
	            tau[i] = (p2[i] - p1[i]) / (npts[i] - 1)
	    	    if (tau[i] >= 2.0) {

			# If nx or ny is not an integral multiple of the block
			# averaging factor, noldpix is the next larger number
			# which is an integral multiple.  When the image is
			# block averaged pixels will be replicated as necessary
			# to fill the last block out to this size.  

			blksize[i] = int (tau[i] + SI_TOL)
			npix = p2[i] - p1[i] + 1
			noldpix = (npix+blksize[i]-1) / blksize[i] * blksize[i]
			nbavpix = noldpix / blksize[i]
			scalar = real (nbavpix - 1) / real (noldpix - 1)
			p1[i] = (p1[i] - 1.0) * scalar + 1.0
			p2[i] = (p2[i] - 1.0) * scalar + 1.0
			tau[i] = (p2[i] - p1[i]) / (npts[i] - 1)
		    } else
			blksize[i] = 1
		}
	    }
	}

	SI_BAVG(si,1) = blksize[1]
	SI_BAVG(si,2) = blksize[2]

#	if (IS_INDEFI (xblk))
#	    xblk = blksize[1]
#	if (IS_INDEFI (yblk))
#	    yblk = blksize[2]

	# Allocate and initialize the grid arrays, specifying the X and Y
	# coordinates of each pixel in the output image, in units of pixels
	# in the input (possibly block averaged) image.

	do i = 1, SI_MAXDIM {
	    # The X coordinate is special.  We do not want to read entire
	    # input image lines if only a range of input X values are needed.
	    # Since the X grid vector passed to ALUI (the interpolator) must
	    # contain explicit offsets into the vector being interpolated,
	    # we must generate interpolator grid points starting near 1.0.
	    # The X origin, used to read the block averaged input line, is
	    # given by XOFF.

	    if (i == 1) {
		SI_XOFF(si) = int (p1[i] + SI_TOL)
		start = p1[1] - int (p1[i] + SI_TOL) + 1.0
	    } else
	    	start = p1[i]

	    # Do the axes need to be interpolated?
	    if (INTVAL(start,SI_TOL) && (abs (tau[i]-nint(tau[i]))*npts[i] < 1))
		SI_INTERP(si,i) = NO
	    else
		SI_INTERP(si,i) = YES

	    # Allocate grid buffer and set the grid points.
	    iferr (call malloc (gp, npts[i], TY_REAL))
		call erract (EA_FATAL)
	    SI_GRID(si,i) = gp
	    if (SI_ORDER(si) <= 0) {
		do j = 0, npts[i]-1
		    Memr[gp+j] = int (start + (j * tau[i]) + 0.5 + SI_TOL)
	    } else {
		do j = 0, npts[i]-1
		    Memr[gp+j] = start + (j * tau[i])
	    }
	}

	return (si)
end


# SIGM2S -- Get a line of type short from a scaled image.  Block averaging is
# done by a subprocedure; this procedure gets a line from a possibly block
# averaged image and if necessary interpolates it to the grid points of the
# output line.

pointer procedure yigm2s (si, lineno)

pointer	si		# pointer to SI descriptor
int	lineno

pointer	rawline, tempp, gp
int	i, new_y[2], tempi, curbuf, altbuf
int	nraw, npix, nblks_y, ybavg, x1, x2
real	x, y, weight_1, weight_2
pointer	si_blmavgs()
errchk	si_blmavgs

begin
	nraw = IM_LEN(SI_IM(si),1)
	npix = SI_NPIX(si,1)

	# Determine the range of X (in pixels on the block averaged input image)
	# required for the interpolator.

	gp = SI_GRID(si,1)
	x1 = SI_XOFF(si)
	x = Memr[gp+npix-1]
	x2 = x1 + int(x)
	if (INTVAL(x,SI_TOL))
	    x2 = x2 - 1
	x2 = max (x1 + 1, x2)

	gp = SI_GRID(si,2)
	y = Memr[gp+lineno-1]

	# The following is an optimization provided for the case when it is
	# not necessary to interpolate in either X or Y.  Block averaging is
	# permitted.

	if (SI_INTERP(si,1) == NO && SI_INTERP(si,2) == NO)
	    return (si_blmavgs (SI_IM(si), SI_FP(si), x1, x2, int(y),
		SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si)))

	# If we are interpolating in Y two buffers are required, one for each
	# of the two input image lines required to interpolate in Y.  The lines
	# stored in these buffers are interpolated in X to the output grid but
	# not in Y.  Both buffers are not required if we are not interpolating
	# in Y, but we use them anyhow to simplify the code.

	if (SI_INIT(si) == YES) {
	    do i = 1, 2 {
		if (SI_BUF(si,i) != NULL)
		    call mfree (SI_BUF(si,i), SI_TYBUF(si))
		call malloc (SI_BUF(si,i), npix, TY_SHORT)
		SI_TYBUF(si) = TY_SHORT
		SI_BUFY(si,i) = NOTSET
	    }
	    if (OUTBUF(si) != NULL)
		call mfree (OUTBUF(si), SI_TYBUF(si))
	    call malloc (OUTBUF(si), npix, TY_SHORT)
	    SI_INIT(si) = NO
	}

	# If the Y value of the new line is not in range of the contents of the
	# current line buffers, refill one or both buffers.  To refill we must
	# read a (possibly block averaged) input line and interpolate it onto
	# the X grid.  The X and Y values herein are in the coordinate system
	# of the (possibly block averaged) input image.

	new_y[1] = int(y)
	new_y[2] = int(y) + 1

	# Get the pair of lines whose integral Y values form an interval
	# containing the fractional Y value of the output line.  Sometimes the
	# desired line will happen to be in the other buffer already, in which
	# case we just have to swap buffers.  Often the new line will be the
	# current line, in which case nothing is done.  This latter case occurs
	# frequently when the magnification ratio is large.

	curbuf = 1
	altbuf = 2

	do i = 1, 2 {
	    if (new_y[i] == SI_BUFY(si,i)) {
		;
	    } else if (new_y[i] == SI_BUFY(si,altbuf)) {
		SWAPP (SI_BUF(si,1), SI_BUF(si,2))
		SWAPI (SI_BUFY(si,1), SI_BUFY(si,2))

	    } else {
		# Get line and interpolate onto output grid.  If interpolation
		# is not required merely copy data out.  This code is set up
		# to always use two buffers; in effect, there is one buffer of
		# look ahead, even when Y[i] is integral.  This means that we
		# will go out of bounds by one line at the top of the image.
		# This is handled by copying the last line.

		ybavg = SI_BAVG(si,2)
		nblks_y = (IM_LEN (SI_IM(si), 2) + ybavg-1) / ybavg
		if (new_y[i] <= nblks_y)
		    rawline = si_blmavgs (SI_IM(si), SI_FP(si), x1, x2,
			new_y[i], SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si))

		if (SI_INTERP(si,1) == NO) {
		    call amovs (Mems[rawline], Mems[SI_BUF(si,i)], npix)
		} else if (SI_ORDER(si) == 0) {
		    call zi_samples (Mems[rawline], Mems[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		} else if (SI_ORDER(si) == -1) {
		    call zi_maxs (Mems[rawline], nraw,
		    	Memr[SI_GRID(si,1)], Mems[SI_BUF(si,i)], npix)
		} else {
		    call aluis (Mems[rawline], Mems[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		}

		SI_BUFY(si,i) = new_y[i]
	    }

	    SWAPI (altbuf, curbuf)
	}

	# We now have two line buffers straddling the output Y value,
	# interpolated to the X grid of the output line.  To complete the
	# bilinear interpolation operation we take a weighted sum of the two
	# lines.  If the range from SI_BUFY(si,1) to SI_BUFY(si,2) is repeatedly
	# interpolated in Y no additional i/o occurs and the linear
	# interpolation operation (ALUI) does not have to be repeated (only the
	# weighted sum is required).  If the distance of Y from one of the
	# buffers is zero then we do not even have to take a weighted sum.
	# This is not unusual because we may be called with a magnification
	# of 1.0 in Y.

	weight_1 = 1.0 - (y - SI_BUFY(si,1))
	weight_2 = 1.0 - weight_1

	if (weight_1 < SI_TOL)
	    return (SI_BUF(si,2))
	else if (weight_2 < SI_TOL || SI_ORDER(si) == 0)
	    return (SI_BUF(si,1))
	else if (SI_ORDER(si) == -1) {
	    call amaxs (Mems[SI_BUF(si,1)], Mems[SI_BUF(si,2)],
		Mems[OUTBUF(si)], npix)
	    return (OUTBUF(si))
	} else {
	    call awsus (Mems[SI_BUF(si,1)], Mems[SI_BUF(si,2)],
		Mems[OUTBUF(si)], npix, weight_1, weight_2)
	    return (OUTBUF(si))
	}
end


# SIGM2I -- Get a line of type int from a scaled image.  Block averaging is
# done by a subprocedure; this procedure gets a line from a possibly block
# averaged image and if necessary interpolates it to the grid points of the
# output line.

pointer procedure yigm2i (si, lineno)

pointer	si		# pointer to SI descriptor
int	lineno

pointer	rawline, tempp, gp
int	i, new_y[2], tempi, curbuf, altbuf
int	nraw, npix, nblks_y, ybavg, x1, x2
real	x, y, weight_1, weight_2
pointer	yi_blmavgi()
errchk	yi_blmavgi

begin
	nraw = IM_LEN(SI_IM(si),1)
	npix = SI_NPIX(si,1)

	# Determine the range of X (in pixels on the block averaged input image)
	# required for the interpolator.

	gp = SI_GRID(si,1)
	x1 = SI_XOFF(si)
	x = Memr[gp+npix-1]
	x2 = x1 + int(x)
	if (INTVAL(x,SI_TOL))
	    x2 = x2 - 1
	x2 = max (x1 + 1, x2)

	gp = SI_GRID(si,2)
	y = Memr[gp+lineno-1]

	# The following is an optimization provided for the case when it is
	# not necessary to interpolate in either X or Y.  Block averaging is
	# permitted.

	if (SI_INTERP(si,1) == NO && SI_INTERP(si,2) == NO)
	    return (yi_blmavgi (SI_IM(si), SI_FP(si), x1, x2, int(y),
		SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si)))

	# If we are interpolating in Y two buffers are required, one for each
	# of the two input image lines required to interpolate in Y.  The lines
	# stored in these buffers are interpolated in X to the output grid but
	# not in Y.  Both buffers are not required if we are not interpolating
	# in Y, but we use them anyhow to simplify the code.

	if (SI_INIT(si) == YES) {
	    do i = 1, 2 {
		if (SI_BUF(si,i) != NULL)
		    call mfree (SI_BUF(si,i), SI_TYBUF(si))
		call malloc (SI_BUF(si,i), npix, TY_INT)
		SI_TYBUF(si) = TY_INT
		SI_BUFY(si,i) = NOTSET
	    }
	    if (OUTBUF(si) != NULL)
		call mfree (OUTBUF(si), SI_TYBUF(si))
	    call malloc (OUTBUF(si), npix, TY_INT)
	    SI_INIT(si) = NO
	}

	# If the Y value of the new line is not in range of the contents of the
	# current line buffers, refill one or both buffers.  To refill we must
	# read a (possibly block averaged) input line and interpolate it onto
	# the X grid.  The X and Y values herein are in the coordinate system
	# of the (possibly block averaged) input image.

	new_y[1] = int(y)
	new_y[2] = int(y) + 1

	# Get the pair of lines whose integral Y values form an interval
	# containing the fractional Y value of the output line.  Sometimes the
	# desired line will happen to be in the other buffer already, in which
	# case we just have to swap buffers.  Often the new line will be the
	# current line, in which case nothing is done.  This latter case occurs
	# frequently when the magnification ratio is large.

	curbuf = 1
	altbuf = 2

	do i = 1, 2 {
	    if (new_y[i] == SI_BUFY(si,i)) {
		;
	    } else if (new_y[i] == SI_BUFY(si,altbuf)) {
		SWAPP (SI_BUF(si,1), SI_BUF(si,2))
		SWAPI (SI_BUFY(si,1), SI_BUFY(si,2))

	    } else {
		# Get line and interpolate onto output grid.  If interpolation
		# is not required merely copy data out.  This code is set up
		# to always use two buffers; in effect, there is one buffer of
		# look ahead, even when Y[i] is integral.  This means that we
		# will go out of bounds by one line at the top of the image.
		# This is handled by copying the last line.

		ybavg = SI_BAVG(si,2)
		nblks_y = (IM_LEN (SI_IM(si), 2) + ybavg-1) / ybavg
		if (new_y[i] <= nblks_y)
		    rawline = yi_blmavgi (SI_IM(si), SI_FP(si), x1, x2,
			new_y[i], SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si))

		if (SI_INTERP(si,1) == NO) {
		    call amovi (Memi[rawline], Memi[SI_BUF(si,i)], npix)
		} else if (SI_ORDER(si) == 0) {
		    call zi_samplei (Memi[rawline], Memi[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		} else if (SI_ORDER(si) == -1) {
		    call zi_maxi (Memi[rawline], nraw,
		    	Memr[SI_GRID(si,1)], Memi[SI_BUF(si,i)], npix)
		} else {
		    call aluii (Memi[rawline], Memi[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		}

		SI_BUFY(si,i) = new_y[i]
	    }

	    SWAPI (altbuf, curbuf)
	}

	# We now have two line buffers straddling the output Y value,
	# interpolated to the X grid of the output line.  To complete the
	# bilinear interpolation operation we take a weighted sum of the two
	# lines.  If the range from SI_BUFY(si,1) to SI_BUFY(si,2) is repeatedly
	# interpolated in Y no additional i/o occurs and the linear
	# interpolation operation (ALUI) does not have to be repeated (only the
	# weighted sum is required).  If the distance of Y from one of the
	# buffers is zero then we do not even have to take a weighted sum.
	# This is not unusual because we may be called with a magnification
	# of 1.0 in Y.

	weight_1 = 1.0 - (y - SI_BUFY(si,1))
	weight_2 = 1.0 - weight_1

	if (weight_1 < SI_TOL)
	    return (SI_BUF(si,2))
	else if (weight_2 < SI_TOL || SI_ORDER(si) == 0)
	    return (SI_BUF(si,1))
	else if (SI_ORDER(si) == -1) {
	    call amaxi (Memi[SI_BUF(si,1)], Memi[SI_BUF(si,2)],
		Memi[OUTBUF(si)], npix)
	    return (OUTBUF(si))
	} else {
	    call awsui (Memi[SI_BUF(si,1)], Memi[SI_BUF(si,2)],
		Memi[OUTBUF(si)], npix, weight_1, weight_2)
	    return (OUTBUF(si))
	}
end


# SI_BLMAVGI -- Get a line from a block averaged image of type integer.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  If the length of an axis
# is not an integral multiple of the block size then the last pixel in the
# last block will be replicated to fill out the block; the average is still
# defined even if a block is not full.

pointer procedure yi_blmavgi (im, fp, x1, x2, y, xbavg, ybavg, order)

pointer	im			# input image
pointer	fp			# fixpix structure
int	x1, x2			# range of x blocks to be read
int	y			# y block to be read
int	xbavg, ybavg		# X and Y block averaging factors
int	order			# averaging option

real	sum
int	blkmax
pointer	sp, a, b
int	nblks_x, nblks_y, ncols, nlines, xoff, blk1, blk2, i, j, k
int	first_line, nlines_in_sum, npix, nfull_blks, count
pointer	xt_fpi()
errchk	xt_fpi

begin
	call smark (sp)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xoff   = (x1 - 1) * xbavg + 1
	npix   = min (ncols, xoff + (x2 - x1 + 1) * xbavg - 1) - xoff + 1

	if ((xbavg < 1) || (ybavg < 1))
	    call error (1, "si_blmavg: illegal block size")
	else if (x1 < 1 || x2 > ncols)
	    call error (2, "si_blmavg: column index out of bounds")
	else if ((xbavg == 1) && (ybavg == 1))
	    return (xt_fpi (fp, im, y, NULL) + xoff - 1)

	nblks_x = (npix   + xbavg-1) / xbavg
	nblks_y = (nlines + ybavg-1) / ybavg

	if (y < 1 || y > nblks_y)
	    call error (2, "si_blmavg: block number out of range")

	if (ybavg > 1) {
	    call salloc (b, nblks_x, TY_LONG)
	    call aclrl (Meml[b], nblks_x)
	    nlines_in_sum = 0
	}

	# Read and accumulate all input lines in the block.
	first_line = (y - 1) * ybavg + 1

	do i = first_line, min (nlines, first_line + ybavg - 1) {
	    # Get line from input image.
	    a = xt_fpi (fp, im, i, NULL) + xoff - 1

	    # Block average line in X.
	    if (xbavg > 1) {
		# First block average only the full blocks.
		nfull_blks = npix / xbavg
		if (order == -1) {
		    blk1 = a
		    do j = 1, nfull_blks {
			blk2 = blk1 + xbavg
			blkmax = Memi[blk1]
			do k = blk1+1, blk2-1
			    blkmax = max (blkmax, Memi[k])
			Memi[a+j-1] = blkmax
			blk1 = blk2
		    }
		} else
		    call abavi (Memi[a], Memi[a], nfull_blks, xbavg)

		# Now average the final partial block, if any.
		if (nfull_blks < nblks_x) {
		    if (order == -1) {
			blkmax = Memi[blk1]
			do k = blk1+1, a+npix-1
			    blkmax = max (blkmax, Memi[k])
			Memi[a+j-1] = blkmax
		    } else {
			sum = 0.0
			count = 0
			do j = nfull_blks * xbavg + 1, npix {
			    sum = sum + Memi[a+j-1]
			    count = count + 1
			}
			Memi[a+nblks_x-1] = sum / count
		    }
		}
	    }

	    # Add line into block sum.  Keep track of number of lines in sum
	    # so that we can compute block average later.

	    if (ybavg > 1) {
		if (order == -1) {
		    do j = 0, nblks_x-1
			Meml[b+j] = max (Meml[b+j], long (Memi[a+j]))
		} else {
		    do j = 0, nblks_x-1
			Meml[b+j] = Meml[b+j] + Memi[a+j]
		    nlines_in_sum = nlines_in_sum + 1
		}
	    }
	}

	# Compute the block average in Y from the sum of all lines block
	# averaged in X.  Overwrite buffer A, the buffer returned by IMIO.
	# This is kosher because the block averaged line is never longer
	# than an input line.

	if (ybavg > 1) {
	    if (order == -1) {
		do i = 0, nblks_x-1
		    Memi[a+i] = Meml[b+i]
	    } else {
		do i = 0, nblks_x-1
		    Memi[a+i] = Meml[b+i] / real(nlines_in_sum)
	    }
	}

	call sfree (sp)
	return (a)
end



# SIGM2 -- Get a line of data from a scaled image.  Block averaging is
# done by a subprocedure; this procedure gets a line from a possibly block
# averaged image and if necessary interpolates it to the grid points of the
# output line.

pointer procedure zigm2s (mg, si, lineno)

pointer	mg		# pointer to MOSGEOM descriptor
pointer	si		# pointer to SI descriptor
int	lineno

pointer	rawline, tempp, gp
int	i, new_y[2], tempi, curbuf, altbuf
int	nraw, npix, nblks_y, ybavg, x1, x2
real	x, y, weight_1, weight_2
pointer	zi_blmavgs()
errchk	zi_blmavgs

begin
	nraw = IM_LEN(SI_IM(si))
	npix = SI_NPIX(si,1)

	# Determine the range of X (in pixels on the block averaged input image)
	# required for the interpolator.

	gp = SI_GRID(si,1)
	x1 = SI_XOFF(si)
	x = Memr[gp+npix-1]
	x2 = x1 + int(x)
	if (INTVAL(x,SI_TOL))
	    x2 = x2 - 1
	x2 = max (x1 + 1, x2)

	gp = SI_GRID(si,2)
	y = Memr[gp+lineno-1]

	# The following is an optimization provided for the case when it is
	# not necessary to interpolate in either X or Y.  Block averaging is
	# permitted.

	if (SI_INTERP(si,1) == NO && SI_INTERP(si,2) == NO)
	    return (zi_blmavgs (mg, SI_IM(si), SI_FP(si), x1, x2, int(y),
		SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si)))

	# If we are interpolating in Y two buffers are required, one for each
	# of the two input image lines required to interpolate in Y.  The lines
	# stored in these buffers are interpolated in X to the output grid but
	# not in Y.  Both buffers are not required if we are not interpolating
	# in Y, but we use them anyhow to simplify the code.

	if (SI_INIT(si) == YES) {
	    do i = 1, 2 {
		if (SI_BUF(si,i) != NULL)
		    call mfree (SI_BUF(si,i), SI_TYBUF(si))
		call malloc (SI_BUF(si,i), npix, TY_SHORT)
		SI_TYBUF(si) = TY_SHORT
		SI_BUFY(si,i) = NOTSET
	    }
	    if (OUTBUF(si) != NULL)
		call mfree (OUTBUF(si), SI_TYBUF(si))
	    call malloc (OUTBUF(si), npix, TY_SHORT)
	    SI_INIT(si) = NO
	}

	# If the Y value of the new line is not in range of the contents of the
	# current line buffers, refill one or both buffers.  To refill we must
	# read a (possibly block averaged) input line and interpolate it onto
	# the X grid.  The X and Y values herein are in the coordinate system
	# of the (possibly block averaged) input image.

	new_y[1] = int(y)
	new_y[2] = int(y) + 1

	# Get the pair of lines whose integral Y values form an interval
	# containing the fractional Y value of the output line.  Sometimes the
	# desired line will happen to be in the other buffer already, in which
	# case we just have to swap buffers.  Often the new line will be the
	# current line, in which case nothing is done.  This latter case occurs
	# frequently when the magnification ratio is large.

	curbuf = 1
	altbuf = 2

	do i = 1, 2 {
	    if (new_y[i] == SI_BUFY(si,i)) {
		;
	    } else if (new_y[i] == SI_BUFY(si,altbuf)) {
		SWAPP (SI_BUF(si,1), SI_BUF(si,2))
		SWAPI (SI_BUFY(si,1), SI_BUFY(si,2))

	    } else {
		# Get line and interpolate onto output grid.  If interpolation
		# is not required merely copy data out.  This code is set up
		# to always use two buffers; in effect, there is one buffer of
		# look ahead, even when Y[i] is integral.  This means that we
		# will go out of bounds by one line at the top of the image.
		# This is handled by copying the last line.

		ybavg = SI_BAVG(si,2)
		nblks_y = (IM_LEN (SI_IM(si), 2) + ybavg-1) / ybavg
		if (new_y[i] <= nblks_y)
		    rawline = zi_blmavgs (mg, SI_IM(si), SI_FP(si), x1, x2,
			new_y[i], SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si))

		if (SI_INTERP(si,1) == NO) {
		    call amovs (Mems[rawline], Mems[SI_BUF(si,i)], npix)
		} else if (SI_ORDER(si) == 0) {
		    call zi_samples (Mems[rawline], Mems[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		} else if (SI_ORDER(si) == -1) {
		    call zi_maxs (Mems[rawline], nraw,
			Memr[SI_GRID(si,1)], Mems[SI_BUF(si,i)], npix)
		} else {
		    call aluis (Mems[rawline], Mems[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		}

		SI_BUFY(si,i) = new_y[i]
	    }

	    SWAPI (altbuf, curbuf)
	}

	# We now have two line buffers straddling the output Y value,
	# interpolated to the X grid of the output line.  To complete the
	# bilinear interpolation operation we take a weighted sum of the two
	# lines.  If the range from SI_BUFY(si,1) to SI_BUFY(si,2) is repeatedly
	# interpolated in Y no additional i/o occurs and the linear
	# interpolation operation (ALUI) does not have to be repeated (only the
	# weighted sum is required).  If the distance of Y from one of the
	# buffers is zero then we do not even have to take a weighted sum.
	# This is not unusual because we may be called with a magnification
	# of 1.0 in Y.

	weight_1 = 1.0 - (y - SI_BUFY(si,1))
	weight_2 = 1.0 - weight_1

	if (weight_1 < SI_TOL)
	    return (SI_BUF(si,2))
	else if (weight_2 < SI_TOL || SI_ORDER(si) == 0) 
	    return (SI_BUF(si,1))
	else if (SI_ORDER(si) == -1) {
	    call amaxs (Mems[SI_BUF(si,1)], Mems[SI_BUF(si,2)],
		Mems[OUTBUF(si)], npix)
	    return (OUTBUF(si))
	} else {
	    call awsus (Mems[SI_BUF(si,1)], Mems[SI_BUF(si,2)],
		Mems[OUTBUF(si)], npix, weight_1, weight_2)
	    return (OUTBUF(si))
	}
end


# SI_BLMAVG -- Get a line from a block averaged image of type short.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  If the length of an axis
# is not an integral multiple of the block size then the last pixel in the
# last block will be replicated to fill out the block; the average is still
# defined even if a block is not full.

pointer procedure zi_blmavgs (mg, im, fp, x1, x2, y, xbavg, ybavg, order)

pointer	mg			# mosgeom descriptor
pointer	im			# input image
pointer	fp			# fixpix structure
int	x1, x2			# range of x blocks to be read
int	y			# y block to be read
int	xbavg, ybavg		# X and Y block averaging factors
int	order			# averaging option

real	sum
short	blkmax
pointer	sp, a, b
int	nblks_x, nblks_y, ncols, nlines, xoff, blk1, blk2, i, j, k
int	first_line, nlines_in_sum, npix, nfull_blks, count
pointer	zt_fps()
errchk	zt_fps

begin
	call smark (sp)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xoff   = (x1 - 1) * xbavg + 1
	npix   = min (ncols, xoff + (x2 - x1 + 1) * xbavg - 1) - xoff + 1

	if ((xbavg < 1) || (ybavg < 1))
	    call error (1, "zi_blmavg: illegal block size")
	else if (x1 < 1 || x2 > ncols)
	    call error (2, "zi_blmavg: column index out of bounds")
	else if ((xbavg == 1) && (ybavg == 1))
	    return (zt_fps (fp, mg, y, NULL) + xoff - 1)

	nblks_x = (npix   + xbavg-1) / xbavg
	nblks_y = (nlines + ybavg-1) / ybavg

	if (y < 1 || y > nblks_y)
	    call error (2, "zi_blmavg: block number out of range")

	if (ybavg > 1) {
	    call salloc (b, nblks_x, TY_LONG)
	    call aclrl (Meml[b], nblks_x)
	    nlines_in_sum = 0
	}

	# Read and accumulate all input lines in the block.
	first_line = (y - 1) * ybavg + 1

	do i = first_line, min (nlines, first_line + ybavg - 1) {
	    # Get line from input image.
	    a = zt_fps (fp, mg, i, NULL) + xoff - 1

	    # Block average line in X.
	    if (xbavg > 1) {
		# First block average only the full blocks.
		nfull_blks = npix / xbavg
		if (order == -1) {
		    blk1 = a
		    do j = 1, nfull_blks {
			blk2 = blk1 + xbavg
			blkmax = Mems[blk1]
			do k = blk1+1, blk2-1
			    blkmax = max (blkmax, Mems[k])
			Mems[a+j-1] = blkmax
			blk1 = blk2
		    }
		} else
		    call abavs (Mems[a], Mems[a], nfull_blks, xbavg)

		# Now average the final partial block, if any.
		if (nfull_blks < nblks_x) {
		    if (order == -1) {
			blkmax = Mems[blk1]
			do k = blk1+1, a+npix-1
			    blkmax = max (blkmax, Mems[k])
			Mems[a+j-1] = blkmax
		    } else {
			sum = 0.0
			count = 0
			do j = nfull_blks * xbavg + 1, npix {
			    sum = sum + Mems[a+j-1]
			    count = count + 1
			}
			Mems[a+nblks_x-1] = sum / count
		    }
		}
	    }

	    # Add line into block sum.  Keep track of number of lines in sum
	    # so that we can compute block average later.

	    if (ybavg > 1) {
		if (order == -1) {
		    do j = 0, nblks_x-1
			Meml[b+j] = max (Meml[b+j], long (Mems[a+j]))
		} else {
		    do j = 0, nblks_x-1
			Meml[b+j] = Meml[b+j] + Mems[a+j]
		    nlines_in_sum = nlines_in_sum + 1
		}
	    }
	}

	# Compute the block average in Y from the sum of all lines block
	# averaged in X.  Overwrite buffer A, the buffer returned by IMIO.
	# This is kosher because the block averaged line is never longer
	# than an input line.

	if (ybavg > 1) {
	    if (order == -1) {
		do i = 0, nblks_x-1
		    Mems[a+i] = Meml[b+i]
	    } else {
		do i = 0, nblks_x-1
		    Mems[a+i] = Meml[b+i] / real(nlines_in_sum)
	    }
	}

	call sfree (sp)
	return (a)
end


# SI_MAXS -- Resample a line via maximum value.

procedure zi_maxs (a, na, x, b, nb)

short	a[na]			# input array
int	na			# input size
real	x[nb]			# sample grid
short	b[nb]			# output arrays
int	nb			# output size

int	i

begin
	do i = 1, nb
	    b[i] = max (a[int(x[i])], a[min(na,int(x[i]+1))])
end


# XT_FP -- Get the specified line of image data and replace bad pixels by
# interpolation.

pointer procedure zt_fps (fp, mg, line, fd)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

pointer	im, mscl2s(), zt_fpss()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (mscl2s (mg, line))

	im = MG_IM(mg)
	col1 = 1
	col2 = IM_LEN(im,1)
	line1 = 1
	line2 = IM_LEN(im,2)

	return (zt_fpss (fp, mg, line, col1, col2, line1, line2, fd))
end


# XT_FXS -- Get the specified line of image data and replace bad pixels by
# interpolation within a specified section.

pointer procedure zt_fpss (fp, mg, line, col1, col2, line1, line2, fd)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

int	i, j, nc, nl, ncols, c1, c2, l1, l2, l3, l4
long	v[IM_MAXDIM]
real	a, b, c, d, val
short	indef
pointer	im, pm, data, bp

bool	pm_linenotempty()
pointer	mscl2s(), zt_fpvals()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (mscl2s (mg, line))

	# Initialize
	im = MG_IM(mg)
	pm = FP_PM(fp)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	ncols = FP_NCOLS(fp)
	call amovkl (long(1), v, IM_MAXDIM)
	v[2] = line

	# If there might be column interpolation initialize value arrays.
	if (ncols > 0 && FP_PV1(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_SHORT
	    call malloc (FP_PV1(fp), ncols, FP_PIXTYPE(fp))
	    call malloc (FP_PV2(fp), ncols, FP_PIXTYPE(fp))
	    indef = INDEFS
	    call amovks (indef, Mems[FP_V1(fp,1)], ncols)
	    call amovks (indef, Mems[FP_V2(fp,1)], ncols)
	}

	# If there are no bad pixels in the line and the line contains
	# no column interpolation endpoints return the data directly.
	# Otherwise get the line and fill in any endpoints that may
	# be used later.

	if (!pm_linenotempty (pm, v)) {
	    if (line < FP_LMIN(fp) || line > FP_LMAX(fp))
		return (mscl2s (mg, line))
	    else
		return (zt_fpvals (fp, mg, line))
	}

	# Get the pixel mask.
	call malloc (bp, nc, TY_SHORT)
	call pmglps (pm, v, Mems[bp], 0, nc, PIX_SRC)
	bp = bp - 1

	# Check if any column interpolation endpoints are needed and
	# set them.  Set any other endpoints on the same lines at
	# the same time.

	if (line >= FP_LMIN(fp) && line < FP_LMAX(fp)) {
	    j = 1
	    do i = col1, col2 {
		if (Mems[bp+i] == FP_CVAL(fp)) {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			if (line>FP_L1(fp,j) && line<FP_L2(fp,j)) {
			    if (IS_INDEFS(Mems[FP_V1(fp,j)]))
				data = zt_fpvals (fp, im, FP_L1(fp,j))
			    if (IS_INDEFS(Mems[FP_V2(fp,j)]))
				data = zt_fpvals (fp, im, FP_L2(fp,j))
			}
		    }
		}
	    }
	}

	# Fix pixels by column or line interpolation.
	if (FP_DATA(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_SHORT
	    call malloc (FP_DATA(fp), nc, FP_PIXTYPE(fp))
	}
	data = FP_DATA(fp)
	call amovs (Mems[zt_fpvals(fp,im,line)], Mems[data], nc)
	j = 1
	for (c1=col1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
	    ;
	while (c1 <= col2) {
	    c1 = c1 - 1
	    for (c2=c1+2; c2<=col2 && Mems[bp+c2]!=0; c2=c2+1)
		;
	    a = INDEFS
	    do i = c1+1, c2-1 {
		if (Mems[bp+i] == FP_LVAL(fp)) {
		    if (IS_INDEFS(a)) {
			if (c1 < col1 && c2 > col2) {
			    c1 = c2 + 1
			    next
			}
			if (c1 >= col1)
			    a = Mems[data+c1-1]
			else
			    a = Mems[data+c2-1]
			if (c2 <= col2)
			    b = (Mems[data+c2-1] - a) / (c2 - c1)
			else
			    b = 0.
		    }
		    val = a + b * (i - c1)
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargs (Mems[data+i-1])
			    call pargr (val)
			if (c1 >= col1) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (c1)
			    call pargi (line)
			}
			if (c2 <= col2) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (c2)
			    call pargi (line)
			}
			call fprintf (fd, "\n")
		    }
		} else {
		    for (; j<ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<ncols && FP_COL(fp,j)==i; j=j+1) {
			l1 = FP_L1(fp,j)
			l2 = FP_L2(fp,j)
			if (l1 < line1 && l2 > line2)
			    next
			if (line > l1 && line < l2) {
			    if (l1 >= line1)
				c = Mems[FP_V1(fp,j)]
			    else
				c = Mems[FP_V2(fp,j)]
			    if (l2 <= line2) {
				d = (Mems[FP_V2(fp,j)] - c) / (l2 - l1)
				val = c + d * (line - l1)
			    } else
				val = c
			    l3 = l1
			    l4 = l2
			}
		    }
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargs (Mems[data+i-1])
			    call pargr (val)
			if (l1 >= line1) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (i)
			    call pargi (l3)
			}
			if (l2 <= line2) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (i)
			    call pargi (l4)
			}
			call fprintf (fd, "\n")
		    }
		}
		Mems[data+i-1] = nint (val)
	    }
	    for (c1=c2+1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
		;
	}

	call mfree (bp, TY_SHORT)
	return (data)
end


# XT_FPVAL -- Get data for the specified line and set the values for
# all column interpolation endpoints which occur at that line.

pointer procedure zt_fpvals (fp, mg, line)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line

int	i
pointer	im, data, mscl2s()

begin
	# Set out of bounds values to 0.  These are not used but we need
	# to cancel the INDEF values.
	im = MG_IM(mg)
	if (line < 1 || line > IM_LEN(im,2)) {
	    do i = 1, FP_NCOLS(fp) {
		if (line == FP_L1(fp,i))
		    Mems[FP_V1(fp,i)] = 0.
		else if (line == FP_L2(fp,i))
		    Mems[FP_V2(fp,i)] = 0.
	    }
	    return (NULL)
	}

	data = mscl2s (mg, line)
	do i = 1, FP_NCOLS(fp) {
	    if (line == FP_L1(fp,i))
		Mems[FP_V1(fp,i)] = Mems[data+FP_COL(fp,i)-1]
	    else if (line == FP_L2(fp,i))
		Mems[FP_V2(fp,i)] = Mems[data+FP_COL(fp,i)-1]
	}

	return (data)
end

# SI_SAMPLE -- Resample a line via nearest neighbor, rather than linear
# interpolation (ALUI).  The calling sequence is the same as for ALUII.

procedure zi_samples (a, b, x, npix)

short	a[ARB], b[ARB]		# input, output data arrays
real	x[ARB]			# sample grid
int	npix, i

begin
	do i = 1, npix
	    b[i] = a[int(x[i])]
end



# SIGM2 -- Get a line of data from a scaled image.  Block averaging is
# done by a subprocedure; this procedure gets a line from a possibly block
# averaged image and if necessary interpolates it to the grid points of the
# output line.

pointer procedure zigm2i (mg, si, lineno)

pointer	mg		# pointer to MOSGEOM descriptor
pointer	si		# pointer to SI descriptor
int	lineno

pointer	rawline, tempp, gp
int	i, new_y[2], tempi, curbuf, altbuf
int	nraw, npix, nblks_y, ybavg, x1, x2
real	x, y, weight_1, weight_2
pointer	zi_blmavgi()
errchk	zi_blmavgi

begin
	nraw = IM_LEN(SI_IM(si))
	npix = SI_NPIX(si,1)

	# Determine the range of X (in pixels on the block averaged input image)
	# required for the interpolator.

	gp = SI_GRID(si,1)
	x1 = SI_XOFF(si)
	x = Memr[gp+npix-1]
	x2 = x1 + int(x)
	if (INTVAL(x,SI_TOL))
	    x2 = x2 - 1
	x2 = max (x1 + 1, x2)

	gp = SI_GRID(si,2)
	y = Memr[gp+lineno-1]

	# The following is an optimization provided for the case when it is
	# not necessary to interpolate in either X or Y.  Block averaging is
	# permitted.

	if (SI_INTERP(si,1) == NO && SI_INTERP(si,2) == NO)
	    return (zi_blmavgi (mg, SI_IM(si), SI_FP(si), x1, x2, int(y),
		SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si)))

	# If we are interpolating in Y two buffers are required, one for each
	# of the two input image lines required to interpolate in Y.  The lines
	# stored in these buffers are interpolated in X to the output grid but
	# not in Y.  Both buffers are not required if we are not interpolating
	# in Y, but we use them anyhow to simplify the code.

	if (SI_INIT(si) == YES) {
	    do i = 1, 2 {
		if (SI_BUF(si,i) != NULL)
		    call mfree (SI_BUF(si,i), SI_TYBUF(si))
		call malloc (SI_BUF(si,i), npix, TY_INT)
		SI_TYBUF(si) = TY_INT
		SI_BUFY(si,i) = NOTSET
	    }
	    if (OUTBUF(si) != NULL)
		call mfree (OUTBUF(si), SI_TYBUF(si))
	    call malloc (OUTBUF(si), npix, TY_INT)
	    SI_INIT(si) = NO
	}

	# If the Y value of the new line is not in range of the contents of the
	# current line buffers, refill one or both buffers.  To refill we must
	# read a (possibly block averaged) input line and interpolate it onto
	# the X grid.  The X and Y values herein are in the coordinate system
	# of the (possibly block averaged) input image.

	new_y[1] = int(y)
	new_y[2] = int(y) + 1

	# Get the pair of lines whose integral Y values form an interval
	# containing the fractional Y value of the output line.  Sometimes the
	# desired line will happen to be in the other buffer already, in which
	# case we just have to swap buffers.  Often the new line will be the
	# current line, in which case nothing is done.  This latter case occurs
	# frequently when the magnification ratio is large.

	curbuf = 1
	altbuf = 2

	do i = 1, 2 {
	    if (new_y[i] == SI_BUFY(si,i)) {
		;
	    } else if (new_y[i] == SI_BUFY(si,altbuf)) {
		SWAPP (SI_BUF(si,1), SI_BUF(si,2))
		SWAPI (SI_BUFY(si,1), SI_BUFY(si,2))

	    } else {
		# Get line and interpolate onto output grid.  If interpolation
		# is not required merely copy data out.  This code is set up
		# to always use two buffers; in effect, there is one buffer of
		# look ahead, even when Y[i] is integral.  This means that we
		# will go out of bounds by one line at the top of the image.
		# This is handled by copying the last line.

		ybavg = SI_BAVG(si,2)
		nblks_y = (IM_LEN (SI_IM(si), 2) + ybavg-1) / ybavg
		if (new_y[i] <= nblks_y)
		    rawline = zi_blmavgi (mg, SI_IM(si), SI_FP(si), x1, x2,
			new_y[i], SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si))

		if (SI_INTERP(si,1) == NO) {
		    call amovi (Memi[rawline], Memi[SI_BUF(si,i)], npix)
		} else if (SI_ORDER(si) == 0) {
		    call zi_samplei (Memi[rawline], Memi[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		} else if (SI_ORDER(si) == -1) {
		    call zi_maxi (Memi[rawline], nraw,
			Memr[SI_GRID(si,1)], Memi[SI_BUF(si,i)], npix)
		} else {
		    call aluii (Memi[rawline], Memi[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		}

		SI_BUFY(si,i) = new_y[i]
	    }

	    SWAPI (altbuf, curbuf)
	}

	# We now have two line buffers straddling the output Y value,
	# interpolated to the X grid of the output line.  To complete the
	# bilinear interpolation operation we take a weighted sum of the two
	# lines.  If the range from SI_BUFY(si,1) to SI_BUFY(si,2) is repeatedly
	# interpolated in Y no additional i/o occurs and the linear
	# interpolation operation (ALUI) does not have to be repeated (only the
	# weighted sum is required).  If the distance of Y from one of the
	# buffers is zero then we do not even have to take a weighted sum.
	# This is not unusual because we may be called with a magnification
	# of 1.0 in Y.

	weight_1 = 1.0 - (y - SI_BUFY(si,1))
	weight_2 = 1.0 - weight_1

	if (weight_1 < SI_TOL)
	    return (SI_BUF(si,2))
	else if (weight_2 < SI_TOL || SI_ORDER(si) == 0) 
	    return (SI_BUF(si,1))
	else if (SI_ORDER(si) == -1) {
	    call amaxi (Memi[SI_BUF(si,1)], Memi[SI_BUF(si,2)],
		Memi[OUTBUF(si)], npix)
	    return (OUTBUF(si))
	} else {
	    call awsui (Memi[SI_BUF(si,1)], Memi[SI_BUF(si,2)],
		Memi[OUTBUF(si)], npix, weight_1, weight_2)
	    return (OUTBUF(si))
	}
end


# SI_BLMAVG -- Get a line from a block averaged image of type short.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  If the length of an axis
# is not an integral multiple of the block size then the last pixel in the
# last block will be replicated to fill out the block; the average is still
# defined even if a block is not full.

pointer procedure zi_blmavgi (mg, im, fp, x1, x2, y, xbavg, ybavg, order)

pointer	mg			# mosgeom descriptor
pointer	im			# input image
pointer	fp			# fixpix structure
int	x1, x2			# range of x blocks to be read
int	y			# y block to be read
int	xbavg, ybavg		# X and Y block averaging factors
int	order			# averaging option

real	sum
int	blkmax
pointer	sp, a, b
int	nblks_x, nblks_y, ncols, nlines, xoff, blk1, blk2, i, j, k
int	first_line, nlines_in_sum, npix, nfull_blks, count
pointer	zt_fpi()
errchk	zt_fpi

begin
	call smark (sp)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xoff   = (x1 - 1) * xbavg + 1
	npix   = min (ncols, xoff + (x2 - x1 + 1) * xbavg - 1) - xoff + 1

	if ((xbavg < 1) || (ybavg < 1))
	    call error (1, "zi_blmavg: illegal block size")
	else if (x1 < 1 || x2 > ncols)
	    call error (2, "zi_blmavg: column index out of bounds")
	else if ((xbavg == 1) && (ybavg == 1))
	    return (zt_fpi (fp, mg, y, NULL) + xoff - 1)

	nblks_x = (npix   + xbavg-1) / xbavg
	nblks_y = (nlines + ybavg-1) / ybavg

	if (y < 1 || y > nblks_y)
	    call error (2, "zi_blmavg: block number out of range")

	if (ybavg > 1) {
	    call salloc (b, nblks_x, TY_LONG)
	    call aclrl (Meml[b], nblks_x)
	    nlines_in_sum = 0
	}

	# Read and accumulate all input lines in the block.
	first_line = (y - 1) * ybavg + 1

	do i = first_line, min (nlines, first_line + ybavg - 1) {
	    # Get line from input image.
	    a = zt_fpi (fp, mg, i, NULL) + xoff - 1

	    # Block average line in X.
	    if (xbavg > 1) {
		# First block average only the full blocks.
		nfull_blks = npix / xbavg
		if (order == -1) {
		    blk1 = a
		    do j = 1, nfull_blks {
			blk2 = blk1 + xbavg
			blkmax = Memi[blk1]
			do k = blk1+1, blk2-1
			    blkmax = max (blkmax, Memi[k])
			Memi[a+j-1] = blkmax
			blk1 = blk2
		    }
		} else
		    call abavi (Memi[a], Memi[a], nfull_blks, xbavg)

		# Now average the final partial block, if any.
		if (nfull_blks < nblks_x) {
		    if (order == -1) {
			blkmax = Memi[blk1]
			do k = blk1+1, a+npix-1
			    blkmax = max (blkmax, Memi[k])
			Memi[a+j-1] = blkmax
		    } else {
			sum = 0.0
			count = 0
			do j = nfull_blks * xbavg + 1, npix {
			    sum = sum + Memi[a+j-1]
			    count = count + 1
			}
			Memi[a+nblks_x-1] = sum / count
		    }
		}
	    }

	    # Add line into block sum.  Keep track of number of lines in sum
	    # so that we can compute block average later.

	    if (ybavg > 1) {
		if (order == -1) {
		    do j = 0, nblks_x-1
			Meml[b+j] = max (Meml[b+j], long (Memi[a+j]))
		} else {
		    do j = 0, nblks_x-1
			Meml[b+j] = Meml[b+j] + Memi[a+j]
		    nlines_in_sum = nlines_in_sum + 1
		}
	    }
	}

	# Compute the block average in Y from the sum of all lines block
	# averaged in X.  Overwrite buffer A, the buffer returned by IMIO.
	# This is kosher because the block averaged line is never longer
	# than an input line.

	if (ybavg > 1) {
	    if (order == -1) {
		do i = 0, nblks_x-1
		    Memi[a+i] = Meml[b+i]
	    } else {
		do i = 0, nblks_x-1
		    Memi[a+i] = Meml[b+i] / real(nlines_in_sum)
	    }
	}

	call sfree (sp)
	return (a)
end


# SI_MAXS -- Resample a line via maximum value.

procedure zi_maxi (a, na, x, b, nb)

int	a[na]			# input array
int	na			# input size
real	x[nb]			# sample grid
int	b[nb]			# output arrays
int	nb			# output size

int	i

begin
	do i = 1, nb
	    b[i] = max (a[int(x[i])], a[min(na,int(x[i]+1))])
end


# XT_FP -- Get the specified line of image data and replace bad pixels by
# interpolation.

pointer procedure zt_fpi (fp, mg, line, fd)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

pointer	im, mscl2i(), zt_fpsi()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (mscl2i (mg, line))

	im = MG_IM(mg)
	col1 = 1
	col2 = IM_LEN(im,1)
	line1 = 1
	line2 = IM_LEN(im,2)

	return (zt_fpsi (fp, mg, line, col1, col2, line1, line2, fd))
end


# XT_FXS -- Get the specified line of image data and replace bad pixels by
# interpolation within a specified section.

pointer procedure zt_fpsi (fp, mg, line, col1, col2, line1, line2, fd)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

int	i, j, nc, nl, ncols, c1, c2, l1, l2, l3, l4
long	v[IM_MAXDIM]
real	a, b, c, d, val
int	indef
pointer	im, pm, data, bp

bool	pm_linenotempty()
pointer	mscl2i(), zt_fpvali()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (mscl2i (mg, line))

	# Initialize
	im = MG_IM(mg)
	pm = FP_PM(fp)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	ncols = FP_NCOLS(fp)
	call amovkl (long(1), v, IM_MAXDIM)
	v[2] = line

	# If there might be column interpolation initialize value arrays.
	if (ncols > 0 && FP_PV1(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_INT
	    call malloc (FP_PV1(fp), ncols, FP_PIXTYPE(fp))
	    call malloc (FP_PV2(fp), ncols, FP_PIXTYPE(fp))
	    indef = INDEFI
	    call amovki (indef, Memi[FP_V1(fp,1)], ncols)
	    call amovki (indef, Memi[FP_V2(fp,1)], ncols)
	}

	# If there are no bad pixels in the line and the line contains
	# no column interpolation endpoints return the data directly.
	# Otherwise get the line and fill in any endpoints that may
	# be used later.

	if (!pm_linenotempty (pm, v)) {
	    if (line < FP_LMIN(fp) || line > FP_LMAX(fp))
		return (mscl2i (mg, line))
	    else
		return (zt_fpvali (fp, mg, line))
	}

	# Get the pixel mask.
	call malloc (bp, nc, TY_SHORT)
	call pmglps (pm, v, Mems[bp], 0, nc, PIX_SRC)
	bp = bp - 1

	# Check if any column interpolation endpoints are needed and
	# set them.  Set any other endpoints on the same lines at
	# the same time.

	if (line >= FP_LMIN(fp) && line < FP_LMAX(fp)) {
	    j = 1
	    do i = col1, col2 {
		if (Mems[bp+i] == FP_CVAL(fp)) {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			if (line>FP_L1(fp,j) && line<FP_L2(fp,j)) {
			    if (IS_INDEFI(Memi[FP_V1(fp,j)]))
				data = zt_fpvali (fp, im, FP_L1(fp,j))
			    if (IS_INDEFI(Memi[FP_V2(fp,j)]))
				data = zt_fpvali (fp, im, FP_L2(fp,j))
			}
		    }
		}
	    }
	}

	# Fix pixels by column or line interpolation.
	if (FP_DATA(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_INT
	    call malloc (FP_DATA(fp), nc, FP_PIXTYPE(fp))
	}
	data = FP_DATA(fp)
	call amovi (Memi[zt_fpvali(fp,im,line)], Memi[data], nc)
	j = 1
	for (c1=col1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
	    ;
	while (c1 <= col2) {
	    c1 = c1 - 1
	    for (c2=c1+2; c2<=col2 && Mems[bp+c2]!=0; c2=c2+1)
		;
	    a = INDEFI
	    do i = c1+1, c2-1 {
		if (Mems[bp+i] == FP_LVAL(fp)) {
		    if (IS_INDEFI(a)) {
			if (c1 < col1 && c2 > col2) {
			    c1 = c2 + 1
			    next
			}
			if (c1 >= col1)
			    a = Memi[data+c1-1]
			else
			    a = Memi[data+c2-1]
			if (c2 <= col2)
			    b = (Memi[data+c2-1] - a) / (c2 - c1)
			else
			    b = 0.
		    }
		    val = a + b * (i - c1)
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargi (Memi[data+i-1])
			    call pargr (val)
			if (c1 >= col1) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (c1)
			    call pargi (line)
			}
			if (c2 <= col2) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (c2)
			    call pargi (line)
			}
			call fprintf (fd, "\n")
		    }
		} else {
		    for (; j<ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<ncols && FP_COL(fp,j)==i; j=j+1) {
			l1 = FP_L1(fp,j)
			l2 = FP_L2(fp,j)
			if (l1 < line1 && l2 > line2)
			    next
			if (line > l1 && line < l2) {
			    if (l1 >= line1)
				c = Memi[FP_V1(fp,j)]
			    else
				c = Memi[FP_V2(fp,j)]
			    if (l2 <= line2) {
				d = (Memi[FP_V2(fp,j)] - c) / (l2 - l1)
				val = c + d * (line - l1)
			    } else
				val = c
			    l3 = l1
			    l4 = l2
			}
		    }
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargi (Memi[data+i-1])
			    call pargr (val)
			if (l1 >= line1) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (i)
			    call pargi (l3)
			}
			if (l2 <= line2) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (i)
			    call pargi (l4)
			}
			call fprintf (fd, "\n")
		    }
		}
		Memi[data+i-1] = nint (val)
	    }
	    for (c1=c2+1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
		;
	}

	call mfree (bp, TY_SHORT)
	return (data)
end


# XT_FPVAL -- Get data for the specified line and set the values for
# all column interpolation endpoints which occur at that line.

pointer procedure zt_fpvali (fp, mg, line)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line

int	i
pointer	im, data, mscl2i()

begin
	# Set out of bounds values to 0.  These are not used but we need
	# to cancel the INDEF values.
	im = MG_IM(mg)
	if (line < 1 || line > IM_LEN(im,2)) {
	    do i = 1, FP_NCOLS(fp) {
		if (line == FP_L1(fp,i))
		    Memi[FP_V1(fp,i)] = 0.
		else if (line == FP_L2(fp,i))
		    Memi[FP_V2(fp,i)] = 0.
	    }
	    return (NULL)
	}

	data = mscl2i (mg, line)
	do i = 1, FP_NCOLS(fp) {
	    if (line == FP_L1(fp,i))
		Memi[FP_V1(fp,i)] = Memi[data+FP_COL(fp,i)-1]
	    else if (line == FP_L2(fp,i))
		Memi[FP_V2(fp,i)] = Memi[data+FP_COL(fp,i)-1]
	}

	return (data)
end

# SI_SAMPLE -- Resample a line via nearest neighbor, rather than linear
# interpolation (ALUI).  The calling sequence is the same as for ALUII.

procedure zi_samplei (a, b, x, npix)

int	a[ARB], b[ARB]		# input, output data arrays
real	x[ARB]			# sample grid
int	npix, i

begin
	do i = 1, npix
	    b[i] = a[int(x[i])]
end



# SIGM2 -- Get a line of data from a scaled image.  Block averaging is
# done by a subprocedure; this procedure gets a line from a possibly block
# averaged image and if necessary interpolates it to the grid points of the
# output line.

pointer procedure zigm2r (mg, si, lineno)

pointer	mg		# pointer to MOSGEOM descriptor
pointer	si		# pointer to SI descriptor
int	lineno

pointer	rawline, tempp, gp
int	i, new_y[2], tempi, curbuf, altbuf
int	nraw, npix, nblks_y, ybavg, x1, x2
real	x, y, weight_1, weight_2
pointer	zi_blmavgr()
errchk	zi_blmavgr

begin
	nraw = IM_LEN(SI_IM(si))
	npix = SI_NPIX(si,1)

	# Determine the range of X (in pixels on the block averaged input image)
	# required for the interpolator.

	gp = SI_GRID(si,1)
	x1 = SI_XOFF(si)
	x = Memr[gp+npix-1]
	x2 = x1 + int(x)
	if (INTVAL(x,SI_TOL))
	    x2 = x2 - 1
	x2 = max (x1 + 1, x2)

	gp = SI_GRID(si,2)
	y = Memr[gp+lineno-1]

	# The following is an optimization provided for the case when it is
	# not necessary to interpolate in either X or Y.  Block averaging is
	# permitted.

	if (SI_INTERP(si,1) == NO && SI_INTERP(si,2) == NO)
	    return (zi_blmavgr (mg, SI_IM(si), SI_FP(si), x1, x2, int(y),
		SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si)))

	# If we are interpolating in Y two buffers are required, one for each
	# of the two input image lines required to interpolate in Y.  The lines
	# stored in these buffers are interpolated in X to the output grid but
	# not in Y.  Both buffers are not required if we are not interpolating
	# in Y, but we use them anyhow to simplify the code.

	if (SI_INIT(si) == YES) {
	    do i = 1, 2 {
		if (SI_BUF(si,i) != NULL)
		    call mfree (SI_BUF(si,i), SI_TYBUF(si))
		call malloc (SI_BUF(si,i), npix, TY_REAL)
		SI_TYBUF(si) = TY_REAL
		call mfree (SI_BUF(si,i), SI_TYBUF(si))
		call malloc (SI_BUF(si,i), npix, TY_REAL)
		SI_TYBUF(si) = TY_REAL
		SI_BUFY(si,i) = NOTSET
	    }
	    if (OUTBUF(si) != NULL)
		call mfree (OUTBUF(si), SI_TYBUF(si))
	    call malloc (OUTBUF(si), npix, TY_REAL)
	    SI_INIT(si) = NO
	}

	# If the Y value of the new line is not in range of the contents of the
	# current line buffers, refill one or both buffers.  To refill we must
	# read a (possibly block averaged) input line and interpolate it onto
	# the X grid.  The X and Y values herein are in the coordinate system
	# of the (possibly block averaged) input image.

	new_y[1] = int(y)
	new_y[2] = int(y) + 1

	# Get the pair of lines whose integral Y values form an interval
	# containing the fractional Y value of the output line.  Sometimes the
	# desired line will happen to be in the other buffer already, in which
	# case we just have to swap buffers.  Often the new line will be the
	# current line, in which case nothing is done.  This latter case occurs
	# frequently when the magnification ratio is large.

	curbuf = 1
	altbuf = 2

	do i = 1, 2 {
	    if (new_y[i] == SI_BUFY(si,i)) {
		;
	    } else if (new_y[i] == SI_BUFY(si,altbuf)) {
		SWAPP (SI_BUF(si,1), SI_BUF(si,2))
		SWAPI (SI_BUFY(si,1), SI_BUFY(si,2))

	    } else {
		# Get line and interpolate onto output grid.  If interpolation
		# is not required merely copy data out.  This code is set up
		# to always use two buffers; in effect, there is one buffer of
		# look ahead, even when Y[i] is integral.  This means that we
		# will go out of bounds by one line at the top of the image.
		# This is handled by copying the last line.

		ybavg = SI_BAVG(si,2)
		nblks_y = (IM_LEN (SI_IM(si), 2) + ybavg-1) / ybavg
		if (new_y[i] <= nblks_y)
		    rawline = zi_blmavgr (mg, SI_IM(si), SI_FP(si), x1, x2,
			new_y[i], SI_BAVG(si,1), SI_BAVG(si,2), SI_ORDER(si))

		if (SI_INTERP(si,1) == NO) {
		    call amovr (Memr[rawline], Memr[SI_BUF(si,i)], npix)
		} else if (SI_ORDER(si) == 0) {
		    call zi_sampler (Memr[rawline], Memr[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		} else if (SI_ORDER(si) == -1) {
		    call zi_maxr (Memr[rawline], nraw,
			Memr[SI_GRID(si,1)], Memr[SI_BUF(si,i)], npix)
		} else {
		    call aluir (Memr[rawline], Memr[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		}

		SI_BUFY(si,i) = new_y[i]
	    }

	    SWAPI (altbuf, curbuf)
	}

	# We now have two line buffers straddling the output Y value,
	# interpolated to the X grid of the output line.  To complete the
	# bilinear interpolation operation we take a weighted sum of the two
	# lines.  If the range from SI_BUFY(si,1) to SI_BUFY(si,2) is repeatedly
	# interpolated in Y no additional i/o occurs and the linear
	# interpolation operation (ALUI) does not have to be repeated (only the
	# weighted sum is required).  If the distance of Y from one of the
	# buffers is zero then we do not even have to take a weighted sum.
	# This is not unusual because we may be called with a magnification
	# of 1.0 in Y.

	weight_1 = 1.0 - (y - SI_BUFY(si,1))
	weight_2 = 1.0 - weight_1

	if (weight_1 < SI_TOL)
	    return (SI_BUF(si,2))
	else if (weight_2 < SI_TOL || SI_ORDER(si) == 0) 
	    return (SI_BUF(si,1))
	else if (SI_ORDER(si) == -1) {
	    call amaxr (Memr[SI_BUF(si,1)], Memr[SI_BUF(si,2)],
		Memr[OUTBUF(si)], npix)
	    return (OUTBUF(si))
	} else {
	    call awsur (Memr[SI_BUF(si,1)], Memr[SI_BUF(si,2)],
		Memr[OUTBUF(si)], npix, weight_1, weight_2)
	    return (OUTBUF(si))
	}
end


# SI_BLMAVG -- Get a line from a block averaged image of type short.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  If the length of an axis
# is not an integral multiple of the block size then the last pixel in the
# last block will be replicated to fill out the block; the average is still
# defined even if a block is not full.

pointer procedure zi_blmavgr (mg, im, fp, x1, x2, y, xbavg, ybavg, order)

pointer	mg			# mosgeom descriptor
pointer	im			# input image
pointer	fp			# fixpix structure
int	x1, x2			# range of x blocks to be read
int	y			# y block to be read
int	xbavg, ybavg		# X and Y block averaging factors
int	order			# averaging option

real	sum
real	blkmax
pointer	sp, a, b
int	nblks_x, nblks_y, ncols, nlines, xoff, blk1, blk2, i, j, k
int	first_line, nlines_in_sum, npix, nfull_blks, count
pointer	zt_fpr()
errchk	zt_fpr

begin
	call smark (sp)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xoff   = (x1 - 1) * xbavg + 1
	npix   = min (ncols, xoff + (x2 - x1 + 1) * xbavg - 1) - xoff + 1

	if ((xbavg < 1) || (ybavg < 1))
	    call error (1, "zi_blmavg: illegal block size")
	else if (x1 < 1 || x2 > ncols)
	    call error (2, "zi_blmavg: column index out of bounds")
	else if ((xbavg == 1) && (ybavg == 1))
	    return (zt_fpr (fp, mg, y, NULL) + xoff - 1)

	nblks_x = (npix   + xbavg-1) / xbavg
	nblks_y = (nlines + ybavg-1) / ybavg

	if (y < 1 || y > nblks_y)
	    call error (2, "zi_blmavg: block number out of range")

	if (ybavg > 1) {
	    call salloc (b, nblks_x, TY_REAL)
	    call aclrr (Memr[b], nblks_x)
	    nlines_in_sum = 0
	}

	# Read and accumulate all input lines in the block.
	first_line = (y - 1) * ybavg + 1

	do i = first_line, min (nlines, first_line + ybavg - 1) {
	    # Get line from input image.
	    a = zt_fpr (fp, mg, i, NULL) + xoff - 1

	    # Block average line in X.
	    if (xbavg > 1) {
		# First block average only the full blocks.
		nfull_blks = npix / xbavg
		if (order == -1) {
		    blk1 = a
		    do j = 1, nfull_blks {
			blk2 = blk1 + xbavg
			blkmax = Memr[blk1]
			do k = blk1+1, blk2-1
			    blkmax = max (blkmax, Memr[k])
			Memr[a+j-1] = blkmax
			blk1 = blk2
		    }
		} else
		    call abavr (Memr[a], Memr[a], nfull_blks, xbavg)

		# Now average the final partial block, if any.
		if (nfull_blks < nblks_x) {
		    if (order == -1) {
			blkmax = Memr[blk1]
			do k = blk1+1, a+npix-1
			    blkmax = max (blkmax, Memr[k])
			Memr[a+j-1] = blkmax
		    } else {
			sum = 0.0
			count = 0
			do j = nfull_blks * xbavg + 1, npix {
			    sum = sum + Memr[a+j-1]
			    count = count + 1
			}
			Memr[a+nblks_x-1] = sum / count
		    }
		}
	    }

	    # Add line into block sum.  Keep track of number of lines in sum
	    # so that we can compute block average later.
	    if (ybavg > 1) {
		if (order == -1)
		    call amaxr (Memr[a], Memr[b], Memr[b], nblks_x)
		else {
		    call aaddr (Memr[a], Memr[b], Memr[b], nblks_x)
		    nlines_in_sum = nlines_in_sum + 1
		}
	    }
	}

	# Compute the block average in Y from the sum of all lines block
	# averaged in X.  Overwrite buffer A, the buffer returned by IMIO.
	# This is kosher because the block averaged line is never longer
	# than an input line.

	if (ybavg > 1) {
	    if (order == -1)
		call amovr (Memr[b], Memr[a], nblks_x)
	    else
		call adivkr (Memr[b], real(nlines_in_sum), Memr[a], nblks_x)
	}

	call sfree (sp)
	return (a)
end


# SI_MAXS -- Resample a line via maximum value.

procedure zi_maxr (a, na, x, b, nb)

real	a[na]			# input array
int	na			# input size
real	x[nb]			# sample grid
real	b[nb]			# output arrays
int	nb			# output size

int	i

begin
	do i = 1, nb
	    b[i] = max (a[int(x[i])], a[min(na,int(x[i]+1))])
end


# XT_FP -- Get the specified line of image data and replace bad pixels by
# interpolation.

pointer procedure zt_fpr (fp, mg, line, fd)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

pointer	im, mscl2r(), zt_fpsr()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (mscl2r (mg, line))

	im = MG_IM(mg)
	col1 = 1
	col2 = IM_LEN(im,1)
	line1 = 1
	line2 = IM_LEN(im,2)

	return (zt_fpsr (fp, mg, line, col1, col2, line1, line2, fd))
end


# XT_FXS -- Get the specified line of image data and replace bad pixels by
# interpolation within a specified section.

pointer procedure zt_fpsr (fp, mg, line, col1, col2, line1, line2, fd)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

int	i, j, nc, nl, ncols, c1, c2, l1, l2, l3, l4
long	v[IM_MAXDIM]
real	a, b, c, d, val
real	indef
pointer	im, pm, data, bp

bool	pm_linenotempty()
pointer	mscl2r(), zt_fpvalr()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (mscl2r (mg, line))

	# Initialize
	im = MG_IM(mg)
	pm = FP_PM(fp)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	ncols = FP_NCOLS(fp)
	call amovkl (long(1), v, IM_MAXDIM)
	v[2] = line

	# If there might be column interpolation initialize value arrays.
	if (ncols > 0 && FP_PV1(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_REAL
	    call malloc (FP_PV1(fp), ncols, FP_PIXTYPE(fp))
	    call malloc (FP_PV2(fp), ncols, FP_PIXTYPE(fp))
	    indef = INDEFR
	    call amovkr (indef, Memr[FP_V1(fp,1)], ncols)
	    call amovkr (indef, Memr[FP_V2(fp,1)], ncols)
	}

	# If there are no bad pixels in the line and the line contains
	# no column interpolation endpoints return the data directly.
	# Otherwise get the line and fill in any endpoints that may
	# be used later.

	if (!pm_linenotempty (pm, v)) {
	    if (line < FP_LMIN(fp) || line > FP_LMAX(fp))
		return (mscl2r (mg, line))
	    else
		return (zt_fpvalr (fp, mg, line))
	}

	# Get the pixel mask.
	call malloc (bp, nc, TY_SHORT)
	call pmglps (pm, v, Mems[bp], 0, nc, PIX_SRC)
	bp = bp - 1

	# Check if any column interpolation endpoints are needed and
	# set them.  Set any other endpoints on the same lines at
	# the same time.

	if (line >= FP_LMIN(fp) && line < FP_LMAX(fp)) {
	    j = 1
	    do i = col1, col2 {
		if (Mems[bp+i] == FP_CVAL(fp)) {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			if (line>FP_L1(fp,j) && line<FP_L2(fp,j)) {
			    if (IS_INDEFR(Memr[FP_V1(fp,j)]))
				data = zt_fpvalr (fp, im, FP_L1(fp,j))
			    if (IS_INDEFR(Memr[FP_V2(fp,j)]))
				data = zt_fpvalr (fp, im, FP_L2(fp,j))
			}
		    }
		}
	    }
	}

	# Fix pixels by column or line interpolation.
	if (FP_DATA(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_REAL
	    call malloc (FP_DATA(fp), nc, FP_PIXTYPE(fp))
	}
	data = FP_DATA(fp)
	call amovr (Memr[zt_fpvalr(fp,im,line)], Memr[data], nc)
	j = 1
	for (c1=col1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
	    ;
	while (c1 <= col2) {
	    c1 = c1 - 1
	    for (c2=c1+2; c2<=col2 && Mems[bp+c2]!=0; c2=c2+1)
		;
	    a = INDEFR
	    do i = c1+1, c2-1 {
		if (Mems[bp+i] == FP_LVAL(fp)) {
		    if (IS_INDEFR(a)) {
			if (c1 < col1 && c2 > col2) {
			    c1 = c2 + 1
			    next
			}
			if (c1 >= col1)
			    a = Memr[data+c1-1]
			else
			    a = Memr[data+c2-1]
			if (c2 <= col2)
			    b = (Memr[data+c2-1] - a) / (c2 - c1)
			else
			    b = 0.
		    }
		    val = a + b * (i - c1)
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargr (Memr[data+i-1])
			    call pargr (val)
			if (c1 >= col1) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (c1)
			    call pargi (line)
			}
			if (c2 <= col2) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (c2)
			    call pargi (line)
			}
			call fprintf (fd, "\n")
		    }
		} else {
		    for (; j<ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<ncols && FP_COL(fp,j)==i; j=j+1) {
			l1 = FP_L1(fp,j)
			l2 = FP_L2(fp,j)
			if (l1 < line1 && l2 > line2)
			    next
			if (line > l1 && line < l2) {
			    if (l1 >= line1)
				c = Memr[FP_V1(fp,j)]
			    else
				c = Memr[FP_V2(fp,j)]
			    if (l2 <= line2) {
				d = (Memr[FP_V2(fp,j)] - c) / (l2 - l1)
				val = c + d * (line - l1)
			    } else
				val = c
			    l3 = l1
			    l4 = l2
			}
		    }
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargr (Memr[data+i-1])
			    call pargr (val)
			if (l1 >= line1) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (i)
			    call pargi (l3)
			}
			if (l2 <= line2) {
			    call fprintf (fd, "%4d %4d")
			    call pargi (i)
			    call pargi (l4)
			}
			call fprintf (fd, "\n")
		    }
		}
		Memr[data+i-1] = val
	    }
	    for (c1=c2+1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
		;
	}

	call mfree (bp, TY_SHORT)
	return (data)
end


# XT_FPVAL -- Get data for the specified line and set the values for
# all column interpolation endpoints which occur at that line.

pointer procedure zt_fpvalr (fp, mg, line)

pointer	fp			#I FIXPIX pointer
pointer	mg			#I MOSGEOM pointer
int	line			#I Line

int	i
pointer	im, data, mscl2r()

begin
	# Set out of bounds values to 0.  These are not used but we need
	# to cancel the INDEF values.
	im = MG_IM(mg)
	if (line < 1 || line > IM_LEN(im,2)) {
	    do i = 1, FP_NCOLS(fp) {
		if (line == FP_L1(fp,i))
		    Memr[FP_V1(fp,i)] = 0.
		else if (line == FP_L2(fp,i))
		    Memr[FP_V2(fp,i)] = 0.
	    }
	    return (NULL)
	}

	data = mscl2r (mg, line)
	do i = 1, FP_NCOLS(fp) {
	    if (line == FP_L1(fp,i))
		Memr[FP_V1(fp,i)] = Memr[data+FP_COL(fp,i)-1]
	    else if (line == FP_L2(fp,i))
		Memr[FP_V2(fp,i)] = Memr[data+FP_COL(fp,i)-1]
	}

	return (data)
end

# SI_SAMPLE -- Resample a line via nearest neighbor, rather than linear
# interpolation (ALUI).  The calling sequence is the same as for ALUII.

procedure zi_sampler (a, b, x, npix)

real	a[ARB], b[ARB]		# input, output data arrays
real	x[ARB]			# sample grid
int	npix, i

begin
	do i = 1, npix
	    b[i] = a[int(x[i])]
end


