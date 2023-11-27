# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include "mosgeom.h"

# MAXMIN -- Get sampled minimum and maximum pixel values in the TRIMSECTION
# of an image. The image is sampled on an even grid and the min and max
# values of this sample are returned.

procedure maxmin (im, mg, zmin, zmax, nsample_lines)

pointer	im			# IMIO pointer for input image
pointer	mg			# Mosgeom pointer for image
real	zmin, zmax		# min and max intensity values
int	nsample_lines		# amount of image to sample

int	start, step, ncols, nlines, sample_size, imlines, i
real	minval, maxval
pointer	mscl2r()
#include	"iis.com"

begin
	ncols  = TX2(mg) - TX1(mg) + 1
	nlines = TY2(mg) - TY1(mg) + 1

	zmin = MAX_REAL
	zmax = -MAX_REAL

	# Try to include a constant number of pixels in the sample
	# regardless of the image size.  The entire image is used if we
	# have a small image, and at least sample_lines lines are read
	# if we have a large image.

	sample_size = ncols * nsample_lines
	imlines = min(nlines, max(nsample_lines, sample_size / ncols))
	step = nlines / (imlines + 1)

	start = TY1(mg) + step / 2

	do i =  start, nlines, max (1, step) {
	    call alimr (Memr[mscl2r(mg,i)], ncols, minval, maxval)
	    zmin = min (zmin, minval)
	    zmax = max (zmax, maxval)
	}
end
