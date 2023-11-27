include	<imhdr.h>
include	<imset.h>


# CCDMEAN -- Compute mean and add to header if needed.

real procedure ccdmean (im)

pointer	im			# IMIO pointer

int	i, nc, nl, nmean, nsum, hdmgeti()
long	time, clktime()
bool	clgetb()
real	hdmgetr()
double	mean, sum, procmeanr()
pointer	imgl2r()

begin
	# Check if this operation has been done.
	ifnoerr (mean = hdmgetr (im, "ccdmean")) {
	    iferr (time = hdmgeti (im, "ccdmeant")) {
		time = IM_MTIME(im)
		call hdmputi (im, "ccdmeant", int (time))
	    }
	    if (time >= IM_MTIME(im)) {
		return (mean)
	    }
	}

	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Compute mean of image\n")
	    return (INDEF)
	}

	# Compute and record the mean.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	sum = 0.
	nsum = 0
	do i = 1, nl {
	    mean = procmeanr (Memr[imgl2r(im,i)], nc, 2., nmean)
	    sum = sum + nmean * mean
	    nsum = nsum + nmean
	}
	if (nsum > 0)
	    mean = sum / nsum
	else
	    mean = 1.
	time = clktime (long(0))
	call hdmputr (im, "ccdmean", real (mean))
	call hdmputi (im, "ccdmeant", int (time))
	call imseti (im, IM_WHEADER, YES)

	return (real (mean))
end
