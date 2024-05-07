include	<imhdr.h>


# CTALK -- Apply crosstalk correction.
# This is the core of the crosstalk correction operation.  It is optimized
# for multiple dependencies by reading each required input line only once.
# Image I/O buffering may be optimized by setting IM_BUFSIZE externally to
# this routine.

procedure ctalks (stp, ins, outs, x1, x2, y1, y2, inbufs, bufs, coeffs,
	nimages, nc, nl)

pointer	stp				#I Symbol table
pointer	ins[nimages]			#I Input image pointers
pointer	outs[nimages]			#I Output image pointers
int	x1[nimages]			#I Starting pixel
int	x2[nimages]			#I Ending pixel
int	y1[nimages]			#I Starting pixel
int	y2[nimages]			#I Ending pixel
pointer	inbufs[nimages,2]		#I Working memory
pointer	bufs[nimages]			#I Working memory
real	coeffs[nimages]			#I Working memory
int	nimages				#I Number of images
int	nc, nl				#I Image dimensions

int	i, j, k, l, nx, ny
bool	xflip, yflip
pointer	sym, inbuf, outbuf, outbufs[2]
real	val

pointer	imgl2s(), impl2s(), imgs2s(), imps2s(), sthead(), stnext()

begin
	# Loop through each line and correct all dependent output images
	# at the same line.
	do l = 1, nl {

	    # The input lines have not be read initially.
	    call aclri (inbufs, 2*nimages)

	    # Loop through each output image.
	    for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {

		# Get input data buffers and coefficients.
		# Check if the output image requires any crosstalk correction.
		k = 0
		i = Memi[sym]
		do j = 1, nimages {
		    if (j == i || Memr[P2R(sym+j)] == 0.)
		       next
		    k = k + 1
		    xflip = (x1[j] > x2[j])
		    yflip = (y1[j] > y2[j])
		    coeffs[k] = Memr[P2R(sym+j)]
		    if (inbufs[j,1] == NULL) {
			if (xflip) {
			    if (yflip)
				inbufs[j,1] = imgs2s (ins[j], nc, 1,
				    nl-l+1, nl-l+1)
			    else
				inbufs[j,1] = imgs2s (ins[j], nc, 1, l, l)
			    inbufs[j,2] = inbufs[j,1] + nc - x1[j]
			} else {
			    if (yflip)
				inbufs[j,1] = imgl2s (ins[j], nl-l+1)
			    else
				inbufs[j,1] = imgl2s (ins[j], l)
			    inbufs[j,2] = inbufs[j,1] + x1[j] - 1
			}
		    }
		    bufs[k] = inbufs[j,2]
		}
		if (k == 0)
		    next

		nx = abs (x2[i] - x1[i]) + 1
		ny = abs (y2[i] - y1[i]) + 1

		xflip = (x1[i] > x2[i])
		yflip = (y1[i] > y2[i])
		if (inbufs[i,1] == NULL) {
		    if (xflip) {
			if (yflip)
			    inbufs[i,1] = imgs2s (ins[i], nc, 1,
			        nl-l+1, nl-l+1)
			else
			    inbufs[i,1] = imgs2s (ins[i], nc, 1, l, l)
			inbufs[i,2] = inbufs[i,1] + nc - x1[i]
		    } else {
			if (yflip)
			    inbufs[i,1] = imgl2s (ins[i], nl-l+1)
			else
			    inbufs[i,1] = imgl2s (ins[i], l)
			inbufs[i,2] = inbufs[i,1] + x1[i] - 1
		    }
		}
		if (xflip) {
		    if (yflip)
			outbufs[1] = imps2s (outs[i], nc, 1, nl-l+1, nl-l+1)
		    else
			outbufs[1] = imps2s (outs[i], nc, 1, l, l)
		    outbufs[2] = outbufs[1] + nc - x1[i]
		} else {
		    if (yflip)
			outbufs[1] = impl2s (outs[i], nl-l+1)
		    else
			outbufs[1] = impl2s (outs[i], l)
		    outbufs[2] = outbufs[1] + x1[i] - 1
		}

		# Preserve the data outside the data section.
		inbuf = inbufs[i,1]
		outbuf = outbufs[1]
		if (l < min (y1[i], y2[i]) || l > max (y1[i], y2[i])) {
		    call amovs (Mems[inbuf], Mems[outbuf], nc)
		    next
		}
		if (nx < nc) {
		    j = inbufs[i,2] - inbufs[i,1]
		    if (j > 0)
			call amovs (Mems[inbuf], Mems[outbuf], j)
		    j = (inbufs[i,1]+nc-1) - (inbufs[i,2]+nx-1)
		    if (j > 0)
			call amovs (Mems[inbuf+nc-j], Mems[outbuf+nc-j], j)
		}

		# Do the crosstalk correction.  Optimize for cases of
		# three or less source images with in-line operation.
		inbuf = inbufs[i,2]
		outbuf = outbufs[2]
		switch (k) {
		case 1:
		    do i = 0, nx-1 {
			Mems[outbuf+i] = nint (Mems[inbuf+i] -
			    coeffs[1] * Mems[bufs[1]+i])
		    }
		case 2:
		    do i = 0, nx-1 {
			Mems[outbuf+i] = nint (Mems[inbuf+i] -
			    coeffs[1] * Mems[bufs[1]+i] -
			    coeffs[2] * Mems[bufs[2]+i])
		    }
		case 3:
		    do i = 0, nx-1 {
			Mems[outbuf+i] = nint (Mems[inbuf+i] -
			    coeffs[1] * Mems[bufs[1]+i] -
			    coeffs[2] * Mems[bufs[2]+i] -
			    coeffs[3] * Mems[bufs[3]+i])
		    }
		default:
		    do i = 0, nx-1 {
			val = Mems[inbuf+i] -
			    coeffs[1] * Mems[bufs[1]+i] -
			    coeffs[2] * Mems[bufs[2]+i] -
			    coeffs[3] * Mems[bufs[3]+i]
			do k = 4, k
			    val = val - coeffs[k] * Mems[bufs[k]+i]
			Mems[outbuf+i] = nint (val)
		    }
		}

	    }
	}
end


# CTOB -- Apply crosstalk correction and create output bad pixel mask.
# This is the core of the crosstalk correction operation.  It is optimized
# for multiple dependencies by reading each required input line only once.
# Image I/O buffering may be optimized by setting IM_BUFSIZE externally to
# this routine.

procedure ctobs (stp, ins, outs, bpms, x1, x2, y1, y2, inbufs, bufs, coeffs,
	bpmthresh, nimages, nc, nl)

pointer	stp				#I Symbol table
pointer	ins[nimages]			#I Input image pointers
pointer	outs[nimages]			#I Output image pointers
pointer	bpms[nimages]			#I Output bpm pointers
int	x1[nimages]			#I Starting pixel
int	x2[nimages]			#I Ending pixel
int	y1[nimages]			#I Starting pixel
int	y2[nimages]			#I Ending pixel
pointer	inbufs[nimages,2]		#I Working memory
pointer	bufs[nimages]			#I Working memory
real	coeffs[nimages]			#I Working memory
real	bpmthresh			#I Bad pixel mask threshold
int	nimages				#I Number of images
int	nc, nl				#I Image dimensions

int	i, j, k, l, nx, ny
bool	xflip, yflip
pointer	sym, inbuf, outbuf, outbufs[2], bpmbuf, bpmbufs[2]
real	bpmval, val

pointer	imgl2s(), impl2s(), imgs2s(), imps2s(), sthead(), stnext()

begin
	bpmval = abs(bpmthresh)

	# Loop through each line and correct all dependent output images
	# at the same line.
	do l = 1, nl {

	    # The input lines have not be read initially.
	    call aclri (inbufs, 2*nimages)

	    # Loop through each output image.
	    for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {

		# Get input data buffers and coefficients.
		# Check if the output image requires any crosstalk correction.
		k = 0
		i = Memi[sym]
		do j = 1, nimages {
		    if (j == i || Memr[P2R(sym+j)] == 0.)
		       next
		    k = k + 1
		    xflip = (x1[j] > x2[j])
		    yflip = (y1[j] > y2[j])
		    coeffs[k] = Memr[P2R(sym+j)]
		    if (inbufs[j,1] == NULL) {
			if (xflip) {
			    if (yflip)
				inbufs[j,1] = imgs2s (ins[j], nc, 1,
				    nl-l+1, nl-l+1)
			    else
				inbufs[j,1] = imgs2s (ins[j], nc, 1, l, l)
			    inbufs[j,2] = inbufs[j,1] + nc - x1[j]
			} else {
			    if (yflip)
				inbufs[j,1] = imgl2s (ins[j], nl-l+1)
			    else
				inbufs[j,1] = imgl2s (ins[j], l)
			    inbufs[j,2] = inbufs[j,1] + x1[j] - 1
			}
		    }
		    bufs[k] = inbufs[j,2]
		}
		if (k == 0)
		    next

		nx = abs (x2[i] - x1[i]) + 1
		ny = abs (y2[i] - y1[i]) + 1

		xflip = (x1[i] > x2[i])
		yflip = (y1[i] > y2[i])
		if (inbufs[i,1] == NULL) {
		    if (xflip) {
			if (yflip)
			    inbufs[i,1] = imgs2s (ins[i], nc, 1,
			        nl-l+1, nl-l+1)
			else
			    inbufs[i,1] = imgs2s (ins[i], nc, 1, l, l)
			inbufs[i,2] = inbufs[i,1] + nc - x1[i]
		    } else {
			if (yflip)
			    inbufs[i,1] = imgl2s (ins[i], nl-l+1)
			else
			    inbufs[i,1] = imgl2s (ins[i], l)
			inbufs[i,2] = inbufs[i,1] + x1[i] - 1
		    }
		}
		if (xflip) {
		    if (yflip)
			outbufs[1] = imps2s (outs[i], nc, 1, nl-l+1, nl-l+1)
		    else
			outbufs[1] = imps2s (outs[i], nc, 1, l, l)
		    outbufs[2] = outbufs[1] + nc - x1[i]
		} else {
		    if (yflip)
			outbufs[1] = impl2s (outs[i], nl-l+1)
		    else
			outbufs[1] = impl2s (outs[i], l)
		    outbufs[2] = outbufs[1] + x1[i] - 1
		}
		if (xflip) {
		    if (yflip)
			bpmbufs[1] = imps2s (bpms[i], nc, 1, nl-l+1, nl-l+1)
		    else
			bpmbufs[1] = imps2s (bpms[i], nc, 1, l, l)
		    bpmbufs[2] = bpmbufs[1] + nc - x1[i]
		} else {
		    if (yflip)
			bpmbufs[1] = impl2s (bpms[i], nl-l+1)
		    else
			bpmbufs[1] = impl2s (bpms[i], l)
		    bpmbufs[2] = bpmbufs[1] + x1[i] - 1
		}

		# Preserve the data outside the data section.
		inbuf = inbufs[i,1]
		outbuf = outbufs[1]
		bpmbuf = bpmbufs[1]
		if (l < min (y1[i], y2[i]) || l > max (y1[i], y2[i])) {
		    call amovs (Mems[inbuf], Mems[outbuf], nc)
		    call aclrs (Mems[bpmbuf], nc)
		    next
		}
		if (nx < nc) {
		    j = inbufs[i,2] - inbufs[i,1]
		    if (j > 0) {
			call amovs (Mems[inbuf], Mems[outbuf], j)
			call aclrs (Mems[bpmbuf], j)
		    }
		    j = (inbufs[i,1]+nc-1) - (inbufs[i,2]+nx-1)
		    if (j > 0) {
			call amovs (Mems[inbuf+nc-j], Mems[outbuf+nc-j], j)
			call aclrs (Mems[bpmbuf+nc-j], j)
		    }
		}

		# Do the crosstalk correction.  Optimize for cases of
		# three or less source images with in-line operation.
		inbuf = inbufs[i,2]
		outbuf = outbufs[2]
		bpmbuf = bpmbufs[2]
		switch (k) {
		case 1:
		    do i = 0, nx-1 {
			val = coeffs[1] * Mems[bufs[1]+i]
			Mems[outbuf+i] = nint (Mems[inbuf+i] - val)
			if (bpmthresh > 0) {
			    if (Mems[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			} else {
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		case 2:
		    do i = 0, nx-1 {
			val = coeffs[1] * Mems[bufs[1]+i] +
			    coeffs[2] * Mems[bufs[2]+i]
			Mems[outbuf+i] = nint (Mems[inbuf+i] - val)
			if (bpmthresh > 0) {
			    if (Mems[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			} else {
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		case 3:
		    do i = 0, nx-1 {
			val = coeffs[1] * Mems[bufs[1]+i] +
			    coeffs[2] * Mems[bufs[2]+i] +
			    coeffs[3] * Mems[bufs[3]+i]
			Mems[outbuf+i] = nint (Mems[inbuf+i] - val)
			if (bpmthresh > 0) {
			    if (Mems[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[3]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			} else {
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		default:
		    do i = 0, nx-1 {
			val = coeffs[1] * Mems[bufs[1]+i] +
			    coeffs[2] * Mems[bufs[2]+i] +
			    coeffs[3] * Mems[bufs[3]+i]
			do k = 4, k
			    val = val + coeffs[k] * Mems[bufs[k]+i]
			Mems[outbuf+i] = nint (Mems[inbuf+i] - val)
			if (bpmthresh > 0) {
			    if (Mems[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[3]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else {
				Mems[bpmbuf+i] = 0
				do k = 4, k {
				    if (Mems[bufs[k]+i] > bpmthresh) {
					Mems[bpmbuf+i] = 1
					break
				    }
				}
			    }
			} else {
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		}

	    }
	}
end


# CTBPM -- Create output bad pixel mask.
# This is the core of the crosstalk correction operation.  It is optimized
# for multiple dependencies by reading each required input line only once.
# Image I/O buffering may be optimized by setting IM_BUFSIZE externally to
# this routine.

procedure ctbpms (stp, ins, bpms, x1, x2, y1, y2, inbufs, bufs, coeffs,
	bpmthresh, nimages, nc, nl)

pointer	stp				#I Symbol table
pointer	ins[nimages]			#I Input image pointers
pointer	bpms[nimages]			#I Output bpm pointers
int	x1[nimages]			#I Starting pixel
int	x2[nimages]			#I Ending pixel
int	y1[nimages]			#I Starting pixel
int	y2[nimages]			#I Ending pixel
pointer	inbufs[nimages,2]		#I Working memory
pointer	bufs[nimages]			#I Working memory
real	coeffs[nimages]			#I Working memory
real	bpmthresh			#I Bad pixel mask threshold
int	nimages				#I Number of images
int	nc, nl				#I Image dimensions

int	i, j, k, l, nx, ny
bool	xflip, yflip
pointer	sym, inbuf, bpmbuf, bpmbufs[2]
real	bpmval, val

pointer	impl2s(), imps2s()
pointer	imgl2s(), imgs2s(), sthead(), stnext()

begin
	bpmval = abs(bpmthresh)

	# Loop through each line and correct all dependent output images
	# at the same line.
	do l = 1, nl {

	    # The input lines have not be read initially.
	    call aclri (inbufs, 2*nimages)

	    # Loop through each output image.
	    for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {

		# Get input data buffers and coefficients.
		# Check if the output image requires any crosstalk correction.
		k = 0
		i = Memi[sym]
		do j = 1, nimages {
		    if (j == i || Memr[P2R(sym+j)] == 0.)
		       next
		    k = k + 1
		    xflip = (x1[j] > x2[j])
		    yflip = (y1[j] > y2[j])
		    coeffs[k] = Memr[P2R(sym+j)]
		    if (inbufs[j,1] == NULL) {
			if (xflip) {
			    if (yflip)
				inbufs[j,1] = imgs2s (ins[j], nc, 1,
				    nl-l+1, nl-l+1)
			    else
				inbufs[j,1] = imgs2s (ins[j], nc, 1, l, l)
			    inbufs[j,2] = inbufs[j,1] + nc - x1[j]
			} else {
			    if (yflip)
				inbufs[j,1] = imgl2s (ins[j], nl-l+1)
			    else
				inbufs[j,1] = imgl2s (ins[j], l)
			    inbufs[j,2] = inbufs[j,1] + x1[j] - 1
			}
		    }
		    bufs[k] = inbufs[j,2]
		}
		if (k == 0)
		    next

		nx = abs (x2[i] - x1[i]) + 1
		ny = abs (y2[i] - y1[i]) + 1

		xflip = (x1[i] > x2[i])
		yflip = (y1[i] > y2[i])
		if (inbufs[i,1] == NULL) {
		    if (xflip) {
			if (yflip)
			    inbufs[i,1] = imgs2s (ins[i], nc, 1,
			        nl-l+1, nl-l+1)
			else
			    inbufs[i,1] = imgs2s (ins[i], nc, 1, l, l)
			inbufs[i,2] = inbufs[i,1] + nc - x1[i]
		    } else {
			if (yflip)
			    inbufs[i,1] = imgl2s (ins[i], nl-l+1)
			else
			    inbufs[i,1] = imgl2s (ins[i], l)
			inbufs[i,2] = inbufs[i,1] + x1[i] - 1
		    }
		}
		if (xflip) {
		    if (yflip)
			bpmbufs[1] = imps2s (bpms[i], nc, 1, nl-l+1, nl-l+1)
		    else
			bpmbufs[1] = imps2s (bpms[i], nc, 1, l, l)
		    bpmbufs[2] = bpmbufs[1] + nc - x1[i]
		} else {
		    if (yflip)
			bpmbufs[1] = impl2s (bpms[i], nl-l+1)
		    else
			bpmbufs[1] = impl2s (bpms[i], l)
		    bpmbufs[2] = bpmbufs[1] + x1[i] - 1
		}

		# Preserve the data outside the data section.
		inbuf = inbufs[i,1]
		bpmbuf = bpmbufs[1]
		if (l < min (y1[i], y2[i]) || l > max (y1[i], y2[i])) {
		    call aclrs (Mems[bpmbuf], nc)
		    next
		}
		if (nx < nc) {
		    j = bpmbufs[2] - bpmbufs[1]
		    if (j > 0)
			call aclrs (Mems[bpmbuf], j)
		    j = (bpmbufs[1]+nc-1) - (bpmbufs[2]+nx-1)
		    if (j > 0)
			call aclrs (Mems[bpmbuf+nc-j], j)
		}

		# Do the bad pixel mask.  A positive threshold checks the
		# input and a negative checks the correction.
		bpmbuf = bpmbufs[2]
		if (bpmthresh > 0) {
		    switch (k) {
		    case 1:
			do i = 0, nx-1 {
			    if (Mems[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    case 2:
			do i = 0, nx-1 {
			    if (Mems[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    case 3:
			do i = 0, nx-1 {
			    if (Mems[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[3]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    default:
			do i = 0, nx-1 {
			    if (Mems[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Mems[bufs[3]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else {
				Mems[bpmbuf+i] = 0
				do k = 4, k {
				    if (Mems[bufs[k]+i] > bpmthresh) {
					Mems[bpmbuf+i] = 1
					break
				    }
				}
			    }
			}
		    }
		} else {
		    switch (k) {
		    case 1:
			do i = 0, nx-1 {
			    val = coeffs[1] * Mems[bufs[1]+i]
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    case 2:
			do i = 0, nx-1 {
			    val = coeffs[1] * Mems[bufs[1]+i] +
				coeffs[2] * Mems[bufs[2]+i]
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    case 3:
			do i = 0, nx-1 {
			    val = coeffs[1] * Mems[bufs[1]+i] +
				coeffs[2] * Mems[bufs[2]+i] +
				coeffs[3] * Mems[bufs[3]+i]
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    default:
			do i = 0, nx-1 {
			    val = coeffs[1] * Mems[bufs[1]+i] +
				coeffs[2] * Mems[bufs[2]+i] +
				coeffs[3] * Mems[bufs[3]+i]
			    do k = 4, k
				val = val + coeffs[k] * Mems[bufs[k]+i]
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		}
	    }
	}
end

# CTALK -- Apply crosstalk correction.
# This is the core of the crosstalk correction operation.  It is optimized
# for multiple dependencies by reading each required input line only once.
# Image I/O buffering may be optimized by setting IM_BUFSIZE externally to
# this routine.

procedure ctalkr (stp, ins, outs, x1, x2, y1, y2, inbufs, bufs, coeffs,
	nimages, nc, nl)

pointer	stp				#I Symbol table
pointer	ins[nimages]			#I Input image pointers
pointer	outs[nimages]			#I Output image pointers
int	x1[nimages]			#I Starting pixel
int	x2[nimages]			#I Ending pixel
int	y1[nimages]			#I Starting pixel
int	y2[nimages]			#I Ending pixel
pointer	inbufs[nimages,2]		#I Working memory
pointer	bufs[nimages]			#I Working memory
real	coeffs[nimages]			#I Working memory
int	nimages				#I Number of images
int	nc, nl				#I Image dimensions

int	i, j, k, l, nx, ny
bool	xflip, yflip
pointer	sym, inbuf, outbuf, outbufs[2]
real	val

pointer	imgl2r(), impl2r(), imgs2r(), imps2r(), sthead(), stnext()

begin
	# Loop through each line and correct all dependent output images
	# at the same line.
	do l = 1, nl {

	    # The input lines have not be read initially.
	    call aclri (inbufs, 2*nimages)

	    # Loop through each output image.
	    for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {

		# Get input data buffers and coefficients.
		# Check if the output image requires any crosstalk correction.
		k = 0
		i = Memi[sym]
		do j = 1, nimages {
		    if (j == i || Memr[P2R(sym+j)] == 0.)
		       next
		    k = k + 1
		    xflip = (x1[j] > x2[j])
		    yflip = (y1[j] > y2[j])
		    coeffs[k] = Memr[P2R(sym+j)]
		    if (inbufs[j,1] == NULL) {
			if (xflip) {
			    if (yflip)
				inbufs[j,1] = imgs2r (ins[j], nc, 1,
				    nl-l+1, nl-l+1)
			    else
				inbufs[j,1] = imgs2r (ins[j], nc, 1, l, l)
			    inbufs[j,2] = inbufs[j,1] + nc - x1[j]
			} else {
			    if (yflip)
				inbufs[j,1] = imgl2r (ins[j], nl-l+1)
			    else
				inbufs[j,1] = imgl2r (ins[j], l)
			    inbufs[j,2] = inbufs[j,1] + x1[j] - 1
			}
		    }
		    bufs[k] = inbufs[j,2]
		}
		if (k == 0)
		    next

		nx = abs (x2[i] - x1[i]) + 1
		ny = abs (y2[i] - y1[i]) + 1

		xflip = (x1[i] > x2[i])
		yflip = (y1[i] > y2[i])
		if (inbufs[i,1] == NULL) {
		    if (xflip) {
			if (yflip)
			    inbufs[i,1] = imgs2r (ins[i], nc, 1,
			        nl-l+1, nl-l+1)
			else
			    inbufs[i,1] = imgs2r (ins[i], nc, 1, l, l)
			inbufs[i,2] = inbufs[i,1] + nc - x1[i]
		    } else {
			if (yflip)
			    inbufs[i,1] = imgl2r (ins[i], nl-l+1)
			else
			    inbufs[i,1] = imgl2r (ins[i], l)
			inbufs[i,2] = inbufs[i,1] + x1[i] - 1
		    }
		}
		if (xflip) {
		    if (yflip)
			outbufs[1] = imps2r (outs[i], nc, 1, nl-l+1, nl-l+1)
		    else
			outbufs[1] = imps2r (outs[i], nc, 1, l, l)
		    outbufs[2] = outbufs[1] + nc - x1[i]
		} else {
		    if (yflip)
			outbufs[1] = impl2r (outs[i], nl-l+1)
		    else
			outbufs[1] = impl2r (outs[i], l)
		    outbufs[2] = outbufs[1] + x1[i] - 1
		}

		# Preserve the data outside the data section.
		inbuf = inbufs[i,1]
		outbuf = outbufs[1]
		if (l < min (y1[i], y2[i]) || l > max (y1[i], y2[i])) {
		    call amovr (Memr[inbuf], Memr[outbuf], nc)
		    next
		}
		if (nx < nc) {
		    j = inbufs[i,2] - inbufs[i,1]
		    if (j > 0)
			call amovr (Memr[inbuf], Memr[outbuf], j)
		    j = (inbufs[i,1]+nc-1) - (inbufs[i,2]+nx-1)
		    if (j > 0)
			call amovr (Memr[inbuf+nc-j], Memr[outbuf+nc-j], j)
		}

		# Do the crosstalk correction.  Optimize for cases of
		# three or less source images with in-line operation.
		inbuf = inbufs[i,2]
		outbuf = outbufs[2]
		switch (k) {
		case 1:
		    do i = 0, nx-1 {
			Memr[outbuf+i] = Memr[inbuf+i] -
			    coeffs[1] * Memr[bufs[1]+i]
		    }
		case 2:
		    do i = 0, nx-1 {
			Memr[outbuf+i] = Memr[inbuf+i] -
			    coeffs[1] * Memr[bufs[1]+i] -
			    coeffs[2] * Memr[bufs[2]+i]
		    }
		case 3:
		    do i = 0, nx-1 {
			Memr[outbuf+i] = Memr[inbuf+i] -
			    coeffs[1] * Memr[bufs[1]+i] -
			    coeffs[2] * Memr[bufs[2]+i] -
			    coeffs[3] * Memr[bufs[3]+i]
		    }
		default:
		    do i = 0, nx-1 {
			val = Memr[inbuf+i] -
			    coeffs[1] * Memr[bufs[1]+i] -
			    coeffs[2] * Memr[bufs[2]+i] -
			    coeffs[3] * Memr[bufs[3]+i]
			do k = 4, k
			    val = val - coeffs[k] * Memr[bufs[k]+i]
			Memr[outbuf+i] = val
		    }
		}

		# To avoid truncation when the output is an integer round
		# the real corrected value.
		outbuf = outbufs[1]
		switch (IM_PIXTYPE(outs[Memi[sym]])) {
		case TY_USHORT, TY_SHORT, TY_INT, TY_LONG:
		    do i = 0, nc-1
			Memr[outbuf+i] = nint (Memr[outbuf+i])
		}
	    }
	}
end


# CTOB -- Apply crosstalk correction and create output bad pixel mask.
# This is the core of the crosstalk correction operation.  It is optimized
# for multiple dependencies by reading each required input line only once.
# Image I/O buffering may be optimized by setting IM_BUFSIZE externally to
# this routine.

procedure ctobr (stp, ins, outs, bpms, x1, x2, y1, y2, inbufs, bufs, coeffs,
	bpmthresh, nimages, nc, nl)

pointer	stp				#I Symbol table
pointer	ins[nimages]			#I Input image pointers
pointer	outs[nimages]			#I Output image pointers
pointer	bpms[nimages]			#I Output bpm pointers
int	x1[nimages]			#I Starting pixel
int	x2[nimages]			#I Ending pixel
int	y1[nimages]			#I Starting pixel
int	y2[nimages]			#I Ending pixel
pointer	inbufs[nimages,2]		#I Working memory
pointer	bufs[nimages]			#I Working memory
real	coeffs[nimages]			#I Working memory
real	bpmthresh			#I Bad pixel mask threshold
int	nimages				#I Number of images
int	nc, nl				#I Image dimensions

int	i, j, k, l, nx, ny
bool	xflip, yflip
pointer	sym, inbuf, outbuf, outbufs[2], bpmbuf, bpmbufs[2]
real	bpmval, val

pointer	impl2s(), imps2s()
pointer	imgl2r(), impl2r(), imgs2r(), imps2r(), sthead(), stnext()

begin
	bpmval = abs(bpmthresh)

	# Loop through each line and correct all dependent output images
	# at the same line.
	do l = 1, nl {

	    # The input lines have not be read initially.
	    call aclri (inbufs, 2*nimages)

	    # Loop through each output image.
	    for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {

		# Get input data buffers and coefficients.
		# Check if the output image requires any crosstalk correction.
		k = 0
		i = Memi[sym]
		do j = 1, nimages {
		    if (j == i || Memr[P2R(sym+j)] == 0.)
		       next
		    k = k + 1
		    xflip = (x1[j] > x2[j])
		    yflip = (y1[j] > y2[j])
		    coeffs[k] = Memr[P2R(sym+j)]
		    if (inbufs[j,1] == NULL) {
			if (xflip) {
			    if (yflip)
				inbufs[j,1] = imgs2r (ins[j], nc, 1,
				    nl-l+1, nl-l+1)
			    else
				inbufs[j,1] = imgs2r (ins[j], nc, 1, l, l)
			    inbufs[j,2] = inbufs[j,1] + nc - x1[j]
			} else {
			    if (yflip)
				inbufs[j,1] = imgl2r (ins[j], nl-l+1)
			    else
				inbufs[j,1] = imgl2r (ins[j], l)
			    inbufs[j,2] = inbufs[j,1] + x1[j] - 1
			}
		    }
		    bufs[k] = inbufs[j,2]
		}
		if (k == 0)
		    next

		nx = abs (x2[i] - x1[i]) + 1
		ny = abs (y2[i] - y1[i]) + 1

		xflip = (x1[i] > x2[i])
		yflip = (y1[i] > y2[i])
		if (inbufs[i,1] == NULL) {
		    if (xflip) {
			if (yflip)
			    inbufs[i,1] = imgs2r (ins[i], nc, 1,
			        nl-l+1, nl-l+1)
			else
			    inbufs[i,1] = imgs2r (ins[i], nc, 1, l, l)
			inbufs[i,2] = inbufs[i,1] + nc - x1[i]
		    } else {
			if (yflip)
			    inbufs[i,1] = imgl2r (ins[i], nl-l+1)
			else
			    inbufs[i,1] = imgl2r (ins[i], l)
			inbufs[i,2] = inbufs[i,1] + x1[i] - 1
		    }
		}
		if (xflip) {
		    if (yflip)
			outbufs[1] = imps2r (outs[i], nc, 1, nl-l+1, nl-l+1)
		    else
			outbufs[1] = imps2r (outs[i], nc, 1, l, l)
		    outbufs[2] = outbufs[1] + nc - x1[i]
		} else {
		    if (yflip)
			outbufs[1] = impl2r (outs[i], nl-l+1)
		    else
			outbufs[1] = impl2r (outs[i], l)
		    outbufs[2] = outbufs[1] + x1[i] - 1
		}
		if (xflip) {
		    if (yflip)
			bpmbufs[1] = imps2s (bpms[i], nc, 1, nl-l+1, nl-l+1)
		    else
			bpmbufs[1] = imps2s (bpms[i], nc, 1, l, l)
		    bpmbufs[2] = bpmbufs[1] + nc - x1[i]
		} else {
		    if (yflip)
			bpmbufs[1] = impl2s (bpms[i], nl-l+1)
		    else
			bpmbufs[1] = impl2s (bpms[i], l)
		    bpmbufs[2] = bpmbufs[1] + x1[i] - 1
		}

		# Preserve the data outside the data section.
		inbuf = inbufs[i,1]
		outbuf = outbufs[1]
		bpmbuf = bpmbufs[1]
		if (l < min (y1[i], y2[i]) || l > max (y1[i], y2[i])) {
		    call amovr (Memr[inbuf], Memr[outbuf], nc)
		    call aclrs (Mems[bpmbuf], nc)
		    next
		}
		if (nx < nc) {
		    j = inbufs[i,2] - inbufs[i,1]
		    if (j > 0) {
			call amovr (Memr[inbuf], Memr[outbuf], j)
			call aclrs (Mems[bpmbuf], j)
		    }
		    j = (inbufs[i,1]+nc-1) - (inbufs[i,2]+nx-1)
		    if (j > 0) {
			call amovr (Memr[inbuf+nc-j], Memr[outbuf+nc-j], j)
			call aclrs (Mems[bpmbuf+nc-j], j)
		    }
		}

		# Do the crosstalk correction.  Optimize for cases of
		# three or less source images with in-line operation.
		inbuf = inbufs[i,2]
		outbuf = outbufs[2]
		bpmbuf = bpmbufs[2]
		switch (k) {
		case 1:
		    do i = 0, nx-1 {
			val = coeffs[1] * Memr[bufs[1]+i]
			Memr[outbuf+i] = Memr[inbuf+i] - val
			if (bpmthresh > 0) {
			    if (Memr[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			} else {
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		case 2:
		    do i = 0, nx-1 {
			val = coeffs[1] * Memr[bufs[1]+i] +
			    coeffs[2] * Memr[bufs[2]+i]
			Memr[outbuf+i] = Memr[inbuf+i] - val
			if (bpmthresh > 0) {
			    if (Memr[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			} else {
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		case 3:
		    do i = 0, nx-1 {
			val = coeffs[1] * Memr[bufs[1]+i] +
			    coeffs[2] * Memr[bufs[2]+i] +
			    coeffs[3] * Memr[bufs[3]+i]
			Memr[outbuf+i] = Memr[inbuf+i] - val
			if (bpmthresh > 0) {
			    if (Memr[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[3]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			} else {
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		default:
		    do i = 0, nx-1 {
			val = coeffs[1] * Memr[bufs[1]+i] +
			    coeffs[2] * Memr[bufs[2]+i] +
			    coeffs[3] * Memr[bufs[3]+i]
			do k = 4, k
			    val = val + coeffs[k] * Memr[bufs[k]+i]
			Memr[outbuf+i] = Memr[inbuf+i] - val
			if (bpmthresh > 0) {
			    if (Memr[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[3]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else {
				Mems[bpmbuf+i] = 0
				do k = 4, k {
				    if (Memr[bufs[k]+i] > bpmthresh) {
					Mems[bpmbuf+i] = 1
					break
				    }
				}
			    }
			} else {
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		}

		# To avoid truncation when the output is an integer round
		# the real corrected value.
		outbuf = outbufs[1]
		switch (IM_PIXTYPE(outs[Memi[sym]])) {
		case TY_USHORT, TY_SHORT, TY_INT, TY_LONG:
		    do i = 0, nc-1
			Memr[outbuf+i] = nint (Memr[outbuf+i])
		}
	    }
	}
end


# CTBPM -- Create output bad pixel mask.
# This is the core of the crosstalk correction operation.  It is optimized
# for multiple dependencies by reading each required input line only once.
# Image I/O buffering may be optimized by setting IM_BUFSIZE externally to
# this routine.

procedure ctbpmr (stp, ins, bpms, x1, x2, y1, y2, inbufs, bufs, coeffs,
	bpmthresh, nimages, nc, nl)

pointer	stp				#I Symbol table
pointer	ins[nimages]			#I Input image pointers
pointer	bpms[nimages]			#I Output bpm pointers
int	x1[nimages]			#I Starting pixel
int	x2[nimages]			#I Ending pixel
int	y1[nimages]			#I Starting pixel
int	y2[nimages]			#I Ending pixel
pointer	inbufs[nimages,2]		#I Working memory
pointer	bufs[nimages]			#I Working memory
real	coeffs[nimages]			#I Working memory
real	bpmthresh			#I Bad pixel mask threshold
int	nimages				#I Number of images
int	nc, nl				#I Image dimensions

int	i, j, k, l, nx, ny
bool	xflip, yflip
pointer	sym, inbuf, bpmbuf, bpmbufs[2]
real	bpmval, val

pointer	impl2s(), imps2s()
pointer	imgl2r(), imgs2r(), sthead(), stnext()

begin
	bpmval = abs(bpmthresh)

	# Loop through each line and correct all dependent output images
	# at the same line.
	do l = 1, nl {

	    # The input lines have not be read initially.
	    call aclri (inbufs, 2*nimages)

	    # Loop through each output image.
	    for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {

		# Get input data buffers and coefficients.
		# Check if the output image requires any crosstalk correction.
		k = 0
		i = Memi[sym]
		do j = 1, nimages {
		    if (j == i || Memr[P2R(sym+j)] == 0.)
		       next
		    k = k + 1
		    xflip = (x1[j] > x2[j])
		    yflip = (y1[j] > y2[j])
		    coeffs[k] = Memr[P2R(sym+j)]
		    if (inbufs[j,1] == NULL) {
			if (xflip) {
			    if (yflip)
				inbufs[j,1] = imgs2r (ins[j], nc, 1,
				    nl-l+1, nl-l+1)
			    else
				inbufs[j,1] = imgs2r (ins[j], nc, 1, l, l)
			    inbufs[j,2] = inbufs[j,1] + nc - x1[j]
			} else {
			    if (yflip)
				inbufs[j,1] = imgl2r (ins[j], nl-l+1)
			    else
				inbufs[j,1] = imgl2r (ins[j], l)
			    inbufs[j,2] = inbufs[j,1] + x1[j] - 1
			}
		    }
		    bufs[k] = inbufs[j,2]
		}
		if (k == 0)
		    next

		nx = abs (x2[i] - x1[i]) + 1
		ny = abs (y2[i] - y1[i]) + 1

		xflip = (x1[i] > x2[i])
		yflip = (y1[i] > y2[i])
		if (inbufs[i,1] == NULL) {
		    if (xflip) {
			if (yflip)
			    inbufs[i,1] = imgs2r (ins[i], nc, 1,
			        nl-l+1, nl-l+1)
			else
			    inbufs[i,1] = imgs2r (ins[i], nc, 1, l, l)
			inbufs[i,2] = inbufs[i,1] + nc - x1[i]
		    } else {
			if (yflip)
			    inbufs[i,1] = imgl2r (ins[i], nl-l+1)
			else
			    inbufs[i,1] = imgl2r (ins[i], l)
			inbufs[i,2] = inbufs[i,1] + x1[i] - 1
		    }
		}
		if (xflip) {
		    if (yflip)
			bpmbufs[1] = imps2s (bpms[i], nc, 1, nl-l+1, nl-l+1)
		    else
			bpmbufs[1] = imps2s (bpms[i], nc, 1, l, l)
		    bpmbufs[2] = bpmbufs[1] + nc - x1[i]
		} else {
		    if (yflip)
			bpmbufs[1] = impl2s (bpms[i], nl-l+1)
		    else
			bpmbufs[1] = impl2s (bpms[i], l)
		    bpmbufs[2] = bpmbufs[1] + x1[i] - 1
		}

		# Preserve the data outside the data section.
		inbuf = inbufs[i,1]
		bpmbuf = bpmbufs[1]
		if (l < min (y1[i], y2[i]) || l > max (y1[i], y2[i])) {
		    call aclrs (Mems[bpmbuf], nc)
		    next
		}
		if (nx < nc) {
		    j = bpmbufs[2] - bpmbufs[1]
		    if (j > 0)
			call aclrs (Mems[bpmbuf], j)
		    j = (bpmbufs[1]+nc-1) - (bpmbufs[2]+nx-1)
		    if (j > 0)
			call aclrs (Mems[bpmbuf+nc-j], j)
		}

		# Do the bad pixel mask.  A positive threshold checks the
		# input and a negative checks the correction.
		bpmbuf = bpmbufs[2]
		if (bpmthresh > 0) {
		    switch (k) {
		    case 1:
			do i = 0, nx-1 {
			    if (Memr[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    case 2:
			do i = 0, nx-1 {
			    if (Memr[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    case 3:
			do i = 0, nx-1 {
			    if (Memr[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[3]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    default:
			do i = 0, nx-1 {
			    if (Memr[bufs[1]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[2]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else if (Memr[bufs[3]+i] > bpmthresh)
				Mems[bpmbuf+i] = 1
			    else {
				Mems[bpmbuf+i] = 0
				do k = 4, k {
				    if (Memr[bufs[k]+i] > bpmthresh) {
					Mems[bpmbuf+i] = 1
					break
				    }
				}
			    }
			}
		    }
		} else {
		    switch (k) {
		    case 1:
			do i = 0, nx-1 {
			    val = coeffs[1] * Memr[bufs[1]+i]
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    case 2:
			do i = 0, nx-1 {
			    val = coeffs[1] * Memr[bufs[1]+i] +
				coeffs[2] * Memr[bufs[2]+i]
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    case 3:
			do i = 0, nx-1 {
			    val = coeffs[1] * Memr[bufs[1]+i] +
				coeffs[2] * Memr[bufs[2]+i] +
				coeffs[3] * Memr[bufs[3]+i]
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    default:
			do i = 0, nx-1 {
			    val = coeffs[1] * Memr[bufs[1]+i] +
				coeffs[2] * Memr[bufs[2]+i] +
				coeffs[3] * Memr[bufs[3]+i]
			    do k = 4, k
				val = val + coeffs[k] * Memr[bufs[k]+i]
			    if (abs(val) > bpmval)
				Mems[bpmbuf+i] = 1
			    else
				Mems[bpmbuf+i] = 0
			}
		    }
		}
	    }
	}
end

