include	<mach.h>
include	<imhdr.h>

# These routines must be used in a sequential pass through the image.

# BLD_OPEN -- Initialize saturated pixel and bleed trail routines.
# This includes memory allocation and initial line buffering.
# The package parameters are passed in a common block.

procedure bld_open (out, fdnoi, omask, bp, dointerp, sthresh, svalue, sgrow,
	bthresh, bvalue, bgrow, btrail, insec, outsec)

pointer	out			#I Output image
pointer	fdnoi			#I Output no interpolation image
pointer	omask			#I Output mask
pointer	bp			#I Input mask
bool	dointerp		#I Are input pixels interpolated?
real	sthresh			#I Saturation threshold
int	svalue			#I Saturated pixel mask value
int	sgrow			#I Number of pixels to grow saturated pixels
real	bthresh			#I Threshold for candidate bleed pixels
int	bvalue			#I Bleed trail mask value
int	bgrow			#I Number of pixels to grow bleed pixels
int	btrail			#I Minimum bleed trail length
int	insec[2,2]		#I Input section
int	outsec[2,2]		#I Output section

int	l
pointer	obuf
pointer	imgl2s()

include	"bleed.com"

begin
	# Set parameters.
	if (out != NULL) {
	    nc = IM_LEN(out,1)
	    nl = IM_LEN(out,2)
	} else if (omask != NULL) {
	    nc = IM_LEN(omask,1)
	    nl = IM_LEN(omask,2)
	} else
	    call error (1, "BLD_OPEN: No output specified")

	ic1 = insec[1,1]
	il1 = insec[1,2]
	c1 = outsec[1,1]
	c2 = outsec[2,1]
	l1 = outsec[1,2]
	l2 = outsec[2,2]

	if (IS_INDEFR(sthresh))
	    sth = MAX_REAL
	else
	    sth = sthresh
	sval = svalue
	sgrw = sgrow
	bth = bthresh
	bval = bvalue
	bgrw = bgrow
	tlen = min (l2, btrail)
	nbehind = max (tlen + bgrw - 1, sgrw)
	nahead = max (bgrw, sgrw)
	nbufs = min (l2, 1 + nbehind + nahead)

	# Allocate memory and make it one indexed.
	if (out != NULL)
	    call malloc (obufs, nc * nbufs, TY_REAL)
	else
	    obufs = NULL
	if (fdnoi != NULL && dointerp)
	    call malloc (noibufs, nc * nbufs, TY_REAL)
	else
	    noibufs = NULL
	call malloc (ombufs, nc * nbufs, TY_SHORT)
	call malloc (counts, nc, TY_SHORT)
	obufs = obufs - 1
	noibufs = noibufs - 1
	ombufs = ombufs - 1
	counts = counts - 1

	# Set buffered grow lines.
	do l = l1, min (l2, nahead) {
	    obuf = ombufs + nc * mod(l,nbufs) + 1
	    if (bp != NULL)
		call amovs (Mems[imgl2s(bp,l+il1-l1)+ic1-c1], Mems[obuf], nc)
	    else
		call aclrs (Mems[obuf], nc)
	}

	# Initialize the bleed pixel counts.
	call aclrs (Mems[counts+1], nc)
end


# BLD_CLOSE -- Close the saturated pixel and bleed trail package.
# This just consists of freeing buffer memory.

procedure bld_close ()

include	"bleed.com"

begin
	# Free memory.
	obufs = obufs + 1
	noibufs = noibufs + 1
	ombufs = ombufs + 1
	counts = counts + 1
	call mfree (obufs, TY_REAL)
	call mfree (noibufs, TY_REAL)
	call mfree (ombufs, TY_SHORT)
	call mfree (counts, TY_SHORT)
end


# BLD_MASK -- Set mask of saturated and bleed trail pixels.
# If an output descriptor is provided then write out the mask.

procedure bld_mask (omask, l, data, bp)

pointer	omask			#I Output mask
int	l			#I Line
real	data[ARB]		#I Data
pointer	bp			#I Input mask

int	i, j, k, c
pointer	obuf, obufl, imgl2s(), impl2s()

include	"bleed.com"

begin
	# Initialize next buffered output line.
	k = l + nahead
	if (k <= l2) {
	    obuf = ombufs + nc * mod(k,nbufs) + 1
	    if (bp != NULL)
		call amovs (Mems[imgl2s(bp,k+il1-l1)+ic1-c1], Mems[obuf], nc)
	    else
		call aclrs (Mems[obuf], nc)
	}

	# Find the saturation and bleed trails.  Grow if required.
	obufl = ombufs + nc * mod(l,nbufs)
	do c = c1, c2 {
	    # Saturated pixel?
	    if (data[c] >= sth) {
		if (sgrw == 0)
		    Mems[obufl+c] = sval
		else {
		    do k = max(1,l-sgrw), min(l2,l+sgrw) {
			obuf = ombufs + nc * mod(k,nbufs)
			i = sgrw - abs (l - k)
			do j = max(1,c-i), min(nc,c+i) {
			    if (Mems[obuf+j] == 0 || Mems[obuf+j] == bval)
				Mems[obuf+j] = sval
			}
		    }
		}
	    }

	    # Bleed pixel?
	    if (tlen < 1)
		next
	    if (data[c] < bth) {
		Mems[counts+c] = 0
		next
	    }
	    i = Mems[counts+c] + 1
	    Mems[counts+c] = i
	    if (i < tlen) 
		next
	    if (i > tlen) {
		do k = l, min(l2,l+bgrw) {
		    obuf = ombufs + nc * mod(k,nbufs)
		    i = bgrw - abs (k - l)
		    do j = max(1,c-i), min(nc,c+i) {
			if (Mems[obuf+j] == 0)
			    Mems[obuf+j] = bval
		    }
		}
	    } else {
		do k = max(1,l-tlen-bgrw+1), min(l2,l+bgrw) {
		    obuf = ombufs + nc * mod(k,nbufs)
		    if (k < l - tlen + 1)
			i = bgrw - abs (l - tlen + 1 - k)
		    else if (k > l)
			i = bgrw - abs (k - l)
		    else
			i = bgrw
		    do j = max(1,c-i), min(nc,c+i) {
			if (Mems[obuf+j] == 0)
			    Mems[obuf+j] = bval
		    }
		}
	    }
	}

	# Write out completed line(s).
	if (omask != NULL) {
	    j = l + nahead - nbufs + 1
	    if (l == l2)
		k = l2
	    else
		k = j
	    do i = j, k {
		if (i < l1)
		    next
		obuf = ombufs + nc * mod(i,nbufs) + 1
		call amovs (Mems[obuf], Mems[impl2s(omask,i)], nc)
	    }
	}
end


# BLD_INTERP -- Write output image with interpolated saturated and bleed pixels.
# If desired also write an uninterpolated output image.
# This is line interpolation only.
# Only mask values for saturated or bleed trail pixels are interpolated.
# However if an input mask was specified containing the same values
# then those pixels will also be interpolated.

procedure bld_interp (out, fdnoi, l, noidata, data)

pointer	out				#I Output IMIO pointer
pointer	fdnoi				#I Output no interpolation pointer
int	l				#I Line
pointer	noidata				#I Output data (without fixpix)
real	data[ARB]			#I Output data (before interpolation)

int	i, j1, j2, k1, k2, c, mval
real	a, b
pointer	obuf, ombuf, noibuf, noibuf1, impl2r()

include	"bleed.com"

begin
	if (out == NULL)
	    return

	# Save the input data.
	if (noidata != NULL) {
	    obuf = noibufs + nc * mod(l,nbufs)
	    call amovr (Memr[noidata], Memr[obuf+1], nc)
	}
	obuf = obufs + nc * mod(l,nbufs)
	call amovr (data, Memr[obuf+1], nc)

	# Write out completed line.
	k1 = l + nahead - nbufs + 1
	if (l == l2)
	    k2 = l2
	else
	    k2 = k1
	do i = k1, k2 {
	    if (i < l1)
		next
	    obuf = obufs + nc * mod(i,nbufs)
	    noibuf1 = noibufs + nc * mod(i,nbufs)
	    ombuf = ombufs + nc * mod(i,nbufs)
	    if (fdnoi != NULL) {
		noibuf = impl2r (fdnoi, i)
		if (noibufs+1 != NULL)
		    call amovr (Memr[noibuf1+1], Memr[noibuf], nc)
		else
		    call amovr (Memr[obuf+1], Memr[noibuf], nc)
	    }
	    j1 = 0; j2 = 0
	    for (c=c1; c<=c2; c=c+1) {
		mval = Mems[ombuf+c]
		if (mval == 0) {
		    j1 = c
		    next
		}
		if (mval == sval || mval == bval) {
		    if (j2 < c) {
			do j2 = c+1, nc {
			    if (Mems[ombuf+j2] == 0)
				break
			}
			if (j1 < c1 && j2 > c2)
			    break
			if (j1 < c1) {
			    a = Memr[obuf+j2]
			    b = 0
			} else if (j2 > c2) {
			    a = Memr[obuf+j1]
			    b = 0
			} else {
			    a = Memr[obuf+j1]
			    b = (Memr[obuf+j2] - a) / (j2 - j1)
			}
		    }
		    #Memr[obuf+c] = a + b * (c - j1)

		    # The following is a design question.  Should non-saturated
		    # pixels be interpolated if they border a saturated
		    # pixel?  In this case we say yes.
		    for (c=max(1,j1+1); c<=min(nc,j2-1); c=c+1)
			Memr[obuf+c] = a + b * (c - j1)
		}
	    }
	    call amovr (Memr[obuf+1], Memr[impl2r(out,i)], nc)
	}
end
