include <imhdr.h>
include "mosim.h"
include "mosgeom.h"
include "mosproc.h"

define	LINE	Meml[IMIDX($1)+1]	# Current line count in index array

# NOTE: These routines only realy work for 2D images!
#       Could be generalised but life's too short just now.



# MINL_INITx -- Set starting vectors for MIGNLx calls

procedure minl_inits (mi, v)

pointer	mi		#I MOSIM Pointer for mosaic.
long	v[IM_MAXDIM]	#I Initial value for loop counter.

int	nimage, onx, ony, ocy1, ocy2, icy1, icy2, idy1, image
pointer	omg, sb, obuf, img

include "mosproc.com"

begin
        nimage = MI_NIMS(mi)
        omg    = MI_MG(mi, nimage+1)
        onx    = NX(omg)
	ony    = NY(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

        if (MI_SB(mi) == NULL) {
            call malloc (sb,   LEN_SECBUFF, TY_STRUCT)
            call malloc (obuf, onx,         TY_SHORT)
            MI_SB(mi)      = sb
            SB_DATA(sb)    = obuf
        } else {
	    sb = MI_SB(mi)

	    # Free old data buffer if any and allocate a new one
	    if (SB_DATA(sb) != NULL)
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))
            call malloc (obuf, onx,         TY_SHORT)
            SB_DATA(sb)    = obuf
	}

	# Set data buffer pixel type
	SB_PIXTYPE(sb) = TY_SHORT

	# Convert line counter from image to ccd coordinates
	v[2] = v[2] + ocy1 - 1
	# Initialise loop counters
	call amovl (v, Meml[IMIDX(omg)], IM_MAXDIM)

	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    call amovl (v, Meml[IMIDX(img)], IM_MAXDIM)

	    if (trim) {
		icy1 = CTY1(img)
		icy2 = CTY2(img)
		idy1 = TY1(img)
	    } else {
		icy1 = CY1(img)
		icy2 = CY2(img)
		idy1 = DY1(img)
	    }

	    if (v[2] <= icy1) {
		LINE(img) = idy1
	    } else if (v[2] <= icy2) {
		LINE(img) = v[2] - (ocy1 - icy1) + idy1
	    } else {
	        LINE(img) = NY(img) + 1   
	    }
	}
end

# MIGNLx -- Get and process next line from sub-image of mosaic.

int procedure mignls (mi, obuf, v)

pointer	mi		#I MOSIM Pointer for mosaic.
pointer	obuf		#O On output pointer to data values.
long	v[IM_MAXDIM]	#I Loop counter.
# function value	#O Number of pixels in line or EOF.

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	onx, ocx1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, line, iline, nx, image, novr

int	mscnls()

errchk	mscnls, linebiass, malloc, syserrs

include	"mosproc.com"

begin
	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ocx1   = CX1(omg)
	ocy2   = CY2(omg)

	# Perform zero-trip test (assumes 2D image)
	if (v[2] > ocy2)
	    return (EOF)

	# Perform first time initialisation
        if (MI_SB(mi) == NULL) {
	    call minl_inits (mi, v)
	} else {
	    # Reinitialise if caller has changed v since last call.
	    if (LINE(omg) != v[2])	# Assumes 2D image.
		call minl_inits (mi, v)
	}

	# Fill output buffer from input images.
	#
	sb   = MI_SB(mi)
	obuf = SB_DATA(sb)
	line = LINE(omg)

	# Initialise  buffer with blank value
	call amovks (short(blank), Mems[obuf], onx)

	# Loop over input images, skipping those which have no data in
	# current line.
	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    icy1 = CY1(img)
	    icy2 = CY2(img)
	    if (line < icy1 || line > icy2)
		next
	    icx1 = CX1(img)
	    icx2 = CX2(img)
	    idx1 = DX1(img)
	    idy1 = DY1(img)

	    # Get corresponding line of input image
	    iim   = MI_IM(mi, image)
	    if (mscnls (img, ibuf, Meml[IMIDX(img)]) == EOF)
		next

	    # Process input image line  writing to output buffer
	    iptr  = ibuf + idx1 - 1
	    optr  = obuf + icx1 - ocx1
	    nx    = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

	    call miprocs (img, Mems[iptr], Mems[optr], nx, iline,
	    Mems[ovr], novr)
	}

	# Bump loop counters ready for next trip (assumes 2D)
	v[2] = v[2] + long(1)
	LINE(omg) = v[2]

	return (onx)
end



# MINL_INITx -- Set starting vectors for MIGNLx calls

procedure minl_initi (mi, v)

pointer	mi		#I MOSIM Pointer for mosaic.
long	v[IM_MAXDIM]	#I Initial value for loop counter.

int	nimage, onx, ony, ocy1, ocy2, icy1, icy2, idy1, image
pointer	omg, sb, obuf, img

include "mosproc.com"

begin
        nimage = MI_NIMS(mi)
        omg    = MI_MG(mi, nimage+1)
        onx    = NX(omg)
	ony    = NY(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

        if (MI_SB(mi) == NULL) {
            call malloc (sb,   LEN_SECBUFF, TY_STRUCT)
            call malloc (obuf, onx,         TY_INT)
            MI_SB(mi)      = sb
            SB_DATA(sb)    = obuf
        } else {
	    sb = MI_SB(mi)

	    # Free old data buffer if any and allocate a new one
	    if (SB_DATA(sb) != NULL)
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))
            call malloc (obuf, onx,         TY_INT)
            SB_DATA(sb)    = obuf
	}

	# Set data buffer pixel type
	SB_PIXTYPE(sb) = TY_INT

	# Convert line counter from image to ccd coordinates
	v[2] = v[2] + ocy1 - 1
	# Initialise loop counters
	call amovl (v, Meml[IMIDX(omg)], IM_MAXDIM)

	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    call amovl (v, Meml[IMIDX(img)], IM_MAXDIM)

	    if (trim) {
		icy1 = CTY1(img)
		icy2 = CTY2(img)
		idy1 = TY1(img)
	    } else {
		icy1 = CY1(img)
		icy2 = CY2(img)
		idy1 = DY1(img)
	    }

	    if (v[2] <= icy1) {
		LINE(img) = idy1
	    } else if (v[2] <= icy2) {
		LINE(img) = v[2] - (ocy1 - icy1) + idy1
	    } else {
	        LINE(img) = NY(img) + 1   
	    }
	}
end

# MIGNLx -- Get and process next line from sub-image of mosaic.

int procedure mignli (mi, obuf, v)

pointer	mi		#I MOSIM Pointer for mosaic.
pointer	obuf		#O On output pointer to data values.
long	v[IM_MAXDIM]	#I Loop counter.
# function value	#O Number of pixels in line or EOF.

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	onx, ocx1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, line, iline, nx, image, novr

int	mscnli()

errchk	mscnli, linebiasi, malloc, syserrs

include	"mosproc.com"

begin
	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ocx1   = CX1(omg)
	ocy2   = CY2(omg)

	# Perform zero-trip test (assumes 2D image)
	if (v[2] > ocy2)
	    return (EOF)

	# Perform first time initialisation
        if (MI_SB(mi) == NULL) {
	    call minl_initi (mi, v)
	} else {
	    # Reinitialise if caller has changed v since last call.
	    if (LINE(omg) != v[2])	# Assumes 2D image.
		call minl_initi (mi, v)
	}

	# Fill output buffer from input images.
	#
	sb   = MI_SB(mi)
	obuf = SB_DATA(sb)
	line = LINE(omg)

	# Initialise  buffer with blank value
	call amovki (int(blank), Memi[obuf], onx)

	# Loop over input images, skipping those which have no data in
	# current line.
	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    icy1 = CY1(img)
	    icy2 = CY2(img)
	    if (line < icy1 || line > icy2)
		next
	    icx1 = CX1(img)
	    icx2 = CX2(img)
	    idx1 = DX1(img)
	    idy1 = DY1(img)

	    # Get corresponding line of input image
	    iim   = MI_IM(mi, image)
	    if (mscnli (img, ibuf, Meml[IMIDX(img)]) == EOF)
		next

	    # Process input image line  writing to output buffer
	    iptr  = ibuf + idx1 - 1
	    optr  = obuf + icx1 - ocx1
	    nx    = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

	    call miproci (img, Memi[iptr], Memi[optr], nx, iline,
	    Memi[ovr], novr)
	}

	# Bump loop counters ready for next trip (assumes 2D)
	v[2] = v[2] + long(1)
	LINE(omg) = v[2]

	return (onx)
end



# MINL_INITx -- Set starting vectors for MIGNLx calls

procedure minl_initl (mi, v)

pointer	mi		#I MOSIM Pointer for mosaic.
long	v[IM_MAXDIM]	#I Initial value for loop counter.

int	nimage, onx, ony, ocy1, ocy2, icy1, icy2, idy1, image
pointer	omg, sb, obuf, img

include "mosproc.com"

begin
        nimage = MI_NIMS(mi)
        omg    = MI_MG(mi, nimage+1)
        onx    = NX(omg)
	ony    = NY(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

        if (MI_SB(mi) == NULL) {
            call malloc (sb,   LEN_SECBUFF, TY_STRUCT)
            call malloc (obuf, onx,         TY_LONG)
            MI_SB(mi)      = sb
            SB_DATA(sb)    = obuf
        } else {
	    sb = MI_SB(mi)

	    # Free old data buffer if any and allocate a new one
	    if (SB_DATA(sb) != NULL)
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))
            call malloc (obuf, onx,         TY_LONG)
            SB_DATA(sb)    = obuf
	}

	# Set data buffer pixel type
	SB_PIXTYPE(sb) = TY_LONG

	# Convert line counter from image to ccd coordinates
	v[2] = v[2] + ocy1 - 1
	# Initialise loop counters
	call amovl (v, Meml[IMIDX(omg)], IM_MAXDIM)

	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    call amovl (v, Meml[IMIDX(img)], IM_MAXDIM)

	    if (trim) {
		icy1 = CTY1(img)
		icy2 = CTY2(img)
		idy1 = TY1(img)
	    } else {
		icy1 = CY1(img)
		icy2 = CY2(img)
		idy1 = DY1(img)
	    }

	    if (v[2] <= icy1) {
		LINE(img) = idy1
	    } else if (v[2] <= icy2) {
		LINE(img) = v[2] - (ocy1 - icy1) + idy1
	    } else {
	        LINE(img) = NY(img) + 1   
	    }
	}
end

# MIGNLx -- Get and process next line from sub-image of mosaic.

int procedure mignll (mi, obuf, v)

pointer	mi		#I MOSIM Pointer for mosaic.
pointer	obuf		#O On output pointer to data values.
long	v[IM_MAXDIM]	#I Loop counter.
# function value	#O Number of pixels in line or EOF.

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	onx, ocx1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, line, iline, nx, image, novr

int	mscnll()

errchk	mscnll, linebiasl, malloc, syserrs

include	"mosproc.com"

begin
	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ocx1   = CX1(omg)
	ocy2   = CY2(omg)

	# Perform zero-trip test (assumes 2D image)
	if (v[2] > ocy2)
	    return (EOF)

	# Perform first time initialisation
        if (MI_SB(mi) == NULL) {
	    call minl_initl (mi, v)
	} else {
	    # Reinitialise if caller has changed v since last call.
	    if (LINE(omg) != v[2])	# Assumes 2D image.
		call minl_initl (mi, v)
	}

	# Fill output buffer from input images.
	#
	sb   = MI_SB(mi)
	obuf = SB_DATA(sb)
	line = LINE(omg)

	# Initialise  buffer with blank value
	call amovkl (long(blank), Meml[obuf], onx)

	# Loop over input images, skipping those which have no data in
	# current line.
	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    icy1 = CY1(img)
	    icy2 = CY2(img)
	    if (line < icy1 || line > icy2)
		next
	    icx1 = CX1(img)
	    icx2 = CX2(img)
	    idx1 = DX1(img)
	    idy1 = DY1(img)

	    # Get corresponding line of input image
	    iim   = MI_IM(mi, image)
	    if (mscnll (img, ibuf, Meml[IMIDX(img)]) == EOF)
		next

	    # Process input image line  writing to output buffer
	    iptr  = ibuf + idx1 - 1
	    optr  = obuf + icx1 - ocx1
	    nx    = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

	    call miprocl (img, Meml[iptr], Meml[optr], nx, iline,
	    Meml[ovr], novr)
	}

	# Bump loop counters ready for next trip (assumes 2D)
	v[2] = v[2] + long(1)
	LINE(omg) = v[2]

	return (onx)
end



# MINL_INITx -- Set starting vectors for MIGNLx calls

procedure minl_initr (mi, v)

pointer	mi		#I MOSIM Pointer for mosaic.
long	v[IM_MAXDIM]	#I Initial value for loop counter.

int	nimage, onx, ony, ocy1, ocy2, icy1, icy2, idy1, image
pointer	omg, sb, obuf, img

include "mosproc.com"

begin
        nimage = MI_NIMS(mi)
        omg    = MI_MG(mi, nimage+1)
        onx    = NX(omg)
	ony    = NY(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

        if (MI_SB(mi) == NULL) {
            call malloc (sb,   LEN_SECBUFF, TY_STRUCT)
            call malloc (obuf, onx,         TY_REAL)
            MI_SB(mi)      = sb
            SB_DATA(sb)    = obuf
        } else {
	    sb = MI_SB(mi)

	    # Free old data buffer if any and allocate a new one
	    if (SB_DATA(sb) != NULL)
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))
            call malloc (obuf, onx,         TY_REAL)
            SB_DATA(sb)    = obuf
	}

	# Set data buffer pixel type
	SB_PIXTYPE(sb) = TY_REAL

	# Convert line counter from image to ccd coordinates
	v[2] = v[2] + ocy1 - 1
	# Initialise loop counters
	call amovl (v, Meml[IMIDX(omg)], IM_MAXDIM)

	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    call amovl (v, Meml[IMIDX(img)], IM_MAXDIM)

	    if (trim) {
		icy1 = CTY1(img)
		icy2 = CTY2(img)
		idy1 = TY1(img)
	    } else {
		icy1 = CY1(img)
		icy2 = CY2(img)
		idy1 = DY1(img)
	    }

	    if (v[2] <= icy1) {
		LINE(img) = idy1
	    } else if (v[2] <= icy2) {
		LINE(img) = v[2] - (ocy1 - icy1) + idy1
	    } else {
	        LINE(img) = NY(img) + 1   
	    }
	}
end

# MIGNLx -- Get and process next line from sub-image of mosaic.

int procedure mignlr (mi, obuf, v)

pointer	mi		#I MOSIM Pointer for mosaic.
pointer	obuf		#O On output pointer to data values.
long	v[IM_MAXDIM]	#I Loop counter.
# function value	#O Number of pixels in line or EOF.

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	onx, ocx1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, line, iline, nx, image, novr

int	mscnlr()

errchk	mscnlr, linebiasr, malloc, syserrs

include	"mosproc.com"

begin
	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ocx1   = CX1(omg)
	ocy2   = CY2(omg)

	# Perform zero-trip test (assumes 2D image)
	if (v[2] > ocy2)
	    return (EOF)

	# Perform first time initialisation
        if (MI_SB(mi) == NULL) {
	    call minl_initr (mi, v)
	} else {
	    # Reinitialise if caller has changed v since last call.
	    if (LINE(omg) != v[2])	# Assumes 2D image.
		call minl_initr (mi, v)
	}

	# Fill output buffer from input images.
	#
	sb   = MI_SB(mi)
	obuf = SB_DATA(sb)
	line = LINE(omg)

	# Initialise  buffer with blank value
	call amovkr (real(blank), Memr[obuf], onx)

	# Loop over input images, skipping those which have no data in
	# current line.
	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    icy1 = CY1(img)
	    icy2 = CY2(img)
	    if (line < icy1 || line > icy2)
		next
	    icx1 = CX1(img)
	    icx2 = CX2(img)
	    idx1 = DX1(img)
	    idy1 = DY1(img)

	    # Get corresponding line of input image
	    iim   = MI_IM(mi, image)
	    if (mscnlr (img, ibuf, Meml[IMIDX(img)]) == EOF)
		next

	    # Process input image line  writing to output buffer
	    iptr  = ibuf + idx1 - 1
	    optr  = obuf + icx1 - ocx1
	    nx    = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

	    call miprocr (img, Memr[iptr], Memr[optr], nx, iline,
	    Memr[ovr], novr)
	}

	# Bump loop counters ready for next trip (assumes 2D)
	v[2] = v[2] + long(1)
	LINE(omg) = v[2]

	return (onx)
end



# MINL_INITx -- Set starting vectors for MIGNLx calls

procedure minl_initd (mi, v)

pointer	mi		#I MOSIM Pointer for mosaic.
long	v[IM_MAXDIM]	#I Initial value for loop counter.

int	nimage, onx, ony, ocy1, ocy2, icy1, icy2, idy1, image
pointer	omg, sb, obuf, img

include "mosproc.com"

begin
        nimage = MI_NIMS(mi)
        omg    = MI_MG(mi, nimage+1)
        onx    = NX(omg)
	ony    = NY(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

        if (MI_SB(mi) == NULL) {
            call malloc (sb,   LEN_SECBUFF, TY_STRUCT)
            call malloc (obuf, onx,         TY_DOUBLE)
            MI_SB(mi)      = sb
            SB_DATA(sb)    = obuf
        } else {
	    sb = MI_SB(mi)

	    # Free old data buffer if any and allocate a new one
	    if (SB_DATA(sb) != NULL)
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))
            call malloc (obuf, onx,         TY_DOUBLE)
            SB_DATA(sb)    = obuf
	}

	# Set data buffer pixel type
	SB_PIXTYPE(sb) = TY_DOUBLE

	# Convert line counter from image to ccd coordinates
	v[2] = v[2] + ocy1 - 1
	# Initialise loop counters
	call amovl (v, Meml[IMIDX(omg)], IM_MAXDIM)

	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    call amovl (v, Meml[IMIDX(img)], IM_MAXDIM)

	    if (trim) {
		icy1 = CTY1(img)
		icy2 = CTY2(img)
		idy1 = TY1(img)
	    } else {
		icy1 = CY1(img)
		icy2 = CY2(img)
		idy1 = DY1(img)
	    }

	    if (v[2] <= icy1) {
		LINE(img) = idy1
	    } else if (v[2] <= icy2) {
		LINE(img) = v[2] - (ocy1 - icy1) + idy1
	    } else {
	        LINE(img) = NY(img) + 1   
	    }
	}
end

# MIGNLx -- Get and process next line from sub-image of mosaic.

int procedure mignld (mi, obuf, v)

pointer	mi		#I MOSIM Pointer for mosaic.
pointer	obuf		#O On output pointer to data values.
long	v[IM_MAXDIM]	#I Loop counter.
# function value	#O Number of pixels in line or EOF.

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	onx, ocx1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, line, iline, nx, image, novr

int	mscnld()

errchk	mscnld, linebiasd, malloc, syserrs

include	"mosproc.com"

begin
	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ocx1   = CX1(omg)
	ocy2   = CY2(omg)

	# Perform zero-trip test (assumes 2D image)
	if (v[2] > ocy2)
	    return (EOF)

	# Perform first time initialisation
        if (MI_SB(mi) == NULL) {
	    call minl_initd (mi, v)
	} else {
	    # Reinitialise if caller has changed v since last call.
	    if (LINE(omg) != v[2])	# Assumes 2D image.
		call minl_initd (mi, v)
	}

	# Fill output buffer from input images.
	#
	sb   = MI_SB(mi)
	obuf = SB_DATA(sb)
	line = LINE(omg)

	# Initialise  buffer with blank value
	call amovkd (double(blank), Memd[obuf], onx)

	# Loop over input images, skipping those which have no data in
	# current line.
	do image = 1, nimage {
	    img = MI_MG(mi, image)
	    icy1 = CY1(img)
	    icy2 = CY2(img)
	    if (line < icy1 || line > icy2)
		next
	    icx1 = CX1(img)
	    icx2 = CX2(img)
	    idx1 = DX1(img)
	    idy1 = DY1(img)

	    # Get corresponding line of input image
	    iim   = MI_IM(mi, image)
	    if (mscnld (img, ibuf, Meml[IMIDX(img)]) == EOF)
		next

	    # Process input image line  writing to output buffer
	    iptr  = ibuf + idx1 - 1
	    optr  = obuf + icx1 - ocx1
	    nx    = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

	    call miprocd (img, Memd[iptr], Memd[optr], nx, iline,
	    Memd[ovr], novr)
	}

	# Bump loop counters ready for next trip (assumes 2D)
	v[2] = v[2] + long(1)
	LINE(omg) = v[2]

	return (onx)
end


