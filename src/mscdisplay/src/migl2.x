include <syserr.h>
include "mosim.h"
include "mosgeom.h"
include "mosproc.h"



# MIGL2x -- Get and process line from sub-image of mosaic.

pointer procedure migl2s (mi, linein)

pointer	mi		#I MOSIM Pointer for mosaic.
int	linein		#I Line required.
pointer	obuf		#O pointer to data values

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	line
int	onx, ony, ocx1, ocx2, ocy1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, iline, nx, image, novr

pointer	mscl2s()

errchk	mscl2s, malloc, syserrs

include	"mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

	line = linein + ocy1
	if (line < ocy1 || line > ocy2) {
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])
	}

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    call malloc (obuf, onx,         TY_SHORT) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ocx1
	    SB_X2(sb)      = ocx2
	    SB_Y1(sb)      = line
	    SB_Y2(sb)      = line
	    SB_PIXTYPE(sb) = TY_SHORT
	} else {
	    sb = MI_SB(mi)

	    # The required data is already in the buffer
	    if ((SB_Y1(sb) == line) && (SB_Y2(sb) == line) &&
		(SB_X1(sb) == ocx1) && (SB_X2(sb) == ocx2)) { 

		# and is the correct data type. We are done!
		if (SB_PIXTYPE(sb) == TY_SHORT) {
		    return (SB_DATA(sb))

		# Change data type and return
		} else {
		    call malloc (obuf, onx, TY_SHORT)
		    switch (SB_PIXTYPE(sb)) {
		    case TY_SHORT:
			call achtss (Mems[SB_DATA(sb)], Mems[obuf], onx)
#		    case TY_USHORT:
#			call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], onx)
		    case TY_INT:
			call achtis (Memi[SB_DATA(sb)], Mems[obuf], onx)
		    case TY_LONG:
			call achtls (Meml[SB_DATA(sb)], Mems[obuf], onx)
		    case TY_REAL:
			call achtrs (Memr[SB_DATA(sb)], Mems[obuf], onx)
		    case TY_DOUBLE:
			call achtds (Memd[SB_DATA(sb)], Mems[obuf], onx)
		    }

		    # Free old buffer
		    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		    # Return new buffer
		    SB_DATA(sb)    = obuf
		    SB_PIXTYPE(sb) = TY_SHORT
		    call pargi (SB_DATA(sb))
		    return (SB_DATA(sb))
		}

	    } else {

		# Free old and allocate new data buffer.
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		call malloc (obuf, onx, TY_SHORT) 
		SB_DATA(sb)    = obuf
		SB_X1(sb)      = ocx1
		SB_X2(sb)      = ocx2
		SB_Y1(sb)      = line
		SB_Y2(sb)      = line
		SB_PIXTYPE(sb) = TY_SHORT
	    }
	}

	# Fill output buffer from input images.
	#
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
	    iline = line - (icy1 - ocy1) + idy1 - 1
	    ibuf  = mscl2s (img, iline)

	    # Process input image line  writing to output buffer
	    iptr = ibuf + idx1 - 1
	    optr = obuf + icx1 - ocx1
	    nx   = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

            call miprocs (img, Mems[iptr], Mems[optr], nx, iline,
	    Mems[ovr], novr)

	}

	return (SB_DATA(sb))
end



# MIGL2x -- Get and process line from sub-image of mosaic.

pointer procedure migl2i (mi, linein)

pointer	mi		#I MOSIM Pointer for mosaic.
int	linein		#I Line required.
pointer	obuf		#O pointer to data values

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	line
int	onx, ony, ocx1, ocx2, ocy1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, iline, nx, image, novr

pointer	mscl2i()

errchk	mscl2i, malloc, syserrs

include	"mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

	line = linein + ocy1
	if (line < ocy1 || line > ocy2) {
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])
	}

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    call malloc (obuf, onx,         TY_INT) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ocx1
	    SB_X2(sb)      = ocx2
	    SB_Y1(sb)      = line
	    SB_Y2(sb)      = line
	    SB_PIXTYPE(sb) = TY_INT
	} else {
	    sb = MI_SB(mi)

	    # The required data is already in the buffer
	    if ((SB_Y1(sb) == line) && (SB_Y2(sb) == line) &&
		(SB_X1(sb) == ocx1) && (SB_X2(sb) == ocx2)) { 

		# and is the correct data type. We are done!
		if (SB_PIXTYPE(sb) == TY_INT) {
		    return (SB_DATA(sb))

		# Change data type and return
		} else {
		    call malloc (obuf, onx, TY_INT)
		    switch (SB_PIXTYPE(sb)) {
		    case TY_SHORT:
			call achtsi (Mems[SB_DATA(sb)], Memi[obuf], onx)
#		    case TY_USHORT:
#			call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], onx)
		    case TY_INT:
			call achtii (Memi[SB_DATA(sb)], Memi[obuf], onx)
		    case TY_LONG:
			call achtli (Meml[SB_DATA(sb)], Memi[obuf], onx)
		    case TY_REAL:
			call achtri (Memr[SB_DATA(sb)], Memi[obuf], onx)
		    case TY_DOUBLE:
			call achtdi (Memd[SB_DATA(sb)], Memi[obuf], onx)
		    }

		    # Free old buffer
		    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		    # Return new buffer
		    SB_DATA(sb)    = obuf
		    SB_PIXTYPE(sb) = TY_INT
		    call pargi (SB_DATA(sb))
		    return (SB_DATA(sb))
		}

	    } else {

		# Free old and allocate new data buffer.
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		call malloc (obuf, onx, TY_INT) 
		SB_DATA(sb)    = obuf
		SB_X1(sb)      = ocx1
		SB_X2(sb)      = ocx2
		SB_Y1(sb)      = line
		SB_Y2(sb)      = line
		SB_PIXTYPE(sb) = TY_INT
	    }
	}

	# Fill output buffer from input images.
	#
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
	    iline = line - (icy1 - ocy1) + idy1 - 1
	    ibuf  = mscl2i (img, iline)

	    # Process input image line  writing to output buffer
	    iptr = ibuf + idx1 - 1
	    optr = obuf + icx1 - ocx1
	    nx   = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

            call miproci (img, Memi[iptr], Memi[optr], nx, iline,
	    Memi[ovr], novr)

	}

	return (SB_DATA(sb))
end



# MIGL2x -- Get and process line from sub-image of mosaic.

pointer procedure migl2l (mi, linein)

pointer	mi		#I MOSIM Pointer for mosaic.
int	linein		#I Line required.
pointer	obuf		#O pointer to data values

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	line
int	onx, ony, ocx1, ocx2, ocy1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, iline, nx, image, novr

pointer	mscl2l()

errchk	mscl2l, malloc, syserrs

include	"mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

	line = linein + ocy1
	if (line < ocy1 || line > ocy2) {
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])
	}

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    call malloc (obuf, onx,         TY_LONG) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ocx1
	    SB_X2(sb)      = ocx2
	    SB_Y1(sb)      = line
	    SB_Y2(sb)      = line
	    SB_PIXTYPE(sb) = TY_LONG
	} else {
	    sb = MI_SB(mi)

	    # The required data is already in the buffer
	    if ((SB_Y1(sb) == line) && (SB_Y2(sb) == line) &&
		(SB_X1(sb) == ocx1) && (SB_X2(sb) == ocx2)) { 

		# and is the correct data type. We are done!
		if (SB_PIXTYPE(sb) == TY_LONG) {
		    return (SB_DATA(sb))

		# Change data type and return
		} else {
		    call malloc (obuf, onx, TY_LONG)
		    switch (SB_PIXTYPE(sb)) {
		    case TY_SHORT:
			call achtsl (Mems[SB_DATA(sb)], Meml[obuf], onx)
#		    case TY_USHORT:
#			call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], onx)
		    case TY_INT:
			call achtil (Memi[SB_DATA(sb)], Meml[obuf], onx)
		    case TY_LONG:
			call achtll (Meml[SB_DATA(sb)], Meml[obuf], onx)
		    case TY_REAL:
			call achtrl (Memr[SB_DATA(sb)], Meml[obuf], onx)
		    case TY_DOUBLE:
			call achtdl (Memd[SB_DATA(sb)], Meml[obuf], onx)
		    }

		    # Free old buffer
		    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		    # Return new buffer
		    SB_DATA(sb)    = obuf
		    SB_PIXTYPE(sb) = TY_LONG
		    call pargi (SB_DATA(sb))
		    return (SB_DATA(sb))
		}

	    } else {

		# Free old and allocate new data buffer.
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		call malloc (obuf, onx, TY_LONG) 
		SB_DATA(sb)    = obuf
		SB_X1(sb)      = ocx1
		SB_X2(sb)      = ocx2
		SB_Y1(sb)      = line
		SB_Y2(sb)      = line
		SB_PIXTYPE(sb) = TY_LONG
	    }
	}

	# Fill output buffer from input images.
	#
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
	    iline = line - (icy1 - ocy1) + idy1 - 1
	    ibuf  = mscl2l (img, iline)

	    # Process input image line  writing to output buffer
	    iptr = ibuf + idx1 - 1
	    optr = obuf + icx1 - ocx1
	    nx   = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

            call miprocl (img, Meml[iptr], Meml[optr], nx, iline,
	    Meml[ovr], novr)

	}

	return (SB_DATA(sb))
end



# MIGL2x -- Get and process line from sub-image of mosaic.

pointer procedure migl2r (mi, linein)

pointer	mi		#I MOSIM Pointer for mosaic.
int	linein		#I Line required.
pointer	obuf		#O pointer to data values

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	line
int	onx, ony, ocx1, ocx2, ocy1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, iline, nx, image, novr

pointer	mscl2r()

errchk	mscl2r, malloc, syserrs

include	"mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

	line = linein + ocy1
	if (line < ocy1 || line > ocy2) {
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])
	}

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    call malloc (obuf, onx,         TY_REAL) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ocx1
	    SB_X2(sb)      = ocx2
	    SB_Y1(sb)      = line
	    SB_Y2(sb)      = line
	    SB_PIXTYPE(sb) = TY_REAL
	} else {
	    sb = MI_SB(mi)

	    # The required data is already in the buffer
	    if ((SB_Y1(sb) == line) && (SB_Y2(sb) == line) &&
		(SB_X1(sb) == ocx1) && (SB_X2(sb) == ocx2)) { 

		# and is the correct data type. We are done!
		if (SB_PIXTYPE(sb) == TY_REAL) {
		    return (SB_DATA(sb))

		# Change data type and return
		} else {
		    call malloc (obuf, onx, TY_REAL)
		    switch (SB_PIXTYPE(sb)) {
		    case TY_SHORT:
			call achtsr (Mems[SB_DATA(sb)], Memr[obuf], onx)
#		    case TY_USHORT:
#			call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], onx)
		    case TY_INT:
			call achtir (Memi[SB_DATA(sb)], Memr[obuf], onx)
		    case TY_LONG:
			call achtlr (Meml[SB_DATA(sb)], Memr[obuf], onx)
		    case TY_REAL:
			call achtrr (Memr[SB_DATA(sb)], Memr[obuf], onx)
		    case TY_DOUBLE:
			call achtdr (Memd[SB_DATA(sb)], Memr[obuf], onx)
		    }

		    # Free old buffer
		    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		    # Return new buffer
		    SB_DATA(sb)    = obuf
		    SB_PIXTYPE(sb) = TY_REAL
		    call pargi (SB_DATA(sb))
		    return (SB_DATA(sb))
		}

	    } else {

		# Free old and allocate new data buffer.
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		call malloc (obuf, onx, TY_REAL) 
		SB_DATA(sb)    = obuf
		SB_X1(sb)      = ocx1
		SB_X2(sb)      = ocx2
		SB_Y1(sb)      = line
		SB_Y2(sb)      = line
		SB_PIXTYPE(sb) = TY_REAL
	    }
	}

	# Fill output buffer from input images.
	#
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
	    iline = line - (icy1 - ocy1) + idy1 - 1
	    ibuf  = mscl2r (img, iline)

	    # Process input image line  writing to output buffer
	    iptr = ibuf + idx1 - 1
	    optr = obuf + icx1 - ocx1
	    nx   = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

            call miprocr (img, Memr[iptr], Memr[optr], nx, iline,
	    Memr[ovr], novr)

	}

	return (SB_DATA(sb))
end



# MIGL2x -- Get and process line from sub-image of mosaic.

pointer procedure migl2d (mi, linein)

pointer	mi		#I MOSIM Pointer for mosaic.
int	linein		#I Line required.
pointer	obuf		#O pointer to data values

pointer	omg, sb, img, iim, ibuf, iptr, optr, ovr
int	line
int	onx, ony, ocx1, ocx2, ocy1, ocy2, icx1, icx2, icy1, icy2, idx1, idy1
int	nimage, iline, nx, image, novr

pointer	mscl2d()

errchk	mscl2d, malloc, syserrs

include	"mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

	line = linein + ocy1
	if (line < ocy1 || line > ocy2) {
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])
	}

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    call malloc (obuf, onx,         TY_DOUBLE) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ocx1
	    SB_X2(sb)      = ocx2
	    SB_Y1(sb)      = line
	    SB_Y2(sb)      = line
	    SB_PIXTYPE(sb) = TY_DOUBLE
	} else {
	    sb = MI_SB(mi)

	    # The required data is already in the buffer
	    if ((SB_Y1(sb) == line) && (SB_Y2(sb) == line) &&
		(SB_X1(sb) == ocx1) && (SB_X2(sb) == ocx2)) { 

		# and is the correct data type. We are done!
		if (SB_PIXTYPE(sb) == TY_DOUBLE) {
		    return (SB_DATA(sb))

		# Change data type and return
		} else {
		    call malloc (obuf, onx, TY_DOUBLE)
		    switch (SB_PIXTYPE(sb)) {
		    case TY_SHORT:
			call achtsd (Mems[SB_DATA(sb)], Memd[obuf], onx)
#		    case TY_USHORT:
#			call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], onx)
		    case TY_INT:
			call achtid (Memi[SB_DATA(sb)], Memd[obuf], onx)
		    case TY_LONG:
			call achtld (Meml[SB_DATA(sb)], Memd[obuf], onx)
		    case TY_REAL:
			call achtrd (Memr[SB_DATA(sb)], Memd[obuf], onx)
		    case TY_DOUBLE:
			call achtdd (Memd[SB_DATA(sb)], Memd[obuf], onx)
		    }

		    # Free old buffer
		    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		    # Return new buffer
		    SB_DATA(sb)    = obuf
		    SB_PIXTYPE(sb) = TY_DOUBLE
		    call pargi (SB_DATA(sb))
		    return (SB_DATA(sb))
		}

	    } else {

		# Free old and allocate new data buffer.
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		call malloc (obuf, onx, TY_DOUBLE) 
		SB_DATA(sb)    = obuf
		SB_X1(sb)      = ocx1
		SB_X2(sb)      = ocx2
		SB_Y1(sb)      = line
		SB_Y2(sb)      = line
		SB_PIXTYPE(sb) = TY_DOUBLE
	    }
	}

	# Fill output buffer from input images.
	#
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
	    iline = line - (icy1 - ocy1) + idy1 - 1
	    ibuf  = mscl2d (img, iline)

	    # Process input image line  writing to output buffer
	    iptr = ibuf + idx1 - 1
	    optr = obuf + icx1 - ocx1
	    nx   = icx2 - icx1 + 1
	    iline = line - CY1(img) + 1
	    ovr   = ibuf + BX1(img) - 1
	    novr  = BX2(img) - BX1(img) + 1

            call miprocd (img, Memd[iptr], Memd[optr], nx, iline,
	    Memd[ovr], novr)

	}

	return (SB_DATA(sb))
end


