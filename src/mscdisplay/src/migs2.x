include <syserr.h>
include "mosim.h"
include "mosgeom.h"
include "mosproc.h"



# MIGS2x -- Get and process a section of a (2D) mosaic image.

pointer procedure migs2s (mi, x1in, x2in, y1in, y2in)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1in, x2in	#I Range of columns in section.
int	y1in, y2in	#I Range of columns in section.
pointer	obuf		#O pointer to data values.

pointer	omg, sb
int	x1, x2, y1, y2
int	onx, ocx1, ocx2, ox1, ox2
int	ony, ocy1, ocy2, oy1, oy2
int	nimage, npix 

errchk	syserrs, malloc, migetss, micpys

include "mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

#	x1 = x1in + ocx1
#	x2 = x2in + ocx1
#	y1 = y1in + ocy1
#	y2 = y2in + ocy1
	x1 = x1in
	x2 = x2in
	y1 = y1in
	y2 = y2in

#call eprintf ("x1=%d x2=%d y1=%d y2=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call eprintf ("nimage=%d ocx1=%d ocx2=%d ocy1=%d ocy2=%d\n")
#call pargi (nimage)
#call pargi (ocx1)
#call pargi (ocx2)
#call pargi (ocy1)
#call pargi (ocy2)

	# Complain if section totaly out of bounds
	if (x2 < ocx1 || x1 > ocx2 || y1 < ocy1 || y1 > ocy2)
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])

	# Clip section at boundary of mosaic
	ox1  = max (x1, ocx1)
	ox2  = min (x2, ocx2)
	oy1  = max (y1, ocy1)
	oy2  = min (y2, ocy2)
	onx  = ox2 - ox1 + 1
	ony  = oy2 - oy1 + 1
	npix = onx * ony

#call eprintf ("ox1=%d ox2=%d oy1=%d oy2=%d onx=%d ony=%d npix=%d\n")
#call pargi (ox1)
#call pargi (ox2)
#call pargi (oy1)
#call pargi (oy2)
#call pargi (onx)
#call pargi (ony)
#call pargi (npix)

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = NULL
	    # Set null section in SB to ensure buffer will be filled
	    SB_X1(sb)      = 0
	    SB_X2(sb)      = 0
	    SB_Y1(sb)      = 0
	    SB_Y2(sb)      = 0
	    SB_PIXTYPE(sb) = TY_SHORT
	} else {
	    sb = MI_SB(mi)
	}

#call eprintf ("sb_x1=%d sb_x2=%d sb_y1=%d sb_y2=%d\n")
#call pargi (SB_X1(sb))
#call pargi (SB_X2(sb))
#call pargi (SB_Y1(sb))
#call pargi (SB_Y2(sb))

	# The requested section is entirely outside the buffer
	 if ((SB_X2(sb) < ox1) || (SB_X1(sb) > ox2) || 
	     (SB_Y2(sb) < oy1) || (SB_Y1(sb) > oy2)) { 

	    # Free old data buffer.
	    if (SB_DATA(sb) != NULL) 
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    # Allocate new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_SHORT) 
	    call amovks (short(blank), Mems[obuf], npix)

	    call migetss (mi, ox1, ox2, oy1, oy2, ox1, ox2, oy1, oy2, obuf)
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_SHORT
	    return (SB_DATA(sb))

	# Exactly the requested section is already in the buffer
	} else if ((SB_X1(sb) == ox1) && (SB_X2(sb) == ox2) &&
	           (SB_Y1(sb) == oy1) && (SB_Y2(sb) == oy2)) { 
	    # and is the correct data type. We are done!
	    if (SB_PIXTYPE(sb) == TY_SHORT) {
		return (SB_DATA(sb))

	    # Change data type and return
	    } else {
		call malloc (obuf, npix, TY_SHORT)
		switch (SB_PIXTYPE(sb)) {
		case TY_SHORT:
		    call achtss (Mems[SB_DATA(sb)], Mems[obuf], npix)
#	        case TY_USHORT:
#		    call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], npix)
		case TY_INT:
		    call achtis (Memi[SB_DATA(sb)], Mems[obuf], npix)
		case TY_LONG:
		    call achtls (Meml[SB_DATA(sb)], Mems[obuf], npix)
		case TY_REAL:
		    call achtrs (Memr[SB_DATA(sb)], Mems[obuf], npix)
		case TY_DOUBLE:
		    call achtds (Memd[SB_DATA(sb)], Mems[obuf], npix)
		}

		# Free old buffer
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		# Return new buffer
		SB_DATA(sb)    = obuf
		SB_PIXTYPE(sb) = TY_SHORT
		return (SB_DATA(sb))
	    }

	# The requested section is entirely contained in the buffer
	} else if ((SB_X1(sb) <= ox1) && (SB_X2(sb) >= ox2) && 
		   (SB_Y1(sb) <= oy1) && (SB_Y2(sb) >= oy2)) { 

	    # Copy the part we need to a new buffer
	    call malloc (obuf, npix, TY_SHORT) 
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_SHORT, obuf)
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_SHORT

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb) = obuf
	    return (SB_DATA(sb))

	# The requested section partialy overlaps the data buffer (oh joy!)
	} else {

	    # Allocate a new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_SHORT) 
	    call amovks (short(blank), Mems[obuf], npix)

	    # Copy what we can from the buffer.
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_SHORT, obuf)

  	    # Fill out the rest by reading the images.
  	    if (oy1 < SB_Y1(sb))
  		call migetss (mi, ox1, ox2, oy1, SB_Y1(sb)-1, 
 		ox1, ox2, oy1, oy2, obuf)
 	    if (ox1 < SB_X1(sb))
 		call migetss (mi, ox1, SB_X1(sb)-1, SB_Y1(sb), SB_Y2(sb), 
		ox1, ox2, oy1, oy2, obuf)
  	    if (ox2 > SB_X2(sb))
  		call migetss (mi, SB_X2(sb)+1, ox2, SB_Y1(sb), SB_Y2(sb),
 		ox1, ox2, oy1, oy2, obuf)
  	    if (oy2 > SB_Y2(sb))
  		 call migetss (mi, ox1, ox2, SB_Y2(sb)+1, oy2,
 		 ox1, ox2, oy1, oy2, obuf)

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_SHORT
	    return (SB_DATA(sb))

	}
end



# MIGS2x -- Get and process a section of a (2D) mosaic image.

pointer procedure migs2i (mi, x1in, x2in, y1in, y2in)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1in, x2in	#I Range of columns in section.
int	y1in, y2in	#I Range of columns in section.
pointer	obuf		#O pointer to data values.

pointer	omg, sb
int	x1, x2, y1, y2
int	onx, ocx1, ocx2, ox1, ox2
int	ony, ocy1, ocy2, oy1, oy2
int	nimage, npix 

errchk	syserrs, malloc, migetsi, micpys

include "mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

#	x1 = x1in + ocx1
#	x2 = x2in + ocx1
#	y1 = y1in + ocy1
#	y2 = y2in + ocy1
	x1 = x1in
	x2 = x2in
	y1 = y1in
	y2 = y2in

#call eprintf ("x1=%d x2=%d y1=%d y2=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call eprintf ("nimage=%d ocx1=%d ocx2=%d ocy1=%d ocy2=%d\n")
#call pargi (nimage)
#call pargi (ocx1)
#call pargi (ocx2)
#call pargi (ocy1)
#call pargi (ocy2)

	# Complain if section totaly out of bounds
	if (x2 < ocx1 || x1 > ocx2 || y1 < ocy1 || y1 > ocy2)
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])

	# Clip section at boundary of mosaic
	ox1  = max (x1, ocx1)
	ox2  = min (x2, ocx2)
	oy1  = max (y1, ocy1)
	oy2  = min (y2, ocy2)
	onx  = ox2 - ox1 + 1
	ony  = oy2 - oy1 + 1
	npix = onx * ony

#call eprintf ("ox1=%d ox2=%d oy1=%d oy2=%d onx=%d ony=%d npix=%d\n")
#call pargi (ox1)
#call pargi (ox2)
#call pargi (oy1)
#call pargi (oy2)
#call pargi (onx)
#call pargi (ony)
#call pargi (npix)

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = NULL
	    # Set null section in SB to ensure buffer will be filled
	    SB_X1(sb)      = 0
	    SB_X2(sb)      = 0
	    SB_Y1(sb)      = 0
	    SB_Y2(sb)      = 0
	    SB_PIXTYPE(sb) = TY_INT
	} else {
	    sb = MI_SB(mi)
	}

#call eprintf ("sb_x1=%d sb_x2=%d sb_y1=%d sb_y2=%d\n")
#call pargi (SB_X1(sb))
#call pargi (SB_X2(sb))
#call pargi (SB_Y1(sb))
#call pargi (SB_Y2(sb))

	# The requested section is entirely outside the buffer
	 if ((SB_X2(sb) < ox1) || (SB_X1(sb) > ox2) || 
	     (SB_Y2(sb) < oy1) || (SB_Y1(sb) > oy2)) { 

	    # Free old data buffer.
	    if (SB_DATA(sb) != NULL) 
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    # Allocate new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_INT) 
	    call amovki (int(blank), Memi[obuf], npix)

	    call migetsi (mi, ox1, ox2, oy1, oy2, ox1, ox2, oy1, oy2, obuf)
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_INT
	    return (SB_DATA(sb))

	# Exactly the requested section is already in the buffer
	} else if ((SB_X1(sb) == ox1) && (SB_X2(sb) == ox2) &&
	           (SB_Y1(sb) == oy1) && (SB_Y2(sb) == oy2)) { 
	    # and is the correct data type. We are done!
	    if (SB_PIXTYPE(sb) == TY_INT) {
		return (SB_DATA(sb))

	    # Change data type and return
	    } else {
		call malloc (obuf, npix, TY_INT)
		switch (SB_PIXTYPE(sb)) {
		case TY_SHORT:
		    call achtsi (Mems[SB_DATA(sb)], Memi[obuf], npix)
#	        case TY_USHORT:
#		    call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], npix)
		case TY_INT:
		    call achtii (Memi[SB_DATA(sb)], Memi[obuf], npix)
		case TY_LONG:
		    call achtli (Meml[SB_DATA(sb)], Memi[obuf], npix)
		case TY_REAL:
		    call achtri (Memr[SB_DATA(sb)], Memi[obuf], npix)
		case TY_DOUBLE:
		    call achtdi (Memd[SB_DATA(sb)], Memi[obuf], npix)
		}

		# Free old buffer
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		# Return new buffer
		SB_DATA(sb)    = obuf
		SB_PIXTYPE(sb) = TY_INT
		return (SB_DATA(sb))
	    }

	# The requested section is entirely contained in the buffer
	} else if ((SB_X1(sb) <= ox1) && (SB_X2(sb) >= ox2) && 
		   (SB_Y1(sb) <= oy1) && (SB_Y2(sb) >= oy2)) { 

	    # Copy the part we need to a new buffer
	    call malloc (obuf, npix, TY_INT) 
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_INT, obuf)
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_INT

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb) = obuf
	    return (SB_DATA(sb))

	# The requested section partialy overlaps the data buffer (oh joy!)
	} else {

	    # Allocate a new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_INT) 
	    call amovki (int(blank), Memi[obuf], npix)

	    # Copy what we can from the buffer.
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_INT, obuf)

  	    # Fill out the rest by reading the images.
  	    if (oy1 < SB_Y1(sb))
  		call migetsi (mi, ox1, ox2, oy1, SB_Y1(sb)-1, 
 		ox1, ox2, oy1, oy2, obuf)
 	    if (ox1 < SB_X1(sb))
 		call migetsi (mi, ox1, SB_X1(sb)-1, SB_Y1(sb), SB_Y2(sb), 
		ox1, ox2, oy1, oy2, obuf)
  	    if (ox2 > SB_X2(sb))
  		call migetsi (mi, SB_X2(sb)+1, ox2, SB_Y1(sb), SB_Y2(sb),
 		ox1, ox2, oy1, oy2, obuf)
  	    if (oy2 > SB_Y2(sb))
  		 call migetsi (mi, ox1, ox2, SB_Y2(sb)+1, oy2,
 		 ox1, ox2, oy1, oy2, obuf)

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_INT
	    return (SB_DATA(sb))

	}
end



# MIGS2x -- Get and process a section of a (2D) mosaic image.

pointer procedure migs2l (mi, x1in, x2in, y1in, y2in)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1in, x2in	#I Range of columns in section.
int	y1in, y2in	#I Range of columns in section.
pointer	obuf		#O pointer to data values.

pointer	omg, sb
int	x1, x2, y1, y2
int	onx, ocx1, ocx2, ox1, ox2
int	ony, ocy1, ocy2, oy1, oy2
int	nimage, npix 

errchk	syserrs, malloc, migetsl, micpys

include "mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

#	x1 = x1in + ocx1
#	x2 = x2in + ocx1
#	y1 = y1in + ocy1
#	y2 = y2in + ocy1
	x1 = x1in
	x2 = x2in
	y1 = y1in
	y2 = y2in

#call eprintf ("x1=%d x2=%d y1=%d y2=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call eprintf ("nimage=%d ocx1=%d ocx2=%d ocy1=%d ocy2=%d\n")
#call pargi (nimage)
#call pargi (ocx1)
#call pargi (ocx2)
#call pargi (ocy1)
#call pargi (ocy2)

	# Complain if section totaly out of bounds
	if (x2 < ocx1 || x1 > ocx2 || y1 < ocy1 || y1 > ocy2)
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])

	# Clip section at boundary of mosaic
	ox1  = max (x1, ocx1)
	ox2  = min (x2, ocx2)
	oy1  = max (y1, ocy1)
	oy2  = min (y2, ocy2)
	onx  = ox2 - ox1 + 1
	ony  = oy2 - oy1 + 1
	npix = onx * ony

#call eprintf ("ox1=%d ox2=%d oy1=%d oy2=%d onx=%d ony=%d npix=%d\n")
#call pargi (ox1)
#call pargi (ox2)
#call pargi (oy1)
#call pargi (oy2)
#call pargi (onx)
#call pargi (ony)
#call pargi (npix)

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = NULL
	    # Set null section in SB to ensure buffer will be filled
	    SB_X1(sb)      = 0
	    SB_X2(sb)      = 0
	    SB_Y1(sb)      = 0
	    SB_Y2(sb)      = 0
	    SB_PIXTYPE(sb) = TY_LONG
	} else {
	    sb = MI_SB(mi)
	}

#call eprintf ("sb_x1=%d sb_x2=%d sb_y1=%d sb_y2=%d\n")
#call pargi (SB_X1(sb))
#call pargi (SB_X2(sb))
#call pargi (SB_Y1(sb))
#call pargi (SB_Y2(sb))

	# The requested section is entirely outside the buffer
	 if ((SB_X2(sb) < ox1) || (SB_X1(sb) > ox2) || 
	     (SB_Y2(sb) < oy1) || (SB_Y1(sb) > oy2)) { 

	    # Free old data buffer.
	    if (SB_DATA(sb) != NULL) 
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    # Allocate new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_LONG) 
	    call amovkl (long(blank), Meml[obuf], npix)

	    call migetsl (mi, ox1, ox2, oy1, oy2, ox1, ox2, oy1, oy2, obuf)
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_LONG
	    return (SB_DATA(sb))

	# Exactly the requested section is already in the buffer
	} else if ((SB_X1(sb) == ox1) && (SB_X2(sb) == ox2) &&
	           (SB_Y1(sb) == oy1) && (SB_Y2(sb) == oy2)) { 
	    # and is the correct data type. We are done!
	    if (SB_PIXTYPE(sb) == TY_LONG) {
		return (SB_DATA(sb))

	    # Change data type and return
	    } else {
		call malloc (obuf, npix, TY_LONG)
		switch (SB_PIXTYPE(sb)) {
		case TY_SHORT:
		    call achtsl (Mems[SB_DATA(sb)], Meml[obuf], npix)
#	        case TY_USHORT:
#		    call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], npix)
		case TY_INT:
		    call achtil (Memi[SB_DATA(sb)], Meml[obuf], npix)
		case TY_LONG:
		    call achtll (Meml[SB_DATA(sb)], Meml[obuf], npix)
		case TY_REAL:
		    call achtrl (Memr[SB_DATA(sb)], Meml[obuf], npix)
		case TY_DOUBLE:
		    call achtdl (Memd[SB_DATA(sb)], Meml[obuf], npix)
		}

		# Free old buffer
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		# Return new buffer
		SB_DATA(sb)    = obuf
		SB_PIXTYPE(sb) = TY_LONG
		return (SB_DATA(sb))
	    }

	# The requested section is entirely contained in the buffer
	} else if ((SB_X1(sb) <= ox1) && (SB_X2(sb) >= ox2) && 
		   (SB_Y1(sb) <= oy1) && (SB_Y2(sb) >= oy2)) { 

	    # Copy the part we need to a new buffer
	    call malloc (obuf, npix, TY_LONG) 
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_LONG, obuf)
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_LONG

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb) = obuf
	    return (SB_DATA(sb))

	# The requested section partialy overlaps the data buffer (oh joy!)
	} else {

	    # Allocate a new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_LONG) 
	    call amovkl (long(blank), Meml[obuf], npix)

	    # Copy what we can from the buffer.
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_LONG, obuf)

  	    # Fill out the rest by reading the images.
  	    if (oy1 < SB_Y1(sb))
  		call migetsl (mi, ox1, ox2, oy1, SB_Y1(sb)-1, 
 		ox1, ox2, oy1, oy2, obuf)
 	    if (ox1 < SB_X1(sb))
 		call migetsl (mi, ox1, SB_X1(sb)-1, SB_Y1(sb), SB_Y2(sb), 
		ox1, ox2, oy1, oy2, obuf)
  	    if (ox2 > SB_X2(sb))
  		call migetsl (mi, SB_X2(sb)+1, ox2, SB_Y1(sb), SB_Y2(sb),
 		ox1, ox2, oy1, oy2, obuf)
  	    if (oy2 > SB_Y2(sb))
  		 call migetsl (mi, ox1, ox2, SB_Y2(sb)+1, oy2,
 		 ox1, ox2, oy1, oy2, obuf)

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_LONG
	    return (SB_DATA(sb))

	}
end



# MIGS2x -- Get and process a section of a (2D) mosaic image.

pointer procedure migs2r (mi, x1in, x2in, y1in, y2in)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1in, x2in	#I Range of columns in section.
int	y1in, y2in	#I Range of columns in section.
pointer	obuf		#O pointer to data values.

pointer	omg, sb
int	x1, x2, y1, y2
int	onx, ocx1, ocx2, ox1, ox2
int	ony, ocy1, ocy2, oy1, oy2
int	nimage, npix 

errchk	syserrs, malloc, migetsr, micpys

include "mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

#	x1 = x1in + ocx1
#	x2 = x2in + ocx1
#	y1 = y1in + ocy1
#	y2 = y2in + ocy1
	x1 = x1in
	x2 = x2in
	y1 = y1in
	y2 = y2in

#call eprintf ("x1=%d x2=%d y1=%d y2=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call eprintf ("nimage=%d ocx1=%d ocx2=%d ocy1=%d ocy2=%d\n")
#call pargi (nimage)
#call pargi (ocx1)
#call pargi (ocx2)
#call pargi (ocy1)
#call pargi (ocy2)

	# Complain if section totaly out of bounds
	if (x2 < ocx1 || x1 > ocx2 || y1 < ocy1 || y1 > ocy2)
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])

	# Clip section at boundary of mosaic
	ox1  = max (x1, ocx1)
	ox2  = min (x2, ocx2)
	oy1  = max (y1, ocy1)
	oy2  = min (y2, ocy2)
	onx  = ox2 - ox1 + 1
	ony  = oy2 - oy1 + 1
	npix = onx * ony

#call eprintf ("ox1=%d ox2=%d oy1=%d oy2=%d onx=%d ony=%d npix=%d\n")
#call pargi (ox1)
#call pargi (ox2)
#call pargi (oy1)
#call pargi (oy2)
#call pargi (onx)
#call pargi (ony)
#call pargi (npix)

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = NULL
	    # Set null section in SB to ensure buffer will be filled
	    SB_X1(sb)      = 0
	    SB_X2(sb)      = 0
	    SB_Y1(sb)      = 0
	    SB_Y2(sb)      = 0
	    SB_PIXTYPE(sb) = TY_REAL
	} else {
	    sb = MI_SB(mi)
	}

#call eprintf ("sb_x1=%d sb_x2=%d sb_y1=%d sb_y2=%d\n")
#call pargi (SB_X1(sb))
#call pargi (SB_X2(sb))
#call pargi (SB_Y1(sb))
#call pargi (SB_Y2(sb))

	# The requested section is entirely outside the buffer
	 if ((SB_X2(sb) < ox1) || (SB_X1(sb) > ox2) || 
	     (SB_Y2(sb) < oy1) || (SB_Y1(sb) > oy2)) { 

	    # Free old data buffer.
	    if (SB_DATA(sb) != NULL) 
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    # Allocate new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_REAL) 
	    call amovkr (real(blank), Memr[obuf], npix)

	    call migetsr (mi, ox1, ox2, oy1, oy2, ox1, ox2, oy1, oy2, obuf)
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_REAL
	    return (SB_DATA(sb))

	# Exactly the requested section is already in the buffer
	} else if ((SB_X1(sb) == ox1) && (SB_X2(sb) == ox2) &&
	           (SB_Y1(sb) == oy1) && (SB_Y2(sb) == oy2)) { 
	    # and is the correct data type. We are done!
	    if (SB_PIXTYPE(sb) == TY_REAL) {
		return (SB_DATA(sb))

	    # Change data type and return
	    } else {
		call malloc (obuf, npix, TY_REAL)
		switch (SB_PIXTYPE(sb)) {
		case TY_SHORT:
		    call achtsr (Mems[SB_DATA(sb)], Memr[obuf], npix)
#	        case TY_USHORT:
#		    call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], npix)
		case TY_INT:
		    call achtir (Memi[SB_DATA(sb)], Memr[obuf], npix)
		case TY_LONG:
		    call achtlr (Meml[SB_DATA(sb)], Memr[obuf], npix)
		case TY_REAL:
		    call achtrr (Memr[SB_DATA(sb)], Memr[obuf], npix)
		case TY_DOUBLE:
		    call achtdr (Memd[SB_DATA(sb)], Memr[obuf], npix)
		}

		# Free old buffer
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		# Return new buffer
		SB_DATA(sb)    = obuf
		SB_PIXTYPE(sb) = TY_REAL
		return (SB_DATA(sb))
	    }

	# The requested section is entirely contained in the buffer
	} else if ((SB_X1(sb) <= ox1) && (SB_X2(sb) >= ox2) && 
		   (SB_Y1(sb) <= oy1) && (SB_Y2(sb) >= oy2)) { 

	    # Copy the part we need to a new buffer
	    call malloc (obuf, npix, TY_REAL) 
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_REAL, obuf)
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_REAL

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb) = obuf
	    return (SB_DATA(sb))

	# The requested section partialy overlaps the data buffer (oh joy!)
	} else {

	    # Allocate a new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_REAL) 
	    call amovkr (real(blank), Memr[obuf], npix)

	    # Copy what we can from the buffer.
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_REAL, obuf)

  	    # Fill out the rest by reading the images.
  	    if (oy1 < SB_Y1(sb))
  		call migetsr (mi, ox1, ox2, oy1, SB_Y1(sb)-1, 
 		ox1, ox2, oy1, oy2, obuf)
 	    if (ox1 < SB_X1(sb))
 		call migetsr (mi, ox1, SB_X1(sb)-1, SB_Y1(sb), SB_Y2(sb), 
		ox1, ox2, oy1, oy2, obuf)
  	    if (ox2 > SB_X2(sb))
  		call migetsr (mi, SB_X2(sb)+1, ox2, SB_Y1(sb), SB_Y2(sb),
 		ox1, ox2, oy1, oy2, obuf)
  	    if (oy2 > SB_Y2(sb))
  		 call migetsr (mi, ox1, ox2, SB_Y2(sb)+1, oy2,
 		 ox1, ox2, oy1, oy2, obuf)

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_REAL
	    return (SB_DATA(sb))

	}
end



# MIGS2x -- Get and process a section of a (2D) mosaic image.

pointer procedure migs2d (mi, x1in, x2in, y1in, y2in)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1in, x2in	#I Range of columns in section.
int	y1in, y2in	#I Range of columns in section.
pointer	obuf		#O pointer to data values.

pointer	omg, sb
int	x1, x2, y1, y2
int	onx, ocx1, ocx2, ox1, ox2
int	ony, ocy1, ocy2, oy1, oy2
int	nimage, npix 

errchk	syserrs, malloc, migetsd, micpys

include "mosproc.com"

begin

	nimage = MI_NIMS(mi)
	omg    = MI_MG(mi, nimage+1)
	onx    = NX(omg)
	ony    = NY(omg)
	ocx1   = CX1(omg)
	ocx2   = CX2(omg)
	ocy1   = CY1(omg)
	ocy2   = CY2(omg)

#	x1 = x1in + ocx1
#	x2 = x2in + ocx1
#	y1 = y1in + ocy1
#	y2 = y2in + ocy1
	x1 = x1in
	x2 = x2in
	y1 = y1in
	y2 = y2in

#call eprintf ("x1=%d x2=%d y1=%d y2=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call eprintf ("nimage=%d ocx1=%d ocx2=%d ocy1=%d ocy2=%d\n")
#call pargi (nimage)
#call pargi (ocx1)
#call pargi (ocx2)
#call pargi (ocy1)
#call pargi (ocy2)

	# Complain if section totaly out of bounds
	if (x2 < ocx1 || x1 > ocx2 || y1 < ocy1 || y1 > ocy2)
	    call syserrs (SYS_IMREFOOB, Memc[MI_RNAME(mi)])

	# Clip section at boundary of mosaic
	ox1  = max (x1, ocx1)
	ox2  = min (x2, ocx2)
	oy1  = max (y1, ocy1)
	oy2  = min (y2, ocy2)
	onx  = ox2 - ox1 + 1
	ony  = oy2 - oy1 + 1
	npix = onx * ony

#call eprintf ("ox1=%d ox2=%d oy1=%d oy2=%d onx=%d ony=%d npix=%d\n")
#call pargi (ox1)
#call pargi (ox2)
#call pargi (oy1)
#call pargi (oy2)
#call pargi (onx)
#call pargi (ony)
#call pargi (npix)

	# First trip. Allocate data buffer
	if (MI_SB(mi) == NULL) {
	    call malloc (sb,   LEN_SECBUFF, TY_STRUCT) 
	    MI_SB(mi)      = sb
	    SB_DATA(sb)    = NULL
	    # Set null section in SB to ensure buffer will be filled
	    SB_X1(sb)      = 0
	    SB_X2(sb)      = 0
	    SB_Y1(sb)      = 0
	    SB_Y2(sb)      = 0
	    SB_PIXTYPE(sb) = TY_DOUBLE
	} else {
	    sb = MI_SB(mi)
	}

#call eprintf ("sb_x1=%d sb_x2=%d sb_y1=%d sb_y2=%d\n")
#call pargi (SB_X1(sb))
#call pargi (SB_X2(sb))
#call pargi (SB_Y1(sb))
#call pargi (SB_Y2(sb))

	# The requested section is entirely outside the buffer
	 if ((SB_X2(sb) < ox1) || (SB_X1(sb) > ox2) || 
	     (SB_Y2(sb) < oy1) || (SB_Y1(sb) > oy2)) { 

	    # Free old data buffer.
	    if (SB_DATA(sb) != NULL) 
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    # Allocate new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_DOUBLE) 
	    call amovkd (double(blank), Memd[obuf], npix)

	    call migetsd (mi, ox1, ox2, oy1, oy2, ox1, ox2, oy1, oy2, obuf)
	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_DOUBLE
	    return (SB_DATA(sb))

	# Exactly the requested section is already in the buffer
	} else if ((SB_X1(sb) == ox1) && (SB_X2(sb) == ox2) &&
	           (SB_Y1(sb) == oy1) && (SB_Y2(sb) == oy2)) { 
	    # and is the correct data type. We are done!
	    if (SB_PIXTYPE(sb) == TY_DOUBLE) {
		return (SB_DATA(sb))

	    # Change data type and return
	    } else {
		call malloc (obuf, npix, TY_DOUBLE)
		switch (SB_PIXTYPE(sb)) {
		case TY_SHORT:
		    call achtsd (Mems[SB_DATA(sb)], Memd[obuf], npix)
#	        case TY_USHORT:
#		    call achtu$t (Memu[SB_DATA(sb)], Mem$t[obuf], npix)
		case TY_INT:
		    call achtid (Memi[SB_DATA(sb)], Memd[obuf], npix)
		case TY_LONG:
		    call achtld (Meml[SB_DATA(sb)], Memd[obuf], npix)
		case TY_REAL:
		    call achtrd (Memr[SB_DATA(sb)], Memd[obuf], npix)
		case TY_DOUBLE:
		    call achtdd (Memd[SB_DATA(sb)], Memd[obuf], npix)
		}

		# Free old buffer
		call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

		# Return new buffer
		SB_DATA(sb)    = obuf
		SB_PIXTYPE(sb) = TY_DOUBLE
		return (SB_DATA(sb))
	    }

	# The requested section is entirely contained in the buffer
	} else if ((SB_X1(sb) <= ox1) && (SB_X2(sb) >= ox2) && 
		   (SB_Y1(sb) <= oy1) && (SB_Y2(sb) >= oy2)) { 

	    # Copy the part we need to a new buffer
	    call malloc (obuf, npix, TY_DOUBLE) 
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_DOUBLE, obuf)
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_DOUBLE

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb) = obuf
	    return (SB_DATA(sb))

	# The requested section partialy overlaps the data buffer (oh joy!)
	} else {

	    # Allocate a new buffer and initialise to blank value
	    call malloc (obuf, npix, TY_DOUBLE) 
	    call amovkd (double(blank), Memd[obuf], npix)

	    # Copy what we can from the buffer.
	    call micpy (sb, ox1, ox2, oy1, oy2, TY_DOUBLE, obuf)

  	    # Fill out the rest by reading the images.
  	    if (oy1 < SB_Y1(sb))
  		call migetsd (mi, ox1, ox2, oy1, SB_Y1(sb)-1, 
 		ox1, ox2, oy1, oy2, obuf)
 	    if (ox1 < SB_X1(sb))
 		call migetsd (mi, ox1, SB_X1(sb)-1, SB_Y1(sb), SB_Y2(sb), 
		ox1, ox2, oy1, oy2, obuf)
  	    if (ox2 > SB_X2(sb))
  		call migetsd (mi, SB_X2(sb)+1, ox2, SB_Y1(sb), SB_Y2(sb),
 		ox1, ox2, oy1, oy2, obuf)
  	    if (oy2 > SB_Y2(sb))
  		 call migetsd (mi, ox1, ox2, SB_Y2(sb)+1, oy2,
 		 ox1, ox2, oy1, oy2, obuf)

	    # Free old data buffer.
	    call mfree (SB_DATA(sb), SB_PIXTYPE(sb))

	    SB_DATA(sb)    = obuf
	    SB_X1(sb)      = ox1
	    SB_X2(sb)      = ox2
	    SB_Y1(sb)      = oy1
	    SB_Y2(sb)      = oy2
	    SB_PIXTYPE(sb) = TY_DOUBLE
	    return (SB_DATA(sb))

	}
end





# MIGETSx -- Fill rectangle in output buffer from input images.
# The section requested may be larger or smaller than the output buffer.
# In the former case we only return the piece which maps into the output
# buffer. In the latter case we fill a portion of the output buffer with
# data read from the images.

procedure migetss (mi, x1, x2, y1, y2, ox1, ox2, oy1, oy2, obuf)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1, x2		#I Range of columns in required section.
int	y1, y2		#I Range of columns in required section.
int	ox1, ox2	#I Range of columns in output buffer.
int	oy1, oy2	#I Range of columns in output buffer.
pointer	obuf		#O pointer to data values.

int	image, i, j, k, line, novr, bx1, bx2
int	onx, ox0, ix1, ix2, cx1, cx2, sx1, sx2, nx
int	ony, oy0, iy1, iy2, cy1, cy2, sy1, sy2, ny
real	sdx1, sdx2, sdy1, sdy2
pointer	img, iim, ibuf, bbuf, ptr
short	junk

pointer	mscs2s()

errchk	mscs2s, miprocs()

include "mosproc.com"

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2-1)*nx + ($1-1))
define OPTR	(obuf + ($2+oy0-1)*onx + ($1+ox0-1))
define IPTR1	(ibuf + ($1-1)*nx)
define OPTR1	(obuf + ($1+oy0-1)*onx + ox0)
define BPTR1	(bbuf + ($1-1)*novr)

begin
	# Dimensions of output buffer
	onx = ox2 - ox1 + 1
	ony = oy2 - oy1 + 1

	# Clip requested section at boundary of input buffer.
	ix1 = max (x1, ox1)
	ix2 = min (x2, ox2)
	iy1 = max (y1, oy1)
	iy2 = min (y2, oy2)

	# Loop over input images, skipping those which have no data within
	# the requested section
	do image = 1, MI_NIMS(mi) {
	    img = MI_MG(mi, image)

	    # "CCD" coordinates of corners of DATASEC of this image
	    cx1 = CX1(img)
	    cx2 = CX2(img)
	    cy1 = CY1(img)
	    cy2 = CY2(img)
	    if (ix1 > cx2 || ix2 < cx1 || iy2 < cy1 || iy1 > cy2) {
		next
	    }

	    # "CCD" coordinates of the section of this image we want
	    cx1 = max (ix1, cx1)
	    cx2 = min (ix2, cx2)
	    cy1 = max (iy1, cy1)
	    cy2 = min (iy2, cy2)
	    nx   = cx2 - cx1 + 1
	    ny   = cy2 - cy1 + 1

	    # "IMAGE" coordinates of the section of this image we want
	    sdx1 = DX(img)
	    sdx2 = (DX(img) - 1) / 2.
	    sdy1 = DY(img)
	    sdy2 = (DY(img) - 1) / 2.
	    sx1 = nint ((cx1 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sx2 = nint ((cx2 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sy1 = nint ((cy1 - CY1(img) - sdy2) / sdy1 + DY1(img))
	    sy2 = nint ((cy2 - CY1(img) - sdy2) / sdy1 + DY1(img))

	    # Read required section of this image and replicate if necessary.
	    iim  = MI_IM(mi, image)
	    if (DX(img) == 1 && DY(img) == 1)
		ibuf = mscs2s (img, sx1, sx2, sy1, sy2)
	    else {
		call malloc (ibuf, nx * ny, TY_SHORT)
		ptr = ibuf
		do j = cy1, cy2 {
		    sy1 = nint ((j-CY1(img)-sdy2)/sdy1+DY1(img))
		    bbuf = mscs2s (img, sx1, sx2, sy1, sy1)
		    do i = cx1, cx2 {
			k = nint ((i-CX1(img)-sdx2)/sdx1+DX1(img)) - sx1
			Mems[ptr] = Mems[bbuf+k]
			ptr = ptr + 1
		    }
		}
	    }

	    # Process input image section line by line writing to output buffer

	    # Offsets to starting point in output for data from this image
	    ox0  = cx1 - ox1
	    oy0  = cy1 - oy1

	    # Read corresponding overscan data if needed.
	    if (and(proc, L) != 0) {
		bx1 = BX1(img)
		bx2 = BX2(img)
		novr = bx2 - bx1 + 1
		bbuf = mscs2s (img, bx1, bx2, sy1, sy2)
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miprocs (img,  Mems[IPTR1(j)], Mems[OPTR1(j)], nx,
		    line, Mems[BPTR1(j)], novr)
		}
	    } else {
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miprocs (img,  Mems[IPTR1(j)], Mems[OPTR1(j)], nx,
		    line, junk, 0)
		}
	    }

	    if (DX(img) != 1 || DY(img) != 1)
		call mfree (ibuf, TY_SHORT)
	}
end



# MIGETSx -- Fill rectangle in output buffer from input images.
# The section requested may be larger or smaller than the output buffer.
# In the former case we only return the piece which maps into the output
# buffer. In the latter case we fill a portion of the output buffer with
# data read from the images.

procedure migetsi (mi, x1, x2, y1, y2, ox1, ox2, oy1, oy2, obuf)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1, x2		#I Range of columns in required section.
int	y1, y2		#I Range of columns in required section.
int	ox1, ox2	#I Range of columns in output buffer.
int	oy1, oy2	#I Range of columns in output buffer.
pointer	obuf		#O pointer to data values.

int	image, i, j, k, line, novr, bx1, bx2
int	onx, ox0, ix1, ix2, cx1, cx2, sx1, sx2, nx
int	ony, oy0, iy1, iy2, cy1, cy2, sy1, sy2, ny
real	sdx1, sdx2, sdy1, sdy2
pointer	img, iim, ibuf, bbuf, ptr
int	junk

pointer	mscs2i()

errchk	mscs2i, miproci()

include "mosproc.com"

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2-1)*nx + ($1-1))
define OPTR	(obuf + ($2+oy0-1)*onx + ($1+ox0-1))
define IPTR1	(ibuf + ($1-1)*nx)
define OPTR1	(obuf + ($1+oy0-1)*onx + ox0)
define BPTR1	(bbuf + ($1-1)*novr)

begin
	# Dimensions of output buffer
	onx = ox2 - ox1 + 1
	ony = oy2 - oy1 + 1

	# Clip requested section at boundary of input buffer.
	ix1 = max (x1, ox1)
	ix2 = min (x2, ox2)
	iy1 = max (y1, oy1)
	iy2 = min (y2, oy2)

	# Loop over input images, skipping those which have no data within
	# the requested section
	do image = 1, MI_NIMS(mi) {
	    img = MI_MG(mi, image)

	    # "CCD" coordinates of corners of DATASEC of this image
	    cx1 = CX1(img)
	    cx2 = CX2(img)
	    cy1 = CY1(img)
	    cy2 = CY2(img)
	    if (ix1 > cx2 || ix2 < cx1 || iy2 < cy1 || iy1 > cy2) {
		next
	    }

	    # "CCD" coordinates of the section of this image we want
	    cx1 = max (ix1, cx1)
	    cx2 = min (ix2, cx2)
	    cy1 = max (iy1, cy1)
	    cy2 = min (iy2, cy2)
	    nx   = cx2 - cx1 + 1
	    ny   = cy2 - cy1 + 1

	    # "IMAGE" coordinates of the section of this image we want
	    sdx1 = DX(img)
	    sdx2 = (DX(img) - 1) / 2.
	    sdy1 = DY(img)
	    sdy2 = (DY(img) - 1) / 2.
	    sx1 = nint ((cx1 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sx2 = nint ((cx2 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sy1 = nint ((cy1 - CY1(img) - sdy2) / sdy1 + DY1(img))
	    sy2 = nint ((cy2 - CY1(img) - sdy2) / sdy1 + DY1(img))

	    # Read required section of this image and replicate if necessary.
	    iim  = MI_IM(mi, image)
	    if (DX(img) == 1 && DY(img) == 1)
		ibuf = mscs2i (img, sx1, sx2, sy1, sy2)
	    else {
		call malloc (ibuf, nx * ny, TY_INT)
		ptr = ibuf
		do j = cy1, cy2 {
		    sy1 = nint ((j-CY1(img)-sdy2)/sdy1+DY1(img))
		    bbuf = mscs2i (img, sx1, sx2, sy1, sy1)
		    do i = cx1, cx2 {
			k = nint ((i-CX1(img)-sdx2)/sdx1+DX1(img)) - sx1
			Memi[ptr] = Memi[bbuf+k]
			ptr = ptr + 1
		    }
		}
	    }

	    # Process input image section line by line writing to output buffer

	    # Offsets to starting point in output for data from this image
	    ox0  = cx1 - ox1
	    oy0  = cy1 - oy1

	    # Read corresponding overscan data if needed.
	    if (and(proc, L) != 0) {
		bx1 = BX1(img)
		bx2 = BX2(img)
		novr = bx2 - bx1 + 1
		bbuf = mscs2i (img, bx1, bx2, sy1, sy2)
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miproci (img,  Memi[IPTR1(j)], Memi[OPTR1(j)], nx,
		    line, Memi[BPTR1(j)], novr)
		}
	    } else {
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miproci (img,  Memi[IPTR1(j)], Memi[OPTR1(j)], nx,
		    line, junk, 0)
		}
	    }

	    if (DX(img) != 1 || DY(img) != 1)
		call mfree (ibuf, TY_INT)
	}
end



# MIGETSx -- Fill rectangle in output buffer from input images.
# The section requested may be larger or smaller than the output buffer.
# In the former case we only return the piece which maps into the output
# buffer. In the latter case we fill a portion of the output buffer with
# data read from the images.

procedure migetsl (mi, x1, x2, y1, y2, ox1, ox2, oy1, oy2, obuf)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1, x2		#I Range of columns in required section.
int	y1, y2		#I Range of columns in required section.
int	ox1, ox2	#I Range of columns in output buffer.
int	oy1, oy2	#I Range of columns in output buffer.
pointer	obuf		#O pointer to data values.

int	image, i, j, k, line, novr, bx1, bx2
int	onx, ox0, ix1, ix2, cx1, cx2, sx1, sx2, nx
int	ony, oy0, iy1, iy2, cy1, cy2, sy1, sy2, ny
real	sdx1, sdx2, sdy1, sdy2
pointer	img, iim, ibuf, bbuf, ptr
long	junk

pointer	mscs2l()

errchk	mscs2l, miprocl()

include "mosproc.com"

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2-1)*nx + ($1-1))
define OPTR	(obuf + ($2+oy0-1)*onx + ($1+ox0-1))
define IPTR1	(ibuf + ($1-1)*nx)
define OPTR1	(obuf + ($1+oy0-1)*onx + ox0)
define BPTR1	(bbuf + ($1-1)*novr)

begin
	# Dimensions of output buffer
	onx = ox2 - ox1 + 1
	ony = oy2 - oy1 + 1

	# Clip requested section at boundary of input buffer.
	ix1 = max (x1, ox1)
	ix2 = min (x2, ox2)
	iy1 = max (y1, oy1)
	iy2 = min (y2, oy2)

	# Loop over input images, skipping those which have no data within
	# the requested section
	do image = 1, MI_NIMS(mi) {
	    img = MI_MG(mi, image)

	    # "CCD" coordinates of corners of DATASEC of this image
	    cx1 = CX1(img)
	    cx2 = CX2(img)
	    cy1 = CY1(img)
	    cy2 = CY2(img)
	    if (ix1 > cx2 || ix2 < cx1 || iy2 < cy1 || iy1 > cy2) {
		next
	    }

	    # "CCD" coordinates of the section of this image we want
	    cx1 = max (ix1, cx1)
	    cx2 = min (ix2, cx2)
	    cy1 = max (iy1, cy1)
	    cy2 = min (iy2, cy2)
	    nx   = cx2 - cx1 + 1
	    ny   = cy2 - cy1 + 1

	    # "IMAGE" coordinates of the section of this image we want
	    sdx1 = DX(img)
	    sdx2 = (DX(img) - 1) / 2.
	    sdy1 = DY(img)
	    sdy2 = (DY(img) - 1) / 2.
	    sx1 = nint ((cx1 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sx2 = nint ((cx2 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sy1 = nint ((cy1 - CY1(img) - sdy2) / sdy1 + DY1(img))
	    sy2 = nint ((cy2 - CY1(img) - sdy2) / sdy1 + DY1(img))

	    # Read required section of this image and replicate if necessary.
	    iim  = MI_IM(mi, image)
	    if (DX(img) == 1 && DY(img) == 1)
		ibuf = mscs2l (img, sx1, sx2, sy1, sy2)
	    else {
		call malloc (ibuf, nx * ny, TY_LONG)
		ptr = ibuf
		do j = cy1, cy2 {
		    sy1 = nint ((j-CY1(img)-sdy2)/sdy1+DY1(img))
		    bbuf = mscs2l (img, sx1, sx2, sy1, sy1)
		    do i = cx1, cx2 {
			k = nint ((i-CX1(img)-sdx2)/sdx1+DX1(img)) - sx1
			Meml[ptr] = Meml[bbuf+k]
			ptr = ptr + 1
		    }
		}
	    }

	    # Process input image section line by line writing to output buffer

	    # Offsets to starting point in output for data from this image
	    ox0  = cx1 - ox1
	    oy0  = cy1 - oy1

	    # Read corresponding overscan data if needed.
	    if (and(proc, L) != 0) {
		bx1 = BX1(img)
		bx2 = BX2(img)
		novr = bx2 - bx1 + 1
		bbuf = mscs2l (img, bx1, bx2, sy1, sy2)
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miprocl (img,  Meml[IPTR1(j)], Meml[OPTR1(j)], nx,
		    line, Meml[BPTR1(j)], novr)
		}
	    } else {
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miprocl (img,  Meml[IPTR1(j)], Meml[OPTR1(j)], nx,
		    line, junk, 0)
		}
	    }

	    if (DX(img) != 1 || DY(img) != 1)
		call mfree (ibuf, TY_LONG)
	}
end



# MIGETSx -- Fill rectangle in output buffer from input images.
# The section requested may be larger or smaller than the output buffer.
# In the former case we only return the piece which maps into the output
# buffer. In the latter case we fill a portion of the output buffer with
# data read from the images.

procedure migetsr (mi, x1, x2, y1, y2, ox1, ox2, oy1, oy2, obuf)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1, x2		#I Range of columns in required section.
int	y1, y2		#I Range of columns in required section.
int	ox1, ox2	#I Range of columns in output buffer.
int	oy1, oy2	#I Range of columns in output buffer.
pointer	obuf		#O pointer to data values.

int	image, i, j, k, line, novr, bx1, bx2
int	onx, ox0, ix1, ix2, cx1, cx2, sx1, sx2, nx
int	ony, oy0, iy1, iy2, cy1, cy2, sy1, sy2, ny
real	sdx1, sdx2, sdy1, sdy2
pointer	img, iim, ibuf, bbuf, ptr
real	junk

pointer	mscs2r()

errchk	mscs2r, miprocr()

include "mosproc.com"

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2-1)*nx + ($1-1))
define OPTR	(obuf + ($2+oy0-1)*onx + ($1+ox0-1))
define IPTR1	(ibuf + ($1-1)*nx)
define OPTR1	(obuf + ($1+oy0-1)*onx + ox0)
define BPTR1	(bbuf + ($1-1)*novr)

begin
	# Dimensions of output buffer
	onx = ox2 - ox1 + 1
	ony = oy2 - oy1 + 1

	# Clip requested section at boundary of input buffer.
	ix1 = max (x1, ox1)
	ix2 = min (x2, ox2)
	iy1 = max (y1, oy1)
	iy2 = min (y2, oy2)

	# Loop over input images, skipping those which have no data within
	# the requested section
	do image = 1, MI_NIMS(mi) {
	    img = MI_MG(mi, image)

	    # "CCD" coordinates of corners of DATASEC of this image
	    cx1 = CX1(img)
	    cx2 = CX2(img)
	    cy1 = CY1(img)
	    cy2 = CY2(img)
	    if (ix1 > cx2 || ix2 < cx1 || iy2 < cy1 || iy1 > cy2) {
		next
	    }

	    # "CCD" coordinates of the section of this image we want
	    cx1 = max (ix1, cx1)
	    cx2 = min (ix2, cx2)
	    cy1 = max (iy1, cy1)
	    cy2 = min (iy2, cy2)
	    nx   = cx2 - cx1 + 1
	    ny   = cy2 - cy1 + 1

	    # "IMAGE" coordinates of the section of this image we want
	    sdx1 = DX(img)
	    sdx2 = (DX(img) - 1) / 2.
	    sdy1 = DY(img)
	    sdy2 = (DY(img) - 1) / 2.
	    sx1 = nint ((cx1 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sx2 = nint ((cx2 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sy1 = nint ((cy1 - CY1(img) - sdy2) / sdy1 + DY1(img))
	    sy2 = nint ((cy2 - CY1(img) - sdy2) / sdy1 + DY1(img))

	    # Read required section of this image and replicate if necessary.
	    iim  = MI_IM(mi, image)
	    if (DX(img) == 1 && DY(img) == 1)
		ibuf = mscs2r (img, sx1, sx2, sy1, sy2)
	    else {
		call malloc (ibuf, nx * ny, TY_REAL)
		ptr = ibuf
		do j = cy1, cy2 {
		    sy1 = nint ((j-CY1(img)-sdy2)/sdy1+DY1(img))
		    bbuf = mscs2r (img, sx1, sx2, sy1, sy1)
		    do i = cx1, cx2 {
			k = nint ((i-CX1(img)-sdx2)/sdx1+DX1(img)) - sx1
			Memr[ptr] = Memr[bbuf+k]
			ptr = ptr + 1
		    }
		}
	    }

	    # Process input image section line by line writing to output buffer

	    # Offsets to starting point in output for data from this image
	    ox0  = cx1 - ox1
	    oy0  = cy1 - oy1

	    # Read corresponding overscan data if needed.
	    if (and(proc, L) != 0) {
		bx1 = BX1(img)
		bx2 = BX2(img)
		novr = bx2 - bx1 + 1
		bbuf = mscs2r (img, bx1, bx2, sy1, sy2)
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miprocr (img,  Memr[IPTR1(j)], Memr[OPTR1(j)], nx,
		    line, Memr[BPTR1(j)], novr)
		}
	    } else {
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miprocr (img,  Memr[IPTR1(j)], Memr[OPTR1(j)], nx,
		    line, junk, 0)
		}
	    }

	    if (DX(img) != 1 || DY(img) != 1)
		call mfree (ibuf, TY_REAL)
	}
end



# MIGETSx -- Fill rectangle in output buffer from input images.
# The section requested may be larger or smaller than the output buffer.
# In the former case we only return the piece which maps into the output
# buffer. In the latter case we fill a portion of the output buffer with
# data read from the images.

procedure migetsd (mi, x1, x2, y1, y2, ox1, ox2, oy1, oy2, obuf)

pointer	mi		#I MOSIM Pointer for mosaic.
int	x1, x2		#I Range of columns in required section.
int	y1, y2		#I Range of columns in required section.
int	ox1, ox2	#I Range of columns in output buffer.
int	oy1, oy2	#I Range of columns in output buffer.
pointer	obuf		#O pointer to data values.

int	image, i, j, k, line, novr, bx1, bx2
int	onx, ox0, ix1, ix2, cx1, cx2, sx1, sx2, nx
int	ony, oy0, iy1, iy2, cy1, cy2, sy1, sy2, ny
real	sdx1, sdx2, sdy1, sdy2
pointer	img, iim, ibuf, bbuf, ptr
double	junk

pointer	mscs2d()

errchk	mscs2d, miprocd()

include "mosproc.com"

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2-1)*nx + ($1-1))
define OPTR	(obuf + ($2+oy0-1)*onx + ($1+ox0-1))
define IPTR1	(ibuf + ($1-1)*nx)
define OPTR1	(obuf + ($1+oy0-1)*onx + ox0)
define BPTR1	(bbuf + ($1-1)*novr)

begin
	# Dimensions of output buffer
	onx = ox2 - ox1 + 1
	ony = oy2 - oy1 + 1

	# Clip requested section at boundary of input buffer.
	ix1 = max (x1, ox1)
	ix2 = min (x2, ox2)
	iy1 = max (y1, oy1)
	iy2 = min (y2, oy2)

	# Loop over input images, skipping those which have no data within
	# the requested section
	do image = 1, MI_NIMS(mi) {
	    img = MI_MG(mi, image)

	    # "CCD" coordinates of corners of DATASEC of this image
	    cx1 = CX1(img)
	    cx2 = CX2(img)
	    cy1 = CY1(img)
	    cy2 = CY2(img)
	    if (ix1 > cx2 || ix2 < cx1 || iy2 < cy1 || iy1 > cy2) {
		next
	    }

	    # "CCD" coordinates of the section of this image we want
	    cx1 = max (ix1, cx1)
	    cx2 = min (ix2, cx2)
	    cy1 = max (iy1, cy1)
	    cy2 = min (iy2, cy2)
	    nx   = cx2 - cx1 + 1
	    ny   = cy2 - cy1 + 1

	    # "IMAGE" coordinates of the section of this image we want
	    sdx1 = DX(img)
	    sdx2 = (DX(img) - 1) / 2.
	    sdy1 = DY(img)
	    sdy2 = (DY(img) - 1) / 2.
	    sx1 = nint ((cx1 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sx2 = nint ((cx2 - CX1(img) - sdx2) / sdx1 + DX1(img))
	    sy1 = nint ((cy1 - CY1(img) - sdy2) / sdy1 + DY1(img))
	    sy2 = nint ((cy2 - CY1(img) - sdy2) / sdy1 + DY1(img))

	    # Read required section of this image and replicate if necessary.
	    iim  = MI_IM(mi, image)
	    if (DX(img) == 1 && DY(img) == 1)
		ibuf = mscs2d (img, sx1, sx2, sy1, sy2)
	    else {
		call malloc (ibuf, nx * ny, TY_DOUBLE)
		ptr = ibuf
		do j = cy1, cy2 {
		    sy1 = nint ((j-CY1(img)-sdy2)/sdy1+DY1(img))
		    bbuf = mscs2d (img, sx1, sx2, sy1, sy1)
		    do i = cx1, cx2 {
			k = nint ((i-CX1(img)-sdx2)/sdx1+DX1(img)) - sx1
			Memd[ptr] = Memd[bbuf+k]
			ptr = ptr + 1
		    }
		}
	    }

	    # Process input image section line by line writing to output buffer

	    # Offsets to starting point in output for data from this image
	    ox0  = cx1 - ox1
	    oy0  = cy1 - oy1

	    # Read corresponding overscan data if needed.
	    if (and(proc, L) != 0) {
		bx1 = BX1(img)
		bx2 = BX2(img)
		novr = bx2 - bx1 + 1
		bbuf = mscs2d (img, bx1, bx2, sy1, sy2)
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miprocd (img,  Memd[IPTR1(j)], Memd[OPTR1(j)], nx,
		    line, Memd[BPTR1(j)], novr)
		}
	    } else {
		do j = 1, ny {
		    line = nint ((j - CY1(img) - sdy2) / sdy1 + DY1(img))
		    call miprocd (img,  Memd[IPTR1(j)], Memd[OPTR1(j)], nx,
		    line, junk, 0)
		}
	    }

	    if (DX(img) != 1 || DY(img) != 1)
		call mfree (ibuf, TY_DOUBLE)
	}
end



# MICPY -- Fill rectangle in output buffer by copying from the internal buffer

procedure micpy (sb, x1, x2, y1, y2, otype, obuf)

pointer	sb		#I Pointer to section data sub-structure
int	x1, x2		#I Range of columns in section.
int	y1, y2		#I Range of lines in section.
int	otype		#I Desired type of output data.
pointer	obuf		#O pointer to data values.

begin
	switch (otype) {
	case TY_SHORT:
	    call micpys (sb, x1, x2, y1, y2, obuf)
#	case TY_USHORT:
#	    call micpyu (sb, x1, x2, y1, y2, obuf)
	case TY_INT:
	    call micpyi (sb, x1, x2, y1, y2, obuf)
	case TY_LONG:
	    call micpyl (sb, x1, x2, y1, y2, obuf)
	case TY_REAL:
	    call micpyr (sb, x1, x2, y1, y2, obuf)
	case TY_DOUBLE:
	    call micpyd (sb, x1, x2, y1, y2, obuf)
	}
end



# MICPYx -- Fill rectangle in output buffer by copying from the internal buffer
# The data is type converted if neccesary. The section requested may be
# larger or smaller than that available in the internal buffer. In the
# former case we just fill the part of the output buffer for which we have
# data. In the latter case we copy a subsection of the internal buffer to
# output.

procedure micpys (sb, x1, x2, y1, y2, obuf)

pointer	sb		#I Pointer to section data sub-structure.
int	x1, x2		#I Range of columns in requested section.
int	y1, y2		#I Range of lines in requested section.
pointer	obuf		#O pointer to data values.

int	j, i, btype
int	nx, x0, bnx, bx1, bx2, bx0, sx1, sx2, snx
int	ny, y0, bny, by1, by2, by0, sy1, sy2, sny
pointer	ibuf

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2+by0-1)*bnx + ($1+bx0-1))
define OPTR	(obuf + ($2+y0-1)*nx + ($1+x0-1))

begin

#call eprintf ("MICPY\n")

	# dimensions of output buffer
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

#call eprintf ("\t x1=%d  x2=%d  y1=%d  y2=%d  nx=%d  ny=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call pargi (nx)
#call pargi (ny)

	# Coordinates etc. of internal buffer
	ibuf = SB_DATA(sb)
	bx1 = SB_X1(sb)
	bx2 = SB_X2(sb)
	by1 = SB_Y1(sb)
	by2 = SB_Y2(sb)
	bnx = bx2 - bx1 + 1
	bny = by2 - by1 + 1
	btype = SB_PIXTYPE(sb)

#call eprintf ("\tbx1=%d bx2=%d by1=%d by2=%d bnx=%d bny=%d\n")
#call pargi (bx1)
#call pargi (bx2)
#call pargi (by1)
#call pargi (by2)
#call pargi (bnx)
#call pargi (bny)

	# offset to starting point in output buffer
	x0  = max ((bx1 - x1), 0)
	y0  = max ((by1 - y1), 0)

	# Offset to starting point in internal buffer
	bx0 = max ((x1 - bx1), 0)
	by0 = max ((y1 - by1), 0)

#call eprintf ("\tx0=%d y0=%d bx0=%d bx0=%d\n")
#call pargi (x0)
#call pargi (y0)
#call pargi (bx0)
#call pargi (by0)

	# Coordinates of the piece we have in the internal buffer.
	sx1 = max (x1, bx1)
	sx2 = min (x2, bx2)
	sy1 = max (y1, by1)
	sy2 = min (y2, by2)

	# Number of pixels to copy.
	snx = sx2 - sx1 + 1
	sny = sy2 - sy1 + 1

#call eprintf ("\tsx1=%d sx2=%d sy1=%d sy2=%d snx=%d sny=%d\n")
#call pargi (sx1)
#call pargi (sx2)
#call pargi (sy1)
#call pargi (sy2)
#call pargi (snx)
#call pargi (sny)

	switch (btype) {

	case TY_SHORT:
	    do j = 1, sny {
		do i = 1, snx
		    Mems[OPTR(i, j)] = Mems[IPTR(i,j)]
	    }

	case TY_INT:
	    do j = 1, sny {
		do i = 1, snx
		    Mems[OPTR(i, j)] = Memi[IPTR(i,j)]
	    }

	case TY_LONG:
	    do j = 1, sny {
		do i = 1, snx
		    Mems[OPTR(i, j)] = Meml[IPTR(i,j)]
	    }

	case TY_REAL:
	    do j = 1, sny {
		do i = 1, snx
		    Mems[OPTR(i, j)] = Memr[IPTR(i,j)]
	    }

	case TY_DOUBLE:
	    do j = 1, sny {
		do i = 1, snx
		    Mems[OPTR(i, j)] = Memd[IPTR(i,j)]
	    }
	}

end



# MICPYx -- Fill rectangle in output buffer by copying from the internal buffer
# The data is type converted if neccesary. The section requested may be
# larger or smaller than that available in the internal buffer. In the
# former case we just fill the part of the output buffer for which we have
# data. In the latter case we copy a subsection of the internal buffer to
# output.

procedure micpyi (sb, x1, x2, y1, y2, obuf)

pointer	sb		#I Pointer to section data sub-structure.
int	x1, x2		#I Range of columns in requested section.
int	y1, y2		#I Range of lines in requested section.
pointer	obuf		#O pointer to data values.

int	j, i, btype
int	nx, x0, bnx, bx1, bx2, bx0, sx1, sx2, snx
int	ny, y0, bny, by1, by2, by0, sy1, sy2, sny
pointer	ibuf

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2+by0-1)*bnx + ($1+bx0-1))
define OPTR	(obuf + ($2+y0-1)*nx + ($1+x0-1))

begin

#call eprintf ("MICPY\n")

	# dimensions of output buffer
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

#call eprintf ("\t x1=%d  x2=%d  y1=%d  y2=%d  nx=%d  ny=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call pargi (nx)
#call pargi (ny)

	# Coordinates etc. of internal buffer
	ibuf = SB_DATA(sb)
	bx1 = SB_X1(sb)
	bx2 = SB_X2(sb)
	by1 = SB_Y1(sb)
	by2 = SB_Y2(sb)
	bnx = bx2 - bx1 + 1
	bny = by2 - by1 + 1
	btype = SB_PIXTYPE(sb)

#call eprintf ("\tbx1=%d bx2=%d by1=%d by2=%d bnx=%d bny=%d\n")
#call pargi (bx1)
#call pargi (bx2)
#call pargi (by1)
#call pargi (by2)
#call pargi (bnx)
#call pargi (bny)

	# offset to starting point in output buffer
	x0  = max ((bx1 - x1), 0)
	y0  = max ((by1 - y1), 0)

	# Offset to starting point in internal buffer
	bx0 = max ((x1 - bx1), 0)
	by0 = max ((y1 - by1), 0)

#call eprintf ("\tx0=%d y0=%d bx0=%d bx0=%d\n")
#call pargi (x0)
#call pargi (y0)
#call pargi (bx0)
#call pargi (by0)

	# Coordinates of the piece we have in the internal buffer.
	sx1 = max (x1, bx1)
	sx2 = min (x2, bx2)
	sy1 = max (y1, by1)
	sy2 = min (y2, by2)

	# Number of pixels to copy.
	snx = sx2 - sx1 + 1
	sny = sy2 - sy1 + 1

#call eprintf ("\tsx1=%d sx2=%d sy1=%d sy2=%d snx=%d sny=%d\n")
#call pargi (sx1)
#call pargi (sx2)
#call pargi (sy1)
#call pargi (sy2)
#call pargi (snx)
#call pargi (sny)

	switch (btype) {

	case TY_SHORT:
	    do j = 1, sny {
		do i = 1, snx
		    Memi[OPTR(i, j)] = Mems[IPTR(i,j)]
	    }

	case TY_INT:
	    do j = 1, sny {
		do i = 1, snx
		    Memi[OPTR(i, j)] = Memi[IPTR(i,j)]
	    }

	case TY_LONG:
	    do j = 1, sny {
		do i = 1, snx
		    Memi[OPTR(i, j)] = Meml[IPTR(i,j)]
	    }

	case TY_REAL:
	    do j = 1, sny {
		do i = 1, snx
		    Memi[OPTR(i, j)] = Memr[IPTR(i,j)]
	    }

	case TY_DOUBLE:
	    do j = 1, sny {
		do i = 1, snx
		    Memi[OPTR(i, j)] = Memd[IPTR(i,j)]
	    }
	}

end



# MICPYx -- Fill rectangle in output buffer by copying from the internal buffer
# The data is type converted if neccesary. The section requested may be
# larger or smaller than that available in the internal buffer. In the
# former case we just fill the part of the output buffer for which we have
# data. In the latter case we copy a subsection of the internal buffer to
# output.

procedure micpyl (sb, x1, x2, y1, y2, obuf)

pointer	sb		#I Pointer to section data sub-structure.
int	x1, x2		#I Range of columns in requested section.
int	y1, y2		#I Range of lines in requested section.
pointer	obuf		#O pointer to data values.

int	j, i, btype
int	nx, x0, bnx, bx1, bx2, bx0, sx1, sx2, snx
int	ny, y0, bny, by1, by2, by0, sy1, sy2, sny
pointer	ibuf

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2+by0-1)*bnx + ($1+bx0-1))
define OPTR	(obuf + ($2+y0-1)*nx + ($1+x0-1))

begin

#call eprintf ("MICPY\n")

	# dimensions of output buffer
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

#call eprintf ("\t x1=%d  x2=%d  y1=%d  y2=%d  nx=%d  ny=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call pargi (nx)
#call pargi (ny)

	# Coordinates etc. of internal buffer
	ibuf = SB_DATA(sb)
	bx1 = SB_X1(sb)
	bx2 = SB_X2(sb)
	by1 = SB_Y1(sb)
	by2 = SB_Y2(sb)
	bnx = bx2 - bx1 + 1
	bny = by2 - by1 + 1
	btype = SB_PIXTYPE(sb)

#call eprintf ("\tbx1=%d bx2=%d by1=%d by2=%d bnx=%d bny=%d\n")
#call pargi (bx1)
#call pargi (bx2)
#call pargi (by1)
#call pargi (by2)
#call pargi (bnx)
#call pargi (bny)

	# offset to starting point in output buffer
	x0  = max ((bx1 - x1), 0)
	y0  = max ((by1 - y1), 0)

	# Offset to starting point in internal buffer
	bx0 = max ((x1 - bx1), 0)
	by0 = max ((y1 - by1), 0)

#call eprintf ("\tx0=%d y0=%d bx0=%d bx0=%d\n")
#call pargi (x0)
#call pargi (y0)
#call pargi (bx0)
#call pargi (by0)

	# Coordinates of the piece we have in the internal buffer.
	sx1 = max (x1, bx1)
	sx2 = min (x2, bx2)
	sy1 = max (y1, by1)
	sy2 = min (y2, by2)

	# Number of pixels to copy.
	snx = sx2 - sx1 + 1
	sny = sy2 - sy1 + 1

#call eprintf ("\tsx1=%d sx2=%d sy1=%d sy2=%d snx=%d sny=%d\n")
#call pargi (sx1)
#call pargi (sx2)
#call pargi (sy1)
#call pargi (sy2)
#call pargi (snx)
#call pargi (sny)

	switch (btype) {

	case TY_SHORT:
	    do j = 1, sny {
		do i = 1, snx
		    Meml[OPTR(i, j)] = Mems[IPTR(i,j)]
	    }

	case TY_INT:
	    do j = 1, sny {
		do i = 1, snx
		    Meml[OPTR(i, j)] = Memi[IPTR(i,j)]
	    }

	case TY_LONG:
	    do j = 1, sny {
		do i = 1, snx
		    Meml[OPTR(i, j)] = Meml[IPTR(i,j)]
	    }

	case TY_REAL:
	    do j = 1, sny {
		do i = 1, snx
		    Meml[OPTR(i, j)] = Memr[IPTR(i,j)]
	    }

	case TY_DOUBLE:
	    do j = 1, sny {
		do i = 1, snx
		    Meml[OPTR(i, j)] = Memd[IPTR(i,j)]
	    }
	}

end



# MICPYx -- Fill rectangle in output buffer by copying from the internal buffer
# The data is type converted if neccesary. The section requested may be
# larger or smaller than that available in the internal buffer. In the
# former case we just fill the part of the output buffer for which we have
# data. In the latter case we copy a subsection of the internal buffer to
# output.

procedure micpyr (sb, x1, x2, y1, y2, obuf)

pointer	sb		#I Pointer to section data sub-structure.
int	x1, x2		#I Range of columns in requested section.
int	y1, y2		#I Range of lines in requested section.
pointer	obuf		#O pointer to data values.

int	j, i, btype
int	nx, x0, bnx, bx1, bx2, bx0, sx1, sx2, snx
int	ny, y0, bny, by1, by2, by0, sy1, sy2, sny
pointer	ibuf

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2+by0-1)*bnx + ($1+bx0-1))
define OPTR	(obuf + ($2+y0-1)*nx + ($1+x0-1))

begin

#call eprintf ("MICPY\n")

	# dimensions of output buffer
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

#call eprintf ("\t x1=%d  x2=%d  y1=%d  y2=%d  nx=%d  ny=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call pargi (nx)
#call pargi (ny)

	# Coordinates etc. of internal buffer
	ibuf = SB_DATA(sb)
	bx1 = SB_X1(sb)
	bx2 = SB_X2(sb)
	by1 = SB_Y1(sb)
	by2 = SB_Y2(sb)
	bnx = bx2 - bx1 + 1
	bny = by2 - by1 + 1
	btype = SB_PIXTYPE(sb)

#call eprintf ("\tbx1=%d bx2=%d by1=%d by2=%d bnx=%d bny=%d\n")
#call pargi (bx1)
#call pargi (bx2)
#call pargi (by1)
#call pargi (by2)
#call pargi (bnx)
#call pargi (bny)

	# offset to starting point in output buffer
	x0  = max ((bx1 - x1), 0)
	y0  = max ((by1 - y1), 0)

	# Offset to starting point in internal buffer
	bx0 = max ((x1 - bx1), 0)
	by0 = max ((y1 - by1), 0)

#call eprintf ("\tx0=%d y0=%d bx0=%d bx0=%d\n")
#call pargi (x0)
#call pargi (y0)
#call pargi (bx0)
#call pargi (by0)

	# Coordinates of the piece we have in the internal buffer.
	sx1 = max (x1, bx1)
	sx2 = min (x2, bx2)
	sy1 = max (y1, by1)
	sy2 = min (y2, by2)

	# Number of pixels to copy.
	snx = sx2 - sx1 + 1
	sny = sy2 - sy1 + 1

#call eprintf ("\tsx1=%d sx2=%d sy1=%d sy2=%d snx=%d sny=%d\n")
#call pargi (sx1)
#call pargi (sx2)
#call pargi (sy1)
#call pargi (sy2)
#call pargi (snx)
#call pargi (sny)

	switch (btype) {

	case TY_SHORT:
	    do j = 1, sny {
		do i = 1, snx
		    Memr[OPTR(i, j)] = Mems[IPTR(i,j)]
	    }

	case TY_INT:
	    do j = 1, sny {
		do i = 1, snx
		    Memr[OPTR(i, j)] = Memi[IPTR(i,j)]
	    }

	case TY_LONG:
	    do j = 1, sny {
		do i = 1, snx
		    Memr[OPTR(i, j)] = Meml[IPTR(i,j)]
	    }

	case TY_REAL:
	    do j = 1, sny {
		do i = 1, snx
		    Memr[OPTR(i, j)] = Memr[IPTR(i,j)]
	    }

	case TY_DOUBLE:
	    do j = 1, sny {
		do i = 1, snx
		    Memr[OPTR(i, j)] = Memd[IPTR(i,j)]
	    }
	}

end



# MICPYx -- Fill rectangle in output buffer by copying from the internal buffer
# The data is type converted if neccesary. The section requested may be
# larger or smaller than that available in the internal buffer. In the
# former case we just fill the part of the output buffer for which we have
# data. In the latter case we copy a subsection of the internal buffer to
# output.

procedure micpyd (sb, x1, x2, y1, y2, obuf)

pointer	sb		#I Pointer to section data sub-structure.
int	x1, x2		#I Range of columns in requested section.
int	y1, y2		#I Range of lines in requested section.
pointer	obuf		#O pointer to data values.

int	j, i, btype
int	nx, x0, bnx, bx1, bx2, bx0, sx1, sx2, snx
int	ny, y0, bny, by1, by2, by0, sy1, sy2, sny
pointer	ibuf

# Macros to "simplify" array indexing
define IPTR	(ibuf + ($2+by0-1)*bnx + ($1+bx0-1))
define OPTR	(obuf + ($2+y0-1)*nx + ($1+x0-1))

begin

#call eprintf ("MICPY\n")

	# dimensions of output buffer
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

#call eprintf ("\t x1=%d  x2=%d  y1=%d  y2=%d  nx=%d  ny=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call pargi (nx)
#call pargi (ny)

	# Coordinates etc. of internal buffer
	ibuf = SB_DATA(sb)
	bx1 = SB_X1(sb)
	bx2 = SB_X2(sb)
	by1 = SB_Y1(sb)
	by2 = SB_Y2(sb)
	bnx = bx2 - bx1 + 1
	bny = by2 - by1 + 1
	btype = SB_PIXTYPE(sb)

#call eprintf ("\tbx1=%d bx2=%d by1=%d by2=%d bnx=%d bny=%d\n")
#call pargi (bx1)
#call pargi (bx2)
#call pargi (by1)
#call pargi (by2)
#call pargi (bnx)
#call pargi (bny)

	# offset to starting point in output buffer
	x0  = max ((bx1 - x1), 0)
	y0  = max ((by1 - y1), 0)

	# Offset to starting point in internal buffer
	bx0 = max ((x1 - bx1), 0)
	by0 = max ((y1 - by1), 0)

#call eprintf ("\tx0=%d y0=%d bx0=%d bx0=%d\n")
#call pargi (x0)
#call pargi (y0)
#call pargi (bx0)
#call pargi (by0)

	# Coordinates of the piece we have in the internal buffer.
	sx1 = max (x1, bx1)
	sx2 = min (x2, bx2)
	sy1 = max (y1, by1)
	sy2 = min (y2, by2)

	# Number of pixels to copy.
	snx = sx2 - sx1 + 1
	sny = sy2 - sy1 + 1

#call eprintf ("\tsx1=%d sx2=%d sy1=%d sy2=%d snx=%d sny=%d\n")
#call pargi (sx1)
#call pargi (sx2)
#call pargi (sy1)
#call pargi (sy2)
#call pargi (snx)
#call pargi (sny)

	switch (btype) {

	case TY_SHORT:
	    do j = 1, sny {
		do i = 1, snx
		    Memd[OPTR(i, j)] = Mems[IPTR(i,j)]
	    }

	case TY_INT:
	    do j = 1, sny {
		do i = 1, snx
		    Memd[OPTR(i, j)] = Memi[IPTR(i,j)]
	    }

	case TY_LONG:
	    do j = 1, sny {
		do i = 1, snx
		    Memd[OPTR(i, j)] = Meml[IPTR(i,j)]
	    }

	case TY_REAL:
	    do j = 1, sny {
		do i = 1, snx
		    Memd[OPTR(i, j)] = Memr[IPTR(i,j)]
	    }

	case TY_DOUBLE:
	    do j = 1, sny {
		do i = 1, snx
		    Memd[OPTR(i, j)] = Memd[IPTR(i,j)]
	    }
	}

end


