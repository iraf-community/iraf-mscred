# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include "../mosim.h"
include "../mosgeom.h"
 
# IE_GDATA -- Get image data with boundary checking.
# Version modified for mosaic images. 

pointer procedure ie_gdata (im, x1, x2, y1, y2)
 
pointer	im			# IMIO pointer
int	x1, x2, y1, y2		# Subraster limits (input and output)
 
pointer	mg
int	i, nc, nl
int	cx1, cx2, cy1, cy2

pointer	migs2r()
errchk	migs2r
 
begin
	mg  = MI_CMG(im)
	nc  = NX(mg)
	nl  = NY(mg)
	cx1 = CX1(mg)
	cx2 = CX2(mg)
	cy1 = CY1(mg)
	cy2 = CY2(mg)

	if (IS_INDEFI (x1))
	    x1 = cx1
	if (IS_INDEFI (x2))
	    x2 = cx2
	if (IS_INDEFI (y1))
	    y1 = cy1
	if (IS_INDEFI (y2))
	    y2 = cy2
 
	# Reorder limits if necessary
	i  = max (x1, x2)
	x1 = min (x1, x2)
	x2 = i
	i  = max (y1, y2)
	y1 = min (y1, y2)
	y2 = i

	# Clip coordinates at edge of CCD section
	x1 = max (x1, cx1)
	x2 = min (x2, cx2)
	y1 = max (y1, cy1)
	y2 = min (y2, cy2)

#call eprintf ("IE_GDATA: x1=%d x2=%d y1=%d y2=%d nx=%d ny=%d\n")
#call pargi (x1)
#call pargi (x2)
#call pargi (y1)
#call pargi (y2)
#call pargi (nc)
#call pargi (nl)

	return (migs2r (im, x1, x2, y1, y2))
end
