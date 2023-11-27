# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"mscexam.h"
 
# IE_GDATA -- Get image data with boundary checking.
 
pointer procedure ie_gdata (im, x1, x2, y1, y2)
 
pointer	im			# IMIO pointer
int	x1, x2, y1, y2		# Subraster limits (input and output)
 
int	i, cx1, cx2, cy1, cy2
pointer	mg, imgs2r()
errchk	imgs2r
 
begin
	if (im == NULL)
	    call error (1, "No image defined")

	mg  = MI_CMG(im)
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
 
	i = max (x1, x2)
	x1 = min (x1, x2)
	x2 = i
	i = max (y1, y2)
	y1 = min (y1, y2)
	y2 = i

	x1 = max (x1, cx1)
	x2 = min (x2, cx2)
	y1 = max (y1, cy1)
	y2 = min (y2, cy2)

	return (imgs2r (im, x1, x2, y1, y2))
end
