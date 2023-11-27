# Copyright restrictions apply - see stsdas$copyright.stsdas 

include "wfits.h"


# WFT_ENCODEC -- Procedure to encode an IRAF string parameter into a FITS card.

procedure wft_encodec (keyword, param, maxch, card, comment)

char	keyword[LEN_CARD]	# FITS keyword
char	param[LEN_CARD]	# FITS string parameter
int	maxch		# maximum number of characters in string parameter
char	card[LEN_CARD+1]	# FITS card image
char	comment[LEN_CARD]	# comment string

int	nblanks, maxchar, slashp

begin
	maxchar = max(8, min (maxch, LEN_OBJECT))
	slashp = 32 
	nblanks = LEN_CARD - (slashp + 1)
	if (maxchar > 19) {
	   slashp = 1
	   nblanks = max (LEN_OBJECT - maxchar - slashp+3, 1)
	}
        call sprintf (card, LEN_CARD, "%-8.8s= '%*.*s' %*t/ %*.*s")
	    call pargstr (keyword)
	    call pargi (-maxchar)
	    call pargi (maxchar)
	    call pargstr (param)
	    call pargi (slashp)
	    call pargi (-nblanks)
	    call pargi (nblanks)
	    call pargstr (comment)
end
