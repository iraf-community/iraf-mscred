# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ccdred.h"

# SET_NOI -- Set output no interpolation image

procedure set_noi (ccd, noi)

pointer	ccd			#I CCD structure
char	noi[ARB]		#I Output name

int	imaccess()

begin
	# Check if operation is required.
	if (noi[1] == EOS || imaccess (noi, READ_ONLY) == YES)
	    return

	# Save mask name.
	call strcpy (noi, NOIOUT_NAME(ccd), LEN_CCDSTR)
end
