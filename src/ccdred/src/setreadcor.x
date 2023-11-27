# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ccdred.h"


# SET_READCOR -- Set readcor flags.

procedure set_readcor (ccd)

pointer	ccd			# CCD structure

bool	clgetb(), ccdflag()

begin
	# Check if the user wants this operation or it has been done.
	if (!clgetb ("readcor") || ccdflag (IN_IM(ccd), "readcor"))
	    return

	# If no processing is desired print operation and return.
	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Convert to 1D readout correction.\n")
	    return
	}

	# Set flags.
	CORS(ccd, READCOR) = YES
end
