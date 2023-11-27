# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ccdred.h"

# SET_BPMASK -- Set bad pixel mask output parameters.

procedure set_bpmask (ccd, bpmask)

pointer	ccd			#I CCD structure
char	bpmask[ARB]		#I Output mask name

int	imaccess()

begin
	# Check if operation is required.
	if (bpmask[1] == EOS || imaccess (bpmask, READ_ONLY) == YES)
	    return

	# If not processing list operation only.
	if (LISTPROC(ccd) == YES) {
	    call eprintf ("  [TO BE DONE] Output mask %s\n")
		call pargstr (bpmask)
	    return
	}

	# Set input mask.
	call set_inmask (ccd)

	# Save mask name.
	call strcpy (bpmask, BPOUT_NAME(ccd), LEN_CCDSTR)

	# Set flags.
	COR(ccd) = YES
	CORBPM(ccd) = YES

	# Log output.
	call sprintf (BPOUTLOG(ccd), LEN_LOG, "Output mask is %s")
	    call pargstr (BPOUT_NAME(ccd))
	call timelog (BPOUTLOG(ccd), LEN_LOG)
	call ccdlog (IN_IM(ccd), BPOUTLOG(ccd))
end
