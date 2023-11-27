# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_ZERO -- Set parameters for zero level correction.
#   1.  Return immediately if the zero level correction is not requested or
#	if the image has been previously corrected.
#   2.	Get the zero level correction image.  Return an error if not found.
#   3.	If the zero level image has not been processed call ZEROPROC.
#   4.	Set the processing flag.
#   5.  Log the operation (to user, logfile, and output image header).

procedure set_zero (ccd)

pointer	ccd			# CCD structure

pointer	sp, str, image, im, ccd_cache()
bool	clgetb(), ccdflag(), ccdcheck()
int	nscan, ccdnscan()
errchk	cal_image, ccd_cache, ccdproc1, ccdproc2, set_calsection

begin
	# Check if the user wants this operation or it has been done.
	if (!clgetb ("zerocor") || ccdflag (IN_IM(ccd), "zerocor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the zero level correction image.
	if (clgetb ("scancor"))
	    nscan = ccdnscan (IN_IM(ccd), IN_CCDTYPE(ccd))
	else
	    nscan = 1
	call cal_image (IN_IM(ccd), ZERO, nscan, Memc[image], SZ_FNAME)

	# If no processing is desired print zero correction image and return.
	if (LISTPROC(ccd) == YES) {
	    call eprintf ("  [TO BE DONE] Zero is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Map the image and return on an error.
	# Process the zero image if necessary.
	# If nscan > 1 then the zero may not yet exist so create it
	# from the unscanned zero.

	iferr (im = ccd_cache (Memc[image], ZERO)) {
	    call cal_image (IN_IM(ccd), ZERO, 1, Memc[str], SZ_LINE)
	    im = ccd_cache (Memc[str], ZERO)
	    if (ccdcheck (im, ZERO, "")) {
		call ccd_flush (im)
		switch (CALPROC(ccd)) {
		case CALPROC_YES:
		    call ccdproc1 (Memc[str], Memc[str], ZERO)
		case CALPROC_NO:
		    call ccdproc2 (Memc[str], Memc[str], ZERO)
		    PROC(ccd) = NO
		    call sfree (sp)
		    return
		}
	    }
	    call scancor (Memc[str], Memc[image], nscan, INDEF)
	    im = ccd_cache (Memc[image], ZERO)
	}

	if (ccdcheck (im, ZERO, "")) {
	    call ccd_flush (im)
	    switch (CALPROC(ccd)) {
	    case CALPROC_YES:
		call ccdproc1 (Memc[image], Memc[image], ZERO)
	    case CALPROC_NO:
		call ccdproc2 (Memc[image], Memc[image], ZERO)
		PROC(ccd) = NO
		call sfree (sp)
		return
	    }
	    im = ccd_cache (Memc[image], ZERO)
	}
	ZERO_IM(ccd) = im

	# Check and set data section.
	call set_calsection (ccd, ZERO_IM(ccd), ZERO_C1(ccd), ZERO_C2(ccd),
	    ZERO_L1(ccd), ZERO_L2(ccd))

	COR(ccd) = YES
	COROUT(ccd) = YES
	CORS(ccd, ZEROCOR) = Z

	# Log the operation.
	call sprintf (ZEROLOG(ccd), LEN_LOG, "Zero is %s")
	    call pargstr (Memc[image])
	call timelog (ZEROLOG(ccd), LEN_LOG)
	call ccdlog (IN_IM(ccd), ZEROLOG(ccd))

	call sfree (sp)
end
