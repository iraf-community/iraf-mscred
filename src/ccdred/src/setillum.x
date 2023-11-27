include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_ILLUM -- Set parameters for illumination correction.
#
#   1.  Return immediately if the illumination correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the illumination image and return error if mkillum flag missing.
#   3.  Set the processing flags and record the operation in the output
#   	image and write a log record.

procedure set_illum (ccd)

pointer	ccd			# CCD structure

pointer	sp, str, image, im

bool	clgetb(), ccdflag()
real	ccdmean()
pointer	ccd_cache()
errchk	cal_image, ccd_cache, ccdmean, set_calsection

begin
	# Check if the user wants this operation or if it has been done.
	if (!clgetb ("illumcor") || ccdflag (IN_IM(ccd), "illumcor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the illumcor correction image.
	call cal_image (IN_IM(ccd), ILLUM, 1, Memc[image], SZ_FNAME)

	# If no processing is desired print illumination image name and return.
	if (LISTPROC(ccd) == YES) {
	    call eprintf (
		"  [TO BE DONE] Illumination is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Return a warning if the illumination flag is missing.
	im = ccd_cache (Memc[image], ILLUM)
	if (!ccdflag (im, "mkillum")) {
	    call ccd_flush (im)
	    call error (0, "MKILLUM flag missing from illumination image")
	}
	ILLUM_IM(ccd) = im

	# Check and set data section.
	call set_calsection (ccd, ILLUM_IM(ccd), ILLUM_C1(ccd), ILLUM_C2(ccd),
	    ILLUM_L1(ccd), ILLUM_L2(ccd))

	# Set the scale from the mean.
	ILLUMSCALE (ccd) = ccdmean (im)

	COR(ccd) = YES
	COROUT(ccd) = YES
	CORS(ccd, ILLUMCOR) = I

	# Log the operation.
	call sprintf (ILLUMLOG(ccd), LEN_LOG, "Illumination is %s, scale %g")
	    call pargstr (Memc[image])
	    call pargr (ILLUMSCALE(ccd))
	call timelog (ILLUMLOG(ccd), LEN_LOG)
	call ccdlog (IN_IM(ccd), ILLUMLOG(ccd))

	call sfree (sp)
end
