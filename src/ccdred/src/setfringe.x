include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_FRINGE -- Set parameters for fringe correction.
#
#   1.  Return immediately if the fringe correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the fringe image and return error if the mkfringe flag is missing.
#   3.  Set the processing flags and record the operation in the output
#   	image and write a log record.

procedure set_fringe (ccd)

pointer	ccd			# CCD structure

real	exptime1, exptime2, fringescale
pointer	sp, str, image, im

bool	clgetb(), ccdflag()
real	hdmgetr()
pointer	ccd_cache()
errchk	cal_image, ccd_cache, hdmgetr, set_calsection

begin
	# Check if the user wants this operation or if it has been done.
	if (!clgetb ("fringecor") || ccdflag (IN_IM(ccd), "fringcor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the fringe correction image.
	call cal_image (IN_IM(ccd), FRINGE, 1, Memc[image], SZ_FNAME)

	# If no processing is desired print fringe image name and return.
	if (LISTPROC(ccd) == YES) {
	    call eprintf (
		"  [TO BE DONE] Fringe is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Return an error if the fringe flag is missing.
	im = ccd_cache (Memc[image], FRINGE)
	if (!ccdflag (im, "mkfringe"))
	    call error (0, "MKFRINGE flag missing from fringe image.")
	FRINGE_IM(ccd) = im

	# Check and set data section.
	call set_calsection (ccd, FRINGE_IM(ccd), FRINGE_C1(ccd),
	    FRINGE_C2(ccd), FRINGE_L1(ccd), FRINGE_L2(ccd))

	# Get the scaling factors.  If no fringe scale factor assume 1.
	exptime1 = hdmgetr (IN_IM(ccd), "exptime")
	exptime2 = hdmgetr (im, "exptime")
	iferr (fringescale = hdmgetr (im, "fringscl"))
	    fringescale = 1.

	COR(ccd) = YES
	COROUT(ccd) = YES
	CORS(ccd, FRINGECOR) = Q
	FRINGESCALE(ccd) = exptime1 / exptime2 * fringescale

	# Log the operation.
	call sprintf (FRINGELOG(ccd), LEN_LOG, "Fringe %s, scale %g")
	    call pargstr (Memc[image])
	    call pargr (FRINGESCALE(ccd))
	call timelog (FRINGELOG(ccd), LEN_LOG)
	call ccdlog (IN_IM(ccd), FRINGELOG(ccd))

	call sfree (sp)
end
