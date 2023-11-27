include	<imset.h>
include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_FLAT -- Set parameters for flat field correction.
#
#   1.  Return immediately if the flat field correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the flat field image and return on an error.
#   3.  If the flat field image has not been processed call PROC.
#   4.  Set the processing flags and record the operation in the output
#   	image and write a log record.

procedure set_flat (ccd)

pointer	ccd			# CCD structure

pointer	sp, str, image, im, ccd_cache()
bool	clgetb(), ccdflag(), ccdcheck()
int	nscan, ccdnscan()
real	ccdmean(), hdmgetr()
errchk	cal_image, ccd_cache, ccdproc1, ccdproc2, ccdmean, set_calsection

begin
	# Check if the user wants this operation or if it has been done.
	if (!clgetb ("flatcor") || ccdflag (IN_IM(ccd), "flatcor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the flat field correction image.
	if (clgetb ("scancor"))
	    nscan = ccdnscan (IN_IM(ccd), IN_CCDTYPE(ccd))
	else
	    nscan = 1
	call cal_image (IN_IM(ccd), FLAT, nscan, Memc[image], SZ_FNAME)

	# If no processing is desired print flat field image name and return.
	if (LISTPROC(ccd) == YES) {
	    call eprintf ("  [TO BE DONE] Flat is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Map the image and return on an error.
	# Process the flat field image if necessary.
	# If nscan > 1 then the flat field may not yet exist so create it
	# from the unscanned flat field.

	iferr (im = ccd_cache (Memc[image], FLAT)) {
	    call cal_image (IN_IM(ccd), FLAT, 1, Memc[str], SZ_LINE)
	    im = ccd_cache (Memc[str], FLAT)
	    if (ccdcheck (im, FLAT, "")) {
		call ccd_flush (im)
		switch (CALPROC(ccd)) {
		case CALPROC_YES:
		    call ccdproc1 (Memc[str], Memc[str], FLAT)
		case CALPROC_NO:
		    call ccdproc2 (Memc[str], Memc[str], FLAT)
		    PROC(ccd) = NO
		    call sfree (sp)
		    return
		}
	    }
	    call scancor (Memc[str], Memc[image], nscan, MINREPLACE(ccd))
	    im = ccd_cache (Memc[image], FLAT)
	}

	if (ccdcheck (im, FLAT, "")) {
	    call ccd_flush (im)
	    switch (CALPROC(ccd)) {
	    case CALPROC_YES:
		call ccdproc1 (Memc[image], Memc[image], FLAT)
	    case CALPROC_NO:
		call ccdproc2 (Memc[image], Memc[image], FLAT)
		PROC(ccd) = NO
		call sfree (sp)
		return
	    }
	    im = ccd_cache (Memc[image], FLAT)
	}
	FLAT_IM(ccd) = im

	# Check and set data section.
	call set_calsection (ccd, FLAT_IM(ccd), FLAT_C1(ccd), FLAT_C2(ccd),
	    FLAT_L1(ccd), FLAT_L2(ccd))

	# Set the scaling factor from the mean.
	FLATSCALE(ccd) = ccdmean (im)

	# Set gain scaling.
	iferr (GAINSCALE(ccd) = hdmgetr (IN_IM(ccd), "gainnorm"))
	    GAINSCALE(ccd) = 1.

	COR(ccd) = YES
	COROUT(ccd) = YES
	CORS(ccd, FLATCOR) = F

	# Log the operation.
	if (GAINSCALE(ccd) != 1.) {
	    call sprintf (FLATLOG(ccd), LEN_LOG, "Gain scale is %g")
		call pargr (GAINSCALE(ccd))
	    call timelog (FLATLOG(ccd), LEN_LOG)
	    call ccdlog (IN_IM(ccd), FLATLOG(ccd))
	}
	call sprintf (FLATLOG(ccd), LEN_LOG, "Flat is %s, scale %g")
	    call pargstr (Memc[image])
	    call pargr (FLATSCALE(ccd))
	call timelog (FLATLOG(ccd), LEN_LOG)
	call ccdlog (IN_IM(ccd), FLATLOG(ccd))

	call sfree (sp)
end
