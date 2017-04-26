include	<imset.h>
include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_SFLAT -- Set parameters for second flat field correction.
#
#   1.  Return immediately if the sky flat field correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the sky flat field image and return on an error.
#   3.  If the sky flat field image has not been processed call PROC.
#   4.  Set the processing flags and record the operation in the output
#   	image and write a log record.

procedure set_sflat (ccd)

pointer	ccd			# CCD structure

pointer	sp, str, image, im, ccd_cache()
bool	clgetb(), ccdflag(), ccdcheck()
int	nscan, ccdnscan()
real	ccdmean()
errchk	cal_image, ccd_cache, ccdproc1, ccdproc2, ccdmean, set_calsection

begin
	# Check if the user wants this operation or if it has been done.
	if (!clgetb ("sflatcor") || ccdflag (IN_IM(ccd), "sflatcor"))
	    return

	# Require the first flat field correction to be done.
	#if (!ccdflag (IN_IM(ccd), "flatcor") && CORS(ccd, FLATCOR) != F)
	#    call error (1,
	#	"Sky flat field correction requires flat field correction")

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the sky flat field correction image.
	if (clgetb ("scancor"))
	    nscan = ccdnscan (IN_IM(ccd), IN_CCDTYPE(ccd))
	else
	    nscan = 1
	call cal_image (IN_IM(ccd), SFLAT, nscan, Memc[image], SZ_FNAME)

	# If not processing print sky flat field image name and return.
	if (LISTPROC(ccd) == YES) {
	    call eprintf ("  [TO BE DONE] Sky flat is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Map the image and return on an error.
	# Process the sky flat field image if necessary.
	# If nscan > 1 then the flat field may not yet exist so create it
	# from the unscanned flat field.

	iferr (im = ccd_cache (Memc[image], SFLAT)) {
	    call cal_image (IN_IM(ccd), SFLAT, 1, Memc[str], SZ_LINE)
	    im = ccd_cache (Memc[str], SFLAT)
	    if (ccdcheck (im, SFLAT, "")) {
		call ccd_flush (im)
		switch (CALPROC(ccd)) {
		case CALPROC_YES:
		    call ccdproc1 (Memc[str], Memc[str], SFLAT)
		case CALPROC_NO:
		    call ccdproc2 (Memc[str], Memc[str], SFLAT)
		    PROC(ccd) = NO
		    call sfree (sp)
		    return
		}
	    }
	    call scancor (Memc[str], Memc[image], nscan, MINREPLACE(ccd))
	    im = ccd_cache (Memc[image], SFLAT)
	}

	if (ccdcheck (im, SFLAT, "")) {
	    call ccd_flush (im)
	    switch (CALPROC(ccd)) {
	    case CALPROC_YES:
	    call ccdproc1 (Memc[image], Memc[image], SFLAT)
	    case CALPROC_NO:
		call ccdproc2 (Memc[image], Memc[image], SFLAT)
		PROC(ccd) = NO
		call sfree (sp)
		return
	    }
	    im = ccd_cache (Memc[image], SFLAT)
	}
	SFLAT_IM(ccd) = im

	# Check and set data section.
	call set_calsection (ccd, SFLAT_IM(ccd), SFLAT_C1(ccd), SFLAT_C2(ccd),
	    SFLAT_L1(ccd), SFLAT_L2(ccd))

	# Set the scaling factor from the mean.
	SFLATSCALE(ccd) = ccdmean (im)

	COR(ccd) = YES
	COROUT(ccd) = YES
	CORS(ccd, SFLATCOR) = S

	# Log the operation.
	call sprintf (SFLATLOG(ccd), LEN_LOG, "Sky flat is %s, scale %g")
	    call pargstr (Memc[image])
	    call pargr (SFLATSCALE(ccd))
	call timelog (SFLATLOG(ccd), LEN_LOG)
	call ccdlog (IN_IM(ccd), SFLATLOG(ccd))

	call sfree (sp)
end
