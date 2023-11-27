include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"


# SET_DARK -- Set parameters for dark count correction.
#
#   1.  Return immediately if the dark count correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the dark count correction image and return an error if not found.
#   3.  If the dark count image has not been processed call PROC.
#   4.  Compute the dark count integration time scale factor.
#   5.  Set the processing flags.
#   6.  Log the operation (to user, logfile, and output image header).

procedure set_dark (ccd)

pointer	ccd			# CCD structure

int	nscan
real	darktime1, darktime2
pointer	sp, image, str, im

bool	clgetb(), ccdflag(), ccdcheck()
int	ccdnscan()
real	hdmgetr()
pointer	ccd_cache()
errchk	cal_image, ccd_cache, ccdproc1, ccdproc2, hdmgetr, set_calsection

begin
	# Check if the user wants this operation or it has already been done.
	if (!clgetb ("darkcor") || ccdflag (IN_IM(ccd), "darkcor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the dark count correction image name.
	if (clgetb ("scancor"))
	    nscan = ccdnscan (IN_IM(ccd), IN_CCDTYPE(ccd))
	else
	    nscan = 1
	call cal_image (IN_IM(ccd), DARK, nscan, Memc[image], SZ_FNAME)

	# If no processing is desired print dark count image and return.
	if (LISTPROC(ccd) == YES) {
	    call eprintf ("  [TO BE DONE] Dark is %s.\n")
	        call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Map the image and return on an error.
	# Process the dark count image if necessary.
	# If nscan > 1 then the dark may not yet exist so create it
	# from the unscanned dark.

	iferr (im = ccd_cache (Memc[image], DARK)) {
	    call cal_image (IN_IM(ccd), DARK, 1, Memc[str], SZ_LINE)
	    im = ccd_cache (Memc[str], DARK)
	    if (ccdcheck (im, DARK, "")) {
		call ccd_flush (im)
		switch (CALPROC(ccd)) {
		case CALPROC_YES:
		    call ccdproc1 (Memc[str], Memc[str], DARK)
		case CALPROC_NO:
		    call ccdproc2 (Memc[str], Memc[str], DARK)
		    PROC(ccd) = NO
		    call sfree (sp)
		    return
		}
	    }
	    call scancor (Memc[str], Memc[image], nscan, INDEF)
	    im = ccd_cache (Memc[image], DARK)
	}

	if (ccdcheck (im, DARK, "")) {
	    call ccd_flush (im)
	    switch (CALPROC(ccd)) {
	    case CALPROC_YES:
		call ccdproc1 (Memc[image], Memc[image], DARK)
	    case CALPROC_NO:
		call ccdproc2 (Memc[image], Memc[image], DARK)
		PROC(ccd) = NO
		call sfree (sp)
		return
	    }
	    im = ccd_cache (Memc[image], DARK)
	}
	DARK_IM(ccd) = im

	# Check and set data section.
	call set_calsection (ccd, DARK_IM(ccd), DARK_C1(ccd), DARK_C2(ccd),
	    DARK_L1(ccd), DARK_L2(ccd))

	# Get the dark count integration times.
	# Return an error if not found or zero.

	iferr (darktime1 = hdmgetr (IN_IM(ccd), "darktime"))
	    darktime1 = hdmgetr (IN_IM(ccd), "exptime")
	iferr (darktime2 = hdmgetr (im, "darktime"))
	    darktime2 = hdmgetr (im, "exptime")
	if (darktime2 <= 0.) {
	    call sprintf (Memc[str], SZ_LINE, "Dark time is zero for `%s'")
		call pargstr (Memc[image])
	    call error (1, Memc[str])
	}

	COR(ccd) = YES
	COROUT(ccd) = YES
	CORS(ccd, DARKCOR) = D
	DARKSCALE(ccd) = darktime1 / darktime2

	# Record the operation in the output image and write a log record.
	call sprintf (DARKLOG(ccd), LEN_LOG, "Dark is %s, scale %g")
	    call pargstr (Memc[image])
	    call pargr (DARKSCALE(ccd))
	call timelog (DARKLOG(ccd), LEN_LOG)
	call ccdlog (IN_IM(ccd), DARKLOG(ccd))

	call sfree (sp)
end
