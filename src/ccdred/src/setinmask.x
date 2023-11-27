# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ccdred.h"
include	"ccdtypes.h"

# SET_INMASK -- Set input mask.
# This routine relies on the physical coordinate system and assumes
# XT_PMMAP has taken care of matching the pixel mask to the input image.

procedure set_inmask (ccd)

pointer	ccd			# CCD structure

int	nscan 
pointer	sp, str, image, im
pointer	xt_pmmap()
bool	clgetb(), ccdcheck()
int	ccdnscan()
errchk	xt_pmmap, cal_image, ccdproc1, ccdproc2

begin
	# Check if the input mask has already been opened.
	if (BPIN_IM(ccd) != NULL)
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the bad pixel mask.  Return without an error if not found.
	if (clgetb ("scancor"))
	    nscan = ccdnscan (IN_IM(ccd), IN_CCDTYPE(ccd))
	else
	    nscan = 1
	iferr (call cal_image (IN_IM(ccd),MASK,nscan,Memc[image],SZ_FNAME)) {
	    call strcpy ("NONE", BPIN_NAME(ccd), LEN_CCDSTR)
	    return
	}

	# If no processing just set the mask name.
	if (LISTPROC(ccd) == YES) {
	    call strcpy (Memc[image], BPIN_NAME(ccd), LEN_CCDSTR)
	    call sfree (sp)
	    return
	}

	# Map the image and return on an error.
	# Process the mask image if necessary.
	# If nscan > 1 then the mask may not yet exist so create it
	# from the unscanned mask.

	iferr (im = xt_pmmap (Memc[image], IN_IM(ccd), Memc[image], SZ_FNAME)) {
	    call cal_image (IN_IM(ccd), MASK, 1, Memc[str], SZ_LINE)
	    im = xt_pmmap (Memc[str], IN_IM(ccd), Memc[str], SZ_LINE)
	    if (ccdcheck (im, MASK, "")) {
		call yt_pmunmap (im)
		switch (CALPROC(ccd)) {
		case CALPROC_YES:
		    call ccdproc1 (Memc[str], Memc[str], MASK)
		case CALPROC_NO:
		    call ccdproc2 (Memc[str], Memc[str], MASK)
		    PROC(ccd) = NO
		    call sfree (sp)
		    return
		}
	    }
	    call scancor (Memc[str], Memc[image], nscan, INDEF)
	    im = xt_pmmap (Memc[image], IN_IM(ccd), Memc[image], SZ_FNAME)
	}

	if (ccdcheck (im, MASK, "")) {
	    call yt_pmunmap (im)
	    switch (CALPROC(ccd)) {
	    case CALPROC_YES:
		call ccdproc1 (Memc[image], Memc[image], MASK)
	    case CALPROC_NO:
		call ccdproc2 (Memc[image], Memc[image], MASK)
		PROC(ccd) = NO
		call sfree (sp)
		return
	    }
	    im = xt_pmmap (Memc[image], IN_IM(ccd), Memc[image], SZ_FNAME)
	}

	BPIN_IM(ccd) = im
	call strcpy (Memc[image], BPIN_NAME(ccd), LEN_CCDSTR)
	call sfree (sp)
end
