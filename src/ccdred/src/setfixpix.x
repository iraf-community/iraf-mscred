# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_FIXPIX -- Set parameters and structures for bad pixel correction.
#   1.  Return immediately if the bad pixel correction is not requested or
#	if the image has been previously corrected.
#   2.	Get the bad pixel mask.  Return an error if not found.
#   3.	Set the fixpix interpolation routines.
#   4.  Log the operation (to user, logfile, and output image header).

procedure set_fixpix (ccd)

pointer	ccd			# CCD structure

pointer	str
bool	clgetb(), ccdflag()
int	i, btoi(), strlen(), fnroot()
errchk	set_inmask

begin
	# Check if the user wants this operation or it has been done.
	CORS(ccd,FIXPIX) = btoi (clgetb ("fixpix") &&
	    !ccdflag (IN_IM(ccd), "fixpix"))
	if (CORS(ccd,FIXPIX)==NO)
	    return

	# Get input mask.
	call set_inmask (ccd)

	# Cancel if there is no input mask.
	if (BPIN_IM(ccd) == NULL) {
	    CORS(ccd,FIXPIX) = NO
	    return
	}

	# If no processing is desired print bad pixel mask and return.
	if (LISTPROC(ccd) == YES && CORS(ccd,FIXPIX) == YES) {
	    call eprintf ("  [TO BE DONE] Pixel mask is %s.\n")
		call pargstr (BPIN_NAME(ccd))
	    return
	}

	if (CORS(ccd,FIXPIX) == YES) {
	    COR(ccd) = YES
	    COROUT(ccd) = YES
	    #BPIN_PM(ccd) = imstati (BPIN_IM(ccd), IM_PMDES)
	    #BPIN_FP(ccd) = xx_fpinit (BPIN_PM(ccd), 2, 3)
	    call set_fp (BPIN_IM(ccd), BPIN_FP(ccd))

	    # Log operation.
	    call sprintf (FIXLOG(ccd), LEN_LOG, "Fix pixels in %s")
		call pargstr (BPIN_NAME(ccd))
	    call timelog (FIXLOG(ccd), LEN_LOG)
		call ccdlog (IN_IM(ccd), FIXLOG(ccd))

	    # Set processing keyword.
	    call sprintf (FIXLOG(ccd), LEN_LOG, "Fix %s")
		call pargstr (BPIN_NAME(ccd))
	    if (!IS_INDEF(SATVAL(ccd)))
		call strcat (" + sat", FIXLOG(ccd), LEN_LOG)
	    if (!IS_INDEF(BLDVAL(ccd)))
		call strcat (" + bleed", FIXLOG(ccd), LEN_LOG)
	    if (strlen (FIXLOG(ccd)) > 55) {
		call malloc (str, SZ_LINE, TY_CHAR)
		i = fnroot (BPIN_IM(ccd), Memc[str], SZ_LINE)
		call sprintf (FIXLOG(ccd), LEN_LOG, "Fix %s")
		    call pargstr (Memc[str])
		if (!IS_INDEF(SATVAL(ccd)))
		    call strcat ("+sat", FIXLOG(ccd), LEN_LOG)
		if (!IS_INDEF(BLDVAL(ccd)))
		    call strcat ("+bleed", FIXLOG(ccd), LEN_LOG)
		call mfree (str, TY_CHAR)
	    }
	    call timelog (FIXLOG(ccd), LEN_LOG)
	}
end


# SET_FP -- Set the fixpix mask.
#
# This routine transforms the input mask values into the output mask
# values.  It allows the input mask to have two classes of bad pixels;
# those which are interpolated and those which are not.

procedure set_fp (im, fp)

pointer	im			#I Input mask image pointer
pointer	fp			#O FIXPIX interpolation pointer

int	i, j, nc, nl
long	v[2]
pointer	data1, data2, pm, pmi

int	imstati(), pm_newcopy()
pointer	xx_fpinit()
errchk	malloc, xx_fpinit

begin
	# Set the image size and data buffers.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	call malloc (data1, nc, TY_SHORT)
	call malloc (data2, nc, TY_SHORT)

	# Get the pixel mask from the image.
	pm = imstati (im, IM_PMDES)

	# Extract the pixels to be interpolated.
	pmi = pm_newcopy (pm)
	v[1] = 1
	do j = 1, nl {
	    v[2] = j
	    call pmglps (pm, v, Mems[data1], 0, nc, PIX_SRC)
	    do i = 0, nc-1 {
	        if (Mems[data1+i] > 1)
		    Mems[data1+i] = 0
	    }
	    call pmplps (pmi, v, Mems[data1], 0, nc, PIX_SRC)
	}

	# Set the interpolation.
	fp = xx_fpinit (pmi, 2, 3)

	# Finish up.
	call mfree (data1, TY_SHORT)
	call mfree (data2, TY_SHORT)
	call pm_close (pmi)
end
