include	<imhdr.h>
include	<imset.h>
include	"ccdred.h"

# SET_TRIM -- Set the trim parameters.
#
#   1.  Return immediately if the trim correction is not requested or
#	if the image has been previously corrected.
#   2.	Determine the trim section.  This may be specifed directly or
#	indirectly through the image header or symbol table.
#   3.  Parse the trim section and apply it to the output image.
#   4.  If the image is trimmed then log the operation and reset the output
#	image size.

procedure set_trim (ccd)

pointer	ccd			# CCD structure

int	nc, nl, c1, c2, l1, l2
pointer	sp, str, image
bool	clgetb(), ccdflag()

begin
	# Check if the user wants this operation or it has been done.
	if (!clgetb ("trim") || ccdflag (IN_IM(ccd), "trim")) {
	    TRIM_C1(ccd) = 1
	    TRIM_C2(ccd) = IM_LEN(IN_IM(ccd),1)
	    TRIM_L1(ccd) = 1
	    TRIM_L2(ccd) = IM_LEN(IN_IM(ccd),2)
	    TRIM_DC1(ccd) = 0
	    TRIM_DC2(ccd) = 0
	    TRIM_DL1(ccd) = 0
	    TRIM_DL2(ccd) = 0
	    return
	}

	# Check trim section.
	nc = IM_LEN(IN_IM(ccd),1)
	nl = IM_LEN(IN_IM(ccd),2)
	c1 = TRIM_C1(ccd)
	c2 = TRIM_C2(ccd)
	l1 = TRIM_L1(ccd)
	l2 = TRIM_L2(ccd)
	if ((c1 < 1) || (c2 > nc) || (l1 < 1) || (l2 > nl)) {
	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call salloc (image, SZ_LINE, TY_CHAR)
	    call imstats (IN_IM(ccd), IM_IMAGENAME, Memc[image], SZ_FNAME)
	    call sprintf (Memc[str], SZ_LINE,
		"Error in trim section: image=%s[%d,%d], trimsec=[%d:%d,%d:%d]")
		call pargstr (Memc[image])
		call pargi (nc)
		call pargi (nl)
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call error (0, Memc[str])
	}

	# If no processing is desired print trim section and return.
	if (LISTPROC(ccd) == YES) {
	    call eprintf ("  [TO BE DONE] Trim section is [%d:%d,%d:%d].\n")
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    return
	}

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	TRIM_DC1(ccd) = max (0, c1 - IN_C1(ccd))
	TRIM_DC2(ccd) = min (0, c2 - IN_C2(ccd))
	TRIM_DL1(ccd) = max (0, l1 - IN_L1(ccd))
	TRIM_DL2(ccd) = min (0, l2 - IN_L2(ccd))
	CCD_C1(ccd) = CCD_C1(ccd) + CCD_CS(ccd) * TRIM_DC1(ccd)
	CCD_C2(ccd) = CCD_C2(ccd) + CCD_CS(ccd) * TRIM_DC2(ccd)
	CCD_L1(ccd) = CCD_L1(ccd) + CCD_LS(ccd) * TRIM_DL1(ccd)
	CCD_L2(ccd) = CCD_L2(ccd) + CCD_LS(ccd) * TRIM_DL2(ccd)
	IN_C1(ccd) = IN_C1(ccd) + TRIM_DC1(ccd)
	IN_C2(ccd) = IN_C2(ccd) + TRIM_DC2(ccd)
	IN_L1(ccd) = IN_L1(ccd) + TRIM_DL1(ccd)
	IN_L2(ccd) = IN_L2(ccd) + TRIM_DL2(ccd)
	OUT_C1(ccd) = IN_C1(ccd) - c1 + 1
	OUT_C2(ccd) = IN_C2(ccd) - c1 + 1
	OUT_L1(ccd) = IN_L1(ccd) - l1 + 1
	OUT_L2(ccd) = IN_L2(ccd) - l1 + 1

	COR(ccd) = YES
	COROUT(ccd) = YES
	CORS(ccd, TRIM) = YES

	call sprintf (TRIMLOG(ccd), LEN_LOG, "Trim is [%d:%d,%d:%d]")
	    call pargi (c1)
	    call pargi (c2)
	    call pargi (l1)
	    call pargi (l2)
	call timelog (TRIMLOG(ccd), LEN_LOG)
	call ccdlog (IN_IM(ccd), TRIMLOG(ccd))

	call sfree (sp)
end
