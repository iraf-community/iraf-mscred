include	<imhdr.h>
include	<pmset.h>
include	"ccdred.h"

# SET_PROC -- Set the processing parameter structure pointer.

procedure set_proc (in, proc, calproc, listproc, ccd)

pointer	in			# Input IMIO pointer
pointer	ccd			# CCD structure (returned)
int	proc			# Process image?
int	calproc			# Process calibration images?
int	listproc		# List processing to be done?

int	ccdtypes(), clgwrd(), clscan(), nscan()
real	clgetr()
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate the ccd structure.
	call calloc (ccd, LEN_CCD, TY_STRUCT)

	IN_IM(ccd) = in
	BPIN_IM(ccd) = NULL

	IN_CCDTYPE(ccd) = ccdtypes (in, Memc[str], SZ_LINE)

	PROC(ccd) = proc
	CALPROC(ccd) = calproc
	LISTPROC(ccd) = listproc

	COR(ccd) = NO
	COROUT(ccd) = NO
	CORBPM(ccd) = NO
	CORS(ccd, SATURATE) = NO
	CORS(ccd, FIXPIX) = NO
	CORS(ccd, OVERSCAN) = NO
	CORS(ccd, TRIM) = NO

	READAXIS(ccd) = clgwrd ("readaxis",Memc[str],SZ_LINE,"|line|columns|")
	MINREPLACE(ccd) = clgetr ("minreplace")

	CALCTYPE(ccd) = TY_REAL
	if (clscan ("pixeltype") != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan() == 2) {
	        if (Memc[str] == 'r')
		    CALCTYPE(ccd) = TY_REAL
		else if (Memc[str] == 's')
		    CALCTYPE(ccd) = TY_SHORT
		else
		    call error (1, "Invalid calculation datatype")
	    }
	}

	call sfree (sp)
end


# FREE_PROC -- Free the processing structure pointer.

procedure free_proc (ccd)

pointer	ccd			# CCD structure

begin
	# Unmap calibration images.
	if (BPIN_IM(ccd) != NULL)
	    call yt_pmunmap (BPIN_IM(ccd))
	if (ZERO_IM(ccd) != NULL)
	    call ccd_unmap (ZERO_IM(ccd))
	if (DARK_IM(ccd) != NULL)
	    call ccd_unmap (DARK_IM(ccd))
	if (FLAT_IM(ccd) != NULL)
	    call ccd_unmap (FLAT_IM(ccd))
	if (SFLAT_IM(ccd) != NULL)
	    call ccd_unmap (SFLAT_IM(ccd))
	if (ILLUM_IM(ccd) != NULL)
	    call ccd_unmap (ILLUM_IM(ccd))
	if (FRINGE_IM(ccd) != NULL)
	    call ccd_unmap (FRINGE_IM(ccd))

	# Free memory
	if (OVERSCAN_VEC(ccd) != NULL)
	    call mfree (OVERSCAN_VEC(ccd), TY_REAL)
	if (BPIN_FP(ccd) != NULL) {
	    # Bug fix for V2.11.1
	    if (Memi[BPIN_FP(ccd)+11] == NULL) {
		switch (CALCTYPE (ccd)) {
		case TY_SHORT:
		    Memi[BPIN_FP(ccd)+11] = TY_SHORT
		default:
		    Memi[BPIN_FP(ccd)+11] = TY_REAL
		}
	    }

	    call xx_fpfree (BPIN_FP(ccd))
	}
	call mfree (ccd, TY_STRUCT)
end
