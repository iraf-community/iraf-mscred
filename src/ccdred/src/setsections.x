include	<imhdr.h>
include	<mwset.h>
include	"ccdred.h"

# SET_SECTIONS -- Set the data section, ccd section, trim section and
# bias section.  Also set the WCS.

procedure set_sections (ccd)

pointer	ccd			# CCD structure (returned)

pointer	sp, key, str, mw, lterm, mw_openim()
real	ltv1, ltv2, ltm1_1, ltm2_2
int	i, j, nc, nl, c1, c2, cs, csum, l1, l2, ls, lsum, ndim
bool	cflip, lflip, streq()
int	ctoi(), mw_stati(), btoi()

begin
	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	nc = IM_LEN(IN_IM(ccd),1)
	nl = IM_LEN(IN_IM(ccd),2)

	# The default data section is the entire image.
	c1 = 1
	c2 = nc
	cs = 1
	l1 = 1
	l2 = nl
	ls = 1
	call hdmgstr (IN_IM(ccd), "datasec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	cflip = (cs < 0)
	lflip = (ls < 0)
	if (cflip) {
	    i = c1
	    c1 = c2
	    c2 = i
	    cs = -cs
	}
	if (lflip) {
	    i = l1
	    l1 = l2
	    l2 = i
	    ls = -ls
	}
	if ((c1<1)||(c2>nc)||(l1<1)||(l2>nl)||(cs!=1)||(ls!=1))
	    call error (0, "Error in DATASEC parameter")
	IN_C1(ccd) = c1
	IN_C2(ccd) = c2
	IN_L1(ccd) = l1
	IN_L2(ccd) = l2
	IN_CFLIP(ccd) = btoi (cflip)
	IN_LFLIP(ccd) = btoi (lflip)

	# The default trim section is the data section.
	# Defer limit checking until actually used.
	c1 = IN_C1(ccd)
	c2 = IN_C2(ccd)
	l1 = IN_L1(ccd)
	l2 = IN_L2(ccd)
	call clgstr ("trimsec", Memc[key], SZ_LINE)
	if (streq (Memc[key], "image"))
	    call strcpy ("!trimsec", Memc[key], SZ_LINE)
	if (Memc[key] == '!')
	    call hdmgstr (IN_IM(ccd), Memc[key+1], Memc[str], SZ_LINE)
	else
	    call strcpy (Memc[key], Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((abs(cs)!=1)||(abs(ls)!=1))
	    call error (0, "Error in TRIMSEC parameter")
	TRIM_C1(ccd) = min(c1,c2)
	TRIM_C2(ccd) = max(c1,c2)
	TRIM_L1(ccd) = min(l1,l2)
	TRIM_L2(ccd) = max(l1,l2)

	# The default bias section is the whole image.
	# Defer limit checking until actually used.
	c1 = 1
	c2 = nc
	l1 = 1
	l2 = nl
	call clgstr ("biassec", Memc[key], SZ_LINE)
	if (streq (Memc[key], "image"))
	    call strcpy ("!biassec", Memc[key], SZ_LINE)
	if (Memc[key] == '!')
	    call hdmgstr (IN_IM(ccd), Memc[key+1], Memc[str], SZ_LINE)
	else
	    call strcpy (Memc[key], Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((abs(cs)!=1)||(abs(ls)!=1))
	    call error (0, "Error in BIASSEC parameter")
	BIAS_C1(ccd) = min(c1,c2)
	BIAS_C2(ccd) = max(c1,c2)
	BIAS_L1(ccd) = min(l1,l2)
	BIAS_L2(ccd) = max(l1,l2)

	# The default ccd section is the size of the data section.
	c1 = 1
	c2 = IN_C2(ccd) - IN_C1(ccd) + 1
	l1 = 1
	l2 = IN_L2(ccd) - IN_L1(ccd) + 1
	call hdmgstr (IN_IM(ccd), "ccdsec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((abs(cs) != 1) || (abs(ls) != 1))
	    call error (0, "Error in CCDSEC parameter")
	if (cflip) {
	    i = c1
	    c1 = c2
	    c2 = i
	    cs = -cs
	}
	if (lflip) {
	    i = l1
	    l1 = l2
	    l2 = i
	    ls = -ls
	}
	CCD_C1(ccd) = c1
	CCD_C2(ccd) = c2
	CCD_CS(ccd) = cs
	CCD_L1(ccd) = l1
	CCD_L2(ccd) = l2
	CCD_LS(ccd) = ls

	# Set the binning.
	call hdmgstr (IN_IM(ccd), "ccdsum", Memc[str], SZ_LINE)
	if (Memc[str] == EOS) {
	    csum = 1
	    lsum = 1
	} else {
	    i = 1
	    if (ctoi (Memc[str], i, csum) == 0)
		csum = 0
	    if (ctoi (Memc[str], i, lsum) == 0)
		lsum = 0
	}
	if (csum < 1 || lsum < 1)
	    call error (1, "Error in CCDSUM parameter")
	CCD_CS(ccd) = CCD_CS(ccd) * csum
	CCD_LS(ccd) = CCD_LS(ccd) * lsum

	i = (abs (CCD_C2(ccd) - CCD_C1(ccd)) + 1) / abs (CCD_CS(ccd))
	j = (abs (CCD_L2(ccd) - CCD_L1(ccd)) + 1) / abs (CCD_LS(ccd))
	if (IN_C2(ccd)-IN_C1(ccd)+1!=i || IN_L2(ccd)-IN_L1(ccd)+1!=j)
	    call error (0, "Size of DATASEC and CCDSEC do not agree")

	# The default output data section is the input data section.
	OUT_C1(ccd) = IN_C1(ccd)
	OUT_C2(ccd) = IN_C2(ccd)
	OUT_L1(ccd) = IN_L1(ccd)
	OUT_L2(ccd) = IN_L2(ccd)

	# Set the physical WCS to be CCD coordinates.
	mw = mw_openim (IN_IM(ccd))
	ndim = mw_stati (mw, MW_NPHYSDIM)
	call salloc (lterm, ndim * (1 + ndim), TY_REAL)
	call mw_gltermr (mw, Memr[lterm+ndim], Memr[lterm], ndim)
	ltm1_1 = 1. / CCD_CS(ccd)
	ltm2_2 = 1. / CCD_LS(ccd)
        if (ltm1_1 > 0)
            ltv1 = IN_C1(ccd) - ltm1_1 * (CCD_C1(ccd) + 0.5 * (1 / ltm1_1 - 1))
	else
	    ltv1 = IN_C1(ccd) - ltm1_1 * (CCD_C1(ccd) + 0.5 * (1 / ltm1_1 + 1))
	if (ltm2_2 > 0)
	    ltv2 = IN_L1(ccd) - ltm2_2 * (CCD_L1(ccd) + 0.5 * (1 / ltm2_2 - 1))
	else
            ltv2 = IN_L1(ccd) - ltm2_2 * (CCD_L1(ccd) + 0.5 * (1 / ltm2_2 + 1))
	Memr[lterm] = ltv1
	Memr[lterm+1] = ltv2
	Memr[lterm+ndim] = ltm1_1
	Memr[lterm+ndim+1] = 0.
	Memr[lterm+ndim+ndim] = 0.
	Memr[lterm+ndim+ndim+1] = ltm2_2
	call mw_sltermr (mw, Memr[lterm+ndim], Memr[lterm], ndim)
	call mw_saveim (mw, IN_IM(ccd))
	call mw_close (mw)

	call sfree (sp)
end


# SET_CALSECTION -- Check and set calibration sections.

procedure set_calsection (ccd, im, data_c1, data_c2, data_l1, data_l2)

pointer	ccd			#I CCD structure
pointer	im			#I Calibration image pointer
int	data_c1			#O Calibration image starting column
int	data_c2			#O Calibration image ending column
int	data_l1			#O Calibration image starting line
int	data_l2			#O Calibration image ending line

bool	cflip, lflip
int	i, nc, nl, c1, c2, cs, csum, l1, l2, ls, lsum, ctoi()
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Check data section.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	c1 = 1
	c2 = nc
	l1 = 1
	l2 = nl
	cs = 1
	ls = 1
	call hdmgstr (im, "datasec", Memc[str], SZ_FNAME)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
#	if ((cs<0 && IN_CFLIP(ccd)==NO) || (cs>0 && IN_CFLIP(ccd)==YES) ||
#	    (ls<0 && IN_LFLIP(ccd)==NO) || (ls>0 && IN_LFLIP(ccd)==YES))
#	    call error (1, "Data section flipped relative to target image")
#	cflip = (IN_CFLIP(ccd)==YES)
#	lflip = (IN_LFLIP(ccd)==YES)
	cflip = (cs < 0)
	lflip = (ls < 0)
	if (cflip) {
	    i = c1
	    c1 = c2
	    c2 = i
	    cs = -cs
	}
	if (lflip) {
	    i = l1
	    l1 = l2
	    l2 = i
	    ls = -ls
	}
	if ((c1<1)||(c2>nc)||(l1<1)||(l2>nl)||(cs!=1)||(ls!=1)) {
	    call sprintf (Memc[str], SZ_LINE,
		"Data section error: image size=[%d,%d], datasec=[%d:%d,%d:%d]")
		call pargi (nc)
		call pargi (nl)
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call error (1, Memc[str])
	}
	data_c1 = c1
	data_l1 = l1

	# Check CCDSEC.
	c1 = 1
	c2 = nc
	l1 = 1
	l2 = nl
	call hdmgstr (im, "ccdsec", Memc[str], SZ_FNAME)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((abs (cs) != 1) || (abs (ls) != 1))
	    call error (1, "Error in CCDSEC parameter")
	if (cflip) {
	    i = c1
	    c1 = c2
	    c2 = i
	    cs = -cs
	}
	if (lflip) {
	    i = l1
	    l1 = l2
	    l2 = i
	    ls = -ls
	}
	if (nc == 1) {
	    c1 = CCD_C1(ccd)
	    c2 = CCD_C2(ccd)
	}
	if (nl == 1) {
	    l1 = CCD_L1(ccd)
	    l2 = CCD_L2(ccd)
	}
	call hdmgstr (im, "ccdsum", Memc[str], SZ_FNAME)
	if (Memc[str] == EOS) {
	    csum = 1
	    lsum = 1
	} else {
	    i = 1
	    if (ctoi (Memc[str], i, csum) == 0)
		csum = 0
	    if (ctoi (Memc[str], i, lsum) == 0)
		lsum = 0
	}
	if (csum < 1 || lsum < 1)
	    call error (1, "Error in CCDSUM parameter")
	cs = cs * csum
	ls = ls * lsum
	if (cs != CCD_CS(ccd) || ls != CCD_LS(ccd)) {
	    call sprintf (Memc[str], SZ_LINE,
	        "CCD sum error: target='%d %d', calibration='%d %d'")
		call pargi (CCD_CS(ccd))
		call pargi (CCD_LS(ccd))
		call pargi (cs)
		call pargi (ls)
	    call error (0, Memc[str])
	}

	if ((IN_C2(ccd)-IN_C1(ccd) != (CCD_C2(ccd)-CCD_C1(ccd))/cs) ||
	    (IN_L2(ccd)-IN_L1(ccd) != (CCD_L2(ccd)-CCD_L1(ccd))/ls)) {
	    if ((min(c1,c2) > min(CCD_C1(ccd),CCD_C2(ccd))) ||
		(max(c1,c2) < max(CCD_C1(ccd),CCD_C2(ccd))) ||
		(min(l1,l2) > min(CCD_L1(ccd),CCD_L2(ccd))) ||
		(max(l1,l2) < max(CCD_L1(ccd),CCD_L2(ccd)))) {
		call sprintf (Memc[str], SZ_LINE,
	"CCD section error: target=[%d:%d,%d:%d], calibration=[%d:%d,%d:%d]")
		    call pargi (CCD_C1(ccd))
		    call pargi (CCD_C2(ccd))
		    call pargi (CCD_L1(ccd))
		    call pargi (CCD_L2(ccd))
		    call pargi (c1)
		    call pargi (c2)
		    call pargi (l1)
		    call pargi (l2)
		call error (1, Memc[str])
	    }
	}

	data_c2 = (CCD_C2(ccd) - c1) / cs + data_c1
	data_c1 = (CCD_C1(ccd) - c1) / cs + data_c1
	data_l2 = (CCD_L2(ccd) - l1) / ls + data_l1
	data_l1 = (CCD_L1(ccd) - l1) / ls + data_l1

	call sfree (sp)
end
