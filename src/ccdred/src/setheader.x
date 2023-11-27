include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	"ccdred.h"


define	FUDGE	10	# Number of seconds to fudge the CCDMEANT value.

# SET_HEADER -- Set the output image headers.

procedure set_header (ccd)

pointer	ccd			# CCD structure

int	i, j, nc, nl, c1, c2, l1, l2, cs, ls
real	shift[2]
pointer	sp, str, out[3], bpm, im, mw, mw_openim(), immap()
long	clktime()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	out[1] = OUT_IM(ccd)
	out[2] = NOIOUT_IM(ccd)
	out[3] = BPOUT_IM(ccd)

	do j = 1, 3 {
	    im = out[j]
	    if (im == NULL)
		next

	    nc = IM_LEN(im,1)
	    nl = IM_LEN(im,2)

	    # Set the data section if it is not the whole image.
	    if ((OUT_C1(ccd) != 1) || (OUT_C2(ccd) != nc) ||
		(OUT_L1(ccd) != 1) || (OUT_L2(ccd) != nl)) {
		call sprintf (Memc[str], SZ_LINE, "[%d:%d,%d:%d]")
		    call pargi (OUT_C1(ccd))
		    call pargi (OUT_C2(ccd))
		    call pargi (OUT_L1(ccd))
		    call pargi (OUT_L2(ccd))
		call hdmpstr (im, "datasec", Memc[str])
	    } else {
		iferr (call hdmdelf (im, "datasec"))
		    ;
	    }

	    # Set the CCD section.
	    call sprintf (Memc[str], SZ_LINE, "[%d:%d,%d:%d]")
		call pargi (CCD_C1(ccd))
		call pargi (CCD_C2(ccd))
		call pargi (CCD_L1(ccd))
		call pargi (CCD_L2(ccd))
	    call hdmpstr (im, "ccdsec", Memc[str])

	    # Set the detector section.  The default is CCDSEC.
	    call hdmgstr (IN_IM(ccd), "detsec", Memc[str], SZ_LINE)
	    l1 = CCD_L1(ccd) - CCD_LS(ccd) * TRIM_DL1(ccd)
	    l2 = CCD_L2(ccd) - CCD_LS(ccd) * TRIM_DL2(ccd)
	    c1 = CCD_C1(ccd) - CCD_CS(ccd) * TRIM_DC1(ccd)
	    c2 = CCD_C2(ccd) - CCD_CS(ccd) * TRIM_DC2(ccd)
	    cs = 1
	    ls = 1
	    call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	    if ((abs(cs) != 1) || (abs(ls) != 1))
		call error (0, "Error in DETSEC parameter")
	    if (IN_CFLIP(ccd) == YES) {
		i = c1
		c1 = c2
		c2 = i
		cs = -cs
	    }
	    if (IN_LFLIP(ccd) == YES) {
		i = l1
		l1 = l2
		l2 = i
		ls = -ls
	    }
	    c1 = c1 + CCD_CS(ccd) * TRIM_DC1(ccd)
	    c2 = c2 + CCD_CS(ccd) * TRIM_DC2(ccd)
	    l1 = l1 + CCD_LS(ccd) * TRIM_DL1(ccd)
	    l2 = l2 + CCD_LS(ccd) * TRIM_DL2(ccd)
	    iferr {
		if ((abs(c2-c1) != abs(CCD_C2(ccd)-CCD_C1(ccd))) ||
		    (abs(l2-l1) != abs (CCD_L2(ccd)-CCD_L1(ccd))))
		    call error (0, "Size of DETSEC and CCDSEC do not agree")
		call sprintf (Memc[str], SZ_LINE, "[%d:%d,%d:%d]")
		    call pargi (c1)
		    call pargi (c2)
		    call pargi (l1)
		    call pargi (l2)
		call hdmpstr (im, "detsec", Memc[str])
	    } then {
		call erract (EA_WARN)
		iferr (call hdmdelf (im, "detsec"))
		    ;
	    }

	    # If trimming update trim and bias section parameters and the WCS.
	    if (CORS(ccd, TRIM) == YES) {
		iferr (call hdmdelf (im, "trimsec"))
		    ;
		iferr (call hdmdelf (im, "biassec"))
		    ;
		BIAS_C1(ccd) = max (1, BIAS_C1(ccd) - TRIM_C1(ccd) + 1)
		BIAS_C2(ccd) = min (nc, BIAS_C2(ccd) - TRIM_C1(ccd) + 1)
		BIAS_L1(ccd) = max (1, BIAS_L1(ccd) - TRIM_L1(ccd) + 1)
		BIAS_L2(ccd) = min (nl, BIAS_L2(ccd) - TRIM_L1(ccd) + 1)
		if ((BIAS_C1(ccd)<=BIAS_C2(ccd))&&(BIAS_L1(ccd)<=BIAS_L2(ccd))){
		    call sprintf (Memc[str], SZ_LINE, "[%d:%d,%d:%d]")
			call pargi (BIAS_C1(ccd))
			call pargi (BIAS_C2(ccd))
			call pargi (BIAS_L1(ccd))
			call pargi (BIAS_L2(ccd))
		    call hdmpstr (im, "biassec", Memc[str])
		}

		mw = mw_openim (im)
		shift[1] = 1 - IN_C1(ccd)
		shift[2] = 1 - IN_L1(ccd)
		call mw_shift (mw, shift, 3)
		call mw_saveim (mw, im)
		if (bpm != NULL)
		    call mw_saveim (mw, bpm)
		call mw_close (mw)
	    }

	    # Put log in header.
	    if (SATLOG(ccd) != EOS)
		call hdmpstr (im, "satproc", SATLOG(ccd))
	    if (BLDLOG(ccd) != EOS)
		call hdmpstr (im, "bldproc", BLDLOG(ccd))
	    if (TRIMLOG(ccd) != EOS)
		call hdmpstr (im, "trim", TRIMLOG(ccd))
	    if (FIXLOG(ccd) != EOS)
		call hdmpstr (im, "fixpix", FIXLOG(ccd))
	    if (BIASLOG(ccd) != EOS)
		call hdmpstr (im, "overscan", BIASLOG(ccd))
	    if (ZEROLOG(ccd) != EOS)
		call hdmpstr (im, "zerocor", ZEROLOG(ccd))
	    if (DARKLOG(ccd) != EOS)
		call hdmpstr (im, "darkcor", DARKLOG(ccd))
	    if (FLATLOG(ccd) != EOS)
		call hdmpstr (im, "flatcor", FLATLOG(ccd))
	    if (SFLATLOG(ccd) != EOS)
		call hdmpstr (im, "sflatcor", SFLATLOG(ccd))
	    if (ILLUMLOG(ccd) != EOS)
		call hdmpstr (im, "illumcor", ILLUMLOG(ccd))
	    if (FRINGELOG(ccd) != EOS)
		call hdmpstr (im, "fringcor", FRINGELOG(ccd))

	    # Set new BPM in output image.  If no output set in input.
	    if (out[3] != NULL) {
		if (out[1] == NULL) {
		    call imstats (IN_IM(ccd), IM_IMAGENAME, Memc[str], SZ_LINE)
		    call imunmap (IN_IM(ccd))
		    iferr (IN_IM(ccd) = immap (Memc[str], READ_WRITE, 0))
		        IN_IM(ccd) = immap (Memc[str], READ_ONLY, 0)
		    else {
			call imstats (out[3], IM_IMAGENAME, Memc[str], SZ_LINE)
			call hdmpstr (IN_IM(ccd), "bpm", Memc[str])
		    }
		}
		call imstats (out[3], IM_IMAGENAME, Memc[str], SZ_LINE)
		call hdmpstr (im, "bpm", Memc[str])
	    }

	    # Set mean value if desired.
	    if (CORS(ccd, FINDMEAN) == YES) {
		call hdmputr (im, "ccdmean", MEAN(ccd))
		call hdmputi (im, "ccdmeant", int (clktime (long (0)))+FUDGE)
	    }

	    # Mark image as processed.
	    call sprintf (Memc[str], SZ_LINE, "CCD processing done")
	    call timelog (Memc[str], SZ_LINE)
	    call hdmpstr (im, "ccdproc", Memc[str])

	    # Update OBSID.
	    call xt_procid (im)
	}

	call sfree (sp)
end
