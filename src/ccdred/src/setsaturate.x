# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"ccdred.h"

define	SATUNITS	"|ADUs|electrons|"
define	UNKNOWN		0	# Saturation in unknown units
define	ADU		1	# Saturation in ADU
define	ELECTRON	2	# Saturation in electrons


# SET_SATURATE -- Set saturation parameters.

procedure set_saturate (ccd)

pointer	ccd			#I CCD structure

bool	satproc, bldproc, fixpix
int	i, satunit, bldunit
real	sat, bleed, gain
pointer	sp, str, str1
bool	clgetb(), ccdflag()
int	clgeti(), ctowrd(), ctor(), strdic(), strlen(), fnroot()
real	hdmgetr()
errchk	hdmgetr

begin
	# Check if the operation is required.
	satproc = ccdflag (IN_IM(ccd), "satproc")
	bldproc = ccdflag (IN_IM(ccd), "satproc")
	if (satproc && bldproc)
	    return

	# If there is no output mask or pixel fixing then skip this operation. 
	fixpix = (clgetb ("fixpix") && !ccdflag (IN_IM(ccd), "fixpix"))
	if (CORBPM(ccd) == NO && !fixpix)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)

	# Set saturation value.  Need to set this even if done before in
	# case the bleed value is based on the saturation.
	call clgstr ("saturation", Memc[str], SZ_LINE)
	i = 1
	if (ctor (Memc[str], i, sat) == 0) {
	    if (ctowrd (Memc[str], i, Memc[str1], SZ_LINE) > 0) {
		if (Memc[str1] == '!')
		    call strcpy (Memc[str1+1], Memc[str1], SZ_LINE)
		iferr (sat = hdmgetr (IN_IM(ccd), Memc[str1]))
		    sat = INDEF
	    }
	}
	satunit = strdic (Memc[str+i-1], Memc[str1], SZ_LINE, SATUNITS)
	SATVALE(ccd) = sat
	if (!IS_INDEFR(sat)) {
	    switch (satunit) {
	    case ELECTRON:
		gain = hdmgetr (IN_IM(ccd), "gain")
		sat = sat / gain
	    default:
		iferr (gain = hdmgetr (IN_IM(ccd), "gain"))
		    SATVALE(ccd) = INDEFR
		else
		    SATVALE(ccd) = sat * gain
	    }
	    SATGROW(ccd) = clgeti ("sgrow")
	}
	if (satproc)
	    SATVAL(ccd) = INDEFR
	else
	    SATVAL(ccd) = sat

	# Set bleed value.
	if (!bldproc) {
	    call clgstr ("bleed", Memc[str], SZ_LINE)
	    i = 1
	    if (ctor (Memc[str], i, bleed) == 0) {
		if (ctowrd (Memc[str], i, Memc[str1], SZ_LINE) > 0) {
		    if (Memc[str1] == '!') {
			call strcpy (Memc[str1+1], Memc[str1], SZ_LINE)
			iferr (bleed = hdmgetr (IN_IM(ccd), Memc[str1]))
			    bleed = INDEF
		    } else { 
			call set_bldval (ccd, sat, Memc[str], bleed)
			call strcpy ("ADU", Memc[str], SZ_LINE)
			i = 1
		    }
		}
	    }
	    bldunit = strdic (Memc[str+i-1], Memc[str1], SZ_LINE, SATUNITS)
	    BLDVALE(ccd) = bleed
	    if (!IS_INDEFR(bleed)) {
		switch (bldunit) {
		case ELECTRON:
		    gain = hdmgetr (IN_IM(ccd), "gain")
		    bleed = bleed / gain
		default:
		    iferr (gain = hdmgetr (IN_IM(ccd), "gain"))
			BLDVALE(ccd) = INDEFR
		    else
			BLDVALE(ccd) = bleed * gain
		}
		BLDTRAIL(ccd) = clgeti ("btrail")
		BLDGROW(ccd) = clgeti ("bgrow")
		if (BLDTRAIL(ccd) == 0)
		    bleed = INDEF
		BLDVAL(ccd) = bleed
	    }
	    BLDVAL(ccd) = bleed
	} else
	    BLDVAL(ccd) = INDEFR


	# If there is no correction skip this operation.
	if (IS_INDEF(SATVAL(ccd)) && IS_INDEF(BLDVAL(ccd))) {
	    call sfree (sp)
	    return
	}

	# If not processing list operation only.
	if (LISTPROC(ccd) == YES) {
	    call eprintf (", Sat: %g ADU (%g e-), grw=%d.\n")
		call pargr (SATVAL(ccd))
		call pargr (SATVALE(ccd))
		call pargi (SATGROW(ccd))
	    call eprintf (", Bleed: %g ADU (%g e-), len=%d, grw=%d.\n")
		call pargr (BLDVAL(ccd))
		call pargr (BLDVALE(ccd))
		call pargi (BLDTRAIL(ccd))
		call pargi (BLDGROW(ccd))
	    return
	}

	# Set input mask.
	call set_inmask (ccd)

	# Set flags.
	COR(ccd) = YES
	CORS(ccd,SATURATE) = YES

	# Set pixel fixing and log parameters.
	if (fixpix) {
	    COROUT(ccd) = YES
	    CORS(ccd,FIXPIX) = YES

	    # Log operations.
	    if (!IS_INDEF(SATVAL(ccd))) {
		call sprintf (FIXLOG(ccd), LEN_LOG, "Fix saturated pixels")
		call timelog (FIXLOG(ccd), LEN_LOG)
		call ccdlog (IN_IM(ccd), FIXLOG(ccd))
	    }
	    if (!IS_INDEF(BLDVAL(ccd))) {
		call sprintf (FIXLOG(ccd), LEN_LOG, "Fix bleed pixels")
		call timelog (FIXLOG(ccd), LEN_LOG)
		call ccdlog (IN_IM(ccd), FIXLOG(ccd))
	    }

	    # Set processing keyword.
	    if (BPIN_IM(ccd) != NULL) {
		call sprintf (FIXLOG(ccd), LEN_LOG, "Fix %s")
		    call pargstr (BPIN_NAME(ccd))
		if (!IS_INDEF(SATVAL(ccd)))
		    call strcat (" + sat", FIXLOG(ccd), LEN_LOG)
		if (!IS_INDEF(BLDVAL(ccd)))
		    call strcat (" + bleed", FIXLOG(ccd), LEN_LOG)
	    } else if (!IS_INDEF(SATVAL(ccd))) {
		call sprintf (FIXLOG(ccd), LEN_LOG, "Fix saturated")
		if (!IS_INDEF(BLDVAL(ccd)))
		    call strcat (" and bleed trail", FIXLOG(ccd), LEN_LOG)
		call strcat (" pixels", FIXLOG(ccd), LEN_LOG)
	    } else
		call sprintf (FIXLOG(ccd), LEN_LOG, "Fix bleed trail pixels")
	    if (strlen (FIXLOG(ccd)) > 55) {
		i = fnroot (BPIN_IM(ccd), Memc[str], SZ_LINE)
		call sprintf (FIXLOG(ccd), LEN_LOG, "Fix %s")
		    call pargstr (Memc[str])
		if (!IS_INDEF(SATVAL(ccd)))
		    call strcat ("+sat", FIXLOG(ccd), LEN_LOG)
		if (!IS_INDEF(BLDVAL(ccd)))
		    call strcat ("+bleed", FIXLOG(ccd), LEN_LOG)
	    }
	    call timelog (FIXLOG(ccd), LEN_LOG)
	}

	# Log saturation and bleed trail parameters.
	if (!IS_INDEF(sat)) {
	    call sprintf (SATLOG(ccd), LEN_LOG,
		"Sat: %.6g ADU (%.6g e-), grw=%d")
		call pargr (SATVAL(ccd))
		call pargr (SATVALE(ccd))
		call pargi (SATGROW(ccd))
	    call timelog (SATLOG(ccd), LEN_LOG)
	    call ccdlog (IN_IM(ccd), SATLOG(ccd))
	}
	if (!IS_INDEF(bleed)) {
	    call sprintf (BLDLOG(ccd), LEN_LOG,
		"Bleed: %.6g ADU (%.6g e-), len=%d, grw=%d")
		call pargr (BLDVAL(ccd))
		call pargr (BLDVALE(ccd))
		call pargi (BLDTRAIL(ccd))
		call pargi (BLDGROW(ccd))
	    call timelog (BLDLOG(ccd), LEN_LOG)
	    call ccdlog (IN_IM(ccd), BLDLOG(ccd))
	}
end


define	BLDTRIM		10	# Number of lines and columns to trim in mean
define	BLDSAMPLE	100	# Number of sample ines for the mean

# SET_BLDVAL -- Set the bleed value from a string.
# This routine parses threshold strings of the form
#   [mean|saturation][*|+]<value>
# where value is a numeric value.

procedure set_bldval (ccd, sth, bthresh, bth)

pointer	ccd			#I CCD structure
real	sth			#I Saturation value
char	bthresh[ARB]		#I Threshold for candidate bleed pixels
real	bth			#I Bleed threshold (ADU)

int	i, l, nc, nl, nmean
real	mean, asumr()
pointer	sp, str, in, imgl2r()
int	nowhite(), strncmp(), ctor()
errchk	imgl2r

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Parse the bleed threshold string.
	bth = INDEFR
	i = nowhite (bthresh, Memc[str], SZ_LINE)
	call strlwr (Memc[str])
	if (strncmp (Memc[str], "mean", 4) == 0) {
	    i = 6
	    l = ctor (Memc[str], i, bth)
	    if (l == 0 || (Memc[str+4] != '*' && Memc[str+4] != '+'))
		call error (2, "Syntax error in bleed threshold")
	     
	    # Compute sample mean.
	    in = IN_IM(ccd)
	    nc = IN_C2(ccd) - IN_C1(ccd) + 1
	    nl = IN_L2(ccd) - IN_L1(ccd) + 1
	    mean = 0.
	    nmean = 0
	    i = (nl - 2 * BLDTRIM) / BLDSAMPLE
	    do l = IN_L1(ccd)+BLDTRIM, IN_L2(ccd)-BLDTRIM, i {
		mean = mean + asumr (Memr[imgl2r(in,l)+BLDTRIM],
		    nc-2*BLDTRIM) / (nc - 2*BLDTRIM)
		nmean = nmean + 1
	    }
	    mean = mean / nmean
	    if (Memc[str+4] == '+')
		bth = bth + mean
	    else if (Memc[str+4] == '*')
		bth = bth * mean

	} else if (strncmp (Memc[str], "saturation", 10) == 0) {
	    i = 12
	    l = ctor (Memc[str], i, bth)
	    if (l == 0 || (Memc[str+10] != '/' && Memc[str+10] != '-'))
		call error (2, "Syntax error in bleed threshold")
	    if (IS_INDEFR(sth))
		bth = sth
	    else if (Memc[str+10] == '-')
		bth = sth - bth
	    else if (Memc[str+10] == '/')
		bth = sth / bth
	} else {
	    i = 1
	    if (ctor (Memc[str], i, bth) == 0)
		call error (2, "Syntax error in bleed threshold")
	}
	call sfree (sp)
end
