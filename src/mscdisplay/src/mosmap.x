include <error.h>
include <syserr.h>
include <imhdr.h>
include <imset.h>
include	<ctype.h>
include "mosim.h"
include "mosgeom.h"
include "mosproc.h"


pointer procedure mimap (image, acmode, hdr_arg) 

char    image[ARB]	#I image specification
int     acmode		#I image access mode
int     hdr_arg		#I length of user fields, or structure pointer

pointer	mi		#O Structure pointer

int	rootlen, mef, proc, dobias, dozero, doflat, ninput, err, imext
pointer	sp, im, mg, template, imname, zname, caldir, filter, inlist

bool	clgetb()
pointer	immap(), mg_open(), mg_compgeom(), imtopenp()
int	xt_extns(), imtopen, imtlen(), imtgetim()
int	errget(), btoi(), strlen()

char	errmsg[SZ_LINE]

errchk	mg_open, mg_compgeom, imextensions, imtgetim, miunmap

begin
	call smark (sp)
	call salloc (template, SZ_LINE, TY_CHAR)
	call salloc (imname,   SZ_LINE, TY_CHAR)
	call salloc (zname,    SZ_LINE, TY_CHAR)
	call salloc (caldir,   SZ_LINE, TY_CHAR)
	call salloc (filter,   SZ_LINE, TY_CHAR)

	# Allocate space for mosim structure
	call malloc (mi,           LEN_MOSIM, TY_STRUCT)
	call malloc (MI_RNAME(mi), SZ_LINE,    TY_CHAR)
	MI_NIMS(mi) = 0
	MI_IMS(mi)  = NULL
	MI_MGS(mi)  = NULL
	MI_SB(mi)   = NULL

	# Save rootname in structure
	rootlen = strlen (image)
	call strcpy (image, Memc[MI_RNAME(mi)], SZ_LINE)


	switch (acmode) {

	# Open for input
	case READ_ONLY, READ_WRITE:

	    # First try and open a FITS multiextension image and if no 
	    # extensions are found then open a template.

	    call clgstr ("mimpars.extname", Memc[template], SZ_LINE)
	    inlist = xt_extns (image, "IMAGE", "0-", Memc[template], "",
		NO, YES, NO, NO, "", NO, imext)
	    ninput = imtlen (inlist)
	    if (ninput == 0) {
		call imtclose (inlist)

		# Now try to open as a multi-image mosaic
		call clgstr ("mimpars.exttmplt", Memc[imname], SZ_LINE)
		call sprintf (Memc[template], SZ_LINE, "%s%s")
		    call pargstr (image)
		    call pargstr (Memc[imname])

		call printf("file template: %s\n")
		    call pargstr(Memc[template])

		# Get list of input images
		inlist = imtopen (Memc[template])

		mef = NO
	    } else
		mef = YES

	    # No images match template - Complain.
	    ninput = imtlen (inlist)
	    if (ninput == 0) {
		call sfree (sp)
		call miunmap (mi)
		call sprintf (errmsg, SZ_LINE, "Cannot open mosaic (%s)")
		    call pargstr (image)
		call error (0, errmsg)
	    }

	    # Process?
	    proc = btoi (clgetb ("mimpars.process"))
	    if (proc == YES) {
		dobias = btoi (clgetb ("mimpars.overscan"))
		dozero = btoi (clgetb ("mimpars.zerosub"))
		doflat = btoi (clgetb ("mimpars.flatfield"))
		if (dozero == YES) {
		    im = imtopenp ("zero")
		    if (imtlen (im) > 1)
			call error (1, "Only one zero may be specified")
		    if (imtgetim (im, Memc[zname], SZ_LINE) == EOF)
		        dozero = NO
		    call imtclose (im)
		}
		if (dobias == NO && dozero == NO && doflat == NO)
		    proc = NO
	    }
	    if (proc == YES) {
		call clgstr ("mimpars.caldir", Memc[caldir], SZ_LINE)
		call clgstr ("mimpars.filter", Memc[filter], SZ_LINE)
	    }

	    call malloc (MI_IMS(mi), ninput, TY_INT)
	    call malloc (MI_MGS(mi), ninput, TY_INT)

	    # Map input images and fill out mosgeom structuress
	    ninput = 0
	    while (imtgetim (inlist, Memc[imname], SZ_FNAME) != EOF) {
		iferr (mg = mg_open (Memc[imname], rootlen, acmode, hdr_arg,
		    mef, proc, dobias, dozero, doflat, Memc[zname],
		    Memc[caldir], Memc[filter], ninput, im)) {
		    call sfree (sp)
		    call miunmap (mi)
		    call erract (EA_ERROR)
		}
		ninput = ninput + 1
		MI_IM(mi, ninput) = im
		MI_MG(mi, ninput) = mg

	    }
	    call imtclose (inlist)

	    # No images mapped succesfully - Complain.
	    if (ninput == 0) {
		call sfree (sp)
		call miunmap (mi)
		call sprintf (errmsg, SZ_LINE, "Cannot open mosaic (%s)")
		    call pargstr (image)
		call error (0, errmsg)
	    }

	    # Realocate arrays. One extra mosgeom struct is allocated
	    # to hold the composite geometry for the entire mosaic.
	    MI_NIMS(mi) = ninput
	    call realloc (MI_IMS(mi), ninput,   TY_INT)
	    call realloc (MI_MGS(mi), ninput+1, TY_INT)

	    # Generate mosgeom structure for tiled mosaic and tack it on to
	    # the end of the array of structures.
	    MI_MG(mi, ninput+1) = mg_compgeom (mi)

	case NEW_COPY:
	    iferr (im = immap (image, acmode, MI_IM(hdr_arg, 1))) {
		err = errget (errmsg, SZ_LINE)
		call miunmap (mi)
		call error (err, errmsg)
	    }

	    mg = mg_compgeom (hdr_arg)
	    MI_NIMS(mi) = 1
	    call malloc (MI_IMS(mi), 1, TY_INT)
	    call malloc (MI_MGS(mi), 2, TY_INT)
	    MI_IM(mi, 1) = im
	    MI_MG(mi, 1) = NULL
	    MI_MG(mi, 2) = mg

	    # Reset image dimensions to match that of input mosaic image
	    IM_LEN (im, 1) = NX(mg)
	    IM_LEN (im, 2) = NY(mg)

	default:
	    iferr (im = immap (image, acmode, hdr_arg)) {
		err = errget (errmsg, SZ_LINE)
		call miunmap (mi)
		call error (err, errmsg)
	    }
	    MI_NIMS(mi) = 1
	    call malloc (MI_IMS(mi), 1, TY_INT)
	    MI_IM(mi, 1) = im
	}

	return (mi)

end

procedure miunmap (mi)

pointer	mi		#I Structure pointer

int	i

begin

	if (mi != NULL) {

	    # Free rootname
	    call mfree (MI_RNAME(mi), TY_CHAR)

	    # Unmap images
	    if (MI_IMS(mi) != NULL) {
		do i = 1, MI_NIMS(mi) {
		    call imunmap (MI_IM(mi, i))
		}
		call mfree (MI_IMS(mi), TY_INT)
	    }

	    # Free mosgeom structures (including the composite)
	    if (MI_MGS(mi) != NULL) {
		do i = 1, MI_NIMS(mi) + 1 {
		    call mg_free (MI_MG(mi, i))
		}

		call mfree (MI_MGS(mi), TY_INT)

	    }

	    # Free Section buffer
	    if (MI_SB(mi) != NULL) {
		if (SB_DATA(MI_SB(mi)) != NULL)
		    call mfree (SB_DATA(MI_SB(mi)), SB_PIXTYPE (MI_SB(mi)))
		call mfree (MI_SB(mi), TY_INT)
	    }

	    call mfree (mi, TY_STRUCT)
	}
end


# MG_OPEN -- Create and fill out mosgeom structure for image

pointer procedure mg_open (imname, rootlen, acmode, hdr_arg, mef, proc, dobias,
	dozero, doflat, zero, caldir, filter, nim,  im)

char	imname[ARB]	#I Image name
int	rootlen		#I Length of root part of image name
int	acmode		#I Access mode
pointer	hdr_arg		#I Header argument
int	mef		#I MEF?
int	proc		#I Process?
int	dobias		#I Do bias subtraction?
int	dozero		#I Do zero subtraction?
int	doflat		#I Do flat field division?
char	zero[ARB]	#I Zero image
char	caldir[ARB]	#I Calibration directory
char	filter[ARB]	#I Filter
int	nim		#I Number of extensions previously read
pointer	im		#O IMIO pointer for open image.

pointer	mg		#O Pointer to completed mosgeom structure.

char	ch
pointer	sp, section, tsection, filt, str, key, cdir, dcim
int	i, x1, x2, xs, y1, y2, ys, amp, len, fd
real	rval
double	wref[4], lref[4]

bool	hdmflag(), streq()
int	strdic(), strlen(), open(), fscan(), nscan(), stridxs()
real	hdmgetr(), imgetr()
pointer	immap(), im_pmmap()
errchk	immap, im_pmmap, yt_match, hdmgetr, open, mkdetsec

include "ampinfo.com"

begin
	call smark (sp)
	call salloc (section, SZ_LINE, TY_CHAR)
	call salloc (tsection, SZ_LINE, TY_CHAR)
	call salloc (filt, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (cdir, SZ_LINE, TY_CHAR)

	# Map image.  Return on error.
	im = immap (imname, acmode, hdr_arg)

	# Allocate space for mosgeom structure and initialize.
	call calloc (mg, LEN_MOSGEOM, TY_STRUCT)
	MG_IM(mg) = im
	CKNODATA(mg) = NO
	NODATA(mg) = NO

	# Set pixel type for access.
	if (IM_PIXTYPE(im) == TY_USHORT && mef == YES)
	    MG_USHORT(mg) = YES
	else
	    MG_USHORT(mg) = NO

	# CCD name.
	call hdmgstr (im, "ccdname", Memc[section], SZ_LINE)
	if (Memc[section] != EOS) {
	    len = strlen (Memc[section])
	    call malloc (CCDNAME(mg), len , TY_CHAR)
	    call strcpy (Memc[section], Memc[CCDNAME(mg)], len)
	} else
	    CCDNAME(mg) = NULL
	    
	# Get amplifier id.
	call mg_ampid (im, Memc[section], SZ_LINE)
	if (Memc[section] != EOS) {
	    len = strlen (Memc[section])
	    call malloc (AMPID(mg), len , TY_CHAR)
	    call strcpy (Memc[section], Memc[AMPID(mg)], len)
	} else
	    AMPID(mg) = NULL

	# Set index into amplist if we have one.
	amp = 0
	if (namps != 0)
	    amp = strdic (Memc[section], Memc[section], SZ_LINE, Memc[amplist])

	# Get input image dimensions.
	NX(mg) = IM_LEN(im, 1)
	NY(mg) = IM_LEN(im, 2)

	# Allocate and initialise the Index vector
	call calloc (IMIDX(mg), IM_MAXDIM, TY_LONG)

	# Check for header keywords and set defaults as necessary.
	call mos_header (mg, Memc[key], Memc[section])

	# Set data section (DATASEC keyword)
	Memc[section] = EOS
	call hdmgstr (im, "datasec", Memc[section], SZ_LINE)
	x1 = 1
	x2 = NX(mg)
	xs = 1
	y1 = 1
	y2 = NY(mg)
	ys = 1
	call ccd_section (Memc[section], x1, x2, xs, y1, y2, ys)
	DX1(mg) = x1
	DX2(mg) = x2
	DY1(mg) = y1
	DY2(mg) = y2

	# Set detector section (DETSEC keyword)
	call hdmgstr (im, "detsec", Memc[section], SZ_LINE)
	if (Memc[section] == EOS) {
	    ifnoerr (rval = hdmgetr (im, "crval1"))
		call mkdetsec (mg, nim+1, wref, lref, Memc[section], SZ_LINE)
	    else
		call hdmgstr (im, "ccdsec", Memc[section], SZ_LINE)
	}
	x1 = DX1(mg)
	x2 = DX2(mg)
	xs = 1
	y1 = DY1(mg)
	y2 = DY2(mg)
	ys = 1
	call ccd_section (Memc[section], x1, x2, xs, y1, y2, ys)
	CX1(mg) = x1
	CX2(mg) = x2
	CY1(mg) = y1
	CY2(mg) = y2

	# Set pixel summing factors.
	x1 = CX2(mg) - CX1(mg)
	x2 = DX2(mg) - DX1(mg)
	y1 = CY2(mg) - CY1(mg)
	y2 = DY2(mg) - DY1(mg)
	if (x1 < 0)
	    x1 = x1 - 1
	else
	    x1 = x1 + 1
	if (x2 < 0)
	    x2 = x2 - 1
	else
	    x2 = x2 + 1
	if (y1 < 0)
	    y1 = y1 - 1
	else
	    y1 = y1 + 1
	if (y2 < 0)
	    y2 = y2 - 1
	else
	    y2 = y2 + 1
	DX(mg) = nint (real (x1) / real (x2))
	if (DX(mg) > 0)
	    DX(mg) = max (1, DX(mg))
	else
	    DX(mg) = min (-1, DX(mg))
	DY(mg) = nint (real (y1) / real (y2))
	if (DY(mg) > 0)
	    DY(mg) = max (1, DY(mg))
	else
	    DY(mg) = min (-1, DY(mg))

	# Set TRIM section (TRIMSEC keyword)
	call hdmgstr (im, "trimsec", Memc[section], SZ_LINE)
	x1 = DX1(mg)
	x2 = DX2(mg)
	xs = 1
	y1 = DY1(mg)
	y2 = DY2(mg)
	ys = 1
	call ccd_section (Memc[section], x1, x2, xs, y1, y2, ys)
	TX1(mg) = x1
	TX2(mg) = x2
	TY1(mg) = y1
	TY2(mg) = y2

	CX1(mg) = CX1(mg) + (TX1(mg) - DX1(mg)) * DX(mg)
	CX2(mg) = CX2(mg) + (TX2(mg) - DX2(mg)) * DX(mg)
	CY1(mg) = CY1(mg) + (TY1(mg) - DY1(mg)) * DY(mg)
	CY2(mg) = CY2(mg) + (TY2(mg) - DY2(mg)) * DY(mg)
	DX1(mg) = TX1(mg)
	DX2(mg) = TX2(mg)
	DY1(mg) = TY1(mg)
	DY2(mg) = TY2(mg)

	# Set BIAS section (BIASSEC keyword)
	call hdmgstr (im, "biassec", Memc[section], SZ_LINE)
	x1 = 0
	x2 = 0
	xs = 1
	y1 = 0
	y2 = 0
	ys = 1
	call ccd_section (Memc[section], x1, x2, xs, y1, y2, ys)
	BX1(mg) = x1
	BX2(mg) = x2
	BY1(mg) = y1
	BY2(mg) = y2

	# Flip if needed.
	if (CX1(mg) > CX2(mg)) {
	    x1 = CX1(mg)
	    CX1(mg) = CX2(mg)
	    CX2(mg) = x1
	    x1 = DX1(mg)
	    DX1(mg) = DX2(mg)
	    DX2(mg) = x1
	    x1 = TX1(mg)
	    TX1(mg) = TX2(mg)
	    TX2(mg) = x1
	    x1 = BX1(mg)
	    BX1(mg) = BX2(mg)
	    BX2(mg) = x1
	}
	if (CY1(mg) > CY2(mg)) {
	    y1 = CY1(mg)
	    CY1(mg) = CY2(mg)
	    CY2(mg) = y1
	    y1 = DY1(mg)
	    DY1(mg) = DY2(mg)
	    DY2(mg) = y1
	    y1 = TY1(mg)
	    TY1(mg) = TY2(mg)
	    TY2(mg) = y1
	    y1 = BY1(mg)
	    BY1(mg) = BY2(mg)
	    BY2(mg) = y1
	}

	# Flip image if needed.
	if (DX1(mg) > DX2(mg) || DY1(mg) > DY2(mg)) {
	    if (DX1(mg) < DX2(mg))
		call sprintf (Memc[tsection], SZ_LINE, "[*,-*]")
	    else if (DY1(mg) < DY2(mg))
		call sprintf (Memc[tsection], SZ_LINE, "[-*,*]")
	    else 
		call sprintf (Memc[tsection], SZ_LINE, "[-*,-*]")
	    call sprintf (Memc[section], SZ_LINE, "%s%s")
		call pargstr (imname)
		call pargstr (Memc[tsection])
	    call imunmap (im)
	    im = immap (Memc[section], acmode, hdr_arg)
	    MG_IM(mg) = im
	    if (DX1(mg) > DX2(mg)) {
		DX(mg) = -DX(mg)
		DX1(mg) = IM_LEN(im,1) - DX1(mg) + 1
		DX2(mg) = IM_LEN(im,1) - DX2(mg) + 1
		BX1(mg) = IM_LEN(im,1) - BX1(mg) + 1
		BX2(mg) = IM_LEN(im,1) - BX2(mg) + 1
	    }
	    if (DY1(mg) > DY2(mg)) {
		DY(mg) = -DY(mg)
		DY1(mg) = IM_LEN(im,2) - DY1(mg) + 1
		DY2(mg) = IM_LEN(im,2) - DY2(mg) + 1
		BY1(mg) = IM_LEN(im,2) - BY1(mg) + 1
		BY2(mg) = IM_LEN(im,2) - BY2(mg) + 1
	    }
	} else
	    Memc[tsection] = EOS

	# Setup OTF calibration processing.
	PROC(mg) = proc
	DOBIAS(mg) = dobias
	DOZERO(mg) = dozero
	DOFLAT(mg) = doflat
	DZIM(mg) = NULL
	DFIM(mg) = NULL
	if (PROC(mg) == YES) {
	    if (hdmflag (im, "overscan")) 
		DOBIAS(mg) = NO
	    if (hdmflag (im, "zerocor")) 
		DOZERO(mg) = NO
	    if (hdmflag (im, "flatcor")) 
		DOFLAT(mg) = NO
	    if (DOBIAS(mg) == NO && DOZERO(mg) == NO && DOFLAT(mg) == NO)
		PROC(mg) = NO
	}
	if (PROC(mg) == YES) {
	    if (DOZERO(mg) == YES) {
		# Set zero calibration.
		if (zero[1] == '!') {
		    iferr (call imgstr (im, zero[2], ZERONAME(mg),
		        LEN_ZERONAME)) {
		       call eprintf ("Can't find keyword %s (%s)\n")
			   call pargstr (zero[2])
			   call pargstr (imname)
			ZERONAME(mg) = EOS
		    } else {
			call sprintf (Memc[section], SZ_LINE, "%s%s")
			    call pargstr (ZERONAME(mg))
			    call pargstr (Memc[tsection])
		    }
		} else {
		    call strcpy (zero, ZERONAME(mg), LEN_ZERONAME)
		    call sprintf (Memc[section], SZ_LINE, "%s%s%s")
			call pargstr (ZERONAME(mg))
			call pargstr (imname[rootlen+1])
			call pargstr (Memc[tsection])
		}
		if (ZERONAME(mg) != EOS) {
		    i = stridxs ("[", Memc[section])
		    if (i > 0)
			call strcpy (Memc[section], ZERONAME(mg), i-1)
		    iferr (DZIM(mg) = immap (Memc[section], READ_ONLY, NULL)) {
			DZIM(mg) = NULL
			DOZERO(mg) = NO
			if (nim == 0)
			    call eprintf ("No zero calibration performed.\n")
		    }
		} else {
		    DZIM(mg) = NULL
		    DOZERO(mg) = NO
		    if (nim == 0)
			call eprintf ("No zero calibration performed.\n")
		}
	    }

	    if (DOFLAT(mg) == YES) {
		# Get full filter name.
		if (filter[1] == '!')
		    call hdmgstr (im, filter[2], Memc[filt], SZ_LINE)
		else
		    call strcpy (filter, Memc[filt], SZ_LINE)

		# Translate to directory identification.
		call strcpy (Memc[filt], Memc[str], SZ_LINE)
		for (i=0;;i=i+1) {
		    ch = Memc[str+i]
		    if (ch == EOS || IS_WHITE(ch))
			break
		    else if (!(IS_ALNUM(ch)||ch=='.'))
			Memc[str+i] = '_'
		}
		Memc[str+i] = EOS

		# Look for translation menu.
		iferr {
		    if (caldir[1] == '!')
			call hdmgstr (im, caldir[2], Memc[cdir], SZ_LINE)
		    else
			call strcpy (caldir, Memc[cdir], SZ_LINE)

		    call sprintf (Memc[section], SZ_LINE, "%scal.men")
			call pargstr (Memc[cdir])
		    fd = open (Memc[section], READ_ONLY, TEXT_FILE)
		    while (fscan(fd) != EOF) {
			call gargwrd (Memc[section], SZ_LINE)
			call gargwrd (Memc[key], SZ_LINE)
			if (nscan() != 2)
			    next
			if (streq (Memc[section], Memc[filt]) ||
			    streq (Memc[key], Memc[filt])) {
			    call strcpy (Memc[key], Memc[str], SZ_LINE)
			    break
			}
		    }
		    call close (fd)
		} then
		    #call erract (EA_WARN)
		    ;

		# Find calibration data.
		iferr {
		    call sprintf (Memc[section], SZ_LINE, "%s%s/flat%s.pl")
			call pargstr (Memc[cdir])
			call pargstr (Memc[str])
			call pargstr (Memc[AMPID(mg)])
		    iferr (dcim = im_pmmap (Memc[section], READ_ONLY, NULL)) {
			if (nim == 0) {
			    call eprintf (
				"Calibration for filter %s not found (%s%s).\n")
				call pargstr (Memc[filt])
				call pargstr (Memc[cdir])
				call pargstr (Memc[str])
			}
			call strcpy ("default", Memc[str], SZ_LINE)
			call sprintf (Memc[section], SZ_LINE, "%s%s/flat%s.pl")
			    call pargstr (Memc[cdir])
			    call pargstr (Memc[str])
			    call pargstr (Memc[AMPID(mg)])
			iferr (dcim = im_pmmap (Memc[section],READ_ONLY,NULL)) {
			    if (nim == 0)
				call eprintf (
				    "Default calibration not found (%s%s).\n")
				    call pargstr (Memc[cdir])
				    call pargstr (Memc[str])
			    call erract (EA_ERROR)
			}
			if (nim == 0)
			    call eprintf ("Using default calibration.\n")
		    }
		    DFIM(mg) = dcim
		    iferr (CCDMEAN(mg) = hdmgetr (DFIM(mg), "ccdmean")) {
			if (nim == 0)
			    call eprintf ("CCDMEAN keyword not found.\n")
			call erract (EA_ERROR)
		    }

		    iferr (rval = imgetr (im, "LTM1_1")) {
			call imaddr (im, "LTM1_1", 1./DX(mg))
			call imaddr (im, "LTM2_2", 1./DY(mg))
			call imaddr (im, "LTV1", real (DX1(mg)-1))
			call imaddr (im, "LTV2", 0.)
		    }
		    iferr (call yt_match (DFIM(mg), im, "physical")) {
			if (nim == 0)
			    call eprintf (
			    "Cannot match calibration pixels to data pixels.\n")
			call erract (EA_ERROR)
		    }

		    call strcpy (Memc[str], FLATNAME(mg), LEN_FLATNAME)
		} then {
		    if (DFIM(mg) != NULL)
			call imunmap (DFIM(mg))
		    DFIM(mg) = NULL
		    if (nim == 0)
			call eprintf ("No flat field calibration performed.\n")
		}
		if (DFIM(mg) == NULL)
		    DOFLAT(mg) = NO
	    }
	    if (DOBIAS(mg)==NO && DOZERO(mg)==NO && DOFLAT(mg)==NO)
		PROC(mg) = NO
	}

	call sfree (sp)
	return (mg)

end


# MOS_HEADER -- Set header keywords required for display.
# If any of DETSEC, DATASEC, CRVAL1 are present assume there is a populated
# header; i.e. not real-time readout.  Otherwise use the translation file for
# real-time readout defaults.

procedure mos_header (mg, key, section)

pointer	mg			#I MG structure.
char	key[SZ_LINE]		#I Working string
char	section[SZ_LINE]	#I Working string

int	i, j, nx, ny, nx1, ctoi(), strlen()
pointer	im

begin
	im = MG_IM(mg)

	# Check for keywords that would indicate there is a header.
	call hdmgstr (im, "detsec", section, SZ_LINE)
	if (section[1] != EOS)
	    return
	call hdmgstr (im, "datasec", section, SZ_LINE)
	if (section[1] != EOS)
	    return
	call hdmgstr (im, "crval1", section, SZ_LINE)
	if (section[1] != EOS)
	    return

	# In order to get defaults from translation file we require AMPID.
	if (AMPID(mg) == NULL)
	    return

	# Set DETSEC.
	call sprintf (key, SZ_LINE, "det%.5s")
	    call pargstr (Memc[AMPID(mg)])
	call hdmgstr (im, key, section, SZ_LINE)
	call hdmpstr (im, "detsec", section)

	# Set DATASEC.
	call sprintf (key, SZ_LINE, "dat%.5s")
	    call pargstr (Memc[AMPID(mg)])
	call hdmgstr (im, key, section, SZ_LINE)
	DX1(mg) = 1
	DX2(mg) = NX(mg)
	i = 1
	DY1(mg) = 1
	DY2(mg) = NY(mg)
	j = 1
	call ccd_section (section, DX1(mg), DX2(mg), i,
	    DY1(mg), DY2(mg), j)

	# Set BIASSEC.  New format is a section and old format is number
	# of pixels to left or right of data section.
	call sprintf (key, SZ_LINE, "bias%.5s")
	    call pargstr (Memc[AMPID(mg)])
	call hdmgstr (im, key, section, SZ_LINE)
	BX1(mg) = 1
	BX2(mg) = NX(mg)
	i = 1
	BY1(mg) = 1
	BY2(mg) = NY(mg)
	j = 1
	if (section[1] == '[') {
	    call ccd_section (section, BX1(mg), BX2(mg), i,
		BY1(mg), BY2(mg), j)
	    nx = INDEFI
	} else {
	    i = 1
	    j = ctoi (section, i, nx)
	    if (nx < 0) {
		BX1(mg) = DX1(mg) + nx
		BX2(mg) = DX1(mg) - 1
		nx = DX2(mg)
	    } else {
		BX1(mg) = DX2(mg) + 1
		BX2(mg) = DX2(mg) + nx
		nx = BX2(mg)
	    }
	    ny = DY2(mg)
	}

	# Adjust for binning based on unbinned size.
	if (IS_INDEFI(nx)) {
	    call sprintf (key, SZ_LINE, "size%.5s")
		call pargstr (Memc[AMPID(mg)])
	    call hdmgstr (im, key, section, SZ_LINE)
	    nx = NX(mg)
	    ny = NY(mg)
	    if (section[1] != EOS) {
		j = ctoi (section, i, nx)
		j = ctoi (section, i, ny)
	    }
	}
	nx1 = BX2(mg) - BX1(mg) + 1
	DX(mg) = nint (real(nx - nx1) / (NX(mg) - nx1))
	nx1 = (DX2(mg) - DX1(mg) + 1) / DX(mg)
	nx = nx - NX(mg)
	ny = ny - NY(mg)

	if (BX1(mg) > DX2(mg)) {
	    DX2(mg) = min (NX(mg), DX2(mg) - nx)
	    DX1(mg) = max (1, DX2(mg) - nx1 + 1)
	    DX2(mg) = min (NX(mg), DX1(mg) + nx1 - 1)
	    BX1(mg) = max (DX2(mg) + 1, BX1(mg) - nx)
	    BX2(mg) = max (BX1(mg) + 1, BX2(mg) - nx)
	} else {
	    DX2(mg) = min (NX(mg), DX1(mg) + nx1 - 1)
	}
	BY2(mg) = min (NY(mg), BY2(mg) - ny)
	DY2(mg) = min (NY(mg), DY2(mg) - ny)

	# Record DATASEC and BIASSEC.
	call sprintf (section, SZ_LINE, "[%d:%d,%d:%d]")
	    call pargi (DX1(mg))
	    call pargi (DX2(mg))
	    call pargi (DY1(mg))
	    call pargi (DY2(mg))
	call hdmpstr (im, "datasec", section)

	call sprintf (section, SZ_LINE, "[%d:%d,%d:%d]")
	    call pargi (BX1(mg))
	    call pargi (BX2(mg))
	    call pargi (BY1(mg))
	    call pargi (BY2(mg))
	call hdmpstr (im, "biassec", section)

	# Set CCDNAME.
	if (CCDNAME(mg) == NULL) {
	    call sprintf (key, SZ_LINE, "ccd%.5s")
		call pargstr (Memc[AMPID(mg)])
	    call hdmgstr (im, key, section, SZ_LINE)
	    if (section[1] != EOS) {
		i = strlen (section)
		call malloc (CCDNAME(mg), i, TY_CHAR)
		call strcpy (section, Memc[CCDNAME(mg)], i)
	    }
	}

	# Set OTF directory.
	call hdmgstr (im, "otfdir", section, SZ_LINE)
	if (section[1] != EOS)
	    call hdmpstr (im, "otfdir", section)
end


# MOS_COMPGEOM -- Calculate mosgeom structure for composite of input images.

pointer procedure mg_compgeom (mi)

pointer	mi		#I Pointer to mosim structure for input images.

pointer	mgout		#O Pointer to composite mosgeom structure.

pointer	mgin1, mgin2, ptr
int	i, j, k, ninput, xgap, ygap, gap, x1, x2, y1, y2, clgeti(), mg_sort()
bool	streq()
extern	mg_sort

#include "mosproc.com"

define	xadjust_	10
define	yadjust_	20

begin
	# Return null pointer if input structure is null.
	if (mi == NULL)
	    return (NULL)

	ninput = MI_NIMS(mi)

	call qsort (MI_MG(mi,1), ninput, mg_sort)
	do i = 1, ninput
	    MI_IM(mi,i) = MG_IM(MI_MG(mi,i))

	# Add gaps if requested.
	xgap = clgeti ("mimpars.xgap")
	if (xgap > 0) {
xadjust_    do i = 1, ninput-1 {
		mgin1 = MI_MG(mi,i)
		do j = i+1, ninput {
		    mgin2 = MI_MG(mi,j)
		    if (CX1(mgin2) < CX2(mgin1)) {
			ptr = mgin1
			mgin1 = mgin2
			mgin2 = ptr
		    }
		    x2 = CX2(mgin1)
		    gap = CX1(mgin2) - x2
		    if (gap < 1 || gap >= xgap)
			next
		    if (CCDNAME(mgin1)!=NULL && CCDNAME(mgin2)!=NULL)
			if (streq (Memc[CCDNAME(mgin1)],
			    Memc[CCDNAME(mgin2)]))
			    next
		    if (CY1(mgin2) > CY2(mgin1) || CY2(mgin2) < CY1(mgin1))
			next
		    gap = xgap - gap
		    do k = 1, ninput {
			mgin2 = MI_MG(mi,k)
			if (CX1(mgin2) <= x2)
			    next
			if (CCDNAME(mgin1)!=NULL && CCDNAME(mgin2)!=NULL)
			    if (streq (Memc[CCDNAME(mgin1)],
				Memc[CCDNAME(mgin2)]))
				next
			CX1(mgin2) = CX1(mgin2) + gap
			CX2(mgin2) = CX2(mgin2) + gap
		    }
		    goto xadjust_
		}
	    }
	}
	ygap = clgeti ("mimpars.ygap")
	if (ygap > 0) {
yadjust_    do i = 1, ninput-1 {
		mgin1 = MI_MG(mi,i)
		do j = i+1, ninput {
		    mgin2 = MI_MG(mi,j)
		    if (CY1(mgin2) < CY2(mgin1)) {
			ptr = mgin1
			mgin1 = mgin2
			mgin2 = ptr
		    }
		    y2 = CY2(mgin1)
		    gap = CY1(mgin2) - y2
		    if (gap < 1 || gap >= ygap)
			next
		    if (CCDNAME(mgin1)!=NULL && CCDNAME(mgin2)!=NULL)
			if (streq (Memc[CCDNAME(mgin1)],
			    Memc[CCDNAME(mgin2)]))
			    next
		    if (CX1(mgin2) > CX2(mgin1) || CX2(mgin2) < CX1(mgin1))
			next
		    gap = ygap - gap
		    do k = 1, ninput {
			mgin2 = MI_MG(mi,k)
			if (CY1(mgin2) <= y2)
			    next
			if (CCDNAME(mgin1)!=NULL && CCDNAME(mgin2)!=NULL)
			    if (streq (Memc[CCDNAME(mgin1)],
				Memc[CCDNAME(mgin2)]))
				next
			CY1(mgin2) = CY1(mgin2) + gap
			CY2(mgin2) = CY2(mgin2) + gap
		    }
		    goto yadjust_
		}
	    }
	}

	# Allocate space for mosgeom structure
	call calloc (mgout, LEN_MOSGEOM, TY_STRUCT)

	# Allocate and initialise the Index vector
	call calloc (IMIDX(mgout), IM_MAXDIM, TY_LONG)

	# Determine dimensions of composite image
	# Retain the rectangle which contains the union of the CCD sections

	mgin1 = MI_MG(mi, 1)
	x1 = CX1(mgin1)
	x2 = CX2(mgin1)
	y1 = CY1(mgin1)
	y2 = CY2(mgin1)

	do i = 2, ninput {
	    mgin1 = MI_MG(mi, i)
	    x1 = min (x1, CX1(mgin1))
	    x2 = max (x2, CX2(mgin1))
	    y1 = min (y1, CY1(mgin1))
	    y2 = max (y2, CY2(mgin1))
	}

	NX(mgout)  = x2 - x1 + 1
	NY(mgout)  = y2 - y1 + 1

	CX1(mgout) = x1
	CX2(mgout) = x2
	CY1(mgout) = y1
	CY2(mgout) = y2

	DX1(mgout) = 1
	DX2(mgout) = NX(mgout)
	DY1(mgout) = 1
	DY2(mgout) = NY(mgout)

	TX1(mgout) = 1
	TX2(mgout) = NX(mgout)
	TY1(mgout) = 1
	TY2(mgout) = NY(mgout)

	BX1(mgout) = 0
	BX2(mgout) = 0
	BY1(mgout) = 0
	BY2(mgout) = 0

	CCDNAME(mgout) = NULL
	AMPID(mgout)  = NULL
	PROC(mgout) = NO
	DOBIAS(mgout) = NO
	DOZERO(mgout) = NO
	DOFLAT(mgout) = NO
	DZIM(mgout) = NULL
	DFIM(mgout) = NULL
	BIAS(mgout)   = 0.0
	OVRSCN(mgout) = NULL
	GAIN(mgout)   = 1.0
	DARK(mgout)   = 0.0
	CKNODATA(mgout) = NO
	NODATA(mgout) = NO


	return (mgout)
end


# MG_SORT -- Sort elements.

int procedure mg_sort (mi1, mi2)

pointer	mi1, mi2

begin
	if (CY1(mi1) < CY1(mi2))
	    return (-1)
	else if (CY1(mi1) > CY1(mi2))
	    return (1)
	else if (CX1(mi1) < CX1(mi2))
	    return (-1)
	else if (CX1(mi1) > CX1(mi2))
	    return (1)
	else
	    return (0)
end


# MG_FREE -- Free associated mosgeom structure.
procedure mg_free (mg)

pointer	mg		#I Pointer to mosgeom structure for image

begin
	if (mg != NULL) {

	    if (DZIM(mg) != NULL)
		call imunmap (DZIM(mg))

	    if (DFIM(mg) != NULL)
		call imunmap (DFIM(mg))

	    if (IMIDX(mg) != NULL)
		call mfree (IMIDX(mg), TY_LONG)

	    if (CCDNAME(mg) != NULL)
		call mfree (CCDNAME(mg), TY_CHAR)

	    if (AMPID(mg) != NULL)
		call mfree (AMPID(mg), TY_CHAR)

	    if (OVRSCN(mg) != NULL)
		call mfree (OVRSCN(mg), TY_REAL)

	    call mfree (mg, TY_STRUCT)
	}
end

# MG_DUMP -- Dump contents of mosgeom structure
procedure mg_dump(mg)

pointer	mg		# Mosgeom strudture to dump.

begin
	if (CCDNAME(mg) != NULL) {
	call eprintf ("CCDNAME=%s\n")
	    call pargstr (Memc[CCDNAME(mg)])
	} else {
	    call pargstr ("undefined")
	}
	if (AMPID(mg) != NULL) {
	call eprintf ("AMPID=%s\n")
	    call pargstr (Memc[AMPID(mg)])
	} else {
	    call pargstr ("undefined")
	}
	call eprintf ("NX=%d NY= %d\n")
	    call pargi (NX(mg))
	    call pargi (NY(mg))
	call eprintf ("IMIDX=%d: ")
	    call pargi (IMIDX(mg))
	if (IMIDX(mg) != NULL) {
	    call eprintf ("(%d %d %d %d %d %d %d)\n")
		call pargi (Meml(IMIDX(mg)))
		call pargi (Meml(IMIDX(mg)+1))
		call pargi (Meml(IMIDX(mg)+2))
		call pargi (Meml(IMIDX(mg)+3))
		call pargi (Meml(IMIDX(mg)+4))
		call pargi (Meml(IMIDX(mg)+5))
		call pargi (Meml(IMIDX(mg)+6))
	} else {
	    call eprintf ("undefined\n")
	}
	call eprintf (" CCDSEC: x1=%d \tx2=%d \ty1=%d \ty2=%d\n")
	    call pargi (CX1(mg))
	    call pargi (CX2(mg))
	    call pargi (CY1(mg))
	    call pargi (CY2(mg))
	call eprintf ("DATASEC: x1=%d \tx2=%d \ty1=%d \ty2=%d\n")
	    call pargi (DX1(mg))
	    call pargi (DX2(mg))
	    call pargi (DY1(mg))
	    call pargi (DY2(mg))
	call eprintf ("TRIMSEC: x1=%d \tx2=%d \ty1=%d \ty2=%d\n")
	    call pargi (TX1(mg))
	    call pargi (TX2(mg))
	    call pargi (TY1(mg))
	    call pargi (TY2(mg))
	call eprintf ("BIASSEC: x1=%d \tx2=%d \ty1=%d \ty2=%d\n")
	    call pargi (BX1(mg))
	    call pargi (BX2(mg))
	    call pargi (BY1(mg))
	    call pargi (BY2(mg))
	call eprintf ("Offset=%f Gain=%f Dark=%f -->Overscan=%d\n")
	    call pargr (BIAS(mg))
	    call pargr (GAIN(mg))
	    call pargr (DARK(mg))
	    call pargi (OVRSCN(mg))
end


# MG_AMPID -- Get AMPID for an image.

procedure mg_ampid (im, ampid, maxch)

pointer	im		#I IMIO pointer for image.
char	ampid[ARB]	#O amplifier id string
int	maxch		#I Maximum length of ampid.

begin
	call hdmgstr (im, "amp", ampid, maxch) 
	if (ampid[1] == EOS)
	    call hdmgstr (im, "imageid", ampid, maxch) 
	if (ampid[1] == EOS) {
	    call hdmgstr (im, "extname", ampid, maxch) 
	    if (ampid[1] == EOS)
		call mg_ampname (im, ampid, maxch)
	    else
		call hdmname (ampid, ampid, maxch) 
	}
end

# MG_AMPNAME -- Derive the amplifier name from the image name.
# The assumption is that the image name is of the form
#
#       rootname_ampname.imh

procedure mg_ampname (im, ampname, maxch)

pointer im	              	#I IMIO pointer for image.
char    ampname[maxch]		#O Amplifier name.
int    maxch                   #I Max chars in amplname.

int     brk, brkend
pointer sp, imname, imroot

int     stridxs()

begin

    call smark (sp)
    call salloc (imname, SZ_LINE, TY_CHAR)
    call salloc (imroot, SZ_LINE, TY_CHAR)

    call imstats (im, IM_IMAGENAME, Memc[imname], SZ_LINE)
    call xt_imroot (Memc[imname], Memc[imroot], SZ_LINE)
    brk = stridxs ("_", Memc[imroot])
    brkend = stridxs (".", Memc[imroot+brk])
    if (brk > 0) {

#	# Beware of filenames of the form ../*.imh and ./*.imh with no ampid !
#	if (strncmp (Memc[imroot+brk], "/", 1) == 0) {
#	    ampname[1] = EOS
#
#	} else {
	    Memc[imroot+brk+brkend-1] = EOS
	    call strcpy (Memc[imroot+brk], ampname, maxch)
#	}

    } else {
	ampname[1] = EOS
    }

    call sfree (sp)

end

# MG_ROOTNAME -- Derive the rootname from the image name.
# The assumption is that the image name is of the form
#
#       rootname_ampname.imh
#
# Our job is to strip off the .ampname and image type extension.
# If there is no extension then return the entire image name.


procedure mg_rootname (image, rootname, maxch)

char 	image[ARB]       	#I Image name.
char    rootname[maxch]		#O Rootname of image.
int    maxch                    #I Max chars in rootname.

int     brk
pointer sp, imroot

int     stridxs()

begin

    call smark (sp)
    call salloc (imroot, SZ_LINE, TY_CHAR)

    call xt_imroot (image, Memc[imroot], SZ_LINE)
    brk = stridxs ("_", Memc[imroot])
    if (brk > 0) {

#	# Beware of filenames of the form ../*.imh and ./*.imh with no ampid !
#	if (strncmp (Memc[imroot+brk], "/", 1) == 0) {
#	    call strcpy (Memc[imroot], rootname, maxch)
#
#	} else {
	    brk = min (brk-1, maxch)
	    call strcpy (Memc[imroot], rootname, brk)
#	}

    } else {
	call strcpy (Memc[imroot], rootname, maxch)
    }

    call sfree (sp)

end


# MG_C2IM -- Convert a mosaic coordinate to an image coordinate.

procedure mg_c2im (mi, x, y, im, xim, yim)

pointer	mi		#I MOSIM structure
real	x, y		#I Coordinate in composite geometry
pointer	im		#O Image pointer
real	xim, yim	#O Coordinate in image

int	i
real	cx1, cx2, cy1, cy2
pointer	cmg, mg

begin
	cmg = MI_CMG(mi)
	do i = 1, MI_NIMS(mi) {
	    mg = MI_MG(mi,i)
	    cx1 = CX1(mg) - 0.5
	    cx2 = CX2(mg) + 0.5
	    cy1 = CY1(mg) - 0.5
	    cy2 = CY2(mg) + 0.5
	    if (x < cx1 || x > cx2 || y < cy1 || y > cy2)
		next
	    im = MI_IM(mi,i)
	    xim = (x - CX1(mg) - (DX(mg) - 1) / 2.) / DX(mg) + DX1(mg)
	    yim = (y - CY1(mg) - (DY(mg) - 1) / 2.) / DY(mg) + DY1(mg)
	    return
	}

	call error (1, "Mosaic coordinate outside any image")
end


# MG_IM2C -- Convert an image coordinate to a mosaic coordinate.

procedure mg_im2c (im, xim, yim, mi, x, y)

pointer	im		#I Image pointer
real	xim, yim	#I Coordinate in image
pointer	mi		#I MOSIM structure
real	x, y		#0 Coordinate in composite geometry

int	i
pointer	mg

begin
	do i = 1, MI_NIMS(mi) {
	    if (im != MI_IM(mi,i))
		next
	    mg = MI_MG(mi,i)
	    x = (xim - DX1(mg)) * DX(mg) + CX1(mg) + (DX(mg) - 1) / 2.
	    y = (yim - DY1(mg)) * DY(mg) + CY1(mg) + (DY(mg) - 1) / 2.
	    return
	}

	call error (1, "Image is not in the mosaic")
end


# MKDETSEC -- Make a detector section from the WCS.
# This is relative to the image where nim is zero.

procedure mkdetsec (mg, nim, wref, lref, section, maxchar)

pointer	mg			#I Mosaic geometry structure
int	nim			#I Image number
double	wref[4]			#U Reference WCS coordinate
double	lref[4]			#U Reference logical coordinate
char	section[maxchar]	#O Detector section
int	maxchar			#I Maximum characters in detector section

double	l[4], c1, c2, l1, l2
pointer	mw, ct, mw_openim(), mw_sctran
errchk	mw_openim, mw_sctran, mw_ctrand

begin
	# Open WCS.
	mw = mw_openim (MG_IM(mg))

	# Use the tangent point of the first image as the reference.
	if (nim == 1) {
	    call mw_gwtermd (mw, lref, wref, l, 2)

	    ct = mw_sctran (mw, "world", "logical", 3)
	    call mw_c2trand (ct, wref[1], wref[2], lref[1], lref[2])

	    # Compute second reference coordinate for determining flips.
	    if (DX1(mg) < DX2(mg))
		lref[3] = lref[1] + 1
	    else
		lref[3] = lref[1] - 1
	    if (DY1(mg) < DY2(mg))
		lref[4] = lref[2] + 1
	    else
		lref[4] = lref[2] - 1
	    ct = mw_sctran (mw, "logical", "world", 3)
	    call mw_c2trand (ct, lref[3], lref[4], wref[3], wref[4])

	    call amovd (lref, l, 4)

	# Find offsets relative to the first image reference coordinates.
	} else {
	    ct = mw_sctran (mw, "world", "logical", 3)
	    call mw_c2trand (ct, wref[1], wref[2], l[1], l[2])
	    call mw_c2trand (ct, wref[3], wref[4], l[3], l[4])
	}

	# Compute logical detector coordinates.
	if ((lref[3]-lref[1])*(l[3]-l[1]) > 0.) {
	    c1 = DX1(mg) + (lref[1] - l[1])
	    c2 = DX2(mg) + (lref[1] - l[1])
	} else {
	    c1 = DX2(mg) + (lref[1] - (NX(mg) - l[1] + 1))
	    c2 = DX1(mg) + (lref[1] - (NX(mg) - l[1] + 1))
	}
	if ((lref[4]-lref[2])*(l[4]-l[2]) > 0.) {
	    l1 = DY1(mg) + (lref[2] - l[2])
	    l2 = DY2(mg) + (lref[2] - l[2])
	} else {
	    l1 = DY2(mg) + (lref[2] - (NY(mg) - l[2] + 1))
	    l2 = DY1(mg) + (lref[2] - (NY(mg) - l[2] + 1))
	}

	call mw_close (mw)

	# Format the detector section.
	call sprintf (section, maxchar, "[%d:%d,%d:%d]")
	    call pargi (nint(c1))
	    call pargi (nint(c2))
	    call pargi (nint(l1))
	    call pargi (nint(l2))
end
