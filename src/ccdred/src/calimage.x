include	<error.h>
include	<imset.h>
include	<imhdr.h>
include	"ccdtypes.h"
include	"calimage.h"


# CAL_IMAGE -- Return a calibration image for a specified input image.
# CAL_OPEN  -- Open the calibration image list.
# CAL_CLOSE -- Close the calibration image list.
# CAL_LIST  -- Add images to the calibration image list.
#
# The open procedure is called first to get the calibration image
# lists and add them to an internal list.  Calibration images from the
# input list are also added so that calibration images may be specified
# either from the calibration image list parameters or in the input image list.
# Existence errors and duplicate calibration images are ignored.
# Validity checks are made when the calibration images are requested.
#
# During processing the calibration image names are requested for each input
# image.  The calibration image list is searched for a calibration image of
# the right type, amplifier, and subset.  If more than one is found the first
# one is returned and a warning given for the others.  The warning is only
# issued once.  If no calibration image is found then an error is returned.
# 
# The calibration image list must be closed at the end of processing the
# input images.


# CAL_IMAGE -- Return a calibration image of a particular type.
# Search the calibration list for the first calibration image of the desired
# type, amplifier/ccd, and subset.  Print a warning if there  is more than one
# possible calibration image and return an error if there is no calibration
# image.

procedure cal_image (im, ccdtype, nscan, image, maxchars)

pointer	im		# Image to be processed
int	ccdtype		# Calibration CCD image type desired
int	nscan		# Number of scan rows desired
char	image[maxchars]	# Calibration image (returned)
int	maxchars	# Maximum number chars in image name

int	i, m, n
pointer	sp, amp, ccd, subset, str
bool	strne(), ccd_cmp(), ccdflag()
include	"calimage.com"

begin
	call smark (sp)
	call salloc (amp, SZ_FNAME, TY_CHAR)
	call salloc (ccd, SZ_FNAME, TY_CHAR)
	call salloc (subset, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Match by amplifier if input has not been merged by amplifer.
	m = 0
	n = 0
	if (!ccdflag (im, "ampmerge")) {
	    switch (ccdtype) {
	    case MASK, ZERO, DARK:
		call ccdamp (im, Memc[amp], SZ_FNAME)
		do i = 1, ncal {
		    if (CCDTYPE(cal,i) != ccdtype)
			next
		    if (FLAG(cal,i) == YES)
			iferr (call cal_get (i, im))
			    next
		    if (AMPMERGE(cal,i) == YES)
			next
		    if (strne (AMP(cal,i), Memc[amp]))
			next
		    n = n + 1
		    if (n == 1) {
			m = i
		    } else {
			if (NSCAN(cal,i) == NSCAN(cal,m)) {
#			call eprintf (
#			    "Warning: Extra calibration image %s ignored\n")
#			    call pargstr (IMAGE(cal,i))

			    # Reset image type to eliminate further warnings.
			    CCDTYPE(cal,i) = UNKNOWN
			} else if (NSCAN(cal,m) != nscan &&
				   (NSCAN(cal,i)==nscan || NSCAN(cal,i)==1)) {
			    m = i
			}
		    }
		}
	    case FLAT, SFLAT, ILLUM, FRINGE:
		call ccdamp (im, Memc[amp], SZ_FNAME)
		call ccdsubset (im, Memc[subset], SZ_FNAME)

		do i = 1, ncal {
		    if (CCDTYPE(cal,i) != ccdtype)
			next
		    if (FLAG(cal,i) == YES)
			iferr (call cal_get (i, im))
			    next
		    if (AMPMERGE(cal,i) == YES)
			next
		    if (strne (AMP(cal,i), Memc[amp]))
			next
		    if (strne (SUBSET(cal,i), Memc[subset]))
			next
		    n = n + 1
		    if (n == 1) {
			m = i
		    } else {
			if (NSCAN(cal,i) == NSCAN(cal,m)) {
#			call eprintf (
#			    "Warning: Extra calibration image %s ignored\n")
#			    call pargstr (IMAGE(cal,i))

			    # Reset image type to eliminate further warnings.
			    CCDTYPE(cal,i) = UNKNOWN
			} else if (NSCAN(cal,m) != nscan &&
				   (NSCAN(cal,i)==nscan || NSCAN(cal,i)==1)) {
			    m = i
			}
		    }
		}
	    }
	}

	# Match by CCD against calibrations which have been merged by amp.
	# Note that it is ok if the input has not been merged in which case
	# the part of the merged calibration will be matched with the amplifier.
	if (m == 0) {
	    switch (ccdtype) {
	    case MASK, ZERO, DARK:
		call ccdname (im, Memc[ccd], SZ_FNAME)
		do i = 1, ncal {
		    if (CCDTYPE(cal,i) != ccdtype)
			next
		    if (FLAG(cal,i) == YES)
			iferr (call cal_get (i, im))
			    next
		    if (AMPMERGE(cal,i) == NO)
			next
		    if (strne (CCD(cal,i), Memc[ccd]))
			next
		    n = n + 1
		    if (n == 1) {
			m = i
		    } else {
			if (NSCAN(cal,i) == NSCAN(cal,m)) {
#			call eprintf (
#			    "Warning: Extra calibration image %s ignored\n")
#			    call pargstr (IMAGE(cal,i))

			    # Reset image type to eliminate further warnings.
#			    CCDTYPE(cal,i) = UNKNOWN
			    ;
			} else if (NSCAN(cal,m) != nscan &&
				   (NSCAN(cal,i)==nscan || NSCAN(cal,i)==1)) {
			    m = i
			}
		    }
		}
	    case FLAT, SFLAT, ILLUM, FRINGE:
		call ccdname (im, Memc[ccd], SZ_FNAME)
		call ccdsubset (im, Memc[subset], SZ_FNAME)

		do i = 1, ncal {
		    if (CCDTYPE(cal,i) != ccdtype)
			next
		    if (FLAG(cal,i) == YES)
			iferr (call cal_get (i, im))
			    next
		    if (strne (CCD(cal,i), Memc[ccd]))
			next
		    if (strne (SUBSET(cal,i), Memc[subset]))
			next
		    if (AMPMERGE(cal,i) == NO) {
			call eprintf (
	    "Warning: Multiple amps used for image but not for calibration\n")
			next
		    }
		    n = n + 1
		    if (n == 1) {
			m = i
		    } else {
			if (NSCAN(cal,i) == NSCAN(cal,m)) {
#			call eprintf (
#			    "Warning: Extra calibration image %s ignored\n")
#			    call pargstr (IMAGE(cal,i))

			    # Reset image type to eliminate further warnings.
#			    CCDTYPE(cal,i) = UNKNOWN
			    ;
			} else if (NSCAN(cal,m) != nscan &&
				   (NSCAN(cal,i)==nscan || NSCAN(cal,i)==1)) {
			    m = i
			}
		    }
		}
	    }
	}

	# If no calibration image is found then it is an error.
	if (m == 0) {
	    switch (ccdtype) {
	    case MASK:
	        call error (0, "No bad pixel mask found")
	    case ZERO:
	        call error (0, "No zero level calibration image found")
	    case DARK:
	        call error (0, "No dark count calibration image found")
	    case FLAT:
		call sprintf (Memc[str], SZ_LINE,
	             "No flat field calibration image of subset %s found")
		     call pargstr (Memc[subset])
	        call error (0, Memc[str])
	    case SFLAT:
		call sprintf (Memc[str], SZ_LINE,
	             "No sky flat field calibration image of subset %s found")
		     call pargstr (Memc[subset])
	        call error (0, Memc[str])
	    case ILLUM:
		call sprintf (Memc[str], SZ_LINE,
	             "No illumination calibration image of subset %s found")
		     call pargstr (Memc[subset])
	        call error (0, Memc[str])
	    case FRINGE:
		call sprintf (Memc[str], SZ_LINE,
	             "No fringe calibration image of subset %s found")
		     call pargstr (Memc[subset])
	        call error (0, Memc[str])
	    }
	}

	call strcpy (IMAGE(cal,m), image, maxchars)
	if (nscan != NSCAN(cal,m)) {
	    if (nscan != 1 && NSCAN(cal,m) == 1)
		call cal_scan (nscan, image, maxchars)
	    else {
		call sprintf (Memc[str], SZ_LINE,
	             "Cannot find or create calibration with nscan of %d")
		     call pargi (nscan)
	        call error (0, Memc[str])
	    }
	}

	# Check that the input image is not the same as the calibration image.
	call imstats (im, IM_IMAGENAME, Memc[str], SZ_LINE)
	if (ccd_cmp (Memc[str], IMAGE(cal,m))) {
	    call sprintf (Memc[str], SZ_LINE,
		"Calibration image %s is the same as the input image")
		call pargstr (image)
	    call error (0, Memc[str])
	}

	call sfree (sp)
end


# CAL_OPEN -- Create a list of calibration images from the input image list
# and the calibration image lists.

procedure cal_open (list)

int	list		# List of input images
int	list1		# List of calibration images

pointer	sp, str
bool	type
int	ccdtype, bplist, ccdtypecl, imtopenp(), nowhite()
bool	clgetb()
errchk	cal_list
include	"calimage.com"

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	ccdtype = ccdtypecl ("ccdtype", Memc[str], SZ_LINE)
	call clgstr ("bpmasks", Memc[str], SZ_LINE)
	bplist = nowhite (Memc[str], Memc[str], SZ_LINE)

	# Add calibration images to list.
	cal = NULL
	ncal = 0
	type = (ccdtype != MASK)
	if (type && (clgetb ("fixpix") || bplist!=0)) {
	    list1 = imtopenp ("fixfile")
	    call cal_list (list1, MASK)
	    call imtclose (list1)
	}
	type = (type && ccdtype != ZERO)
	if (type && clgetb ("zerocor")) {
	    list1 = imtopenp ("zero")
	    call cal_list (list1, ZERO)
	    call imtclose (list1)
	}
	type = (type && ccdtype != DARK)
	if (type && clgetb ("darkcor")) {
	    list1 = imtopenp ("dark")
	    call cal_list (list1, DARK)
	    call imtclose (list1)
	}
	type = (type && ccdtype != FLAT)
	if (type && clgetb ("flatcor")) {
	    list1 = imtopenp ("flat")
	    call cal_list (list1, FLAT)
	    call imtclose (list1)
	}
	type = (type && ccdtype != SFLAT)
	if (type && clgetb ("sflatcor")) {
	    list1 = imtopenp ("sflat")
	    call cal_list (list1, SFLAT)
	    call imtclose (list1)
	}
	if (type && ccdtype != ILLUM && clgetb ("illumcor")) {
	    list1 = imtopenp ("illum")
	    call cal_list (list1, ILLUM)
	    call imtclose (list1)
	}
	if (type && ccdtype != FRINGE && clgetb ("fringecor")) {
	    list1 = imtopenp ("fringe")
	    call cal_list (list1, FRINGE)
	    call imtclose (list1)
	}
	if (list != NULL) {
	    call cal_list (list, UNKNOWN)
	    call imtrew (list)
	}

	call sfree (sp)
end


# CAL_CLOSE -- Free memory from the internal calibration image list.

procedure cal_close ()

int	i
include	"calimage.com"

begin
	if (cal != NULL) {
	    do i = 1, ncal {
		call mfree (CAL_KEY(cal,i), TY_CHAR)
		call mfree (CAL_IMAGE(cal,i), TY_CHAR)
		call mfree (CAL_AMP(cal,i), TY_CHAR)
		call mfree (CAL_CCD(cal,i), TY_CHAR)
		call mfree (CAL_SUBSET(cal,i), TY_CHAR)
	    }
	    call mfree (cal, TY_STRUCT)
	}
end


# CAL_LIST -- Add calibration images to an internal list.
# Map each image and get the CCD image type, amplifier, and subset.
# If the ccdtype is given as a procedure argument this overrides the
# image header type.  For the calibration images add the type, amp, subset,
# and image name to dynamic arrays.  Ignore duplicate names.

procedure cal_list (list, listtype)

pointer	list		# Image list
int	listtype	# CCD type of image in list.
			# Overrides header type if not UNKNOWN.

int	i, ccdtype, ccdtypes(), ccdnscan(), imtgetim(), btoi()
pointer	sp, image, str, im, immap()
bool	streq(), ccdflag()
include	"calimage.com"

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    # If possible check the image.  Ignore non-images in the input.
	    if (listtype == UNKNOWN) {
		iferr (im = immap (Memc[image], READ_ONLY, 0))
		    next
		ccdtype = ccdtypes (im, Memc[str], SZ_LINE)
	    } else if (Memc[image] == '!' || streq (Memc[image], "BPM")) {
		im = NULL
		ccdtype = listtype
	    } else {
		iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		    #if (listtype != MASK)
			#call erract (EA_ERROR)
		    im = NULL
		}
		ccdtype = listtype
	    }

	    # Ignore any images with no data.
	    if (im != NULL) {
		if (IM_NDIM(im) == 0) {
		    call imunmap (im)
		    next
		}
	    }

	    switch (ccdtype) {
	    case MASK, ZERO, DARK, FLAT, SFLAT, ILLUM, FRINGE:
		# Check for duplication.
		for (i=1; i<=ncal; i=i+1)
		    if (streq (Memc[image], KEY(cal,i))  &&
			CCDTYPE(cal,i) == ccdtype)
			break
		if (i <= ncal)
		    break

		# Allocate memory for a new image.
		if (cal == NULL)
		    call malloc (cal, CAL_LEN(10), TY_STRUCT)
		else if (mod (ncal,10) == 0)
		    call realloc (cal, CAL_LEN(ncal+10), TY_STRUCT)
		call malloc (CAL_KEY(cal,i), SZ_FNAME, TY_CHAR)
		call malloc (CAL_IMAGE(cal,i), SZ_FNAME, TY_CHAR)
		call malloc (CAL_AMP(cal,i), SZ_FNAME, TY_CHAR)
		call malloc (CAL_CCD(cal,i), SZ_FNAME, TY_CHAR)
		call malloc (CAL_SUBSET(cal,i), SZ_FNAME, TY_CHAR)

		# Enter the key, image,  ccdtype, amp, and subset.
		call strcpy (Memc[image], KEY(cal,i), SZ_FNAME)
		CCDTYPE(cal,i) = ccdtype
		if (im == NULL)
		    FLAG(cal,i) = YES
		else {
		    FLAG(cal,i) = NO
		    call strcpy (Memc[image], IMAGE(cal,i), SZ_FNAME)
		    call ccdamp (im, AMP(cal,i), SZ_FNAME)
		    call ccdname (im, CCD(cal,i), SZ_FNAME)
		    call ccdsubset (im, SUBSET(cal,i), SZ_FNAME)
		    NSCAN(cal,i) = ccdnscan (im, ccdtype)
		    AMPMERGE(cal,i) = btoi (ccdflag (im, "ampmerge"))
		}
		ncal = i
	    }
	    if (im != NULL)
		call imunmap (im)
	}
	call sfree (sp)
end


# CAL_SCAN -- Generate name for scan corrected calibration image.

procedure cal_scan (nscan, image, maxchar)

int	nscan			#I Number of scan lines
char	image[maxchar]		#U Input root name, output scan name
int	maxchar			#I Maximum number of chars in image name

bool	clgetb()
pointer	sp, root, ext

begin
	# Check if this operation is desired.
	if (!clgetb ("scancor") || nscan == 1)
	    return

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (ext, SZ_FNAME, TY_CHAR)

	call xt_imroot (image, Memc[root], SZ_FNAME)
	call xt_imext (image, Memc[ext], SZ_FNAME)
	if (IS_INDEFI (nscan)) {
	    call sprintf (image, maxchar, "%s.1d%s")
		call pargstr (Memc[root])
		call pargstr (Memc[ext])
	} else {
	    call sprintf (image, maxchar, "%s.%d%s")
		call pargstr (Memc[root])
		call pargi (nscan)
		call pargstr (Memc[ext])
	}

	call sfree (sp)
end


# CAL_GET -- Get the calibration information.
# This will return the information previously saved or open calibration
# images specified in the reference image header or requiring the reference
# image header (i.e. a bad pixel file).

procedure cal_get (i, refim)

int	i			#I Index
pointer	refim			#I Reference image

pointer im, xt_pmmap(), immap()
bool	check, streq(), ccdflag()
int	ccdnscan(), btoi()
errchk	xt_pmmap, immap, hdmgstr, ccdamp, ccdname, ccdsubset, ccdnscan
include	"calimage.com"

begin
	switch (CCDTYPE(cal,i)) {
	case MASK:
	    if (KEY(cal,i) != '!') {
		iferr (im = xt_pmmap (KEY(cal,i),refim,IMAGE(cal,i),SZ_FNAME)) {
		    #call erract (EA_WARN)
		    call erract (EA_ERROR)
		}
		if (streq (KEY(cal,i), "BPM"))
		    check = false
		else
		    check = true
	    } else {
		call hdmgstr (refim, Memc[CAL_KEY(cal,i)+1], IMAGE(cal,i),
		    SZ_FNAME)
		iferr (im=xt_pmmap (IMAGE(cal,i),refim,IMAGE(cal,i),SZ_FNAME)) {
		    #call erract (EA_WARN)
		    call erract (EA_ERROR)
		}
		check = false
	    }
	    if (im != NULL)
		call yt_pmunmap (im)
	    if (IMAGE(cal,i) == EOS) {
		call strcpy ("EMPTY", IMAGE(cal,i), SZ_FNAME)
		check = false
	    }
	    if (check) {
		ifnoerr (im = immap (IMAGE(cal,i), READ_ONLY,0)) {
		    call ccdamp (im, AMP(cal,i), SZ_FNAME)
		    call ccdname (im, CCD(cal,i), SZ_FNAME)
		    call ccdsubset (im, SUBSET(cal,i), SZ_FNAME)
		    NSCAN(cal,i) = ccdnscan (im, CCDTYPE(cal,i))
		    AMPMERGE(cal,i) = btoi (ccdflag (im, "ampmerge"))
		    call imunmap (im)
		} else {
		    call ccdamp (refim, AMP(cal,i), SZ_FNAME)
		    call ccdname (refim, CCD(cal,i), SZ_FNAME)
		    call ccdsubset (refim, SUBSET(cal,i), SZ_FNAME)
		    NSCAN(cal,i) = ccdnscan (refim, CCDTYPE(cal,i))
		    AMPMERGE(cal,i) = btoi (ccdflag (refim, "ampmerge"))
		}
	    } else {
		call ccdamp (refim, AMP(cal,i), SZ_FNAME)
		call ccdname (refim, CCD(cal,i), SZ_FNAME)
		call ccdsubset (refim, SUBSET(cal,i), SZ_FNAME)
		NSCAN(cal,i) = ccdnscan (refim, CCDTYPE(cal,i))
		AMPMERGE(cal,i) = btoi (ccdflag (refim, "ampmerge"))
	    }
	default:
	    call hdmgstr (refim, Memc[CAL_KEY(cal,i)+1], IMAGE(cal,i), SZ_FNAME)
	    im = immap (IMAGE(cal,i), READ_ONLY, 0)
	    call ccdamp (im, AMP(cal,i), SZ_FNAME)
	    call ccdname (im, CCD(cal,i), SZ_FNAME)
	    call ccdsubset (im, SUBSET(cal,i), SZ_FNAME)
	    NSCAN(cal,i) = ccdnscan (im, CCDTYPE(cal,i))
	    AMPMERGE(cal,i) = btoi (ccdflag (im, "ampmerge"))
	    call imunmap (im)
	}
end
