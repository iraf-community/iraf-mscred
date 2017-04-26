include	<error.h>
include	<imset.h>
include	"ccdred.h"
include	"ccdtypes.h"

define	MEMUNIT	1000000.	# Units for memory specifications

# CCDPROC -- Process CCD images.
#
# This is the main procedure for processing CCD images.  The images are
# corrected for bad pixels, overscan levels, zero levels, dark counts, flat
# field response, illumination errors, and fringe response.  They may also be
# trimmed.  The input is a list of images to be processed, a list of output
# images for the result, the CCD type to be select from the input list.
# If the output list is empty or the image names in the output list match the
# input list then the processing is done to a temporary image which
# eventually replaces the input image.  The checking of whether to apply each
# correction, getting the required parameters, and logging the operations is
# left to separate procedures, one for each correction.  The actual
# processing is done by a specialized procedure designed to be very
# efficient.  There are two data type paths; one for short pixel types and
# one for all other pixel types (usually real).

procedure ccdproc (inlist, outlist, noilist, bpmlist, onerror, selecttype,
	proctype, calproc)

int	inlist			#I List of input CCD images
int	outlist			#I List of output CCD images
int	noilist			#I List of output no interpolation images
int	bpmlist			#I List of output bad pixel masks
int	onerror			#I Error action
char	selecttype[ARB]		#I CCD type to select (if not null)
char	proctype[ARB]		#I CCD processing type (if not null)
int	calproc			#I Process calibration images?

int	ccdcode, interactive, proc, listproc, noutlist, nbpmlist, nnoilist
int	max_cache, bufsize, last_cache, last_bufsize
pointer	sp, input, output, bpmask, noi, temp, str, in, out, bpm, fdnoi, ccd

bool	clgetb(), streq(), ccdcheck()
real	clgetr()
int	imtlen(), imtgetim(), ccdtypes(), ccdtype()
pointer	immap()
errchk	immap, set_input, set_output, ccddelete, cal_open
errchk	set_fixpix, set_overscan, set_zero, set_dark, set_flat
errchk	set_illum, set_fringe

data	last_cache/-1/

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (bpmask, SZ_FNAME, TY_CHAR)
	call salloc (noi, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Initialize the instrument transation, the calibration images,
	# caching, and log output.

	call clgstr ("instrument", Memc[str], SZ_LINE)
	call hdmopen (Memc[str])

	# Initialize calibration images.
	call cal_open (inlist)

	call ccdlog_open (10 * SZ_LINE)
	call set_interactive ("", interactive)
	if (clgetb ("noproc")) {
	    proc = NO
	    listproc = YES
	} else {
	    proc = YES
	    listproc = NO
	}

	# Set calibration image caching and calibration image buffer size.
	#if (imtlen (inlist) < 3)
	#    max_cache = 0.
	#else
	    max_cache = MEMUNIT * clgetr ("max_cache")
	bufsize = max (1024., MEMUNIT * clgetr ("im_bufsize"))
	if (last_cache>=0 && (max_cache!=last_cache || bufsize!=last_bufsize)) {
	    call ccd_close ()
	    last_cache = -1
	}
	if (last_cache == -1) {
	    call ccd_open (max_cache, bufsize)
	    last_cache = max_cache
	    last_bufsize = bufsize
	}

	# Process the images.
	noutlist = imtlen (outlist)
	if (noutlist == 1)
	    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		call error (1, "Error in output list")
	nnoilist = imtlen (noilist)
	if (nnoilist == 0)
	    Memc[noi] = EOS
	nbpmlist = imtlen (bpmlist)
	if (nbpmlist == 0)
	    Memc[bpmask] = EOS
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (noutlist == 0)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    else if (noutlist > 1)
		if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		    call error (1, "Error in output list")
	    if (streq (Memc[input], Memc[output]))
		call mktemp ("tmp", Memc[temp], SZ_FNAME)
	    else
		call strcpy (Memc[output], Memc[temp], SZ_FNAME)
	    if (nnoilist > 0)
		if (imtgetim (noilist, Memc[noi], SZ_FNAME) == EOF)
		    call error (1, "Error in output no interpolation list")
	    if (nbpmlist > 0)
		if (imtgetim (bpmlist, Memc[bpmask], SZ_FNAME) == EOF)
		    call error (1, "Error in bad pixel mask list")

	    if (listproc == YES) {
		call printf ("%s:\n")
		    call pargstr (Memc[input])
	    }


	    # Map the input image and check its type.
	    iferr (in = immap (Memc[input], READ_ONLY, 0)) {
		switch (onerror) {
		case ONERR_ABORT:
		    call erract (EA_ERROR)
		case ONERR_EXIT:
		    call erract (EA_WARN)
		    break
		default:
		    call erract (EA_WARN)
		    next
		}
	    }
	    call imseti (in, IM_BUFSIZE, bufsize)
	    ccdcode = ccdtypes (in, Memc[str], SZ_LINE)
	    if (selecttype[1] != EOS && !streq (Memc[str], selecttype)) {
		call imunmap (in)
		next
	    }

	    # Check if the image needs to be processed.
	    if (proctype[1] != EOS)
		ccdcode = ccdtype (proctype, NO, Memc[str], SZ_LINE)
	    if (!ccdcheck (in, ccdcode, Memc[bpmask])) {
		call imunmap (in)
		next
	    }

	    # Process the image.
	    iferr {

		# Set the processing parameters.
		ccd = NULL
		call set_proc (in, proc, calproc, listproc, ccd)
		call set_noi (ccd, Memc[noi])
		call set_bpmask (ccd, Memc[bpmask])

		switch (ccdcode) {
		case MASK:
		    call set_sections (ccd)
		case ZERO:
		    call set_sections (ccd)
		    call set_trim (ccd)
		    call set_overscan (ccd)
		    call set_fixpix (ccd)
		    call set_readcor (ccd)
		    call set_saturate (ccd)
		case DARK:
		    call set_sections (ccd)
		    call set_trim (ccd)
		    call set_overscan (ccd)
		    call set_fixpix (ccd)
		    call set_zero (ccd)
		    call set_saturate (ccd)
		case FLAT:
		    call set_sections (ccd)
		    call set_trim (ccd)
		    call set_overscan (ccd)
		    call set_fixpix (ccd)
		    call set_zero (ccd)
		    call set_dark (ccd)
		    call set_saturate (ccd)
		    CORS(ccd, FINDMEAN) = YES
		    CORS(ccd, MINREP) = YES
		case SFLAT:
		    call set_sections (ccd)
		    call set_trim (ccd)
		    call set_overscan (ccd)
		    call set_fixpix (ccd)
		    call set_zero (ccd)
		    call set_dark (ccd)
		    call set_flat (ccd)
		    call set_saturate (ccd)
		    CORS(ccd, FINDMEAN) = YES
		    CORS(ccd, MINREP) = YES
		case ILLUM:
		    call set_sections (ccd)
		    call set_trim (ccd)
		    call set_overscan (ccd)
		    call set_fixpix (ccd)
		    call set_zero (ccd)
		    call set_dark (ccd)
		    call set_flat (ccd)
		    call set_sflat (ccd)
		    call set_saturate (ccd)
		    CORS(ccd, FINDMEAN) = YES
		case OBJECT, COMP:
		    call set_sections (ccd)
		    call set_trim (ccd)
		    call set_overscan (ccd)
		    call set_fixpix (ccd)
		    call set_zero (ccd)
		    call set_dark (ccd)
		    call set_flat (ccd)
		    call set_sflat (ccd)
		    iferr {
			call set_illum (ccd)
			call set_fringe (ccd)
		    } then
			call erract (EA_WARN)
		    call set_saturate (ccd)
		    CORS(ccd, FINDMEAN) = YES
		default:
		    call set_sections (ccd)
		    call set_trim (ccd)
		    call set_overscan (ccd)
		    call set_fixpix (ccd)
		    call set_zero (ccd)
		    call set_dark (ccd)
		    call set_flat (ccd)
		    call set_sflat (ccd)
		    iferr {
			call set_illum (ccd)
			call set_fringe (ccd)
		    } then
			call erract (EA_WARN)
		    call set_saturate (ccd)
		    CORS(ccd, FINDMEAN) = YES
		}

		# Do the processing.
		if (PROC(ccd) == YES) {
		    if (COR(ccd) == YES) {
			call ccdlog_flush ()
			call set_output (Memc[temp], Memc[noi], Memc[bpmask],
			    ccd, in, out, fdnoi, bpm)
			call doproc (ccd)
			call set_header (ccd)
			in = IN_IM(ccd)
			if (fdnoi != NULL)
			    call imunmap (fdnoi)
			if (bpm != NULL)
			    call imunmap (bpm)
			if (out != NULL)
			    call imunmap (out)
			call imunmap (in)
			if (CORS(ccd, READCOR) == YES)
			    call readcor (Memc[temp], Memc[temp])
		    } else {
			call imunmap (in)

			if (CORS(ccd, READCOR) == YES)
			    call readcor (Memc[input], Memc[temp])
			else
			    Memc[temp] = EOS
		    }

		    # Replace input image by the processed image if needed.
		    if (streq(Memc[input],Memc[output])&&Memc[temp]!=EOS) {
			iferr (call ccddelete (Memc[input])) {
			    call eprintf (
			    "Warning: Can't delete or make backup of `%s'.")
				call pargstr (Memc[input])
			    call eprintf ("  Processed image is `%s'.\n")
				call pargstr (Memc[temp])
			    PROC(ccd) = NO
			} else
			    call imrename (Memc[temp], Memc[output])
		    }
		} else
		    call ccdlog_clear ()

		call ccdlog_flush ()
		if (ccd != NULL)
		    call free_proc (ccd)
		if (in != NULL)
		    call imunmap (in)
	    } then {
		call ccdlog_clear ()
		if (ccd != NULL)
		    call free_proc (ccd)
		if (in != NULL)
		    call imunmap (in)
		switch (onerror) {
		case ONERR_WARN:
		    call erract (EA_WARN)
		    next
		case ONERR_EXIT:
		    call erract (EA_WARN)
		    break
		default:
		    call erract (EA_ERROR)
		}
	    }
	}

	# Finish up.
	call cal_close ()
	call ccdlog_close ()
	#call ccd_close ()
	#last_cache = -1
	call hdmclose ()
	call sfree (sp)
end



# CCDPROC1 -- Process a single CCD image with a specified type.
#
# This procedure processes a single image and is used to recursively process
# a calibration image.  It is assumed that the calling procedure has already
# determined that the image needs to be processed (there is no call to
# CCDCHECK) and that it will not try and process a calibration image (since
# this would cause a recursive call).  This procedure is like CCDPROC
# called with a single image except that it does not need to initialize
# the translations or the calibration image database and cache.

procedure ccdproc1 (input, output, ccdcode)

char	input[ARB]		#I Input CCD image to process
char	output[ARB]		#I Output processed CCD image
int	ccdcode			#I CCD type of image (independent of header).

int	proc, bufsize
pointer	sp, temp, in, out, bpm, fdnoi, ccd, immap()
bool	streq()
real	clgetr()
errchk	immap, set_output, ccddelete
errchk	set_fixpix, set_zero, set_dark, set_flat, set_illum, set_fringe

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)

	if (streq (input, output))
	    call mktemp ("tmp", Memc[temp], SZ_FNAME)
	else
	    call strcpy (output, Memc[temp], SZ_FNAME)

	# Process the image.
	in = immap (input, READ_ONLY, 0)
	bufsize = MEMUNIT * clgetr ("im_bufsize")
	call imseti (in, IM_BUFSIZE, bufsize)

	# Set the processing parameters.
	proc = YES
	call set_proc (in, proc, NO, NO, ccd)
	call set_bpmask (ccd, "")
	call set_noi (ccd, "")
	switch (ccdcode) {
	case MASK:
	    call set_sections (ccd)
	case ZERO:
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_readcor (ccd)
	    call set_overscan (ccd)
	case DARK:
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_zero (ccd)
	    call set_overscan (ccd)
	case FLAT:
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_overscan (ccd)
	    CORS(ccd, FINDMEAN) = YES
	    CORS(ccd, MINREP) = YES
	case SFLAT:
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_flat (ccd)
	    call set_overscan (ccd)
	    CORS(ccd, FINDMEAN) = YES
	    CORS(ccd, MINREP) = YES
	case ILLUM:
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_flat (ccd)
	    call set_sflat (ccd)
	    call set_overscan (ccd)
	case OBJECT, COMP:
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_flat (ccd)
	    call set_sflat (ccd)
	    iferr {
		call set_illum (ccd)
		call set_fringe (ccd)
	    } then
		call erract (EA_WARN)
	    call set_overscan (ccd)
	default:
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_flat (ccd)
	    call set_sflat (ccd)
	    iferr {
		call set_illum (ccd)
		call set_fringe (ccd)
	    } then
		call erract (EA_WARN)
	    call set_overscan (ccd)
	    CORS(ccd, FINDMEAN) = YES
	}

	# Do the processing.
	if (PROC(ccd) == YES) {
	    if (COR(ccd) == YES) {
		call ccdlog_flush ()
		call set_output (Memc[temp], "", "", ccd, in, out, fdnoi, bpm)
		call doproc (ccd)
		call set_header (ccd)
		call imunmap (out)
		call imunmap (in)
		if (CORS(ccd, READCOR) == YES)
		    call readcor (Memc[temp], Memc[temp])
	    } else {
		call imunmap (in)

		if (CORS(ccd, READCOR) == YES)
		    call readcor (input, Memc[temp])
		else
		    Memc[temp] = EOS
	    }

	    # Replace the input image by the processed image if needed.
	    if (streq (input, output) && Memc[temp] != EOS) {
		iferr (call ccddelete (input)) {
		    call eprintf (
			"Warning: Can't delete or make backup of `%s'.")
			call pargstr (input)
		    call eprintf ("  Processed image is `%s'.\n")
			call pargstr (Memc[temp])
		    PROC(ccd) = NO
		} else
		    call imrename (Memc[temp], output)
	    }
	} else {
	    call imunmap (in)
	    call ccdlog_clear ()
	}
	proc = PROC(ccd)

	call free_proc (ccd)
	call ccdlog_flush ()
	call sfree (sp)

	if (proc == NO)
	    call error (2, "Error processing calibration image")
end


# CCDPROC2 -- Action if not processing calibration images.

procedure ccdproc2 (input, output, ccdcode)

char	input[ARB]		#I Input CCD image to process
char	output[ARB]		#I Output processed CCD image
int	ccdcode			#I CCD type of image (independent of header).

int	fd, open()
pointer	sp, fname
errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call clgstr ("calproc", Memc[fname], SZ_FNAME)
	fd = open (Memc[fname], APPEND, TEXT_FILE)
	call ccdstr (ccdcode, Memc[fname], SZ_FNAME)
	call fprintf (fd, "%s %s\n")
	    call pargstr (input)
	    call pargstr (Memc[fname])
	call close (fd)
	call sfree (sp)
end
