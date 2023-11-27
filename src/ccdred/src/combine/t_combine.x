include	<imhdr.h>
include	<error.h>
include	<syserr.h>
include	<mach.h>
include	<pmset.h>
include	"../ccdred.h"
include	"src/icombine.h"

# Symbol table definitions from hdrmap.x.
define	LEN_INDEX	32		# Length of symtab index
define	LEN_STAB	1024		# Length of symtab string buffer
define	SZ_SBUF		128		# Size of symtab string buffer

define	SZ_NAME		79		# Size of translation symbol name
define	SZ_DEFAULT	79		# Size of default string
define	SYMLEN		80		# Length of symbol structure

# Symbol table structure
define	NAME		Memc[P2C($1)]		# Translation name for symbol
define	DEFAULT		Memc[P2C($1+40)]	# Default value of parameter

define	GRPAMP		1	# Group by amplifier
define	GRPCCD		2	# Group by ccd

define	ONEIMAGE	99	# Error code for one image to combine


# T_COMBINE -- Combine images.

procedure t_combine ()

int	i, list, nout, imtopenp()
pointer	sp, fname, outnames
errchk	cmbine

int	grp
common	/grpcom/ grp

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Get the list of images and open the header translation which
	# is needed to determine the amps, ccds, subsets and ccdtypes.

	call clgstr ("instrument", Memc[fname], SZ_FNAME)
	call hdmopen (Memc[fname])
	grp = GRPAMP

	list = imtopenp ("input")
	call clgstr ("output", Memc[fname], SZ_FNAME)
	call xt_imroot (Memc[fname], Memc[fname], SZ_FNAME)

	iferr (call cmbine (list, Memc[fname], YES, outnames, nout))
	    call erract (EA_WARN)

	do i = 1, nout
	    call mfree (Memi[outnames+i-1], TY_CHAR)
	call mfree (outnames, TY_POINTER)
	call imtclose (list)
	call hdmclose ()
	call sfree (sp)
end


# T_COUTPUT -- List of output images.

procedure t_coutput ()

int	list, imtopenp()
pointer	sp, fname

int	grp
common	/grpcom/ grp

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Get the list of images and open the header translation which
	# is needed to determine the amps, ccds, subsets and ccdtypes.

	call clgstr ("instrument", Memc[fname], SZ_FNAME)
	call hdmopen (Memc[fname])
	grp = GRPAMP

	list = imtopenp ("input")
	call clgstr ("output", Memc[fname], SZ_FNAME)
	call xt_stripwhite (Memc[fname])

	iferr (call coutput (list, Memc[fname]))
	    call erract (EA_WARN)

	call imtclose (list)
	call hdmclose ()
	call sfree (sp)
end


# T_AMPMERGE -- Merge amplifiers from multiple amp per CCD data.
#
# It merges extensions with the same ccd.
# If all extensions merge to a single image then a single image format is
# produced.

procedure t_ampmerge ()

int	i, j, fd, nout, nmerge, inlist, outlist, bplist, list1
real	c1, c2, c3, c4, l1, l2, l3, l4, ltm[2,2], ltv[2]
pointer	sp, outnames, fname, input, output, outmask, outputs
pointer	im, mw, ct

bool	ccdflag()
int	imtopenp(), imtopen(), imtgetim(), imtlen(), imgeti()
int	open(), errcode(), nowhite()
real	imgetr()
pointer	immap(),  mw_openim(), mw_sctran()
errchk	open, cmbine, immap, imgetr, maskmerge

int	grp
common	/grpcom/ grp

define	skip_	10

begin
	call smark (sp)
	call salloc (outnames, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (outmask, SZ_FNAME, TY_CHAR)

	# Get the list of images and open the header translation which
	# is needed to determine the amps, ccds, subsets and ccdtypes.

	call clgstr ("instrument", Memc[fname], SZ_FNAME)
	call hdmopen (Memc[fname])
	grp = GRPCCD

	# Do each image separately.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	bplist = imtopenp ("outmasks")
	if (imtlen (inlist) != imtlen (outlist) && imtlen (outlist) != 1)
	    call error (1, "Input and output lists don't match")
	if (imtlen (bplist) != 0 && imtlen (bplist) != imtlen (outlist))
	    call error (1, "Output data and mask lists don't match")
	call clgstr ("outnames", Memc[outnames], SZ_FNAME)
	i = nowhite (Memc[outnames], Memc[outnames], SZ_FNAME)
	fd = NULL
	while (imtgetim (outlist, Memc[output], SZ_FNAME) != EOF) {
	    call xt_imroot (Memc[output], Memc[output], SZ_FNAME)
	    if (imtgetim (inlist, Memc[input], SZ_FNAME) == EOF)
		break
	    if (imtlen (outlist) == 1) {
		call imtrew (inlist)
		list1 = inlist
	    } else
		list1 = imtopen (Memc[input])
	    if (imtgetim (bplist, Memc[outmask], SZ_FNAME) == EOF)
		Memc[outmask] = EOS
	    outputs = NULL

	    iferr {
		# Check processing.
		call sprintf (Memc[fname], SZ_FNAME, "%s[1]")
		    call pargstr (Memc[input])
		iferr (im = immap (Memc[fname], READ_ONLY, 0))
		    im = immap (Memc[input], READ_ONLY, 0)
		if (ccdflag (im, "ampmerge")) {
		    call imunmap (im)
		    goto skip_
		}

		if (!ccdflag (im, "trim")) {
		    call imunmap (im)
		    call sprintf (Memc[fname], SZ_FNAME,
		"Data must be overscan corrected and trimmed for merging (%s)")
			call pargstr (Memc[input])
		    call error (1, Memc[fname])
		}
		call imunmap (im)

		# Merge the amplifier images.
		iferr (call cmbine (list1, Memc[output], NO, outputs, nout)) {
		    if (errcode() == ONEIMAGE)
			goto skip_
		    call erract (EA_ERROR)
		}

		# Update the headers.
		do i = 1, nout {
		    do j = 0, ARB {
			call sprintf (Memc[fname], SZ_FNAME, "%s[%d]")
			    call pargstr (Memc[Memi[outputs+i-1]])
			    call pargi (j)
			iferr (im = immap (Memc[fname], READ_WRITE, 0)) {
			    switch (errcode()) {
			    case SYS_FXFRFEOF, SYS_IKIOPEN:
				break
			    case SYS_IKIEXTN:
				next
			    default:
				call erract (EA_ERROR)
			    }
			}

			# Write names if desired.
			if (fd == NULL && Memc[outnames] != EOS)
			    fd = open (Memc[outnames], NEW_FILE, TEXT_FILE)
			if (j == 0 && fd != NULL) {
			    call fprintf (fd, "%s\n")
				call pargstr (Memc[Memi[outputs+i-1]])
			}

			iferr (call imdelf (im, "nextend"))
			    ;
			if (IM_NDIM(im) == 0) {
			    call imunmap (im)
			    next
			}

			# Remove NEXTEND,  NCOMBINE and put in AMPMERGE flag.
			iferr (call imdelf (im, "nextend"))
			    ;
			iferr (nmerge = imgeti (im, "ncombine"))
			    nmerge = 0
			iferr (call imdelf (im, "ncombine"))
			    ;
			call sprintf (Memc[fname], SZ_FNAME, "Merged %d amps")
			    call pargi (nmerge)
			call timelog (Memc[fname], SZ_FNAME)
			call imastr (im, "ampmerge", Memc[fname])

			# Update CCDSEC.
			mw = mw_openim (im)
			ct = mw_sctran (mw, "logical", "physical", 3)
			call mw_c2tranr (ct, 0.501, 0.501, c1, l1)
			call mw_c2tranr (ct, real(IM_LEN(im,1)+0.499),
			    real(IM_LEN(im,2)+0.499), c2, l2)
			call sprintf (Memc[fname], SZ_FNAME, "[%d:%d,%d:%d]")
			    call pargi (nint(c1))
			    call pargi (nint(c2))
			    call pargi (nint(l1))
			    call pargi (nint(l2))
			call imastr (im, "ccdsec", Memc[fname])
			call mw_ctfree (ct)

			# Update DETSEC.
			iferr {
			    ltv[1] = imgetr (im, "dtv1")
			    ltv[2] = imgetr (im, "dtv2")
			    ltm[1,1] = imgetr (im, "dtm1_1")
			    ltm[1,2] = 0.
			    ltm[2,1] = 0.
			    ltm[2,2] = imgetr (im, "dtm2_2")
			    call mw_sltermr (mw, ltm, ltv, 2)
			    ct = mw_sctran (mw, "physical", "logical", 3)
			    call mw_c2tranr (ct, c1, l1, c3, l3)
			    call mw_c2tranr (ct, c2, l2, c4, l4)
			    call sprintf (Memc[fname], SZ_FNAME,
				"[%d:%d,%d:%d]")
				call pargi (nint(c3))
				call pargi (nint(c4))
				call pargi (nint(l3))
				call pargi (nint(l4))
			    call imastr (im, "detsec", Memc[fname])
			} then
			    ;

			# Update AMPSEC.
			iferr {
			    ltv[1] = imgetr (im, "atv1")
			    ltv[2] = imgetr (im, "atv2")
			    ltm[1,1] = imgetr (im, "atm1_1")
			    ltm[1,2] = 0.
			    ltm[2,1] = 0.
			    ltm[2,2] = imgetr (im, "atm2_2")
			    call mw_sltermr (mw, ltm, ltv, 2)
			    ct = mw_sctran (mw, "physical", "logical", 3)
			    call mw_c2tranr (ct, c1, l1, c3, l3)
			    call mw_c2tranr (ct, c2, l2, c4, l4)
			    call sprintf (Memc[fname], SZ_FNAME,
				"[%d:%d,%d:%d]")
				call pargi (nint(c3))
				call pargi (nint(c4))
				call pargi (nint(l3))
				call pargi (nint(l4))
			    call imastr (im, "ampsec", Memc[fname])
			} then
			    ;

			call mw_close (mw)

			# Merge masks.
			if (Memc[outmask] != EOS) {
			    ifnoerr (call imgstr (im, "EXTNAME",
				Memc[input], SZ_FNAME)) {
				if (nowhite (Memc[input], Memc[input],
				    SZ_FNAME)==0)
				    ;
				call sprintf (Memc[fname], SZ_FNAME,
				    "bpmm_%s.pl")
				    call pargstr (Memc[input])
				call maskmerge (im, Memc[outmask], Memc[fname])
			    } else
				call maskmerge (im, "", Memc[outmask])
			}
			call imunmap (im)
		    }
		}

skip_		i = 0

	    } then
		call erract (EA_WARN)

	    if (imtlen (outlist) != 1)
		call imtclose (list1)
	    if (outputs != NULL) {
		do i = 1, nout
		    call mfree (Memi[outputs+i-1], TY_CHAR)
		call mfree (outputs, TY_POINTER)
	    }
	}

	if (fd != NULL)
	    call close (fd)
	call imtclose (inlist)
	call hdmclose ()
	call sfree (sp)
end


# MASKMERGE -- Merge masks.

procedure maskmerge (in, dir, output)

pointer	in			#I Input merged image pointer
char	dir[ARB]		#I Output directory name
char	output[ARB]		#I Ouput mask name

int	i, j, n, nc
pointer	sp, outname, key, fname, fnames, im, ims, out, outbuf

bool	streq(), pm_empty()
int	access()
pointer	pm_open(), immap(), yt_pmmap(), impl2i(), imgl2i()
errchk	pm_loadim, immap, yt_pmmap, imgstr, imdelete, imrename, fmkdir

begin

	if (output[1] == EOS)
	    return

	call smark (sp)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (key, 8, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Set the output name.
	call sprintf (Memc[outname], SZ_FNAME, "%s%s")
	    call pargstr (dir)
	    call pargstr (output)

	# Get the masks from the output image and the headers of the parent
	# images given by the IMCMB keywords.  Get only unique and
	# non-empty masks.

	n = 0
	do i = 0, ARB {
	    if (i == 0) {
		iferr (call imgstr (in, "BPM", Memc[fname], SZ_FNAME))
		    next
	    } else {
		call sprintf (Memc[key], 8, "IMCMB%03d")
		    call pargi (i)
		iferr (call imgstr (in, Memc[key], Memc[fname], SZ_FNAME))
		    break
		im = immap (Memc[fname], READ_ONLY, 0)
		iferr (call imgstr (im, "BPM", Memc[fname], SZ_FNAME)) {
		    call imunmap (im)
		    next
		}
		call imunmap (im)
	    }

	    # Check if the mask has already been seen.
	    do j = 0, n-1
		if (streq (Memc[fname], Memc[Memi[fnames+j]]))
		    break
	    if (j < n)
		next

	    # Check for an empty mask.
	    im = pm_open (NULL)
	    call pm_loadim (im, Memc[fname], Memc[key], 8)
	    if (pm_empty (im)) {
		call pm_close (im)
		if (i == 0) {
		    iferr {
			call imdelete (Memc[fname])
			call imdelf (in, "BPM")
		    } then
			;
		}
		next
	    } else
		call pm_close (im)

	    # Save the name.
	    if (fnames == NULL)
		call malloc (fnames, 10, TY_POINTER)
	    else if (mod (n, 10) == 0)
		call realloc (fnames, n, TY_POINTER)
	    Memi[fnames+n] = fname
	    n = n + 1
	    call salloc (fname, SZ_FNAME, TY_CHAR)
	}

	# If there are no masks just return.
	if (n == 0) {
	    call sfree (sp)
	    return
	}

	# If there is only one mask just set the BPM keyword.
	if (n == 1) {
	    iferr (call imgstr (in, "BPM", Memc[fname], SZ_FNAME))
		Memc[fname] = EOS
	    if (streq (Memc[fname], Memc[Memi[fnames]])) {
		if (dir[1] != EOS && access (dir, 0, 0) == NO)
		    call fmkdir (dir)
		call imrename (Memc[fname], Memc[outname])
		call imastr (in, "BPM", Memc[outname])
	    } else
		call imastr (in, "BPM", Memc[Memi[fnames]])
	    call mfree (fnames, TY_POINTER)
	    call sfree (sp)
	    return
	}

	# Combine the masks.
	call salloc (ims, n, TY_POINTER)
	call aclri (Memi[ims], n)
	iferr {
	    # Map the masks and register them to the input image.
	    do i = 0, n-1 {
		im = yt_pmmap (Memc[Memi[fnames+i]], in, Memc[fname], SZ_FNAME)
		Memi[ims+i] = im
	    }

	    # Map the output.
	    if (dir[1] != EOS && access (dir, 0, 0) == NO)
		call fmkdir (dir)
	    out = immap (Memc[outname], NEW_COPY, in)

	    # Merge the masks using a maximum.
	    nc = IM_LEN(in,1)
	    do j = 1, IM_LEN(in,2) {
		outbuf = impl2i (out, j)
		call aclri (Memi[outbuf], nc)
		do i = 0, n-1 {
		    im = Memi[ims+i]
		    call amaxi (Memi[imgl2i(im,j)], Memi[outbuf], Memi[outbuf],
			nc)
		}
	    }

	    call imunmap (out)

	    # Delete any existing BPM and set keyword to new BPM.
	    iferr {
		call imgstr (in, "BPM", Memc[fname], SZ_FNAME)
		call imdelete (Memc[fname])
	    } then
		;
	    call imastr (in, "BPM", Memc[outname])
	} then {
	    call erract (EA_WARN)
	    if (out != NULL) {
		call imunmap (out)
		iferr (call imdelete (Memc[outname]))
		    ;
	    }
	}

	# Finish up.
	do i = 0, n-1 {
	    im = Memi[ims+i]
	    if (im != NULL)
		call imunmap (im)
	}
	call mfree (fnames, TY_POINTER)
	call sfree (sp)
end


# CMBINE -- Combine images.
#
# This is a version of IMCOMBINE which groups data by CCD types, subsets
# (such as filter), and amplifier.  It also uses header keyword translation.
# The main routine takes care of sorting the input (both individual images
# and MEF files) by subset and amplifer using the routine cmb_images.  It
# then creates output root names and calls routines to do the combining of
# each group.

procedure cmbine (list, outroot, oneimage, outnames, nsubsets)

int	list			# List of images
char	outroot[SZ_FNAME]	# Output root image name
int	oneimage		# Allow only a single image to combine?
pointer	outnames		# Pointer to array of string pointers
pointer	subsets			# Subsets

pointer	images			# Images
pointer	hroot			# Headers root name
pointer	broot			# Bad pixel mask root name
pointer	rroot			# Rejection pixel mask root name
pointer	nrroot			# Number rejected mask root name
pointer	eroot			# Exposure mask root name
pointer	sigroot			# Sigma image name
pointer	logfile			# Log filename

pointer	scales			# Scales
pointer	zeros			# Zeros
pointer	wts			# Weights
pointer	extns			# Image extensions for each subset
pointer	nimages			# Number of images in each subset
int	nsubsets		# Number of subsets
int	delete			# Delete input images?

int	i, mef, list1
pointer	sp, output, headers, bmask, rmask, nrmask, emask, sigma

bool	clgetb()
int	clgeti(), clgwrd(), btoi(), errcode(), ic_mklist(), imtgetim()
real	clgetr()
errchk	cmb_images, icombine, mefcombine, ic_mklist

include	"src/icombine.com"

begin
	call smark (sp)
	call salloc (hroot, SZ_FNAME, TY_CHAR)
	call salloc (broot, SZ_FNAME, TY_CHAR)
	call salloc (rroot, SZ_FNAME, TY_CHAR)
	call salloc (nrroot, SZ_FNAME, TY_CHAR)
	call salloc (eroot, SZ_FNAME, TY_CHAR)
	call salloc (sigroot, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	call salloc (headers, SZ_FNAME, TY_CHAR)
	call salloc (bmask, SZ_FNAME, TY_CHAR)
	call salloc (rmask, SZ_FNAME, TY_CHAR)
	call salloc (nrmask, SZ_FNAME, TY_CHAR)
	call salloc (emask, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (expkeyword, SZ_FNAME, TY_CHAR)
	call salloc (statsec, SZ_FNAME, TY_CHAR)
	call salloc (gain, SZ_FNAME, TY_CHAR)
	call salloc (snoise, SZ_FNAME, TY_CHAR)
	call salloc (rdnoise, SZ_FNAME, TY_CHAR)

	# Get the input images.  There must be a least one image to continue.
	call cmb_images (list, images, scales, zeros, wts, extns, subsets,
	    nimages, nsubsets, mef)
	if (nsubsets == 0) {
	    call cmb_images_free (images, scales, zeros, wts, extns, subsets,
		nimages, nsubsets)
	    call error (0, "No data to combine")
	}

	# Check for more than one image.  MEF files are handled later.
	if (mef == NO && oneimage == NO) {
	    do i = 1, nsubsets {
		if (Memi[nimages+i-1] > 1)
		    break
	    }
	    if (i > nsubsets) {
		call cmb_images_free (images, scales, zeros, wts, extns,
		    subsets, nimages, nsubsets)
		call error (ONEIMAGE, "Only a single image to combine") 
		return
	    }
	}

	# Get task parameters.  Some additional parameters are obtained later.
	call clgstr ("headers", Memc[hroot], SZ_FNAME)
	call clgstr ("bpmasks", Memc[broot], SZ_FNAME)
	call clgstr ("rejmasks", Memc[rroot], SZ_FNAME)
	call clgstr ("nrejmasks", Memc[nrroot], SZ_FNAME)
	call clgstr ("expmasks", Memc[eroot], SZ_FNAME)
	call clgstr ("sigmas", Memc[sigroot], SZ_FNAME)
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	call xt_stripwhite (Memc[hroot])
	call xt_stripwhite (Memc[broot])
	call xt_stripwhite (Memc[rroot])
	call xt_stripwhite (Memc[nrroot])
	call xt_stripwhite (Memc[eroot])
	call xt_stripwhite (Memc[sigroot])
	call xt_stripwhite (Memc[logfile])

	project = clgetb ("project")
	combine = clgwrd ("combine", Memc[statsec], SZ_FNAME, COMBINE)
        reject = clgwrd ("reject", Memc[statsec], SZ_FNAME, REJECT)
        blank = clgetr ("blank")
	call strcpy ("exptime", Memc[expkeyword], SZ_FNAME)
	call clgstr ("statsec", Memc[statsec], SZ_FNAME)
        call clgstr ("gain", Memc[gain], SZ_FNAME)
        call clgstr ("rdnoise", Memc[rdnoise], SZ_FNAME)
        call clgstr ("snoise", Memc[snoise], SZ_FNAME)
        lthresh = clgetr ("lthreshold")
        hthresh = clgetr ("hthreshold")
        lsigma = clgetr ("lsigma")
	pclip = clgetr ("pclip")
	flow = clgetr ("nlow")
	fhigh = clgetr ("nhigh")
	nkeep = clgeti ("nkeep")
        hsigma = clgetr ("hsigma")
        grow = clgetr ("grow")
        mclip = clgetb ("mclip")
        sigscale = clgetr ("sigscale")
	verbose = clgetb ("verbose")
	delete = btoi (clgetb ("delete"))

	# Translate keywords.
	call hdmname (Memc[expkeyword], Memc[expkeyword], SZ_FNAME)
	call hdmname (Memc[gain], Memc[gain], SZ_FNAME)
	call hdmname (Memc[rdnoise], Memc[rdnoise], SZ_FNAME)
	call hdmname (Memc[snoise], Memc[snoise], SZ_FNAME)

        # Check parameters, map INDEFs, and set threshold flag
	if (pclip == 0. && reject == PCLIP)
	    call error (1, "Pclip parameter may not be zero")
        if (IS_INDEFR (blank))
            blank = 0.
        if (IS_INDEFR (lsigma))
            lsigma = MAX_REAL
        if (IS_INDEFR (hsigma))
            hsigma = MAX_REAL
	if (IS_INDEFR (pclip))
	    pclip = -0.5
	if (IS_INDEFR (flow))
	    flow = 0.
	if (IS_INDEFR (fhigh))
	    fhigh = 0.
        if (IS_INDEFR (grow))
            grow = 0.
        if (IS_INDEF (sigscale))
            sigscale = 0.

        if (IS_INDEF(lthresh) && IS_INDEF(hthresh))
            dothresh = false
        else {
            dothresh = true
            if (IS_INDEF(lthresh))
                lthresh = -MAX_REAL
            if (IS_INDEF(hthresh))
                hthresh = MAX_REAL
        }

	# Combine each input subset.
	call calloc (outnames, nsubsets, TY_POINTER)
	do i = 1, nsubsets {
	    # Set the output, names with subset extension.
	    call malloc (Memi[outnames+i-1], SZ_FNAME, TY_CHAR)

	    output = Memi[outnames+i-1]
	    call strcpy (outroot, Memc[output], SZ_FNAME)
	    call sprintf (Memc[output], SZ_FNAME, "%s%s")
		call pargstr (outroot)
		call pargstr (Memc[Memi[extns+i-1]])

	    call strcpy (Memc[hroot], Memc[headers], SZ_FNAME)
	    if (Memc[headers] != EOS) {
		call sprintf (Memc[headers], SZ_FNAME, "%s%s")
		    call pargstr (Memc[hroot])
		    call pargstr (Memc[Memi[extns+i-1]])
	    }

	    call strcpy (Memc[broot], Memc[bmask], SZ_FNAME)
	    if (Memc[bmask] != EOS) {
		call sprintf (Memc[bmask], SZ_FNAME, "%s%s")
		    call pargstr (Memc[broot])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    call strcpy (Memc[rroot], Memc[rmask], SZ_FNAME)
	    if (Memc[rmask] != EOS) {
		call sprintf (Memc[rmask], SZ_FNAME, "%s%s")
		    call pargstr (Memc[rroot])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    call strcpy (Memc[nrroot], Memc[nrmask], SZ_FNAME)
	    if (Memc[nrmask] != EOS) {
		call sprintf (Memc[nrmask], SZ_FNAME, "%s%s")
		    call pargstr (Memc[nrmask])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    call strcpy (Memc[eroot], Memc[emask], SZ_FNAME)
	    if (Memc[emask] != EOS) {
		call sprintf (Memc[emask], SZ_FNAME, "%s%s")
		    call pargstr (Memc[eroot])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    call strcpy (Memc[sigroot], Memc[sigma], SZ_FNAME)
	    if (Memc[sigma] != EOS) {
		call sprintf (Memc[sigma], SZ_FNAME, "%s%s")
		    call pargstr (Memc[sigroot])
		    call pargstr (Memc[Memi[extns+i-1]])
	    }

	    # Combine all images from the (subset) list.
	    iferr {
		if (mef == YES)
		    call mefcombine (Memc[Memi[images+i-1]],
			Memr[Memi[scales+i-1]], Memr[Memi[zeros+i-1]],
			Memr[Memi[wts+i-1]], Memi[nimages+i-1],
			Memc[output], Memc[headers], Memc[bmask], Memc[rmask],
			Memc[nrmask], Memc[emask], Memc[sigma],
			Memc[logfile], NO, delete, oneimage)
		else {
		    list1 = ic_mklist (Memc[Memi[images+i-1]],
			Memi[nimages+i-1])

		    call icombine (list1, Memc[output], Memc[headers],
			Memc[bmask], Memc[rmask], Memc[nrmask],
			Memc[emask], Memc[sigma], Memc[logfile],
			Memr[Memi[scales+i-1]], Memr[Memi[zeros+i-1]],
			Memr[Memi[wts+i-1]], NO, NO, NO)

		    if (!project && delete == YES) {
			call imtrew (list1)
			while (imtgetim (list1, Memc[output], SZ_FNAME) != EOF)
			    call ccddelete (Memc[output])
		    }
		    call imtclose (list1)
		}
	    } then {
		if (errcode() == ONEIMAGE)
		    call erract (EA_ERROR)
		call erract (EA_WARN)
	    }
	    call mfree (Memi[images+i-1], TY_CHAR)
	    call mfree (Memi[scales+i-1], TY_REAL)
	    call mfree (Memi[zeros+i-1], TY_REAL)
	    call mfree (Memi[wts+i-1], TY_REAL)
	    call mfree (Memi[extns+i-1], TY_CHAR)
	    call mfree (Memi[subsets+i-1], TY_CHAR)
	}

	# Finish up.
	call cmb_images_free (images, scales, zeros, wts, extns, subsets,
	    nimages, nsubsets)
	call sfree (sp)
end


# CMB_IMAGES_FREE -- Free memory allocated by CMB_IMAGES.

procedure cmb_images_free (images, scales, zeros, wts, extns, subsets, nimages,
	nsubsets)

pointer	images			#U Pointer to image names in subset
pointer	scales			#U Pointer to scales in subset
pointer	zeros			#U Pointer to zeros in subset
pointer	wts			#U Pointer to weights in subset
pointer	extns			#U Pointer to extension name in subset
pointer	subsets			#U Pointer to subset name in subset
pointer	nimages			#U Pointer to number of images in subset
int	nsubsets		#I Number of subsets

int	i

begin
	do i = 1, nsubsets {
	    call mfree (Memi[images+i-1], TY_CHAR)
	    call mfree (Memi[scales+i-1], TY_REAL)
	    call mfree (Memi[zeros+i-1], TY_REAL)
	    call mfree (Memi[wts+i-1], TY_REAL)
	    call mfree (Memi[extns+i-1], TY_CHAR)
	    call mfree (Memi[subsets+i-1], TY_CHAR)
	}
	call mfree (images, TY_POINTER)
	call mfree (scales, TY_POINTER)
	call mfree (zeros, TY_POINTER)
	call mfree (wts, TY_POINTER)
	call mfree (extns, TY_POINTER)
	call mfree (subsets, TY_POINTER)
	call mfree (nimages, TY_INT)
end


# COUTPUT -- Print list of combine output images.
#
# This routine prints the output names that COMBINE will use.

procedure coutput (inlist, outroot)

int	inlist			# List of input images
char	outroot[ARB]		# Output root image name
pointer	images			# Images
pointer	hroot			# Headers
pointer	broot			# Bad pixels masks
pointer	rroot			# Rejection pixel masks
pointer	nrroot			# Number rejected pixel masks
pointer	eroot			# Exposure masks
pointer	sigroot			# Output root sigma image name
pointer	list			# Output list of names

pointer	scales			# Scales
pointer	zeros			# Zeros
pointer	wts			# Weights
pointer	extns			# Image extensions for each subset
pointer	subsets			# Subsets
pointer	nimages			# Number of images in each subset
int	nsubsets		# Number of subsets

int	i, mef, fd, open()
pointer	sp

errchk	cmb_images, open

include	"src/icombine.com"

begin
	call smark (sp)
	call salloc (hroot, SZ_FNAME, TY_CHAR)
	call salloc (broot, SZ_FNAME, TY_CHAR)
	call salloc (rroot, SZ_FNAME, TY_CHAR)
	call salloc (nrroot, SZ_FNAME, TY_CHAR)
	call salloc (eroot, SZ_FNAME, TY_CHAR)
	call salloc (sigroot, SZ_FNAME, TY_CHAR)
	call salloc (list, SZ_FNAME, TY_CHAR)

	# Get the input images.  There must be a least one image to continue.
	call cmb_images (inlist, images, scales, zeros, wts, extns, subsets,
	    nimages, nsubsets, mef)
	if (nsubsets == 0) {
	    call cmb_images_free (images, scales, zeros, wts, extns, subsets,
		nimages, nsubsets)
	    call error (0, "No data to combine")
	}

	# Get task parameters.  Some additional parameters are obtained later.

	call clgstr ("headers", Memc[hroot], SZ_FNAME)
	call clgstr ("bpmasks", Memc[broot], SZ_FNAME)
	call clgstr ("rejmasks", Memc[rroot], SZ_FNAME)
	call clgstr ("nrejmasks", Memc[nrroot], SZ_FNAME)
	call clgstr ("expmasks", Memc[eroot], SZ_FNAME)
	call clgstr ("sigmas", Memc[sigroot], SZ_FNAME)
	call clgstr ("list", Memc[list], SZ_FNAME)
	call xt_stripwhite (Memc[hroot])
	call xt_stripwhite (Memc[broot])
	call xt_stripwhite (Memc[rroot])
	call xt_stripwhite (Memc[nrroot])
	call xt_stripwhite (Memc[eroot])
	call xt_stripwhite (Memc[sigroot])

	# Print output images.
	fd = open (Memc[list], NEW_FILE, TEXT_FILE)
	do i = 1, nsubsets {
	    call fprintf (fd, "%s%s")
		call pargstr (outroot)
		call pargstr (Memc[Memi[extns+i-1]])
	    if (Memc[hroot] != EOS) {
		call fprintf (fd, " %s%s")
		    call pargstr (Memc[hroot])
		    call pargstr (Memc[Memi[extns+i-1]])
	    }
	    if (Memc[broot] != EOS) {
		call fprintf (fd, " %s%s")
		    call pargstr (Memc[broot])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }
	    if (Memc[rroot] != EOS) {
		call fprintf (fd, " %s%s")
		    call pargstr (Memc[rroot])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }
	    if (Memc[nrroot] != EOS) {
		call fprintf (fd, " %s%s")
		    call pargstr (Memc[nrroot])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }
	    if (Memc[eroot] != EOS) {
		call fprintf (fd, " %s%s")
		    call pargstr (Memc[eroot])
		    # Use this if we can append pl files.
		    #call pargstr (Memc[Memi[extns+i-1]])
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }
	    if (Memc[sigroot] != EOS) {
		call fprintf (fd, " %s%s")
		    call pargstr (Memc[sigroot])
		    call pargstr (Memc[Memi[extns+i-1]])
	    }
	    call fprintf (fd, "\n")

	    call mfree (Memi[images+i-1], TY_CHAR)
	    call mfree (Memi[scales+i-1], TY_REAL)
	    call mfree (Memi[zeros+i-1], TY_REAL)
	    call mfree (Memi[wts+i-1], TY_REAL)
	    call mfree (Memi[extns+i-1], TY_CHAR)
	    call mfree (Memi[subsets+i-1], TY_CHAR)
	}
	call close (fd)

	# Finish up.
	call cmb_images_free (images, scales, zeros, wts, extns, subsets,
	    nimages, nsubsets)
	call sfree (sp)
end


# CMB_IMAGES -- Get images, scales, zeros, and weights from a list of images.
# The images are filtered by ccdtype and sorted by amplifier and subset.
# The allocated lists must be freed by the caller.

procedure cmb_images (list, images, scales, zeros, wts, extns, subsets,
	nimages, nsubsets, mef)

int	list		# List of input images
pointer	images		# Pointer to lists of subsets (allocated)
pointer	scales		# Pointer to array of scales (allocated)
pointer	zeros		# Pointer to array of zeros (allocated)
pointer	wts		# Pointer to array of weights (allocated)
pointer	extns		# Image extensions for each subset (allocated)
pointer	subsets		# Subset names (allocated)
pointer	nimages		# Number of images in subset (allocated)
int	nsubsets	# Number of subsets
int	mef		#O MEF data?

bool	doamps		# Divide input into subsets by amplifier?
bool	dosubsets	# Divide input into subsets by subset parameter?

int	i, j, nims, nimage, ccdtype, fd
pointer	sp, type, image, extn, subset, str, scale, zero, wt, ptr, im

int	imtlen(), imtgetim(), errcode(), ccdtypecl(), ccdtypes()
int	nowhite(), open(), fscan(), nscan()
pointer	immap()
bool	clgetb(), streq()
errchk	immap, open

begin
	# Check that there is at least one image.
	nsubsets = 0
	nims = imtlen (list)
	if (nims == 0)
	    return

	# Determine whether to divide images into subsets and append extensions.
	doamps = clgetb ("amps")
	dosubsets = clgetb ("subsets")

	call smark (sp)
	call salloc (type, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (subset, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (scale, nims, TY_REAL)
	call salloc (zero, nims, TY_REAL)
	call salloc (wt, nims, TY_REAL)

	# Since we may eliminate images or reorder them we need to get the
	# scale, zero and weight values from input files where the values
	# are in the same order as the input images.

	call clgstr ("scale", Memc[str], SZ_FNAME)
	j = nowhite (Memc[str], Memc[str], SZ_FNAME)
	if (Memc[str] == '@') {
	    fd = open (Memc[str+1], READ_ONLY, TEXT_FILE)
	    j = 0
	    while (fscan (fd) != EOF) {
		call gargr (Memr[scale+j])
		if (nscan() != 1)
		    next
		if (j == nims) {
                  call eprintf (
                       "Warning: Ignoring additional %s values in %s\n")
                       call pargstr ("scale")
                       call pargstr (Memc[str+1])
                   break
                }
                j = j + 1
	    }
	    call close (fd)

	    if (j < nims) {
		call sprintf (Memc[type], SZ_FNAME,
		    "Insufficient scale values in %s")
		    call pargstr (Memc[str+1])
		call error (1, Memc[type])
	    }
	} else
	    call amovkr (INDEFR, Memr[scale], nims)

	call clgstr ("zero", Memc[str], SZ_FNAME)
	j = nowhite (Memc[str], Memc[str], SZ_FNAME)
	if (Memc[str] == '@') {
	    fd = open (Memc[str+1], READ_ONLY, TEXT_FILE)
	    j = 0
	    while (fscan (fd) != EOF) {
		call gargr (Memr[zero+j])
		if (nscan() != 1)
		    next
		if (j == nims) {
                  call eprintf (
                       "Warning: Ignoring additional %s values in %s\n")
                       call pargstr ("zero")
                       call pargstr (Memc[str+1])
                   break
                }
                j = j + 1
	    }
	    call close (fd)

	    if (j < nims) {
		call sprintf (Memc[type], SZ_FNAME,
		    "Insufficient zero values in %s")
		    call pargstr (Memc[str+1])
		call error (1, Memc[type])
	    }
	} else
	    call amovkr (INDEFR, Memr[zero], nims)

	call clgstr ("weight", Memc[str], SZ_FNAME)
	j = nowhite (Memc[str], Memc[str], SZ_FNAME)
	if (Memc[str] == '@') {
	    fd = open (Memc[str+1], READ_ONLY, TEXT_FILE)
	    j = 0
	    while (fscan (fd) != EOF) {
		call gargr (Memr[wt+j])
		if (nscan() != 1)
		    next
		if (j == nims) {
                  call eprintf (
                       "Warning: Ignoring additional %s values in %s\n")
                       call pargstr ("weight")
                       call pargstr (Memc[str+1])
                   break
                }
                j = j + 1
	    }
	    call close (fd)

	    if (j < nims) {
		call sprintf (Memc[type], SZ_FNAME,
		    "Insufficient weight values in %s")
		    call pargstr (Memc[str+1])
		call error (1, Memc[type])
	    }
	} else
	    call amovkr (INDEFR, Memr[wt], nims)

	# Go through the input list and eliminate images not satisfying the
	# CCD image type.  Separate into subsets if desired.  Create image,
	# scale, zero, weight, and subset lists.  Determine if the input
	# is MEF data.

	ccdtype = ccdtypecl ("ccdtype", Memc[type], SZ_FNAME)

	mef = INDEFI
	j = 0
	while (imtgetim (list, Memc[image], SZ_FNAME)!=EOF) {
	    j = j + 1
	    iferr {
		if (IS_INDEFI(mef)) {
		    ifnoerr (im = immap (Memc[image], READ_ONLY, 0))
			mef = NO
		    else {
			switch (errcode()) {
			case SYS_FXFOPNOEXTNV:
			    call sprintf (Memc[str], SZ_FNAME, "%s[1]")
				call pargstr (Memc[image])
			    im = immap (Memc[str], READ_ONLY, 0)
			    mef = YES
			default:
			    call erract (EA_ERROR)
			}
		    }
		} else if (mef == NO)
		    im = immap (Memc[image], READ_ONLY, 0)
		else {
		    call sprintf (Memc[str], SZ_FNAME, "%s[1]")
			call pargstr (Memc[image])
		    im = immap (Memc[str], READ_ONLY, 0)
		}
	    } then {
		call erract (EA_WARN)
		next
	    }
	    ccdtype = ccdtypes (im, Memc[str], SZ_FNAME)
	    if (Memc[type] != EOS && !streq (Memc[str], Memc[type]))
		next
	    
	    Memc[extn] = EOS
	    Memc[subset] = EOS
	    if (doamps) {
		call ic_grp (im, Memc[str], SZ_FNAME)
		if (mef == NO)
		    call strcat (Memc[str], Memc[extn], SZ_FNAME)
		call strcat (Memc[str], Memc[subset], SZ_FNAME)
	    }
	    if (dosubsets) {
		call ccdsubset (im, Memc[str], SZ_FNAME)
		call strcat (Memc[str], Memc[extn], SZ_FNAME)
		call strcat (Memc[str], Memc[subset], SZ_FNAME)
	    }
	    for (i=1; i <= nsubsets; i=i+1)
		if (streq (Memc[subset], Memc[Memi[subsets+i-1]]))
		    break

	    if (i > nsubsets) {
		if (nsubsets == 0) {
		    call malloc (images, nims, TY_POINTER)
		    call malloc (scales, nims, TY_POINTER)
		    call malloc (zeros, nims, TY_POINTER)
		    call malloc (wts, nims, TY_POINTER)
		    call malloc (extns, nims, TY_POINTER)
		    call malloc (subsets, nims, TY_POINTER)
		    call malloc (nimages, nims, TY_INT)
		} else if (mod (nsubsets, nims) == 0) {
		    call realloc (images, nsubsets+nims, TY_POINTER)
		    call realloc (scales, nsubsets+nims, TY_POINTER)
		    call realloc (zeros, nsubsets+nims, TY_POINTER)
		    call realloc (wts, nsubsets+nims, TY_POINTER)
		    call realloc (extns, nsubsets+nims, TY_POINTER)
		    call realloc (subsets, nsubsets+nims, TY_POINTER)
		    call realloc (nimages, nsubsets+nims, TY_INT)
		}
		nsubsets = i
		nimage = 1
		Memi[nimages+i-1] = nimage
		call malloc (Memi[images+i-1], nimage * SZ_FNAME, TY_CHAR)
		call malloc (Memi[scales+i-1], nimage, TY_REAL)
		call malloc (Memi[zeros+i-1], nimage, TY_REAL)
		call malloc (Memi[wts+i-1], nimage, TY_REAL)
		call malloc (Memi[extns+i-1], SZ_FNAME, TY_CHAR)
		call malloc (Memi[subsets+i-1], SZ_FNAME, TY_CHAR)

		call strcpy (Memc[extn], Memc[Memi[extns+i-1]], SZ_FNAME)
		call strcpy (Memc[subset], Memc[Memi[subsets+i-1]], SZ_FNAME)
	    } else {
		nimage = Memi[nimages+i-1] + 1
		Memi[nimages+i-1] = nimage
		call realloc (Memi[images+i-1], nimage * SZ_FNAME, TY_CHAR)
		call realloc (Memi[scales+i-1], nimage, TY_REAL)
		call realloc (Memi[zeros+i-1], nimage, TY_REAL)
		call realloc (Memi[wts+i-1], nimage, TY_REAL)
	    }

	    nimage = Memi[nimages+i-1]
	    ptr = Memi[images+i-1] + (nimage - 1) * SZ_FNAME
	    call strcpy (Memc[image], Memc[ptr], SZ_FNAME-1)
	    Memr[Memi[scales+i-1]+nimage-1] = Memr[scale+j-1]
	    Memr[Memi[zeros+i-1]+nimage-1] = Memr[zero+j-1]
	    Memr[Memi[wts+i-1]+nimage-1] = Memr[wt+j-1]

	    call imunmap (im)
	}
	call realloc (images, nsubsets, TY_POINTER)
	call realloc (scales, nsubsets, TY_POINTER)
	call realloc (zeros, nsubsets, TY_POINTER)
	call realloc (wts, nsubsets, TY_POINTER)
	call realloc (extns, nsubsets, TY_POINTER)
	call realloc (subsets, nsubsets, TY_POINTER)
	call realloc (nimages, nsubsets, TY_INT)
	call sfree (sp)
end


# MEFCOMBINE -- Combine MEF data.
#
# This routine receives a list of input MEF files already sorted by
# subset (i.e. filter) with appropriate output file names.  This routine
# must then group the image extensions by amplifier and set up the
# scaling factors, which are the same for all extensions from the
# same image.  At the end of combining all the extensions it averages
# any CCDMEAN keywords so that there is a common value for all the extensions.
#
# If there is only one output extension then an PHU only image is produced.

procedure mefcombine (ims, scale, zero, wt, nims, output, headers, broot,
	rroot, nrroot, eroot, sigma, logfile, stack, delete, oneimage)

char	ims[SZ_FNAME-1, nims]		# Input images
real	scale[nims]			# Scales
real	zero[nims]			# Zeros
real	wt[nims]			# Weights
int	nims				# Number of images in list
char	output[ARB]			# Output image
char	headers[ARB]			# Header files
char	broot[ARB]			# Bad pixel mask
char	rroot[ARB]			# Rejection pixel mask
char	nrroot[ARB]			# Number rejected pixel mask
char	eroot[ARB]			# Exposure mask
char	sigma[ARB]			# Output sigma image
char	logfile[ARB]			# Log filename
int	stack				# Stack input images?
int	delete				# Delete input images?
int	oneimage			# Allow just a single image?

int	i, j, k, nsubsets, nimage, ghdr, list
real	ccdmean, sum
pointer	sp, image, subset, bmask, rmask, nrmask, emask, im, ptr
pointer	images, iimage, scales, zeros, wts, subsets, nimages

real	imgetr()
bool	streq()
int	errcode(), imaccess(), ic_mklist()
pointer	immap()
errchk	immap, imcopy, icombine, mefscales, ic_mklist

include	"src/icombine.com"

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (subset, SZ_FNAME, TY_CHAR)
	call salloc (bmask, SZ_FNAME, TY_CHAR)
	call salloc (rmask, SZ_FNAME, TY_CHAR)
	call salloc (nrmask, SZ_FNAME, TY_CHAR)
	call salloc (emask, SZ_FNAME, TY_CHAR)

	# Expand MEF files and group by amplifier.
	ghdr = NO
	nsubsets = 0
	do k = 1, nims {
	    do j = 0, ARB {
		call sprintf (Memc[image], SZ_FNAME, "%s[%d]")
		    call pargstr (ims[1,k])
		    call pargi (j)
		iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		    switch (errcode()) {
		    case SYS_FXFRFEOF, SYS_IKIOPEN:
			break
		    case SYS_IKIEXTN:
			next
		    default:
			call erract (EA_ERROR)
		    }
		}
		if (IM_NDIM(im) == 0) {
		    if (j == 0)
			ghdr = YES
		    call imunmap (im)
		    next
		}

		call ic_grp (im, Memc[subset], SZ_FNAME)
		if (Memc[subset] == EOS) {
		    call sprintf (Memc[subset], SZ_FNAME, "%d")
			call pargi (j)
		}
		for (i=1; i <= nsubsets; i=i+1)
		    if (streq (Memc[subset], Memc[Memi[subsets+i-1]]))
			break

		if (i > nsubsets) {
		    if (nsubsets == 0) {
			call malloc (images, nims, TY_POINTER)
			call malloc (iimage, nims, TY_POINTER)
			call malloc (scales, nims, TY_POINTER)
			call malloc (zeros, nims, TY_POINTER)
			call malloc (wts, nims, TY_POINTER)
			call malloc (subsets, nims, TY_POINTER)
			call malloc (nimages, nims, TY_INT)
		    } else if (mod (nsubsets, nims) == 0) {
			call realloc (images, nsubsets+nims, TY_POINTER)
			call realloc (iimage, nsubsets+nims, TY_POINTER)
			call realloc (scales, nsubsets+nims, TY_POINTER)
			call realloc (zeros, nsubsets+nims, TY_POINTER)
			call realloc (wts, nsubsets+nims, TY_POINTER)
			call realloc (subsets, nsubsets+nims, TY_POINTER)
			call realloc (nimages, nsubsets+nims, TY_INT)
		    }
		    nsubsets = i
		    nimage = 1
		    Memi[nimages+i-1] = nimage
		    call malloc (Memi[images+i-1], nimage * SZ_FNAME, TY_CHAR)
		    call malloc (Memi[iimage+i-1], nimage, TY_INT)
		    call malloc (Memi[scales+i-1], nimage, TY_REAL)
		    call malloc (Memi[zeros+i-1], nimage, TY_REAL)
		    call malloc (Memi[wts+i-1], nimage, TY_REAL)
		    call malloc (Memi[subsets+i-1], SZ_FNAME, TY_CHAR)

		    call strcpy (Memc[subset], Memc[Memi[subsets+i-1]],
			SZ_FNAME)
		} else {
		    nimage = Memi[nimages+i-1] + 1
		    Memi[nimages+i-1] = nimage
		    call realloc (Memi[images+i-1], nimage * SZ_FNAME, TY_CHAR)
		    call realloc (Memi[iimage+i-1], nimage, TY_INT)
		    call realloc (Memi[scales+i-1], nimage, TY_REAL)
		    call realloc (Memi[zeros+i-1], nimage, TY_REAL)
		    call realloc (Memi[wts+i-1], nimage, TY_REAL)
		}

		nimage = Memi[nimages+i-1]
		ptr = Memi[images+i-1] + (nimage - 1) * SZ_FNAME
		call strcpy (Memc[image], Memc[ptr], SZ_FNAME-1)
		Memi[Memi[iimage+i-1]+nimage-1] = k
		Memr[Memi[scales+i-1]+nimage-1] = scale[k]
		Memr[Memi[zeros+i-1]+nimage-1] = zero[k]
		Memr[Memi[wts+i-1]+nimage-1] = wt[k]

		call imunmap (im)
	    }
	}
	call realloc (images, nsubsets, TY_POINTER)
	call realloc (iimage, nsubsets, TY_POINTER)
	call realloc (scales, nsubsets, TY_POINTER)
	call realloc (zeros, nsubsets, TY_POINTER)
	call realloc (wts, nsubsets, TY_POINTER)
	call realloc (subsets, nsubsets, TY_POINTER)
	call realloc (nimages, nsubsets, TY_INT)

	# Check number of images.
	if (oneimage == NO) {
	    do i = 1, nsubsets {
		if (Memi[nimages+i-1] > 1)
		    break
	    }
	    if (i > nsubsets) {
		do i = 1, nsubsets {
		    call mfree (Memi[images+i-1], TY_CHAR)
		    call mfree (Memi[iimage+i-1], TY_INT)
		    call mfree (Memi[scales+i-1], TY_REAL)
		    call mfree (Memi[zeros+i-1], TY_REAL)
		    call mfree (Memi[wts+i-1], TY_REAL)
		    call mfree (Memi[subsets+i-1], TY_CHAR)
		}
		call mfree (images, TY_POINTER)
		call mfree (iimage, TY_POINTER)
		call mfree (scales, TY_POINTER)
		call mfree (zeros, TY_POINTER)
		call mfree (wts, TY_POINTER)
		call mfree (subsets, TY_POINTER)
		call mfree (nimages, TY_INT)
		call error (ONEIMAGE, "Only single images to combine") 
	    }
	}

	# Compute scaling factors if needed.
	call mefscales (Memi[images], Memi[iimage], Memi[nimages], nsubsets,
	    scale, zero, wt, nims)
	do i = 1, nsubsets {
	    do j = 1, Memi[nimages+i-1] {
		k = Memi[Memi[iimage+i-1]+j-1]
		Memr[Memi[scales+i-1]+j-1] = scale[k]
		Memr[Memi[zeros+i-1]+j-1] = zero[k]
		Memr[Memi[wts+i-1]+j-1] = wt[k]
	    }
	}

	# Create the global headers.
	if (ghdr == YES && nsubsets > 1) {
	    if (imaccess (output, 0) == YES) {
		call sprintf (Memc[image], SZ_FNAME,
		    "Output `%s' already exists")
		    call pargstr (output)
		call error (1, Memc[image])
	    }
	    call sprintf (Memc[image], SZ_FNAME, "%s[0]")
		call pargstr (ims[1,1])
	    im = immap (Memc[image], READ_ONLY, 0)
	    call sprintf (Memc[image], SZ_FNAME, "%s[noappend]")
		call pargstr (output)
	    ptr = immap (Memc[image], NEW_COPY, im)
	    call imunmap (ptr)
	    if (sigma[1] != EOS) {
		call sprintf (Memc[image], SZ_FNAME, "%s[noappend]")
		    call pargstr (sigma)
		ptr = immap (Memc[image], NEW_COPY, im)
		call imunmap (ptr)
	    }
	    call imunmap (im)
	}

	# Combine each extension.
	do i = 1, nsubsets {
	    # Add inherit parameter to output name.
	    if (nsubsets > 1) {
		call sprintf (Memc[image], SZ_FNAME, "%s[inherit]")
		    call pargstr (output)
	    } else
		call strcpy (output, Memc[image], SZ_FNAME)

	    # Since we can't append pl files add an extension.
	    call strcpy (broot, Memc[bmask], SZ_FNAME)
	    if (Memc[bmask] != EOS) {
		call sprintf (Memc[bmask], SZ_FNAME, "%s%s")
		    call pargstr (broot)
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    # Since we can't append pl files add an extension.
	    call strcpy (rroot, Memc[rmask], SZ_FNAME)
	    if (Memc[rmask] != EOS) {
		call sprintf (Memc[rmask], SZ_FNAME, "%s%s")
		    call pargstr (rroot)
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    # Since we can't append pl files add an extension.
	    call strcpy (nrroot, Memc[nrmask], SZ_FNAME)
	    if (Memc[nrmask] != EOS) {
		call sprintf (Memc[nrmask], SZ_FNAME, "%s%s")
		    call pargstr (rroot)
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    # Since we can't append pl files add an extension.
	    call strcpy (eroot, Memc[emask], SZ_FNAME)
	    if (Memc[emask] != EOS) {
		call sprintf (Memc[emask], SZ_FNAME, "%s%s")
		    call pargstr (eroot)
		    call pargstr (Memc[Memi[subsets+i-1]])
	    }

	    # Combine all images from the (subset) list.
	    list = ic_mklist (Memc[Memi[images+i-1]], Memi[nimages+i-1])

	    iferr (call icombine (list, Memc[image],  headers, Memc[bmask],
		Memc[rmask], Memc[nrmask], Memc[emask], sigma, logfile,
		Memr[Memi[scales+i-1]], Memr[Memi[zeros+i-1]],
		Memr[Memi[wts+i-1]], stack, NO, NO)) {
		iferr (call imdelete (output))
		    ;
		call erract (EA_ERROR)
	    }

	    call imtclose (list)
	    call mfree (Memi[images+i-1], TY_CHAR)
	    call mfree (Memi[iimage+i-1], TY_INT)
	    call mfree (Memi[scales+i-1], TY_REAL)
	    call mfree (Memi[zeros+i-1], TY_REAL)
	    call mfree (Memi[wts+i-1], TY_REAL)
	    call mfree (Memi[subsets+i-1], TY_CHAR)
	}
	call mfree (images, TY_POINTER)
	call mfree (iimage, TY_POINTER)
	call mfree (scales, TY_POINTER)
	call mfree (zeros, TY_POINTER)
	call mfree (wts, TY_POINTER)
	call mfree (subsets, TY_POINTER)
	call mfree (nimages, TY_INT)

	# Reset MEF header.
	# Set global ccdmean.
	if (nsubsets > 1) {
	    sum = 0
	    i = 0.
	    do j = nsubsets, 0, -1 {
		call sprintf (Memc[image], SZ_FNAME, "%s[%d]")
		    call pargstr (output)
		    call pargi (j)
		im = immap (Memc[image], READ_WRITE, 0)
		if (j > 0) {
		    ifnoerr (ccdmean = imgetr (im, "ccdmean")) {
			sum = sum + ccdmean
			i = i + 1
			call imdelf (im, "ccdmean")
		    }
		} else if (i > 0) {
		    ccdmean = sum / i
		    call imaddr (im, "ccdmean", ccdmean)
		}
		call imunmap (im)
	    }
	}

	# Delete input images.
	if (delete == YES) {
	    do i = 1, nims
		call ccddelete (ims[1,i])
	}

	call sfree (sp)
end


procedure ic_grp (im, amp, maxchar)

pointer	im		#I IMIO pointer
char	amp[ARB]	#O Grouping string
int	maxchar		#I Size of grouping string

int	grp
common	/grpcom/ grp

begin
	switch (grp) {
	case GRPAMP:
	    call ccdamp (im, amp, maxchar)
	case GRPCCD:
	    call ccdname (im, amp, maxchar)
	default:
	    call ccdamp (im, amp, maxchar)
	}
end


# IC_MKLIST -- Convert images names into an image list.

int procedure ic_mklist (images, nimages)

char	images[SZ_FNAME-1,nimages]		#I Image names
int	nimages					#I Number of images
int	list					#O Image list

int	i, fd, stropen(), imtopen()
pointer	sp, str
errchk	salloc, stropen, imtopen

begin
	call smark (sp)
	call salloc (str, nimages*SZ_FNAME, TY_CHAR)

	fd = stropen (Memc[str], nimages*SZ_FNAME, NEW_FILE)
	do i = 1, nimages {
	    call fprintf (fd, "%s,")
		call pargstr (images[1,i])
	}
	call close (fd)
	list = imtopen (Memc[str])

	call sfree (sp)
	return (list)
end
