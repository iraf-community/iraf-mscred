include	<error.h>
include	<imhdr.h>


# T_MSCGMASK -- Get masks for specified images.
# This takes a parent mask and matches it with an image in WCS and outputs
# mask of the same size as the input image.

procedure t_mscgmask ()

int	ilist			# List of input images
int	olist			# List of output masks
int	mlist			# List of parent masks
short	mval			# Mask value
bool	empty			# All masks empty?

int	i, j
long	vin[IM_MAXDIM], vout[IM_MAXDIM]
pointer	sp, input, output, mask, im, pmim, mw, tmp, bufin, bufout

short	clgets()
bool	im_pmlnev()
int	imtopenp(), imtgetim(), imtlen(), strmatch(), imgnls(), impnls()
pointer	immap(), mw_openim(), yt_pmmap()
errchk	strmatch, immap, mw_openim, yt_pmmap, mw_saveim

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (mask, SZ_FNAME, TY_CHAR)

	# Get lists and check if they match.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	mlist = imtopenp ("masks")
	mval = clgets ("mval")
	empty = true

	i = imtlen (ilist)
	j = imtlen (olist)
	if (j != i)
	    call error (1, "Input and output lists don't match")
	    
	j = imtlen (mlist)
	if (j == 0 || (j > 1 && j != i))
	    call error (1, "Image and mask lists are incompatible")

	# Extract the masks.
	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    iferr {
		i = imtgetim (olist, Memc[output], SZ_FNAME)
		if (strmatch (Memc[output], ".pl$") == 0)
		    call strcat (".pl", Memc[output], SZ_FNAME)

		if (imtgetim (mlist, Memc[mask], SZ_FNAME) == EOF) {
		    call imtrew (mlist)
		    i = imtgetim (mlist, Memc[mask], SZ_FNAME)
		}

		im = NULL
		mw = NULL
		pmim = NULL

		tmp = immap (Memc[input], READ_ONLY, 0)
		im = tmp
		tmp = mw_openim (im)
		mw = tmp
		tmp = yt_pmmap (Memc[mask], im, Memc[mask], SZ_FNAME)
		pmim = tmp
		if (tmp == NULL) {
		    tmp = yt_pmmap ("EMPTY", im, Memc[mask], SZ_FNAME)
		    pmim = tmp
		}
		call imunmap (im)
		tmp = immap (Memc[output], NEW_COPY, pmim)
		im = tmp
		call mw_saveim (mw, im)
		call mw_close (mw)

		j = IM_LEN(im,1)
		call amovkl (long(1), vin, IM_MAXDIM)
		call amovkl (long(1), vout, IM_MAXDIM)
		while (impnls (im, bufout, vout) != EOF) {
		    if (im_pmlnev (pmim, vin)) {
			i = imgnls (pmim, bufin, vin)
			do i = 0, j-1 {
			    if (Mems[bufin+i] == 0)
				Mems[bufout+i] = 0
			    else
				Mems[bufout+i] = mval
			}
			empty = false
		    } else {
			call amovl (vout, vin, IM_MAXDIM)
			call aclrs (Mems[bufout], j)
		    }
		}
	    } then
		call erract (EA_WARN)

	    if (mw != NULL)
		call mw_close (mw)
	    if (im != NULL)
		call imunmap (im)
	    if (pmim != NULL)
		call imunmap (pmim)

	}
	call clputb ("empty", empty)

	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (mlist)
	call sfree (sp)
end
