# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<imhdr.h>
include <mach.h>
include <fset.h>
include <tbset.h>
include	"rfits.h"

# RGF_READ_TFITS --  Read FITS table in row order as character strings.
# This routine is called only when the user has asked to convert
# a FITS file with an extra dimension and a table attached to it
# to a multigroup file.

procedure rgf_read_tfits (fits_fd, im, fits)

int	fits_fd		# FITS file descriptor
pointer im		# Image descriptor
pointer	fits		# FITS data structure

int	i, rowlen, blksize, nch
int	ngroups, gn, pcount
pointer	sp, bfp

int	rft_init_read_pixels(), rft_read_pixels()
int	npix_record, fstati(), gi_gstfval()

errchk	salloc, sfree, rft_init_read_pixels, rft_read_pixels
errchk	smark

include	"rfits.com"
include "tab.com"

begin

	rowlen = FITS_ROWLEN(fits)
	ngroups = GCOUNT(fits)

	# Reset value of PSIZE to the real one since rgf_get_table_val
	# will use it to calculate the size of the gpb.
	call gi_pstfval (im,"PSIZE", OPSIZE(fits))

	pcount = gi_gstfval (im, "PCOUNT")

	call smark (sp)
	call salloc (bfp, rowlen, TY_CHAR)

	npix_record = len_record * FITS_BYTE / BITPIX(fits)
	i = rft_init_read_pixels (npix_record, BITPIX(fits), LSBF, TY_CHAR)
	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, 2880) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1

	# Put EOS at the end, rft_red_pixels does not put one at rowlen.
	Memc[bfp+rowlen] = EOS
	do gn = 1, ngroups {

	    # Read in table row
	    nch = rft_read_pixels (fits_fd, Memc[bfp], rowlen,
	        NRECORDS(fits), blksize)
	    if (nch != rowlen)
		call printf ("Error reading FITS data\n")

	    # Write table row
	    call gi_crgpb (im, Memc[bfp], tbcol, gn)
	}

	call sfree (sp)

end
