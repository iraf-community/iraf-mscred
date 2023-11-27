# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<imhdr.h>
include <mach.h>
include <tbset.h>
include <fset.h>
include	"rfits.h"

# RFT_READ_TFITS --  Read FITS table in row order as character strings
# and convert them to IRAF table rows.

procedure rtb_read_tfits (fits_fd, fits, tp)

int	fits_fd		# FITS file descriptor
pointer	fits		# FITS data structure
pointer	tp 		# IRAF table descriptor

int	i, rowlen, blksize, nch
long	nlines, il, ncols
pointer	sp, bfp
int	colptr[SZ_MAXCOL]

int	rft_init_read_pixels(), rft_read_pixels()
int	tbpsta(), npix_record, fstati(), tbcnum()

errchk	salloc, sfree, rft_init_read_pixels, rft_read_pixels, rft_scale_pix
errchk	rft_change_pix, rft_put_image_line, rft_pix_limits, smark

include	"rfits.com"
include "tab.com"

begin

	rowlen = FITS_ROWLEN(fits)
	nlines = FITS_NROWS(fits)
	ncols  = tbpsta (tp, TBL_MAXCOLS)
	do i = 1, ncols {
	   colptr[i] = tbcnum (tp, i)
	}

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
	do il = 1, nlines {

	    # Read in table row
	    nch = rft_read_pixels (fits_fd, Memc[bfp], rowlen,
	        NRECORDS(fits), blksize)
	    if (nch != rowlen)
		call printf ("Error reading FITS data\n")

	    # Write table row
	    call rft_put_table_row (tp, colptr, Memc[bfp], rowlen, ncols, il)
	}

	call sfree (sp)
end
