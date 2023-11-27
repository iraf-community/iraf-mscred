# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include <mach.h>
include <imhdr.h>
include	"rfits.h"

# RFT_READ_HEADER -- Read a FITS header.
# If BSCALE and BZERO are different from 1.0 and 0.0  scale is set to true
# otherwise scale is false.
# EOT is detected by an EOF on the first read and EOF is returned to the calling
# routine.  Any error is passed to the calling routine.

int procedure rft_read_header (fits_fd, fd_usr, fits)

int	fits_fd			# FITS file descriptor
int	fd_usr			# Fits header spool file pointer
pointer	fits			# FITS data structure

int	i, stat, nread, max_lenuser, ndiscard
char	card[LEN_CARD+1]

int	rft_decode_card(), rft_init_read_pixels(), rft_read_pixels(), strmatch()
int	maxcards

errchk	rft_decode_card, rft_init_read_pixels, rft_read_pixels
errchk	close

include "rfits.com"

begin
	card[LEN_CARD + 1] = '\n'
	card[LEN_CARD + 2] = EOS

	# Initialization
	FITS_BSCALE(fits) = 1.0d0
	FITS_BZERO(fits) = 0.0d0
	BLANKS(fits) = NO
	BLANK_VALUE(fits) = INDEFL
	SCALE(fits) = NO
	SIMPLE(fits) = YES
	# This xtension was set in rft_read_header
	if (EXTEND(fits) != IMAGE_IUE)
	    EXTEND(fits) = NO
	NRECORDS(fits) = 0
	ndiscard = 0
	OBJECT(fits) = EOS
	IRAFNAME(fits) = EOS
	OPSIZE(fits) = -1
	GCOUNT(fits) = -1


        # The max_lenuser value should be smaller by the length of the 
	# group parameter block in case we exhaust the buffer with header
	# cards and then we would not have space to put the gpb cards
	# that the stf kernel would put to the existing header.

	max_lenuser = ARB - 3200
	maxcards = max_lenuser/LEN_CARD

#	fd_usr = open ("fits_hdr", READ_WRITE, SPOOL_FILE)

	# Do not call again if coming from ftb_rheader
	if (EXTEND(fits) != IMAGE_IUE)
	   i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)

	# Loop until the END card is encountered
	nread = 0
	repeat {
	    i = rft_read_pixels (fits_fd, card, LEN_CARD, NRECORDS(fits), 1)

	    if ((i == EOF) && (nread == 0)) {		# At EOT
		call close(fd_usr)
		return (EOF)
	    } else if (EXTEND(fits) == IMAGE_IUE) {
		nread = nread + 1
		EXTEND(fits) = NO
	    } else if ((nread == 0) && strmatch (card, "^SIMPLE  ") == 0) {
		call flush (STDOUT)
		call error (30, "RFT_READ_HEADER: Not a FITS file")
	    } else if (i != LEN_CARD) {
	        call error (2, "RFT_READ_HEADER: Error reading FITS header")
	    } else
	        nread = nread + 1

	    # Print FITS card images if long_header option specified
	    if (long_header == YES) {
		call printf ("%s")
		    call pargstr (card)
	    }
	    if (maxcards == nread)
	       ndiscard = 1
	    stat = rft_decode_card (fits, fd_usr, card, ndiscard)
	} until (stat == YES)   # stat == YES if END card encountered.

	if (OPSIZE(fits) == -1 && gkey == TO_MG) {   # NO OPSIZE keyword
	   gkey = DEF_GPB 
	   call printf ("Warning: fits file cannot be convert to multigroup\n")
	}
	if (ndiscard > 0) {
	    call printf ("Warning: User area too small %d card images discarded\n")
	    call pargi (ndiscard)
	}

	return (nread)
end


define	NBITS_CHAR	(SZB_CHAR * NBITS_BYTE)
# RFT_DECODE_CARD -- Decode a FITS card and return YES when the END
# card is encountered.  The keywords understood are given in fits.h.

int procedure rft_decode_card (fits, fd_usr, card, ndiscard)

pointer	fits		# FITS data structure
int	fd_usr		# file descriptor of user area
char	card[LEN_CARD]	# FITS card
int	ndiscard	# Number of cards for which no space available

pointer pn
char	cval, str[LEN_CARD], cdpat[SZ_LINE]
double	dval
int	nchar, i, j, k, len, ndim

bool	rft_equald()
int	strmatch(), ctoi(), ctol(), ctod(), cctoc(), rft_hms()
int	patmake(), patmatch(), date, origin
errchk	putline

include	"rfits.com"

begin
	i = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	    return(YES)
	} else if (strmatch (card, "^SIMPLE  ") != 0) {
	    nchar = cctoc (card, i, cval)
	    if (cval != 'T') {
		call printf("RFT_DECODE_CARD: Non-standard FITS format \n")
		SIMPLE(fits) = NO
	    }
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	    nchar = ctoi (card, i, BITPIX(fits))
	    ieee = NO
	    if (BITPIX(fits) < 0) {
	       ieee = YES
	       BITPIX(fits) = -BITPIX(fits)
	    }
	    nchar = patmake ("CD[1-7]_[1-7]", cdpat, SZ_LINE)
	} else if (strmatch (card, "^BLANK   ") != 0) {
	    BLANKS(fits) = YES
	    nchar = ctol (card, i, BLANK_VALUE(fits))
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	    nchar = ctoi (card, i, NAXIS(fits))
	    if (NAXIS(fits) > IM_MAXDIM)
		call error (5, "RFT_DECODE_CARD: FITS NAXIS too large")
	    # assume default values for CWS
	    ndim = NAXIS(fits)
	    do k = 1, ndim {
	       pn = WCS_PDES(fits,k)
	       CRVAL(pn) = 1.0
	       CRPIX(pn) = 1.0
     	       CDELT(pn) = 1.0
	       CROTA(pn) = 0.0
	       call strcpy ("PIXEL", CTYPE(pn), SZ_WCSCTYPE)
	       do j = 1, ndim {
	          if (k == j)
		     CDMATRIX(pn,j) = 1.0
		  else
		     CDMATRIX(pn,j) = 0.0
	       }
	    }
	    date= YES
	    origin = YES
	    MAKE_CD(fits) = YES
	} else if (strmatch (card, "^NAXIS") != 0) {
	    k = strmatch (card, "^NAXIS")
	    nchar = ctoi (card, k, j)
	    nchar = ctol (card, i, NAXISN(fits,j))
	    call strcpy ("  ", RA(fits), LEN_CARD)
	    call strcpy ("  ", DEC(fits), LEN_CARD)
	    call strcpy ("  ", DATE(fits), LEN_CARD)
	} else if (strmatch (card, "^BLOCKED ") != 0) {
	    # Just ignore the card
	} else if (strmatch (card, "^GROUPS  ") != 0) {
	    nchar = cctoc (card, i, cval)
	    if (cval == 'T') {
		call error (6, "RFT_DECODE_CARD: Group data not implemented")
	    }
	} else if (strmatch (card, "^SDASMGNU") != 0) {
	    nchar = ctoi (card, i, GCOUNT(fits))
	    if (gkey != TO_MG)
	       gkey = NON_GPB
	    # If the number of rows is zero, then there is no attached
	    # table, since the original file has PCOUNT = 0.
	    if (GCOUNT(fits) >= 1 && gkey != TO_MG)
	       call putline (fd_usr, card)
	    MAKE_CD(fits) = NO
	} else if (strmatch (card, "^EXTEND  ") != 0) {
	    nchar = cctoc (card, i, cval)
	    if (cval == 'T') {
		EXTEND(fits) = YES
	    }
	} else if (strmatch (card, "^EXTNAME ") != 0) {
	    call rft_get_fits_string (card, OBJECT(fits), LEN_CARD)
	    call strcat (" (Xtension)",OBJECT(fits), LEN_CARD)
	    call putline (fd_usr, card)
	} else if (strmatch (card, "^BSCALE  ") != 0) {
	    nchar = ctod (card, i, dval)
	    if (! rft_equald (dval, 1.0d0) && scale == YES)
		SCALE(fits) = YES
	    FITS_BSCALE(fits) = dval
	} else if (strmatch (card, "^BZERO   ") != 0) {
	    nchar = ctod (card, i, dval)
	    if (! rft_equald (dval, 0.0d0) && scale == YES)
		SCALE(fits) = YES
	    FITS_BZERO(fits) = dval
	} else if (strmatch (card, "^DATAMAX ") != 0) {
	    if (gkey != DEF_GPB)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^DATAMIN ") != 0) {
	    if (gkey != DEF_GPB)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^IRAF-MAX") != 0) {
	    if (gkey < 0)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^IRAF-MIN") != 0) {
	    if (gkey < 0)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^IRAF-B/P") != 0) {
	    if (gkey < 0)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^IRAFTYPE") != 0) {
	    call rft_get_fits_string (card, FITSTYPE(fits), LEN_CARD)
	    if (gkey <0) 
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^OBJECT") != 0) {
	    call rft_get_fits_string (card, OBJECT(fits), LEN_CARD)
	    call putline (fd_usr, card)
	} else if (strmatch (card, "^IRAFNAME") != 0) {
	    call rft_get_fits_string (card, IRAFNAME(fits), LEN_CARD)
	} else if (strmatch (card, "^ORIGIN  ") != 0) {
	    if (origin == NO)  # don'take the first one if more than one
	       call putline (fd_usr, card)
	    origin = NO
	} else if (strmatch (card, "^OPSIZE  ") !=  0) {
	    # Save if we want to create a multigroup image
	    if (gkey == TO_MG)
	       nchar = ctoi (card, i, OPSIZE(fits))
	} else if (strmatch (card, "^FITSDATE") !=  0) {
	       # dont put in image header
	} else if (strmatch (card, "^DATE    ") !=  0) {
	    if (date == YES)
	       call rft_get_fits_string (card, DATE(fits), LEN_CARD)
	    call putline (fd_usr, card)
	    date = NO
	} else if (strmatch (card, "^HISTORY ") != 0) {
	    # put all the history that "imuserarea" allows
	    if (ndiscard >= 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^CRVAL") != 0) {
	    k = strmatch (card, "^CRVAL")
	    nchar = ctoi (card, k, j)
	    pn = WCS_PDES(fits,j)
	    nchar = ctod (card, i, dval)
	    CRVAL(pn) = dval
	    if (gkey != DEF_GPB)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^CRPIX") != 0) {
	    k = strmatch (card, "^CRPIX")
	    nchar = ctoi (card, k, j)
	    pn = WCS_PDES(fits,j)
	    nchar = ctod (card, i, dval)
	    CRPIX(pn) = dval
	    if (gkey != DEF_GPB)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^CDELT") != 0) {
	    k = strmatch (card, "^CDELT")
	    nchar = ctoi (card, k, j)
	    pn = WCS_PDES(fits,j)
	    nchar = ctod (card, i, dval)
	    CDELT(pn) = dval
	    call putline (fd_usr, card)
	} else if (strmatch (card, "^CROTA") != 0) {
	    k = strmatch (card, "^CROTA")
	    nchar = ctoi (card, k, j)
	    pn = WCS_PDES(fits,j)
	    nchar = ctod (card, i, dval)
	    CROTA(pn) = dval
	    call putline (fd_usr, card)
	} else if (strmatch (card, "^CTYPE") != 0) {
	    k = strmatch (card, "^CTYPE")
	    nchar = ctoi (card, k, j)
	    pn = WCS_PDES(fits,j)
	    call rft_get_fits_string (card, CTYPE(pn), SZ_OBJECT)
	    if (gkey != DEF_GPB)
	       call putline (fd_usr, card)
	} else if (patmatch (card, cdpat) != 0) {
	    k = strmatch (card, "^CD")
	    nchar = ctoi (card, k, j)
	    pn = WCS_PDES(fits,j)
	    k = strmatch (card, "^CD?_")
	    nchar = ctoi (card, k, j)
	    nchar = ctod (card, i, dval)
	    CDMATRIX(pn,j) = dval
	    MAKE_CD(fits) = NO
	    if (gkey != DEF_GPB)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^UT      ") != 0) {
	    len = rft_hms (card, str, LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("UT", str, len, card, "right ascension")
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^ZD      ") != 0) {
	    len = rft_hms (card, str, LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("ZD", str, len, card, "zenith distance")
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^ST      ") != 0) {
	    len = rft_hms (card, str, LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("ST", str, len, card, "sidereal time")
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^RA      ") != 0) {
	    call rft_get_fits_string (card, RA(fits), LEN_CARD)
	    len = rft_hms (card, str, LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("RA", str, len, card, "right ascension")
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else if (strmatch (card, "^DEC     ") != 0) {
	    call rft_get_fits_string (card, DEC(fits), LEN_CARD)
	    len = rft_hms (card, str, LEN_CARD)
	    if (len > 0) {
		call wft_encodec ("DEC", str, len, card, "declination")
		card[LEN_CARD+1] = '\n'
		card[LEN_CARD+2] = EOS
	    }
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	} else {
	    if (ndiscard > 1)
		ndiscard = ndiscard + 1
	    else {
	        iferr (call putline (fd_usr, card))
		    ndiscard = ndiscard + 1
	    }
	}
	return (NO)
end
