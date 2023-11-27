# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include <imhdr.h>
include <imio.h>
include <tbset.h>
include <mach.h>
include "rfits.h"

# RTB_READ_HEADER -- Read a FITS header for a table extension.
# EOT is detected by an EOF on the first read and EOF is returned to the calling
# routine.  Any error is passed to the calling routine.

int procedure rtb_read_header (fits_fd, im, fits, tp)

int	fits_fd			# FITS file descriptor
pointer im			# Image descriptor
pointer	fits			# FITS data structure
pointer	tp			# IRAF table descriptor

int	i, stat, ind
char	card[LEN_CARD+1]

int	rft_init_read_pixels(), rft_read_pixels()
int	rtb_decode_card(), strncmp(), strmatch()
errchk	rft_read_pixels
errchk	stropen, close

include "rfits.com"
include "tab.com"

begin
	card[LEN_CARD + 1] = '\n'
	card[LEN_CARD + 2] = EOS

	# Initialization
	BLANKS(fits) = NO
	BLANK_VALUE(fits) = INDEFL
	NRECORDS(fits) = 0
	if (gkey != TO_MG)
           IRAFNAME(fits) = EOS

	# Header is character data in FITS_BYTE form
	i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)

	i = rft_read_pixels (fits_fd, card, LEN_CARD, NRECORDS(fits), 1)
	if (i == EOF) 		# At EOT
	   return (EOF)
	if (strmatch (card, "^XTENSION") != 0) {
	   if (strncmp( card[12], "IMAGE-IUE", 9) == 0) {
	      EXTEND(fits) = IMAGE_IUE
	      return (IMAGE_IUE)
	   }
	   if (strncmp( card[12], "TABLE", 5) != 0)
	        call error (13, "RTB_DECODE_CARD: Fits extension not supported")
	} else {
	   return (EOF)
	}
	# Loop until the END card is encountered
	repeat {
	    i = rft_read_pixels (fits_fd, card, LEN_CARD, NRECORDS(fits), 1)

	    if (i == EOF) {	# At EOT
		return (EOF)
	    } else if (i != LEN_CARD) {
	        call error (2, "RFT_READ_HEADER: Error reading FITS header")
	    }

	    # Print FITS card images if long_header option specified
	    ind = strncmp (card, "        ", 8)
	    if (long_header == YES && ind != 0) {
		call printf ("%s")
		    call pargstr (card)
	    }
	    if (ind != 0)
	       stat = rtb_decode_card (im, fits, tp, card)

	} until (stat == YES)

	return (stat)
end

define MAX_UPARM 50      # define max number of user parameter for a buffer
define LEN_CARD1 81

# RTB_DECODE_CARD -- Decode a FITS card and return YES when the END
# card is encountered.  The keywords understood are given in fits.h.

int procedure rtb_decode_card (im, fits, tp, card)

pointer im		# Image descriptor
pointer	fits		# FITS data structure
pointer	tp		# IRAF table descriptor
char	card[LEN_CARD]	# FITS card

pointer colptr
char	ftnfmt[SZ_COLFMT], pfmt[SZ_COLFMT]
pointer	ppar
int	nchar, ival, dtype, upar, ioff, mtsize
int	i, j, k, jc, tnaxis, npar, ncoln
int	strncmp()
int	strmatch(), ctoi(), ctol(), ctor()

include	"rfits.com"
include "tab.com"
data	upar /NO/
data    ppar /NULL/

begin
	i = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	    # define the last column
	    if (gkey != TO_MG) {
	       call tbcdef (tp, colptr, colname, colunits, colfmt,
			 datat, lendata, 1)
	       call tbpset (tp, TBL_MAXPAR, npar+5)
	       call tbtcre (tp)
	       if (upar == YES) {  
	          # now write the user parameters to the table
	          call ftb_put_upar (tp, npar, Memc[ppar])
	          upar = NO
	          call mfree(ppar, TY_CHAR)
	       }
	    } else {
	       call gi_pdes (im, colname, datat, lendata, ncoln)
	       if (upar == YES) {
	          call gi_gcomm (im, npar, Memc[ppar])
		  upar = NO
	          call mfree(ppar, TY_CHAR)
	       }
	    }
	    return(YES)
	} else if (strmatch (card, "^XTENSION") != 0) {
	    if (strncmp( card[i+1], "TABLE", 5) != 0)
	      call error (13, "RTB_DECODE_CARD: Fits extension not supported")
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	    nchar = ctoi (card, i, BITPIX(fits))
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	    nchar = ctoi (card, i, tnaxis)
	    if (tnaxis > 2) 
		call error (5, "RTB_DECODE_CARD: FITS table NAXIS too large")
	    coln = 1			# init column index
	} else if (strmatch (card, "^NAXIS") != 0) {
	    call strcpy("  ", DATE(fits), LEN_CARD)
	    k = strmatch (card, "^NAXIS")
	    nchar = ctoi (card, k, j)
	    if (j == 1 )
	       nchar = ctol (card, i, FITS_ROWLEN(fits))
	    else
	       nchar = ctol (card, i, FITS_NROWS(fits))
	} else if (strmatch (card, "^PCOUNT  ") != 0) {
	    nchar = ctoi (card, i, ival)
	    if (ival != 0) 
		call error (6, "RTB_DECODE_CARD: PCOUNT is not zero")
	} else if (strmatch (card, "^GCOUNT ") != 0) {
	    nchar = ctoi (card, i, ival)
	    if (ival > 1)
		call eprintf ("Warning: FITS can only read one group per table")
	} else if (strmatch (card, "^TFIELDS ") != 0) {
	    nchar = ctoi (card, i, ival)
	    if (gkey != TO_MG) {
	       # set the number of columns
	       call tbpset (tp, TBL_MAXCOLS, ival)    
	       # initialize defaults values
	    } else {
	       # The number of fields (or columns) in the table is the 
	       # number of parameter for the new GEIS file.
	       call gi_pstfval (im, "PCOUNT", ival)
	       if (ival > 0)
		  # Realloc space needed for the stf descriptor
		  call gi_realloc (im)
	    }
	    do jc = 1, ival {
	       tnull[1,jc] = EOS
	       tzero[jc] = 0.0
	       tscal[jc] = 1.0
	    }
	} else if (strmatch (card, "^EXTNAME ") != 0) {
	    # Do not overwrite if
	    if (gkey != TO_MG)
	     call rft_get_fits_string (card, IRAFNAME(fits), LEN_CARD)
	} else if (strmatch (card, "^DATE    ") != 0) {
	    call rft_get_fits_string (card, DATE(fits), LEN_CARD)
	} else if (strmatch (card, "^TTYPE" ) != 0) {
	    k = strmatch (card, "^TTYPE")
	    nchar = ctoi (card, k, ncoln)	# possible new column number
	    if (ncoln != coln) {
	       if (gkey != TO_MG) {
	          # define previous column
	          call tbcdef (tp, colptr, colname, colunits, colfmt,
			    datat, lendata, 1)
	          colunits[1] = EOS
	       } else
	          call gi_pdes (im, colname, datat, lendata, coln)
	       coln = ncoln
	    }
	    call rft_get_fits_string (card, colname, SZ_COLNAME)
	} else if (strmatch (card, "^TBCOL" ) != 0) {
	    k = strmatch (card, "^TBCOL")
	    nchar = ctoi (card, k, jc)
	    nchar = ctoi (card, i, tbcol[jc])
	} else if (strmatch (card, "^TFORM" ) != 0) {
	    k = strmatch (card, "^TFORM")
	    nchar = ctoi (card, k, ncoln)	# possible new column number
	    if (ncoln != coln) {
	       if (gkey != TO_MG) {
	          # define previous column
	          call tbcdef (tp, colptr, colname, colunits, colfmt,
			    datat, lendata, 1)
	          colunits[1] = EOS
	       } else
	          call gi_pdes (im, colname, datat, lendata, coln)
	       coln = ncoln
	    }
	    call rft_get_fits_string (card, ftnfmt, SZ_COLFMT)
	    call tbgtyp (ftnfmt, datat, tbcw[coln])
	    call tbbaln (datat, dtype, lendata)
	    call tbbftp (ftnfmt, colfmt)
	    if (datat < 0) {		# Change format to left justified text
		call strcpy ("%-", pfmt, SZ_COLFMT)
		call strcat (colfmt[2], pfmt, SZ_COLFMT)
		call strcpy (pfmt, colfmt, SZ_COLFMT)
	    }
	} else if (strmatch (card, "^TUNIT" ) != 0) {
	    k = strmatch (card, "^TUNIT")
	    nchar = ctoi (card, k, ncoln)	# possible new column number
	    if (ncoln != coln) {
	       # define previous column
	       if (gkey != TO_MG) {
	          call tbcdef (tp, colptr, colname, colunits, colfmt,
			    datat, lendata, 1)
	          colunits[1] = EOS
	       } else
	          call gi_pdes (im, colname, datat, lendata, coln)
	       coln = ncoln
	    }
	    call rft_get_fits_string (card, colunits, SZ_COLUNITS)
	} else if (strmatch (card, "^TNULL" ) != 0) {
	    k = strmatch (card, "^TNULL")
	    nchar = ctoi (card, k, jc)
	    call rft_get_fits_string (card, tnull[1,jc], SZ_COLFMT)
	} else if (strmatch (card, "^TZERO" ) != 0) {
	    k = strmatch (card, "^TZERO")
	    nchar = ctoi (card, k, jc)
	    nchar = ctor (card, i, tzero[jc])
	    # change datatype to real if 'datat' is int.
	    if (datat == TY_INT) {
	       datat = TY_REAL
	       call strcpy ("%-15.7g", colfmt, SZ_COLFMT)
	    }
	} else if (strmatch (card, "^TSCAL" ) != 0) {
	    k = strmatch (card, "^TSCAL")
	    nchar = ctoi (card, k, jc)
	    nchar = ctor (card, i, tscal[jc])
	    # change datatype to real if 'datat' is int.
	    if (datat == TY_INT) {
	       datat = TY_REAL
	       call strcpy ("%-15.7g", colfmt, SZ_COLFMT)
	    }
	} else {
	    # Allow storage for user parameters
	    if (upar == NO) {
	       upar = YES
	       if (ppar != NULL)
		  call mfree (ppar, TY_CHAR)
	       mtsize = (LEN_CARD+1)*MAX_UPARM
	       call malloc (ppar, mtsize, TY_CHAR)
	       ioff = 0
	       npar = 0
	    }
	    # Keep user parameters in a buffer until END
	    call amovc (card, Memc[ppar+ioff], LEN_CARD)	# copy EOS also
	    ioff = ioff + LEN_CARD + 1
	    Memc[ppar+ioff-1] = EOS
	    npar = npar + 1
	    if (npar >= mtsize/(LEN_CARD+1)) {    # increase no. of cards by 10
	       mtsize = mtsize + (LEN_CARD+1)*50
	       call realloc(ppar, mtsize, TY_CHAR)
	    }
	}
	return (NO)
end

# FTB_PUT_UPAR -- Procedure to write user parameters to the table 
# already created.

procedure ftb_put_upar (tp, npar, uparbuf)

pointer tp			     # i: table descriptor
char	uparbuf[LEN_CARD, npar]       # i: buffer with user pars
int	npar			     # I: number of parameters read

char	keyword[SZ_KEYWORD], sval[LEN_CARD]
char 	card[LEN_CARD], squo, cht, chn, dot, blkn
int	i, k, nscan(), stridx(), strmatch()
double  dval
int	bval, ival, iparn

begin
	blkn = ' '
	squo = '\''
	cht = 'T'
	chn = 'F'
	dot = '.'
	do i = 1, npar {
	    do k = 1, 8 {
	       if (uparbuf[k,i] == blkn) {
	          keyword[k] = EOS
	          break
	       }
	       keyword[k] = uparbuf[k,i]
	    }
	    keyword[SZ_KEYWORD+1] = EOS
	    call strcpy (uparbuf[10,i], card, LEN_CARD)
	    if (stridx (squo, uparbuf[1,i]) == 11) {           # is a string
	       call rft_get_fits_string (uparbuf[1,i], sval, LEN_CARD)
	       call tbhadt (tp, keyword, sval)
	    } else if (strmatch(keyword, "^HISTORY") != 0 ) {
	       call strcpy (uparbuf[9,i], sval, LEN_CARD)
	       call trimh (sval)
	       call tbhadt (tp, "HISTORY", sval)
	    } else if (strmatch(keyword, "^COMMENT") != 0 ) {
	       call strcpy (uparbuf[9,i], sval, LEN_CARD)
	       call trimh (sval)
	       call tbhadt (tp, "COMMENT", sval)
	    } else if (strmatch(card, "^        T ") != 0 ) {
	       bval = YES	
	       call tbhadb (tp, keyword, bval)
	    } else if (strmatch(card, "^        F ") != 0 ) {
	       bval = NO
	       call tbhadb (tp, keyword, bval)
	    } else {                   # is a number
	       call sscan(card)
		    call gargd(dval)
	       if (nscan() < 1) {
	          call strcpy (uparbuf[1,i], card, LEN_CARD)
		  # append card regardless of content or keyword
	          call tbhanp (tp, keyword, 't', card[9], iparn)
	       } else {
		  if (stridx(dot, card) == 0) {
	             call sscan(card)
		        call gargi(ival)
		     call tbhadi (tp, keyword, ival)
		  } else
	             call tbhadd (tp, keyword, dval)
	       }
	    }
	
	}
end

# TBGTYPE -- Get datatype and field width from the format specification. 
# Notice that datatype for character format is not spp standard.

procedure tbgtyp (ftnfmt, datatyp, width)

char	ftnfmt[LEN_CARD]	# i: fortran format specification
int	datatyp			# o: data type expressed as an int
int	width			# 0: field width in character (TBFORM value)
#--
int	ctoi(), nchar, ipos

begin
	call strlwr (ftnfmt)

	ipos = 2
	nchar = ctoi (ftnfmt, ipos, width)

	if (ftnfmt[1] == 'e') {
	    datatyp = TY_REAL
	} else if (ftnfmt[1] == 'g') {
	    datatyp = TY_REAL
	} else if (ftnfmt[1] == 'f') {
	    datatyp = TY_REAL
	} else if (ftnfmt[1] == 'd') {
	    datatyp = TY_DOUBLE
	} else if (ftnfmt[1] == 'i') {
	    datatyp = TY_INT
	} else if (ftnfmt[1] == 'b') {
	    datatyp = TY_BOOL
	} else if (ftnfmt[1] == 'a') {
	    datatyp = -width			# NOTE:  not an SPP data type
	} else {
	    call error (5,"table datatype not supported")
	}
end


procedure trimh (card)

char card[LEN_CARD]

int	i , strlen()

begin
	for (i=strlen(card); 
	     i > 1  && (card[i] == ' ' || card[i] == '\n');
	     i=i-1)
	     ;

	     card[i+1] = EOS
	     
end
