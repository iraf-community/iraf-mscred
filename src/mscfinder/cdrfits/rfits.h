# FITS Definitions

# The FITS standard readable by the FITS reader using these definitions:
# 
# 1.  8 bits / byte
# 2.  ASCII character code
# 3.  MII data format (i.e. 8 bit unsigned integers and 16 and 32 bit signed
#     twos complement integers with most significant bytes first.)
#     See mii.h.
#
# The following deviations from the FITS standard are allowed:
# 
# The number of FITS bytes per record is normally 2880 but may be specified
# by the user.

# Define the bits per pixel and precision of the 3 basic FITS types

define	SZ_MAXCOL	256	# Maximum number of columns

define	FITS_BYTE	8	# Bits in a FITS byte
define	FITS_SHORT	16	# Bits in a FITS short
define	FITS_LONG	32	# Bits in a FITS long

define	FITSB_PREC	3	# Decimal digits of precision in a FITS byte
define	FITSS_PREC	5	# Decimal digits of precision in a FITS short
define	FITSL_PREC	10	# Decimal digits of precision in a FITS long

define	LSBF		NO	# Least significant byte first
define	LEN_CARD	80	# Length of FITS card in characters
define	COL_VALUE	11	# Starting column for parameter values


define  IM_MAXDIM	7
define	SZ_OBJECT	79
define	SZ_EXTN		3	# default extension size
define	DEF_GPB		1	# gkey value for default group parameter desc.
define	NONDEF_GPB	2       # Non default gpb desc.; needs template
define	NON_GPB		3       # For image w/o gpb
define  TO_MG		4       # For immediate FITS w/xdim to multigroup.
define  IMAGE_IUE      -99

# Values for the following keywords are stored in the structure below.
define	MAX_PCSTF	7		# max param descriptors for STF files
define	LEN_PDSTF	70

define	LEN_FITS	(67 + 2*70 + 1 + MAX_PCSTF*LEN_PDSTF)

define	FITS_BSCALE	Memd[P2D($1)]	# FITS scaling parameter
define	FITS_BZERO	Memd[P2D($1+2)] # FITS zero parameter
define	BLANK_VALUE	Meml[P2L($1+4)]	# Blank value
define	BLANKS		Memi[$1+5]	# YES if blank keyword in header
define	BITPIX		Memi[$1+6]	# Bits per pixel (Must be an MII type)
define	SCALE		Memi[$1+7]	# Scale the data?
define	SIMPLE		Memi[$1+8]	# Standard FITS format 
define	NRECORDS	Memi[$1+9]	# Number of FITS logical records
define  EXTEND		Memi[$1+10]	# Standard extension flag (tables)
define  FITS_NROWS	Memi[$1+11]	# Number of lines in table
define  FITS_ROWLEN	Memi[$1+12]	# Number of character per row
define  NAXIS		Memi[$1+13]	# Number of dimensions 
define  NAXISN	 	Memi[$1+14+$2-1]	# Length of each axis (up to 7)
define  GCOUNT		Memi[$1+22]
define  OPSIZE          Memi[$1+23]
# extra space
define  RA		Memc[P2C($1+30)] # 
define  DEC		Memc[P2C($1+39)] # 
define  DATE		Memc[P2C($1+48)] # 
define  FITSTYPE        Memc[P2C($1+57)]
define  MAKE_CD		Memi[$1+64]
define  OBJECT          Memc[P2C($1+67)] # Up to 70 characters long
define	IRAFNAME	Memc[P2C($1+138)]# idem

# temporary storage for wcs information to be put on the STF files rigth
# before closing the images.
define	WCS_PDES	(($1) + 67 + 2*70 + ((($2)-1)*LEN_PDSTF))

define	SZ_WCSCTYPE	8
# GPB Parameter descriptor.
define	CRVAL		Memd[P2D($1)]	
define	CRPIX		Memr[$1+2]	
define	CDELT		Memd[P2D($1+4)]	
define	CROTA		Memd[P2D($1+6)]	
define	CDMATRIX	Memr[$1+($2-1)*7+8]
define  CTYPE		Memc[P2C($1+61)]
# Additional IRAF header parameters

define	PIXTYPE		IM_PIXTYPE($1)
define	NBPIX		IM_NBPIX($1)
define	IRAFMAX		IM_MAX($1)
define	IRAFMIN		IM_MIN($1)
define	LIMTIME		IM_LIMTIME($1)

# Mapping of FITS Keywords to IRAF image header

define	HISTORY	 IM_HISTORY($1)
define	UNKNOWN	 Memc[($1+IMU-1)*SZ_STRUCT+1]	# All unrecognized keywords
						# are stored here

define	SZ_OBJECT	SZ_IMTITLE
define	SZ_HISTORY	SZ_IMHIST
define	SZ_FCTYPE	SZ_CTYPE


# FITS cards not recognized by this header are stored in the USER AREA of
# the image header (UNKNOWN) up to a maximum of:
