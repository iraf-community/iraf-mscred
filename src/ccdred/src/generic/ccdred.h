# CCDRED Data Structures and Definitions

# The CCD structure:  This structure is used to communicate processing
# parameters between the package procedures.  It contains pointers to
# data, calibration image IMIO pointers, scaling parameters, and the
# correction flags.  The corrections flags indicate which processing
# operations are to be performed.  The subsection parameters do not
# include a step size.  A step size is assumed.  If arbitrary subsampling
# is desired this would be the next generalization.

define	LEN_CCD		1660			# Length of CCD structure
define	LEN_LOG		199			# Length of log strings
define	LEN_CCDSTR	99			# Length of strings

# Basic control flags
define	PROC		Memi[$1]		# Process input image?
define	CALPROC		Memi[$1+1]		# Process calibration images?
define	LISTPROC	Memi[$1+2]		# List processing to be done?
define	COR		Memi[$1+3]		# Call DOPROC?
define	COROUT		Memi[$1+4]		# Create output image?
define	CORBPM		Memi[$1+5]		# Create output mask?
define	CORS		Memi[$1+6+($2-1)] 	# Individual correction flags

# CCD data coordinates
define	CCD_C1		Memi[$1+20]		# CCD starting column
define	CCD_C2		Memi[$1+21]		# CCD ending column
define	CCD_CS		Memi[$1+22]		# CCD step
define	CCD_L1		Memi[$1+23]		# CCD starting line
define	CCD_L2		Memi[$1+24]		# CCD ending line
define	CCD_LS		Memi[$1+25]		# CCD step

# Input data
define	IN_IM		Memi[$1+30]		# Input image pointer
define	IN_CCDTYPE	Memi[$1+31]		# Input CCD type
define	IN_C1		Memi[$1+32]		# Input data starting column
define	IN_C2		Memi[$1+33]		# Input data ending column
define	IN_L1		Memi[$1+34]		# Input data starting line
define	IN_L2		Memi[$1+35]		# Input data ending line
define	IN_CFLIP	Memi[$1+36]		# Flipped input data section?
define	IN_LFLIP	Memi[$1+37]		# Flipped input data section?

# Input mask data
define	BPIN_IM		Memi[$1+40]		# Input bad pixel mask pointer
define	BPIN_C1		Memi[$1+41]		# Input mask data starting col
define	BPIN_C2		Memi[$1+42]		# Input mask data ending col
define	BPIN_L1		Memi[$1+43]		# Input mask data starting line
define	BPIN_L2		Memi[$1+44]		# Input mask data ending line
define	BPIN_PM		Memi[$1+45]		# Input mask pointer
define	BPIN_FP		Memi[$1+46]		# Input mask fixpix data

# Output data
define	OUT_IM		Memi[$1+50]		# Output image pointer
define	OUT_C1		Memi[$1+51]		# Output data starting col
define	OUT_C2		Memi[$1+52]		# Output data ending col
define	OUT_L1		Memi[$1+53]		# Output data starting line
define	OUT_L2		Memi[$1+54]		# Output data ending line

# Output mask data
define	BPOUT_IM	Memi[$1+55]		# Output mask pointer

# Output no interpolation data
define	NOIOUT_IM	Memi[$1+56]		# Output image pointer

# Saturation and bleed trail data
define	SATVAL		Memr[P2R($1+60)]	# Saturation value in ADU
define	SATVALE		Memr[P2R($1+61)]	# Saturation value in electrons
define	SATGROW		Memi[$1+62]		# Saturated pixel grow radius
define	BLDVAL		Memr[P2R($1+63)]	# Bleed value in ADU
define	BLDVALE		Memr[P2R($1+64)]	# Bleed value in electrons
define	BLDTRAIL	Memi[$1+65]		# Bleed trail minimum length
define	BLDGROW		Memi[$1+66]		# Bleed pixel grow radius

# Zero level data
define	ZERO_IM		Memi[$1+70]		# Zero level image pointer
define	ZERO_C1		Memi[$1+71]		# Zero level data starting col
define	ZERO_C2		Memi[$1+72]		# Zero level data ending col
define	ZERO_L1		Memi[$1+73]		# Zero level data starting line
define	ZERO_L2		Memi[$1+74]		# Zero level data ending line

# Dark count data
define	DARK_IM		Memi[$1+80]		# Dark count image pointer
define	DARK_C1		Memi[$1+81]		# Dark count data starting col
define	DARK_C2		Memi[$1+82]		# Dark count data ending col
define	DARK_L1		Memi[$1+83]		# Dark count data starting line
define	DARK_L2		Memi[$1+84]		# Dark count data ending line
define	DARKSCALE	Memr[P2R($1+85)]	# Dark count scale factor

# Flat field data
define	FLAT_IM		Memi[$1+90]		# Flat field image pointer
define	FLAT_C1		Memi[$1+91]		# Flat field data starting col
define	FLAT_C2		Memi[$1+92]		# Flat field data ending col
define	FLAT_L1		Memi[$1+93]		# Flat field data starting line
define	FLAT_L2		Memi[$1+94]		# Flat field data ending line
define	FLATSCALE	Memr[P2R($1+95)]	# Flat field scale factor
define	GAINSCALE	Memr[P2R($1+96)]	# Gain scale factor

# Sky flat field data
define	SFLAT_IM	Memi[$1+100]		# Sky flat field image pointer
define	SFLAT_C1	Memi[$1+101]		# Sky flat field starting col
define	SFLAT_C2	Memi[$1+102]		# Sky flat field ending col
define	SFLAT_L1	Memi[$1+103]		# Sky flat field starting line
define	SFLAT_L2	Memi[$1+104]		# Sky flat field ending line
define	SFLATSCALE	Memr[P2R($1+105)]	# Sky flat field scale factor

# Illumination data
define	ILLUM_IM	Memi[$1+110]		# Illumination image pointer
define	ILLUM_C1	Memi[$1+111]		# Illumination starting col
define	ILLUM_C2	Memi[$1+112]		# Illumination ending col
define	ILLUM_L1	Memi[$1+113]		# Illumination starting line
define	ILLUM_L2	Memi[$1+114]		# Illumination ending line
define	ILLUMSCALE	Memr[P2R($1+115)]	# Illumination factor

# Fringe data
define	FRINGE_IM	Memi[$1+120]		# Fringe image pointer
define	FRINGE_C1	Memi[$1+121]		# Fringe data starting col
define	FRINGE_C2	Memi[$1+122]		# Fringe data ending col
define	FRINGE_L1	Memi[$1+123]		# Fringe data starting line
define	FRINGE_L2	Memi[$1+124]		# Fringe data ending line
define	FRINGESCALE	Memr[P2R($1+125)]	# Fringe scale factor

# Trim section
define	TRIM_C1		Memi[$1+130]		# Trim starting col
define	TRIM_C2		Memi[$1+131]		# Trim ending col
define	TRIM_L1		Memi[$1+132]		# Trim starting line
define	TRIM_L2		Memi[$1+133]		# Trim ending line
define	TRIM_DC1	Memi[$1+134]		# Trim from data section
define	TRIM_DC2	Memi[$1+135]		# Trim from data section
define	TRIM_DL1	Memi[$1+136]		# Trim from data section
define	TRIM_DL2	Memi[$1+137]		# Trim from data section

# Bias section
define	BIAS_C1		Memi[$1+140]		# Bias starting col
define	BIAS_C2		Memi[$1+141]		# Bias ending col
define	BIAS_L1		Memi[$1+142]		# Bias starting line
define	BIAS_L2		Memi[$1+143]		# Bias ending line
define	OVERSCAN_TYPE	Memi[$1+144]		# Pointer to overscan vector
define	OVERSCAN_VEC	Memi[$1+145]		# Pointer to overscan vector

# Miscellaneous
define	READAXIS	Memi[$1+150]		# Readout axis (1=cols, 2=lines)
define	CALCTYPE	Memi[$1+151]		# Calculation data type
define	MINREPLACE	Memr[P2R($1+152)]	# Minimum replacement value
define	MEAN		Memr[P2R($1+153)]	# Mean of output image

# Strings
define	SATLOG		Memc[P2C($1+160)]	# Saturation log
define	BLDLOG		Memc[P2C($1+260)]	# Bleed log
define	TRIMLOG		Memc[P2C($1+360)]	# Trim log
define	FIXLOG		Memc[P2C($1+460)]	# Fix pixel log
define	BIASLOG		Memc[P2C($1+560)]	# Bias log
define	ZEROLOG		Memc[P2C($1+660)]	# Zero log
define	DARKLOG		Memc[P2C($1+760)]	# Dark count log
define	FLATLOG		Memc[P2C($1+860)]	# Flat field log
define	SFLATLOG	Memc[P2C($1+960)]	# Sky flat field log
define	ILLUMLOG	Memc[P2C($1+1060)]	# Illumination log
define	FRINGELOG	Memc[P2C($1+1160)]	# Fringe log
define	BPOUTLOG	Memc[P2C($1+1260)]	# Output BP mask log
define	BPIN_NAME	Memc[P2C($1+1360)]	# Input bad pixel mask name
define	BPOUT_NAME	Memc[P2C($1+1460)]	# Output bad pixel mask name
define	NOIOUT_NAME	Memc[P2C($1+1560)]	# Output no interpolation name

# The correction array contains the following elements with array indices
# given by the macro definitions.

define	NCORS		13		# Number of corrections

define	SATURATE	1		# Find saturation and bleed trails
define	FIXPIX		2		# Fix bad pixels
define	TRIM		3		# Trim image
define	OVERSCAN	4		# Apply overscan correction
define	ZEROCOR		5		# Apply zero level correction
define	DARKCOR		6		# Apply dark count correction
define	FLATCOR		7		# Apply flat field correction
define	SFLATCOR	8		# Apply flat field correction
define	ILLUMCOR	9		# Apply illumination correction
define	FRINGECOR	10		# Apply fringe correction
define	FINDMEAN	11		# Find the mean of the output image
define	MINREP		12		# Check and replace minimum value
define	READCOR		13		# Apply 1D read correction

# The following definitions identify the correction values in the correction
# array.  They are defined in terms of bit fields so that it is possible to
# add corrections to form unique combination corrections.  Some of
# these combinations are implemented as compound operations for efficiency.

define	O	001B	# overscan
define	Z	002B	# zero level
define	D	004B	# dark count
define	F	010B	# flat field
define	S	020B	# sky flat field
define	I	040B	# Illumination
define	Q	100B	# Fringe

# The following correction combinations are recognized.

define	ZO	003B	# zero level + overscan
define	DO	005B	# dark count + overscan
define	DZ	006B	# dark count + zero level
define	DZO	007B	# dark count + zero level + overscan
define	FO	011B	# flat field + overscan
define	FZ	012B	# flat field + zero level
define	FZO	013B	# flat field + zero level + overscan
define	FD	014B	# flat field + dark count
define	FDO	015B	# flat field + dark count + overscan
define	FDZ	016B	# flat field + dark count + zero level
define	FDZO	017B	# flat field + dark count + zero level + overscan
define	SF	030B	# flat field
define	SFO	031B	# flat field + overscan
define	SFZ	032B	# flat field + zero level
define	SFZO	033B	# flat field + zero level + overscan
define	SFD	034B	# flat field + dark count
define	SFDO	035B	# flat field + dark count + overscan
define	SFDZ	036B	# flat field + dark count + zero level
define	SFDZO	037B	# flat field + dark count + zero level + overscan
define	QI	140B	# fringe + illumination


# The following overscan functions are recognized.
define	OVERSCAN_TYPES "|mean|median|minmax|chebyshev|legendre|spline3|spline1|"
define	OVERSCAN_MEAN	1		# Mean of overscan
define	OVERSCAN_MEDIAN	2		# Median of overscan
define	OVERSCAN_MINMAX	3		# Minmax of overscan
define	OVERSCAN_FIT	4		# Following codes are function fits


# The following are error actions for CCDPROC.
define	ONERROR	"|abort|warn|exit|original|"
define	ONERR_ABORT	1	# Abort on an error
define	ONERR_WARN	2	# Warn on error and continue
define	ONERR_EXIT	3	# Warn on error and exit
define	ONERR_ORIG	4	# Original CCDPROC (warn/error)

# The following are CALPROC actions.
define	CALPROC_YES	1	# Process calibrations
define	CALPROC_NO	2	# Set calibration to be processed externally
define	CALPROC_IGNORE	3	# Ignore calibration processing
