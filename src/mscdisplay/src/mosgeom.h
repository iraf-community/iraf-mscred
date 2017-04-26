# MOSGEOM.H -- Mosaic geometry structure definitions.
#
# There is one such structure for each of the multiple input images
# and one for the output image.

define	LEN_ZERONAME	39		# Length of zero name
define	LEN_FLATNAME	39		# Length of flat field name
define	LEN_MOSGEOM 	81		# Length of mosgeom structure.

define	MG_IM		Memi[$1+1]	# IMIO pointer
define	MG_USHORT	Memi[$1+2]	# Use special FITS USHORT optimization
define	CCDNAME		Memi[$1+3]	# Pointer to CCD name string
define	AMPID		Memi[$1+4]	# Pointer to ampid string
define	NX		Memi[$1+5]	# Image dimension in column direction
define  NY		Memi[$1+6]	# Image dimension in line direction
define	IMIDX		Memi[$1+7]	# Pointer to indices for im{gp}nlt calls

# CCD section
define	CX1		Memi[$1+8]	# CCD section starting column.
define	CX2		Memi[$1+9]	# CCD section ending   column.
define	CY1		Memi[$1+10]	# CCD section starting line.
define	CY2		Memi[$1+11]	# CCD section ending   line.

# DATA section
define	DX1		Memi[$1+12]	# DATA section starting column.
define	DX2		Memi[$1+13]	# DATA section ending   column.
define	DY1		Memi[$1+14]	# DATA section starting line.
define	DY2		Memi[$1+15]	# DATA section ending   line.

# DATA to CCD
define	DX		Memi[$1+16]	# Pixel summing factor
define	DY		Memi[$1+17]	# Pixel summing factor

# TRIM section
define	TX1		Memi[$1+18]	# TRIM section starting column.
define	TX2		Memi[$1+19]	# TRIM section ending   column.
define	TY1		Memi[$1+20]	# TRIM section starting line.
define	TY2		Memi[$1+21]	# TRIM section ending   line.

# BIAS section
define	BX1		Memi[$1+22]	# BIAS section starting column.
define	BX2		Memi[$1+23]	# BIAS section ending   column.
define	BY1		Memi[$1+24]	# BIAS section starting line.
define	BY2		Memi[$1+25]	# BIAS section ending   line.

# Amplifier dependent parameters used for processing/display.
define	PROC		Memi[$1+26]	# Process?
define	DOBIAS		Memi[$1+27]	# Do bias subtraction?
define	DOZERO		Memi[$1+28]	# Do zero subtraction?
define	DOFLAT		Memi[$1+29]	# Do flat field division?
define	ZERONAME	Memc[P2C($1+30)]# Zero name
define	FLATNAME	Memc[P2C($1+50)]# Flat field name
define	DZIM		Memi[$1+70]	# Display zero IMIO pointer
define	DFIM		Memi[$1+71]	# Display flat IMIO pointer
define	CCDMEAN		Memr[P2R($1+72)]# CCDMEAN value
define	BIAS		Memr[P2R($1+73)]# Zero offset (bias level) of readout.
define	OVRSCN		Memi[$1+74]	# Pointer to overscan vector.
define	GAIN		Memr[P2R($1+75)]# Gain of readout.
define	DARK		Memr[P2R($1+76)]# Dark rate of readout.
define	Z1		Memr[P2R($1+77)]# Z1 value for display.
define	Z2		Memr[P2R($1+78)]# Z2 value for display.

# Real-time flags.
define	CKNODATA	Memi[$1+79]	# Check for no data?
define	NODATA		Memi[$1+80]	# No data found?

# Macros to calculate corners of trim section relative to CCD section.
define	CTX1	CX1($1) + (TX1($1) - DX1($1)) * DX($1)
define	CTX2	CX2($1) + (TX2($1) - DX2($1)) * DX($1)
define	CTY1	CY1($1) + (TY1($1) - DY1($1)) * DY($1)
define	CTY2	CY2($1) + (TY2($1) - DY2($1)) * DY($1)
