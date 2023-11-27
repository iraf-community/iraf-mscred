# STARFOCUS

# Types of coordinates
define	SF_TYPES	"|center|mark1|markall|"
define	SF_CENTER	1		# Star at center of image
define	SF_MARK1	2		# Mark stars in first image
define	SF_MARKALL	3		# Mark stars in all images

# Task type
define	STARFOCUS	1
define	PSFMEASURE	2

define	SF_RMIN		16		# Minimum centering search radius
define	MAX_FRAMES	8		# Maximum number of display frames

# Data structures for STARFOCUS

define	SAMPLE		.2		# Subpixel sampling size
define	SF_SZFNAME	79		# Length of file names
define	SF_SZRTYPE	7		# Length of radius type string

# Main data structure
define	SF		38
define	SF_TASK		Memi[$1]	# Task type
define	SF_RTYPE	Memc[P2C($1+1)]	# Radius type string
define	SF_SCALE	Memr[$1+5]	# Pixel scale
define	SF_LEVEL	Memr[$1+6]	# Profile measurement level
define	SF_RADIUS	Memr[$1+7]	# Profile radius
define	SF_SBUF		Memr[$1+8]	# Sky region buffer
define	SF_SWIDTH	Memr[$1+9]	# Sky region width
define	SF_SAT		Memr[$1+10]	# Saturation
define	SF_NIT		Memi[$1+11]	# Number of iterations for radius
define	SF_OVRPLT	Memi[$1+12]	# Overplot the best profile?
define	SF_NCOLS	Memi[$1+13]	# Number of image columns
define	SF_NLINES	Memi[$1+14]	# Number of image lines
define	SF_XF		Memr[$1+15]	# X field center
define	SF_YF		Memr[$1+16]	# Y field center
define	SF_GP		Memi[$1+17]	# GIO pointer
define	SF_F		Memr[$1+18]	# Best focus
define	SF_R		Memr[$1+19]	# Radius at best focus
define	SF_M		Memr[$1+20]	# Brightest star magnitude
define	SF_XP1		Memr[$1+21]	# First derivative point to plot
define	SF_XP2		Memr[$1+22]	# Last derivative point to plot
define	SF_YP1		Memr[$1+23]	# Minimum of derivative profile
define	SF_YP2		Memr[$1+24]	# Maximum of derivative profile
define	SF_N		Memi[$1+25]	# Number of points not deleted
define	SF_NSFD		Memi[$1+26]	# Number of data points
define	SF_SFDS		Memi[$1+27]	# Pointer to data structures
define	SF_NS		Memi[$1+28]	# Number of stars not deleted
define	SF_NSTARS	Memi[$1+29]	# Number of stars
define	SF_STARS	Memi[$1+30]	# Pointer to star groups
define	SF_NF		Memi[$1+31]	# Number of focuses not deleted
define	SF_NFOCUS	Memi[$1+32]	# Number of different focus values
define	SF_FOCUS	Memi[$1+33]	# Pointer to focus groups
define	SF_NI		Memi[$1+34]	# Number of images not deleted
define	SF_NIMAGES	Memi[$1+35]	# Number of images
define	SF_IMAGES	Memi[$1+36]	# Pointer to image groups
define	SF_BEST		Memi[$1+37]	# Pointer to best focus star

define	SF_SFD		Memi[SF_SFDS($1)+$2-1]
define	SF_SFS		Memi[SF_STARS($1)+$2-1]
define	SF_SFF		Memi[SF_FOCUS($1)+$2-1]
define	SF_SFI		Memi[SF_IMAGES($1)+$2-1]

# Basic data structure.
define	SFD		89
define	SFD_IMAGE	Memc[P2C($1)]	# Image name
define	SFD_DATA	Memi[$1+40]	# Pointer to real image raster
define	SFD_RADIUS	Memr[$1+41]	# Profile radius
define	SFD_NP		Memi[$1+42]	# Number of profile points
define	SFD_NPMAX	Memi[$1+43]	# Maximum number of profile points
define	SFD_X1		Memi[$1+44]	# Image raster limits
define	SFD_X2		Memi[$1+45]
define	SFD_Y1		Memi[$1+46]
define	SFD_Y2		Memi[$1+47]
define	SFD_ID		Memi[$1+48]	# Star ID
define	SFD_X		Memr[$1+49]	# Star X position
define	SFD_Y		Memr[$1+50]	# Star Y position
define	SFD_F		Memr[$1+51]	# Focus
define	SFD_R		Memr[$1+52]	# Radius
define	SFD_M		Memr[$1+53]	# Magnitude
define	SFD_E		Memr[$1+54]	# Ellipticity
define	SFD_PA		Memr[$1+55]	# Position angle
define	SFD_ASI1	Memi[$1+56]	# Pointer to enclosed flux profile
define	SFD_ASI2	Memi[$1+57]	# Pointer to derivative profile
define	SFD_YP1		Memr[$1+58]	# Minimum of derivative profile
define	SFD_YP2		Memr[$1+59]	# Maximum of derivative profile
define	SFD_FWHM	Memr[$1+$2+59]	# FWHM vs level=0.5*i (i=1-19)
define	SFD_BKGD	Memr[$1+79]	# Background value
define	SFD_BKGD1	Memr[$1+80]	# Original background value
define	SFD_MISO	Memr[$1+81]	# Moment isophote
define	SFD_PEAK	Memr[$1+82]	# Peak estimate
define	SFD_DFWHM	Memr[$1+83]	# Direct FWHM
define	SFD_STATUS	Memi[$1+84]	# Status
define	SFD_NSAT	Memi[$1+85]	# Number of saturated pixels
define	SFD_SFS		Memi[$1+86]	# Pointer to star group
define	SFD_SFF		Memi[$1+87]	# Pointer to focus group
define	SFD_SFI		Memi[$1+88]	# Pointer to image group


# Structure grouping data by star.
define	SFS		($1+7)
define	SFS_ID		Memi[$1]	# Star ID
define	SFS_F		Memr[$1+1]	# Best focus
define	SFS_R		Memr[$1+2]	# Best radius
define	SFS_M		Memr[$1+3]	# Average magnitude
define	SFS_N		Memi[$1+4]	# Number of points used
define	SFS_NF		Memi[$1+5]	# Number of focuses
define	SFS_NSFD	Memi[$1+6]	# Number of data points
define	SFS_SFD		Memi[$1+$2+6]	# Array of data structures


# Structure grouping stars by focus values.
define	SFF		($1+5)
define	SFF_F		Memr[$1]	# Focus
define	SFF_R		Memr[$1+1]	# Average radius
define	SFF_N		Memi[$1+2]	# Number in average
define	SFF_NI		Memi[$1+3]	# Number of images
define	SFF_NSFD	Memi[$1+4]	# Number of data points
define	SFF_SFD		Memi[$1+$2+4]	# Array of data structures


# Structure grouping stars by image.
define	SFI		($1+42)
define	SFI_IMAGE	Memc[P2C($1)]	# Image
define	SFI_N		Memi[$1+40]	# Number in imagE
define	SFI_NSFD	Memi[$1+41]	# Number of data points
define	SFI_SFD		Memi[$1+$2+41]	# Array of data structures
