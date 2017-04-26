# Data structure for calibration image database.

define	CAL_LEN		(9*($1))		# Length of calib structure
define	CAL_KEY		Memi[$1+($2)*9-1]	# Pointer to key string
define	CAL_FLAG	Memi[$1+($2)*9-2]	# Flag for deferred mapping
define	CAL_AMPMERGE	Memi[$1+($2)*9-3]	# Merged image
define	CAL_IMAGE	Memi[$1+($2)*9-4]	# Pointer to image name string
define	CAL_CCDTYPE	Memi[$1+($2)*9-5]	# CCD type
define	CAL_AMP		Memi[$1+($2)*9-6]	# Pointer to amp identifier
define	CAL_CCD		Memi[$1+($2)*9-7]	# Pointer to ccd identifier
define	CAL_SUBSET	Memi[$1+($2)*9-8]	# Pointer to subset identifier
define	CAL_NSCAN	Memi[$1+($2)*9-9]	# Number of integrated scan lines

define	KEY		Memc[CAL_KEY($1,$2)]
define	FLAG		CAL_FLAG($1,$2)
define	AMPMERGE	CAL_AMPMERGE($1,$2)
define	IMAGE		Memc[CAL_IMAGE($1,$2)]
define	CCDTYPE		CAL_CCDTYPE($1,$2)
define	AMP		Memc[CAL_AMP($1,$2)]
define	CCD		Memc[CAL_CCD($1,$2)]
define	SUBSET		Memc[CAL_SUBSET($1,$2)]
define	NSCAN		CAL_NSCAN($1,$2)
