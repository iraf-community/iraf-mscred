# MOSQPROC.H -- Definitions for values of the proc flag and associated macros
# The value of the flag are defined as bit fields so that it is possible to
# add corrections to form a unique combined correction.

define	D	001B		# subtract Dark
define 	G	002B		# adjust Gain
define	C	010B		# subtract Constant overscan.
define	A	020B		# subtract Average of overscan.
define	L	040B		# subtract Line overscan.
define	F	100B		# subtract Fitted overscan.

# The following combinations are possible.
define	NONE	000B		# No processing
define	DG	003B		# No       overscan + Dark + Gain
define	CD	011B		# Constant overscan + Dark
define	CG	012B		# Constant overscan + Gain
define	CDG	013B		# Constant overscan + Dark + Gain
define	AD	021B		# Average  overscan + Dark
define	AG	022B		# Average  overscan + Gain
define	ADG	023B		# Average  overscan + Dark + Gain
define	LD	041B		# Line     overscan + Dark
define	LG	042B		# Line     overscan + Gain
define	LDG	043B		# Line     overscan + Dark + Gain
define	FD	101B		# Fitted   overscan + Dark
define	FG	102B		# Fitted   overscan + Gain
define	FDG	103B		# Fitted   overscan + Dark + Gain

define	OT_DICT		"|none|constant|average|line|fit|"
define	OT_NONE		1
define	OT_CONST	2
define	OT_AVG		3
define	OT_LINE		4
define	OT_FIT		5

# Sample size for average as fraction of height of overscan strip
define	SAMPLE		0.0025	
