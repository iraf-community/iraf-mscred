# MOSIM.H -- Definition for mosaic image structure.
#
define	LEN_MOSIM	5			# Length of mosaic structure.

define	MI_NIMS		Memi[$1]		# Number of sub-images mapped.
define	MI_RNAME	Memi[$1+1]		# Rootname of mosaic images.
define	MI_IMS		Memi[$1+2]		# Array of IMIO pointers.
define	MI_MGS		Memi[$1+3]		# Array of mosgeom pointers.
define	MI_SB		Memi[$1+4]		# Section data structure.

# Macros to return individual array items
define 	MI_IM		Memi[MI_IMS($1)+$2-1] 	# IMIO    pointer for image $2.
define 	MI_MG		Memi[MI_MGS($1)+$2-1] 	# Mosgeom pointer for image $2.
define 	MI_DATA		SB_DATA(MI_SB($1)) 	# Pointer to data buffer
# Mosgeom pointer for composite
define 	MI_CMG		Memi[MI_MGS($1)+MI_NIMS($1)] 


# Section data sub-structure. This is used to store a (small) section of the
# tiled mosaic in memory.  A substructure is used to simplify future extension
# to multiple buffers.
#
# The saved section coordinates are relative to the mosaic CCDSEC.
define	LEN_SECBUFF	6
define	SB_DATA		Memi[$1]		# Pointer to data buffer.
define	SB_X1		Memi[$1+1]		# Section starting column.
define	SB_X2		Memi[$1+2]		# Section ending   column.
define	SB_Y1		Memi[$1+3]		# Section starting line.
define	SB_Y2		Memi[$1+4]		# Section ending   line.
define	SB_PIXTYPE	Memi[$1+5]		# Data type of array.

# Valid mosaic image names have the form rootname.ampid.??h.
define	MI_TEMPLATE	"%s_%s.??h"		# Template image name.
define	MI_DEFEXT	"*"			# Default extension.
