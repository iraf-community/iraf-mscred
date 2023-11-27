include "mosproc.h"
include	"mosgeom.h"

# PROCSET -- Set value of proc flag according to task parameters.

procedure procset ()

#pointer	sp, buffer
#int	otype
#
#int	clgwrd()
#bool	clgetb()
#real	clgetr()

include	"mosproc.com"

begin
	proc = NONE

#	call smark (sp)
#	call salloc (buffer, SZ_LINE, TY_CHAR)
#
#	trim = clgetb ("trim")
#
#	# Overscan subtraction
#	otype = clgwrd ("overscan", Memc[buffer], SZ_LINE, OT_DICT)
#	switch (otype) {
#	case OT_CONST:
#	    proc = C
#	case OT_AVG:
#	    proc = A
#	case OT_LINE:
#	    proc = L
#	case OT_FIT:
#	    proc = F
#	default:
#	    proc = NONE
#	}
#
#	# Gain correction
#	if (clgetb ("gain"))
#	    proc = proc + G
#
#	# Dark subtraction
#	if (clgetb ("dark"))
#	    proc = proc + D
#
#	blank = clgetr("blank")
#
#	# For now use a fixed sample size
#	sample = SAMPLE
end


# Bias, Dark and Gain correct (or uncorrect) the z1 and z2 values.

procedure zproc (inmg, z1, z2, ninput, proc, direction)

pointer	inmg[ARB]		# Array of mosgeom structures.
real	z1[ARB]			# Array of z1 values.
real	z2[ARB]			# Array of z2 values.
int	ninput			# Number of values.
int	proc			# Processing flag.
int	direction		# UNCORRECT = 0, CORRECT = 1

int	i
real	k1, k2

begin

	if (direction == 1) {
	    switch (proc) {

	    # return imediately if no processing is required.
	    case NONE:
		return

	    case D:
		do i = 1, ninput {
		    k1 = DARK(inmg[i])
		    z1[i] = z1[i] - k1
		    z2[i] = z2[i] - k1
		}
	    case C, A:
		do i = 1, ninput {
		    k1 = BIAS(inmg[i])
		    z1[i] = z1[i] - k1
		    z2[i] = z2[i] - k1
		}
	    case CD, AD:
		do i = 1, ninput {
		    k1 = BIAS(inmg[i]) + DARK(inmg[i])
		    z1[i] = z1[i] - k1
		    z2[i] = z2[i] - k1
		}
	    case G:
		do i = 1, ninput {
		    k2 = GAIN(inmg[i])
		    z1[i] = z1[i] * k2
		    z2[i] = z2[i] * k2
		}
	    case DG:
		do i = 1, ninput {
		    k1 = DARK(inmg[i])
		    k2 = GAIN(inmg[i])
		    z1[i] = (z1[i] - k1) * k2
		    z2[i] = (z2[i] - k1) * k2
		}
	    case CG, AG:
		do i = 1, ninput {
		    k1 = BIAS(inmg[i])
		    k2 = GAIN(inmg[i])
		    z1[i] = (z1[i] - k1) * k2
		    z2[i] = (z2[i] - k1) * k2
		}
	    case CDG, ADG:
		do i = 1, ninput {
		    k1 = BIAS(inmg[i]) + DARK(inmg[i])
		    k2 = GAIN(inmg[i])
		    z1[i] = (z1[i] - k1) * k2
		    z2[i] = (z2[i] - k1) * k2
		}
	    }
		
	} else {

	    switch (proc) {

	    case NONE:
		return

	    case D:
		do i = 1, ninput {
		    k1 = DARK(inmg[i])
		    z1[i] = z1[i] + k1
		    z2[i] = z2[i] + k1
		}
	    case C, A:
		do i = 1, ninput {
		    k1 = BIAS(inmg[i])
		    z1[i] = z1[i] + k1
		    z2[i] = z2[i] + k1
		}
	    case CD, AD:
		do i = 1, ninput {
		    k1 = BIAS(inmg[i]) + DARK(inmg[i])
		    z1[i] = z1[i] + k1
		    z2[i] = z2[i] + k1
		}
	    case G:
		do i = 1, ninput {
		    k2 = GAIN(inmg[i])
		    z1[i] = z1[i] / k2
		    z2[i] = z2[i] / k2
		}
	    case DG:
		do i = 1, ninput {
		    k1 = DARK(inmg[i])
		    k2 = GAIN(inmg[i])
		    z1[i] = k1 + (z1[i] / k2)
		    z2[i] = k1 + (z2[i] / k2)
		}
	    case CG, AG:
		do i = 1, ninput {
		    k1 = BIAS(inmg[i])
		    k2 = GAIN(inmg[i])
		    z1[i] = k1 + (z1[i] / k2)
		    z2[i] = k1 + (z2[i] / k2)
		}
	    case CDG, ADG:
		do i = 1, ninput {
		    k1 = BIAS(inmg[i]) + DARK(inmg[i])
		    k2 = GAIN(inmg[i])
		    z1[i] = k1 + (z1[i] / k2)
		    z2[i] = k1 + (z2[i] / k2)
		}
	    }
	}
end
