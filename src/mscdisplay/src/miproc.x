include	"mosgeom.h"
include	"mosproc.h"



# MIPROCx -- Perform quick look processing on data array

procedure miprocs (mg, in, out, nx, line, overscan, novr)
pointer	mg			#I Mosgeom pointer.
short	in[ARB]			#I Raw data array.
short	out[ARB]		#O Processed array.
int	nx			#I Number of pixels.
int	line			#I image line.
short	overscan[ARB]		#I Overscan vector.
int	novr			#I Number of pixels in overscan vector.

int	i
real	k1, k2

real	linebiass()

errchk	linebiass

include "mosproc.com"

begin
	switch (proc) {

	case NONE:
	    do i = 1, nx 
		out[i] = in[i]

	case D:
	    k1 = DARK(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case G:
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = in[i] * k2

	    case DG:
	    k1 = DARK(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case C, A:
	    k1 = BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CD, AD:
	    k1 = DARK(mg) + BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CG, AG:
	    k1 = BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case CDG, ADG:
	    k1 = DARK(mg) + BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case L:
	    k1 = linebiass (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LD:
	    k1 = DARK(mg) + linebiass (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LG:
	    k1 = linebiass (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case LDG:
	    k1 = DARK(mg) + linebiass (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case F:
	    k1 = Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FD:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FG:
	    k1 = Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case FDG:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	}
end



# MIPROCx -- Perform quick look processing on data array

procedure miproci (mg, in, out, nx, line, overscan, novr)
pointer	mg			#I Mosgeom pointer.
int	in[ARB]			#I Raw data array.
int	out[ARB]		#O Processed array.
int	nx			#I Number of pixels.
int	line			#I image line.
int	overscan[ARB]		#I Overscan vector.
int	novr			#I Number of pixels in overscan vector.

int	i
real	k1, k2

real	linebiasi()

errchk	linebiasi

include "mosproc.com"

begin
	switch (proc) {

	case NONE:
	    do i = 1, nx 
		out[i] = in[i]

	case D:
	    k1 = DARK(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case G:
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = in[i] * k2

	    case DG:
	    k1 = DARK(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case C, A:
	    k1 = BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CD, AD:
	    k1 = DARK(mg) + BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CG, AG:
	    k1 = BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case CDG, ADG:
	    k1 = DARK(mg) + BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case L:
	    k1 = linebiasi (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LD:
	    k1 = DARK(mg) + linebiasi (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LG:
	    k1 = linebiasi (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case LDG:
	    k1 = DARK(mg) + linebiasi (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case F:
	    k1 = Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FD:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FG:
	    k1 = Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case FDG:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	}
end



# MIPROCx -- Perform quick look processing on data array

procedure miprocl (mg, in, out, nx, line, overscan, novr)
pointer	mg			#I Mosgeom pointer.
long	in[ARB]			#I Raw data array.
long	out[ARB]		#O Processed array.
int	nx			#I Number of pixels.
int	line			#I image line.
long	overscan[ARB]		#I Overscan vector.
int	novr			#I Number of pixels in overscan vector.

int	i
real	k1, k2

real	linebiasl()

errchk	linebiasl

include "mosproc.com"

begin
	switch (proc) {

	case NONE:
	    do i = 1, nx 
		out[i] = in[i]

	case D:
	    k1 = DARK(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case G:
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = in[i] * k2

	    case DG:
	    k1 = DARK(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case C, A:
	    k1 = BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CD, AD:
	    k1 = DARK(mg) + BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CG, AG:
	    k1 = BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case CDG, ADG:
	    k1 = DARK(mg) + BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case L:
	    k1 = linebiasl (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LD:
	    k1 = DARK(mg) + linebiasl (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LG:
	    k1 = linebiasl (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case LDG:
	    k1 = DARK(mg) + linebiasl (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case F:
	    k1 = Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FD:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FG:
	    k1 = Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case FDG:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	}
end



# MIPROCx -- Perform quick look processing on data array

procedure miprocr (mg, in, out, nx, line, overscan, novr)
pointer	mg			#I Mosgeom pointer.
real	in[ARB]			#I Raw data array.
real	out[ARB]		#O Processed array.
int	nx			#I Number of pixels.
int	line			#I image line.
real	overscan[ARB]		#I Overscan vector.
int	novr			#I Number of pixels in overscan vector.

int	i
real	k1, k2

real	linebiasr()

errchk	linebiasr

include "mosproc.com"

begin
	switch (proc) {

	case NONE:
	    do i = 1, nx 
		out[i] = in[i]

	case D:
	    k1 = DARK(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case G:
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = in[i] * k2

	    case DG:
	    k1 = DARK(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case C, A:
	    k1 = BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CD, AD:
	    k1 = DARK(mg) + BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CG, AG:
	    k1 = BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case CDG, ADG:
	    k1 = DARK(mg) + BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case L:
	    k1 = linebiasr (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LD:
	    k1 = DARK(mg) + linebiasr (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LG:
	    k1 = linebiasr (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case LDG:
	    k1 = DARK(mg) + linebiasr (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case F:
	    k1 = Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FD:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FG:
	    k1 = Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case FDG:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	}
end



# MIPROCx -- Perform quick look processing on data array

procedure miprocd (mg, in, out, nx, line, overscan, novr)
pointer	mg			#I Mosgeom pointer.
double	in[ARB]			#I Raw data array.
double	out[ARB]		#O Processed array.
int	nx			#I Number of pixels.
int	line			#I image line.
double	overscan[ARB]		#I Overscan vector.
int	novr			#I Number of pixels in overscan vector.

int	i
real	k1, k2

real	linebiasd()

errchk	linebiasd

include "mosproc.com"

begin
	switch (proc) {

	case NONE:
	    do i = 1, nx 
		out[i] = in[i]

	case D:
	    k1 = DARK(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case G:
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = in[i] * k2

	    case DG:
	    k1 = DARK(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case C, A:
	    k1 = BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CD, AD:
	    k1 = DARK(mg) + BIAS(mg)
	    do i = 1, nx
		out[i] = in[i] - k1

	case CG, AG:
	    k1 = BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case CDG, ADG:
	    k1 = DARK(mg) + BIAS(mg)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case L:
	    k1 = linebiasd (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LD:
	    k1 = DARK(mg) + linebiasd (overscan, novr)
	    do i = 1, nx
		out[i] = in[i] - k1

	case LG:
	    k1 = linebiasd (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case LDG:
	    k1 = DARK(mg) + linebiasd (overscan, novr)
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case F:
	    k1 = Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FD:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line -1]
	    do i = 1, nx
		out[i] = in[i] - k1

	case FG:
	    k1 = Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	case FDG:
	    k1 = DARK(mg) + Memr[OVRSCN(mg)+line-1]
	    k2 = GAIN(mg)
	    do i = 1, nx
		out[i] = (in[i] - k1) * k2

	}
end


