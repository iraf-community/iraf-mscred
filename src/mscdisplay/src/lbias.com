# LINEBIAS.COM -- Common block used to pass parameters to linebias
int	itmax		# Maximum number of rejection iterations
real	ksigma		# K-sigma clipping factor
real	sigcor		# Correction to K-clipped sigma

common	/lbias/ itmax, ksigma, sigcor
