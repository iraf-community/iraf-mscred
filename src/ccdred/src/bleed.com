int	nc, nl			#I Image size
int	c1, c2, l1, l2		#I Section (in output pixels) to be searched
int	ic1, il1		#I Offset between input and output images
real	sth			#I Saturation threshold
int	sgrw			#I Grow radius for saturated pixels
int	sval			#I Saturated pixel mask value
real	bth			#I Bleed pixel threshold
int	bgrw			#I Grow radius for bleed pixels
int	bval			#I Bleed trail mask value
int	tlen			#I Minimum bleed trail length
int	nbehind			#I Number of pixels behind line to buffer
int	nahead			#I Number of pixels ahead of line to buffer
int	nbufs			#I Number of output buffered lines
pointer	obufs			#I Output data buffers
pointer	ombufs			#I Output mask buffers
pointer	noibufs			#I Fixpix data buffers
pointer	counts			#I Array of bleed pixel counts.
common	/bleedcom/ nc, nl, c1, c2, l1, l2, ic1, il1, sth, sgrw, sval,
	bth, bgrw, bval, tlen, nbehind, nahead, nbufs, obufs, ombufs, noibufs,
	counts
