# MOSPROC.COM -- Common for processing flags.

bool	trim		# Trim images
int	proc		# Proc flag
real	blank		# Blank fill value
real	sample		# Sample size for determining average overscan level

common	/mosproc/ trim, proc, blank, sample
