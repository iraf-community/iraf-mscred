include	<mach.h>

# AKAVG -- Compute the mean and standard deviation of a sample array by
# iteratively rejecting points further than KSIG from the mean.
# The k-clipped sigma is corrected to match the classical sigma for a
# normal distribution. The number of pixels remaining in the sample upon
# termination is returned as the function value.



int procedure akavgs (a, npix, mean, sigma, kclip, itmax)

short	a[ARB]			# input data array
real	mean, sigma, kclip
int	npix, itmax, ngpix
int	akavrs()

double  corr, signorm()

begin

	# Compute ratio of k-clipped sigma to classical sigma for a normal
        # distribution
	corr  = signorm (double(kclip))

	ngpix = akavrs (a, npix, mean, sigma, kclip, real(corr), itmax)

	return (ngpix)
end

# AKAVRx -- As AKAVGx except that the sigma normalisation is supplied by
# the caller.

int procedure akavrs (a, npix, mean, sigma, kclip, signorm, itmax)

short	a[ARB]			# input data array
real	mean, sigma, kclip, signorm, deviation, lcut, hcut
int	itmax, npix, ngpix, old_ngpix, iter, awvgs()

begin
	lcut = -MAX_REAL				# no rejection to start
	hcut =  MAX_REAL
	ngpix = MAX_INT

	# Iteratively compute mean and sigma and reject outliers.
	# We exit when no more pixels are rejected, when there are
	# no more pixels, or when we have completed itmax iterations.

	do iter = 1, itmax {
	    old_ngpix = ngpix
	    ngpix = awvgs (a, npix, mean, sigma, lcut, hcut)

#	    call eprintf ("iter=%d nrej=%d mean=%f sigma=%f")
#		call pargi (iter)
#		call pargi (npix - ngpix)
#		call parg$t (mean)
#		call parg$t (sigma)

	    # Correct for clipping
	    if (iter > 1 && signorm > EPSILOND)
		sigma = sigma / signorm

#	    call eprintf (" sigma_corr=%f\n")
#		call parg$t (sigma)

	    if (ngpix <= 1 || sigma <= EPSILONR)
		break

	    if (ngpix >= old_ngpix)
		break

	    deviation = sigma * kclip

	    lcut = mean - deviation			# compute window
	    hcut = mean + deviation
	}

	return (ngpix)

end



int procedure akavgi (a, npix, mean, sigma, kclip, itmax)

int	a[ARB]			# input data array
real	mean, sigma, kclip
int	npix, itmax, ngpix
int	akavri()

double  corr, signorm()

begin

	# Compute ratio of k-clipped sigma to classical sigma for a normal
        # distribution
	corr  = signorm (double(kclip))

	ngpix = akavri (a, npix, mean, sigma, kclip, real(corr), itmax)

	return (ngpix)
end

# AKAVRx -- As AKAVGx except that the sigma normalisation is supplied by
# the caller.

int procedure akavri (a, npix, mean, sigma, kclip, signorm, itmax)

int	a[ARB]			# input data array
real	mean, sigma, kclip, signorm, deviation, lcut, hcut
int	itmax, npix, ngpix, old_ngpix, iter, awvgi()

begin
	lcut = -MAX_REAL				# no rejection to start
	hcut =  MAX_REAL
	ngpix = MAX_INT

	# Iteratively compute mean and sigma and reject outliers.
	# We exit when no more pixels are rejected, when there are
	# no more pixels, or when we have completed itmax iterations.

	do iter = 1, itmax {
	    old_ngpix = ngpix
	    ngpix = awvgi (a, npix, mean, sigma, lcut, hcut)

#	    call eprintf ("iter=%d nrej=%d mean=%f sigma=%f")
#		call pargi (iter)
#		call pargi (npix - ngpix)
#		call parg$t (mean)
#		call parg$t (sigma)

	    # Correct for clipping
	    if (iter > 1 && signorm > EPSILOND)
		sigma = sigma / signorm

#	    call eprintf (" sigma_corr=%f\n")
#		call parg$t (sigma)

	    if (ngpix <= 1 || sigma <= EPSILONR)
		break

	    if (ngpix >= old_ngpix)
		break

	    deviation = sigma * kclip

	    lcut = mean - deviation			# compute window
	    hcut = mean + deviation
	}

	return (ngpix)

end



int procedure akavgl (a, npix, mean, sigma, kclip, itmax)

long	a[ARB]			# input data array
double	mean, sigma, kclip
int	npix, itmax, ngpix
int	akavrl()

double  corr, signorm()

begin

	# Compute ratio of k-clipped sigma to classical sigma for a normal
        # distribution
	corr  = signorm (kclip)

	ngpix = akavrl (a, npix, mean, sigma, kclip, corr, itmax)

	return (ngpix)
end

# AKAVRx -- As AKAVGx except that the sigma normalisation is supplied by
# the caller.

int procedure akavrl (a, npix, mean, sigma, kclip, signorm, itmax)

long	a[ARB]			# input data array
double	mean, sigma, kclip, signorm, deviation, lcut, hcut
int	itmax, npix, ngpix, old_ngpix, iter, awvgl()

begin
	lcut = -MAX_REAL				# no rejection to start
	hcut =  MAX_REAL
	ngpix = MAX_INT

	# Iteratively compute mean and sigma and reject outliers.
	# We exit when no more pixels are rejected, when there are
	# no more pixels, or when we have completed itmax iterations.

	do iter = 1, itmax {
	    old_ngpix = ngpix
	    ngpix = awvgl (a, npix, mean, sigma, lcut, hcut)

#	    call eprintf ("iter=%d nrej=%d mean=%f sigma=%f")
#		call pargi (iter)
#		call pargi (npix - ngpix)
#		call parg$t (mean)
#		call parg$t (sigma)

	    # Correct for clipping
	    if (iter > 1 && signorm > EPSILOND)
		sigma = sigma / signorm

#	    call eprintf (" sigma_corr=%f\n")
#		call parg$t (sigma)

	    if (ngpix <= 1 || sigma <= EPSILOND)
		break

	    if (ngpix >= old_ngpix)
		break

	    deviation = sigma * kclip

	    lcut = mean - deviation			# compute window
	    hcut = mean + deviation
	}

	return (ngpix)

end



int procedure akavgr (a, npix, mean, sigma, kclip, itmax)

real	a[ARB]			# input data array
real	mean, sigma, kclip
int	npix, itmax, ngpix
int	akavrr()

double  corr, signorm()

begin

	# Compute ratio of k-clipped sigma to classical sigma for a normal
        # distribution
	corr  = signorm (double(kclip))

	ngpix = akavrr (a, npix, mean, sigma, kclip, real(corr), itmax)

	return (ngpix)
end

# AKAVRx -- As AKAVGx except that the sigma normalisation is supplied by
# the caller.

int procedure akavrr (a, npix, mean, sigma, kclip, signorm, itmax)

real	a[ARB]			# input data array
real	mean, sigma, kclip, signorm, deviation, lcut, hcut
int	itmax, npix, ngpix, old_ngpix, iter, awvgr()

begin
	lcut = -MAX_REAL				# no rejection to start
	hcut =  MAX_REAL
	ngpix = MAX_INT

	# Iteratively compute mean and sigma and reject outliers.
	# We exit when no more pixels are rejected, when there are
	# no more pixels, or when we have completed itmax iterations.

	do iter = 1, itmax {
	    old_ngpix = ngpix
	    ngpix = awvgr (a, npix, mean, sigma, lcut, hcut)

#	    call eprintf ("iter=%d nrej=%d mean=%f sigma=%f")
#		call pargi (iter)
#		call pargi (npix - ngpix)
#		call parg$t (mean)
#		call parg$t (sigma)

	    # Correct for clipping
	    if (iter > 1 && signorm > EPSILOND)
		sigma = sigma / signorm

#	    call eprintf (" sigma_corr=%f\n")
#		call parg$t (sigma)

	    if (ngpix <= 1 || sigma <= EPSILONR)
		break

	    if (ngpix >= old_ngpix)
		break

	    deviation = sigma * kclip

	    lcut = mean - deviation			# compute window
	    hcut = mean + deviation
	}

	return (ngpix)

end



int procedure akavgd (a, npix, mean, sigma, kclip, itmax)

double	a[ARB]			# input data array
double	mean, sigma, kclip
int	npix, itmax, ngpix
int	akavrd()

double  corr, signorm()

begin

	# Compute ratio of k-clipped sigma to classical sigma for a normal
        # distribution
	corr  = signorm (kclip)

	ngpix = akavrd (a, npix, mean, sigma, kclip, corr, itmax)

	return (ngpix)
end

# AKAVRx -- As AKAVGx except that the sigma normalisation is supplied by
# the caller.

int procedure akavrd (a, npix, mean, sigma, kclip, signorm, itmax)

double	a[ARB]			# input data array
double	mean, sigma, kclip, signorm, deviation, lcut, hcut
int	itmax, npix, ngpix, old_ngpix, iter, awvgd()

begin
	lcut = -MAX_REAL				# no rejection to start
	hcut =  MAX_REAL
	ngpix = MAX_INT

	# Iteratively compute mean and sigma and reject outliers.
	# We exit when no more pixels are rejected, when there are
	# no more pixels, or when we have completed itmax iterations.

	do iter = 1, itmax {
	    old_ngpix = ngpix
	    ngpix = awvgd (a, npix, mean, sigma, lcut, hcut)

#	    call eprintf ("iter=%d nrej=%d mean=%f sigma=%f")
#		call pargi (iter)
#		call pargi (npix - ngpix)
#		call parg$t (mean)
#		call parg$t (sigma)

	    # Correct for clipping
	    if (iter > 1 && signorm > EPSILOND)
		sigma = sigma / signorm

#	    call eprintf (" sigma_corr=%f\n")
#		call parg$t (sigma)

	    if (ngpix <= 1 || sigma <= EPSILOND)
		break

	    if (ngpix >= old_ngpix)
		break

	    deviation = sigma * kclip

	    lcut = mean - deviation			# compute window
	    hcut = mean + deviation
	}

	return (ngpix)

end


