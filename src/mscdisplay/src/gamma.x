include <mach.h>

# SIGNORM -- Calculate the ratio of the k-clipped sigma to the classical 
# sigma for a normal distribution.

double procedure signorm (ksigma)

double	ksigma			#I Clipping threshold expressed in sigma.
double	result			#O Ratio of clipped to classical sigma.

double	x
double	gammp()

begin
	x = 0.5d0 * ksigma**2
	if (x > 0.0d0) {
	    result = sqrt (gammp (1.5d0, x) / gammp (0.5d0, x))
	} else {
	    result = 0.0d0
	}
	return (result)
end
	
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Copyright(c) 1986 Numerical Recipes Software.

# GAMMP -- returns the incomplete gamma function P(a,x)
double procedure gammp (a, x)

double	a
double	x

double	result

double	gamcf(), gamser()

begin
	if (x < 0.0d0)
	    call error (0, "GAMMP -- X must be non-negative")

	if (a <= 0.0d0)
	    call error (0, "GAMMP -- A must be positive")


	if (x < a + 1.0d0) {
	   result = gamser (a, x)
	} else {
	    result = 1.0d0 - gamcf (a, x)
	}

	return (result)
end

# GAMMQ -- returns the incomplete gamma function Q(a, x) = 1.0 - P(a,x)
double procedure gammq (a, x)

double	a
double	x

double	gammp

double	gamcf(), gamser()

begin
	if (x < 0.0d0)
	    call error (0, "GAMMP -- X must be non-negative")

	if (a <= 0.0d0)
	    call error (0, "GAMMP -- A must be positive")


	if (x < a + 1.0d0) {
	   gammp = 1.0d0 - gamser (a, x)
	} else {
	    gammp = gamcf (a, x)
	}

	return (gammp)
end

define	ITMAX	500
define	EPS	EPSILOND * 2.0d0

# Evaluates the incomplete gamma function P(a,x) by its series representation.
double procedure gamser (a, x)

double	a
double	x

double	result

double	gln, ap, sum, del
int	n

double	dgammln()

begin	
	gln = dgammln (a)
	if (x <= 0.0d0) {
	    if (x < 0.0d0)
		call error (0, "GSER -- X must be non-negative")
	    result = 0.0d0
	    return (result)
	}

	ap  = a
	sum = 1.0d0 / a
	del = sum

	n = 1
	repeat {
	    ap  = ap + 1.0d0
	    del = del * x  / ap
	    sum = sum + del
	    n = n + 1
	    if (n > ITMAX)
		call error (0, "GAMSER -- A to large or itmax too small")
	} until (abs(del) < abs (sum) * EPS) 
	
	result = sum * exp (-x + a*log(x) - gln)

	return (result)
end

# GAMCF -- Evaluates incomplete gamma function Q(a, x) via its continued 
# fraction representation.

double	procedure gamcf (a, x)

double	a
double	x

double	result

double	gln, gold, g, an, ana, a0, a1, b0, b1, fac, anf
int	n

double	dgammln()

begin

	gln = dgammln (a)
	gold = 0.0d0
	a0   = 1.0d0
	a1   = x
	b0   = 0.0d0
	b1   = 1.0d0
	fac  = 1.0d0

	n = 1
	repeat {

	    an  = double (n)
	    ana = an - a
	    a0  = (a1 + a0 * ana) * fac
	    b0  = (b1 + b0 * ana) * fac
	    anf = an * fac
	    a1  = x * a0 + anf * a1
	    b1  = x * b0 + anf * b1

	    if (a1 != 0.0d0) {
		fac = 1.0d0 / a1
		g   = b1 * fac
		if (abs ((g - gold) / g) < EPS)
		    break

		gold = g
	    }
	    n = n + 1
	    if (n > ITMAX)
		call error (0, "GAMCF -- A to large or itmax too small")
	}

	result = g * exp (-x + a * log(x) - gln)
	return (result)
end
