include	"mosgeom.h"

# SET_LINEBIAS	-- Set up parameters for linebias routine in common /linebias/
 
procedure set_linebias ()


int	clgeti()
real	clgetr()
double	signorm()

include "lbias.com"

begin
	itmax  = clgeti ("niterate")
	ksigma = max (clgetr ("low_reject"), clgetr ("high_reject"))
	sigcor = real (signorm (double (ksigma)))
end



# LINEBIASx -- Calculate bias level for current line as k-clipped mean across
# overscan strip
 
real procedure linebiass (data, npix)

short	data[ARB]		#I Overscan vector
int	npix			#I Number of overscan pixels
real	bias			#O Computed Bias level.

int	ngpix
real	mean, sigma, kclip, corr

int	akavrs()
errchk	akavrs()

include "lbias.com"

begin
	if (npix <=0)
	    return (0.0)
	kclip = ksigma
	corr  = sigcor
	ngpix = akavrs (data, npix, mean, sigma, kclip, corr, itmax)
	bias = real(mean)

	return (bias)
end


# LINEBIASx -- Calculate bias level for current line as k-clipped mean across
# overscan strip
 
real procedure linebiasi (data, npix)

int	data[ARB]		#I Overscan vector
int	npix			#I Number of overscan pixels
real	bias			#O Computed Bias level.

int	ngpix
real	mean, sigma, kclip, corr

int	akavri()
errchk	akavri()

include "lbias.com"

begin
	if (npix <=0)
	    return (0.0)
	kclip = ksigma
	corr  = sigcor
	ngpix = akavri (data, npix, mean, sigma, kclip, corr, itmax)
	bias = real(mean)

	return (bias)
end


# LINEBIASx -- Calculate bias level for current line as k-clipped mean across
# overscan strip
 
real procedure linebiasl (data, npix)

long	data[ARB]		#I Overscan vector
int	npix			#I Number of overscan pixels
real	bias			#O Computed Bias level.

int	ngpix
double	mean, sigma, kclip, corr

int	akavrl()
errchk	akavrl()

include "lbias.com"

begin
	if (npix <=0)
	    return (0.0)
	kclip = double(ksigma)
	corr  = double(sigcor)
	ngpix = akavrl (data, npix, mean, sigma, kclip, corr, itmax)
	bias = real(mean)

	return (bias)
end


# LINEBIASx -- Calculate bias level for current line as k-clipped mean across
# overscan strip
 
real procedure linebiasr (data, npix)

real	data[ARB]		#I Overscan vector
int	npix			#I Number of overscan pixels
real	bias			#O Computed Bias level.

int	ngpix
real	mean, sigma, kclip, corr

int	akavrr()
errchk	akavrr()

include "lbias.com"

begin
	if (npix <=0)
	    return (0.0)
	kclip = ksigma
	corr  = sigcor
	ngpix = akavrr (data, npix, mean, sigma, kclip, corr, itmax)
	bias = real(mean)

	return (bias)
end


# LINEBIASx -- Calculate bias level for current line as k-clipped mean across
# overscan strip
 
real procedure linebiasd (data, npix)

double	data[ARB]		#I Overscan vector
int	npix			#I Number of overscan pixels
real	bias			#O Computed Bias level.

int	ngpix
double	mean, sigma, kclip, corr

int	akavrd()
errchk	akavrd()

include "lbias.com"

begin
	if (npix <=0)
	    return (0.0)
	kclip = double(ksigma)
	corr  = double(sigcor)
	ngpix = akavrd (data, npix, mean, sigma, kclip, corr, itmax)
	bias = real(mean)

	return (bias)
end

