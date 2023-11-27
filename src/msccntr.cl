# MSCCNTR -- Use apphot.center to centroid. 
#
# The coords parameter is both the input initial coordinates and the output
# centered coordinates.

procedure msccntr (image, coords)

file	image		{prompt="Input image"}
file	coords		{prompt="Coordinates"}

int	cbox = 11	{prompt="Centering box (pixels)"}
real	maxshift = 2	{prompt="Maximum center shift (pixels)"}
string	noise = "poisson" {prompt="Noise model"}
string	ccdread = ""	{prompt="Read noise keyword"}
string	gain = ""	{prompt="Gain keyword"}
real	readnoise = 0.	{prompt="Read noise value"}
real	epadu = 1.	{prompt="Gain value"}

begin
	file	im, crds, tmp

	im = image
	crds = coords
	tmp = mktemp ("tmp$iraf")

	# Center the input coordinates to the temporary database file.
	apphot.center (im, coords=crds, output=tmp, datapars="", centerpars="",
	    cbox=cbox, maxshift=maxshift, plotfile="", interactive-,
	    verify-, verbose-, calgorithm="centroid", cthreshold=0.,
	    minsnratio=0., cmaxiter=10, clean=no, mkcenter=no, scale=1.,
	    emission=yes, sigma=INDEF, datamin=INDEF, datamax=INDEF,
	    noise=noise, ccdread=ccdread, gain=gain, readnoise=readnoise,
	    epadu=epadu, exposure="", airmass="", filter="", obstime="",
	    itime=1., xairmass=INDEF, ifilter="INDEF", otime="INDEF")

	# Extract the centered coordinates from the database.
	delete (crds, verify-)
	txdump (tmp, "xcenter,ycenter,xshift,yshift,xerr,yerr,cier,cerror", yes,
	    headers-, parameters-, > crds)

	delete (tmp, verify-)
end
