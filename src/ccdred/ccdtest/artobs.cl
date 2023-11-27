# ARTOBS -- Make a CCD observation

procedure artobs (image, exptime, ccdtype)

string	image			{prompt="Image name"}
real	exptime			{prompt="Exposure time"}
string	ccdtype			{prompt="CCD type"}

string	title=""		{prompt="Title"}
file	header=""		{prompt="Header template"}
int	ncols=132		{prompt="Number of columns"}
int	nlines=100		{prompt="Number of lines"}
string	filter=""		{prompt="Filter"}
string	datasec="[1:100,1:100]"	{prompt="Data section"}
string	trimsec="[3:98,3:98]"	{prompt="Trim section"}
string	biassec="[103:130,*]"	{prompt="Bias section"}

file	imdata=""		{prompt="Image data"}
real	skyrate=0.		{prompt="Sky count rate"}
file	badpix=""		{prompt="Bad pixel mask"}
real	biasval=500.		{prompt="Bias value"}
real	badval=500.		{prompt="Bad pixel value"}
real	zeroval=100.		{prompt="Zero level value"}
real	darkrate=1.		{prompt="Dark count rate"}
real	zeroslope=0.01		{prompt="Slope of zero level"}
real	darkslope=0.002		{prompt="Slope of dark count rate"}
real	flatslope=0.0003	{prompt="Flat field slope"}
real	sigma=5.		{prompt="Gaussian sigma"}
int	seed=0			{prompt="Random number seed"}
bool	overwrite=no		{prompt="Overwrite existing image?"}

begin
	int	c1, c2, l1, l2
	real	exp, value1, value2, valslope
	string	im, obstype, s

	im = image
	exp = exptime
	obstype = ccdtype

	if (access (im//".imh") == yes)
	    im = im // ".imh"
	if (access (im//".hhh") == yes)
	    im = im // ".hhh"
	if (access (im) == yes) {
	    if (overwrite == yes)
		imdelete (im, verify=no)
	    else
	        return
	}

	# Create the image.
	mkpattern (im, output="", pattern="constant", option="replace",
	    v1=0., v2=1., size=1, title=title, pixtype="short", ndim=2,
	    ncols=ncols, nlines=nlines, header=header)

	# Add a data image.
	if (access (imdata//".imh") == yes)
	    imdata = imdata // ".imh"
	if (access (imdata//".hhh") == yes)
	    imdata = imdata // ".hhh"
	if (access (imdata) == yes)
	    imcopy (imdata//datasec, im//datasec, verbose=no)

	# Add sky.
	value1 = exp * skyrate
	if (value1 != 0.)
	    mkpattern (im//datasec, output="", pattern="constant",
		option="add", v1=value1, v2=1., size=1)

	# Add flat field response.
	if (flatslope != 0.) {
	    valslope = (ncols + nlines) / 2. * flatslope
	    value1 = 1 - valslope
	    value2 = 1 + valslope
	    mkpattern (im//datasec, output="", pattern="slope",
		option="multiply", v1=value1, v2=value2, size=1)
	}
	    
	# Add zero level and dark count.
	value2 = zeroval + exp * darkrate
	valslope = (ncols + nlines) / 2. * (zeroslope + exp * darkslope)
	value1 = value2 - valslope
	value2 = value2 + valslope
	if ((value1 != 0.) && (value2 != 0.))
	    mkpattern (im//datasec, output="", pattern="slope",
		option="add", v1=value1, v2=value2, size=1)

	# Add bias.
	if (biasval != 0.)
	    mkpattern (im, output="", pattern="constant",
		option="add", v1=biasval, v2=1., size=1)
	
	# Add noise.
	mknoise (im, output="", header="", background=0., gain=1.,
	    rdnoise=sigma, poisson=no, seed=seed, cosrays="", ncosrays=0,
	    energy=30000., radius=0.5, ar=1., pa=0., comments=no)

        # Set bad pixels.
        if (access (badpix)) {
            list = badpix
            while (fscan (list, c1, c2, l1, l2) != EOF) {
                if (nscan() != 4)
                    next
                c1 = max (1, c1)
                c2 = min (ncols, c2)
                l1 = max (1, l1)
                l2 = min (nlines, l2)
                s = "["//c1//":"//c2//","//l1//":"//l2//"]"
		mkpattern (im//s, output="", pattern="constant",
		    option="replace", v1=badval, v2=1., size=1)
	    
            }
        }

#	# Set bad pixels.
#	if (access (badpix) || access (badpix//".pl")) {
#	    imcopy (im//datasec, "tmp$", verbose=no)
#	    imexpr ("c ? a : b", im//datasec, badval, "tmp$"//im,
#		badpix//datasec, verbose=no)
#	    imdelete ("tmp$"//im, verify=no)
#	    hedit (im, "BPM", badpix, add=yes, verify=no, show=no, update=yes)
#	}

	# Set image header
	ccdhedit (im, "exptime", exp, type="real")
	if (obstype != "")
	    ccdhedit (im, "imagetyp", obstype, type="string")
	if (datasec != "")
	    ccdhedit (im, "datasec", datasec, type="string")
	if (trimsec != "")
	    ccdhedit (im, "trimsec", trimsec, type="string")
	if (biassec != "")
	    ccdhedit (im, "biassec", biassec, type="string")
	if (filter != "")
	    ccdhedit (im, "subset", filter, type="string")
end
