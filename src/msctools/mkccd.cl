# MKCCD -- Make a CCD image from a template image.

procedure mkccd (image, template)

string	image			{prompt="Image name"}
file	template		{prompt="Template image"}
int	xsize = 100		{prompt="X size"}
int	ysize = 200		{prompt="Y size"}
int	noverscan = 32		{prompt="Overscan columns (-=left, +=right)"}
int	xoffset = 0		{prompt="X offset in template"}
int	yoffset = 0		{prompt="Y offset in template"}
int	xccdoffset = 0		{prompt="X offset in CCD"}
int	yccdoffset = 0		{prompt="Y offset in CCD"}
int	xdither = 0		{prompt="X dither"}
int	ydither = 0		{prompt="Y dither"}

real	bias = 500.		{prompt="Bias level"}
real	flat = 1.		{prompt="Flat response"}

string	imageid = "1"		{prompt="Image ID"}
string	ccdname	= "ccd1"	{prompt="CCD name"}
string	ampname = "amp1"	{prompt="Amplifier name"}

bool	verbose = yes		{prompt="Verbose?"}

begin
	file	im, tim
	int	x1, x2, y1, y2
	real	ltv1
	string	ccdsec, datasec, biassec

	im = image
	tim = template

	# Data section.
	if (noverscan < 0) {
	    x1 = 1 - noverscan ; x2 = xsize - noverscan
	} else {
	    x1 = 1; x2 = xsize
	}
	printf ("[%d:%d,%d:%d]\n", x1, x2, 1, ysize) | scan (datasec)

	# Bias section.
	if (noverscan < 0) {
	    x1 = 1; x2 = -noverscan
	} else {
	    x1 = 1 + xsize; x2 = xsize + noverscan
	}
	printf ("[%d:%d,%d:%d]\n", x1, x2, 1, ysize) | scan (biassec)
	    
	# Make image.
	if (tim == "zero") {
	    x1 = xsize + abs (noverscan)
	    y1 = ysize
	    mkpattern (im, output="", pattern="constant", option="replace",
		v1=0., v2=1., size=1, title="", pixtype="ushort", ndim=2,
		ncols=x1, nlines=y1, header="")
	} else if (tim == "flat") {
	    x1 = xsize + abs (noverscan)
	    y1 = ysize
	    mkpattern (im, output="", pattern="constant", option="replace",
	        v1=0., v2=1., size=1, title="", pixtype="ushort", ndim=2,
		ncols=x1, nlines=y1, header="")
	    mkpattern (im//datasec, output="", pattern="constant",
	        option="replace", v1=1000., v2=1., size=1)
	} else {
	    x1 = 1 + abs(noverscan) + xoffset + xdither
	    x2 = xsize + abs(noverscan) + xoffset + xdither
	    y1 = 1 + yoffset + ydither
	    y2 = ysize + yoffset + ydither
	    if (noverscan < 0)
		x1 = x1 + noverscan
	    else
		x2 = x2 + noverscan
	    printf ("[%d:%d,%d:%d]\n", x1, x2, y1, y2) | scan (ccdsec)

	    imcopy (tim//ccdsec, im, verbose=verbose)
	    mkpattern (im//biassec, output="", pattern="constant",
	        option="replace", v1=0., v2=1., size=1)
	}

	# Apply response.
	mkpattern (im//datasec, output="", pattern="constant",
	    option="multiply", v1=flat, v2=1., size=1, title="",
	    pixtype="ushort", ndim=2, ncols=x1, nlines=y1, header="")

	# Add bias.
	mkpattern (im, output="", pattern="constant", option="add",
	    v1=bias, v2=1., size=1, title="", pixtype="ushort", ndim=2,
	    ncols=x1, nlines=y1, header="")

	# Adjust header.

	# OBSTYPE
	if (tim == "zero")
	    hedit (im, "obstype", "zero", add+, addonly-, verify-, update+,
		show=verbose)
	else if (tim == "flat")
	    hedit (im, "obstype", "flat", add+, addonly-, verify-, update+,
		show=verbose)
	else
	    hedit (im, "obstype", "object", add+, addonly-, verify-, update+,
		show=verbose)

	# DATASEC, TRIMSEC, BIASSEC
	hedit (im, "datasec", datasec, add+, addonly-, verify-, update+,
	    show=verbose)
	hedit (im, "trimsec", datasec, add+, addonly-, verify-, update+,
	    show=verbose)
	hedit (im, "biassec", biassec, add+, addonly-, verify-, update+,
	    show=verbose)

	# CCDSEC and LTV
	x1 = 1 + xccdoffset; x2 = xsize + xccdoffset
	y1 = 1 + yccdoffset; y2 = ysize + yccdoffset
	printf ("[%d:%d,%d:%d]\n", x1, x2, y1, y2) | scan (ccdsec)
	hedit (im, "ccdsec", ccdsec, add+, addonly-, verify-, update+,
	    show=verbose)
	x1 = -xccdoffset + max (0, -noverscan)
	y1 = -yccdoffset
	hedit (im, "ltv1", x1, add+, addonly-, verify-, update+, show=verbose)
	hedit (im, "ltv2", y1, add+, addonly-, verify-, update+, show=verbose)

	# DETSEC
	x1 = 1 + xoffset; x2 = xsize + xoffset
	y1 = 1 + yoffset; y2 = ysize + yoffset
	printf ("[%d:%d,%d:%d]\n", x1, x2, y1, y2) | scan (ccdsec)
	hedit (im, "detsec", ccdsec, add+, addonly-, verify-, update+,
	    show=verbose)
	hedit (im, "dtv1", xoffset, add+, addo-, verify-, update+, show=verbose)
	hedit (im, "dtv2", yoffset, add+, addo-, verify-, update+, show=verbose)
	hedit (im, "dtm1_1", 1., add+, addonly-, verify-, update+, show=verbose)
	hedit (im, "dtm2_2", 1., add+, addonly-, verify-, update+, show=verbose)

	# IMAGEID, CCDNAME, and AMPNAME
	hedit (im, "imageid", imageid, add+, verify-, update+, show=verbose)
	hedit (im, "ccdname", ccdname, add+, verify-, update+, show=verbose)
	hedit (im, "ampname", ampname, add+, verify-, update+, show=verbose)
end
