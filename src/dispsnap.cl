# DISPSNAP -- Display and snap and image using MSCDISPLAY.
# This task requires an image display server to be running.

procedure dispsnap (input, output, format)

file	input			{prompt="Input mosaic file or single image"}
file	output			{prompt="Output rootname"}
string	format			{prompt="Output format"}
file	coords = ""		{prompt="Coordinate list (optional)"}
string	radii = "40"		{prompt="Radii of coordinate circles"}
string	size = "imt1024"	{prompt="Display buffer size"}
string	outbands = ""		{prompt="Export outbands function"}
file	logfile = ""		{prompt="Logfile"}

begin
	file	in, root, out, logf, temp
	string	fmt

	in = input
	root = output
	fmt = format
	out = root // "." // fmt

	if (access (out))
	    error (1, "DISPSNAP: Output already exists ("//out//")")

	set stdimage = (size)

	logf = logfile
	if (logf == "")
	    logf = "dev$null"

	printf ("DISPSNAP: %s -> %s\n", in, out, >> logf)
	mscdisplay (in, 1, mimpars="", check=no, onepass=no, bpmask="BPM",
	    bpdisplay="none", bpcolors="red", overlay="", ocolors="+203",
	    erase=yes, border_erase=no, select_frame=yes, repeat=no,
	    fill=no, zscale=yes, contrast=0.25, zrange=yes, zmask="",
	    zcombine="auto", nsample=1000, order=0, z1=0., z2=1000.,
	    ztrans="linear", lutfile="", extname="",
	    xgap=72, ygap=36, process=no, >> logf)

	if (coords != "")
	    if (access (coords))
		msctvmark (coords, 1, wcs="world", mark="circle", radii=radii,
		    lengths="0", font="raster", color=204, label=no,
		    nxoffset=0, nyoffset=0, pointsize=3, txsize=1)

	temp = mktemp ("tmp")
	tvmark (1, "", logfile="", autolog=no, outimage=temp,
	    deletions="", commands="", mark="circle", radii="20",
	    lengths="0", font="raster", color=205, label=no, number=no,
	    nxoffset=0, nyoffset=0, pointsize=3, txsize=1, tolerance=1.5,
	    interactive=no)
	
	export (temp, root, fmt, header="yes", outtype="", outbands=outbands,
	    interleave=0, bswap="no", verbose=no, >& "dev$null")

	printf ("<IMG SRC=\"%s\">\n", out, >> logf)

	imdelete (temp, verify-)
end
