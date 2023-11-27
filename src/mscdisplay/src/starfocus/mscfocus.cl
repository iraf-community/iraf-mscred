# MSCFOCUS -- Mosaic focus measuring task.
# This is customized to the NOAO header keywords.

procedure mscfocus (images)

string	images		{prompt="List of Mosaic focus images"}
int	frame = 1	{prompt="Display frame to use"}
real	level = 0.5	{prompt="Measurement level (fraction or percent)"}
string	size = "FWHM"	{prompt="Size to display",
			 enum="Radius|FWHM|GFWHM|MFWHM"}
real	scale = 0.25	{prompt="Pixel scale"}
real	radius = 10.	{prompt="Measurement radius (pixels)"}
real	sbuffer = 5.	{prompt="Sky buffer (pixels)"}
real	swidth = 5.	{prompt="Sky width (pixels)"}
real	saturation = INDEF {prompt="Saturation level"}
bool	ignore_sat = no {prompt="Ignore objects with saturated pixels?"}
int	iterations = 2	{prompt="Number of radius adjustment iterations",
			 min=1}
string	logfile = "logfile" {prompt="Logfile"}

begin
	string	ims, im, gap
	struct	instrum

	ims = images

	sections (ims) | scan (im)
	instrum = ""
	hselect (im//"[0]", "instrume", yes) | scan (instrum)
	if (instrum == "Mosaic1.1")
	    gap = "end"
	else
	    gap = "beginning"

#print ("\nMSCFOCUS: Estimate best focus from Mosaic focus images.")
#print ("  Mark the top star in each sequence unless the display is flipped.")
#print ("  More precisely mark the star with the largest y value.")
print ("Mark the top star (in unflipped display).")

	set erract = "notrace"
	iferr {
	    mscstarfocus (ims, focus="FOCSTART", fstep="FOCSTEP",
	    nexposures="FOCNEXPO", step="FOCSHIFT", direction="+line",
	    gap=gap, coords="markall", display=yes, frame=frame,
	    imagecur="", graphcur="", level=level, size=size, beta=INDEF,
	    scale=scale, radius=radius, sbuffer=sbuffer, swidth=swidth,
	    saturation=saturation, ignore_sat=ignore_sat, xcenter=INDEF,
	    ycenter=INDEF, logfile=logfile, iterations=iterations)
	} print ($errmsg)
end
