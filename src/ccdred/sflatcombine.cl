# SFLATCOMBINE -- Process and combine images into a sky flat.

procedure sflatcombine (input)

string	input			{prompt="List of images to combine"}
file	output="Sflat"		{prompt="Output sky flat field root name"}
string	combine="average"	{prompt="Type of combine operation",
				 enum="average|median"}
string	reject="avsigclip"	{prompt="Type of rejection",
		enum="none|minmax|ccdclip|crreject|sigclip|avsigclip|pclip"}
string	ccdtype="object"	{prompt="CCD image type to combine"}
bool	subsets=yes	{prompt="Combine images by subset parameter?"}
string	masktype = "none"	{prompt="Mask type"}
real	maskvalue = 0.		{prompt="Mask value"}
string	scale="mode"	{prompt="Image scaling",
			 enum="none|mode|median|mean|exposure"}
string	statsec=""	{prompt="Image section for computing statistics"}
int	nkeep=1		{prompt="Minimum to keep (pos) or maximum to reject (neg)"}
int	nlow=1		{prompt="minmax: Number of low pixels to reject"}
int	nhigh=1		{prompt="minmax: Number of high pixels to reject"}
bool	mclip=yes	{prompt="Use median in sigma clipping algorithms?"}
real	lsigma=6.	{prompt="Lower sigma clipping factor"}
real	hsigma=3.	{prompt="Upper sigma clipping factor"}
string	rdnoise="rdnoise" {prompt="ccdclip: CCD readout noise (electrons)"}
string	gain="gain"	{prompt="ccdclip: CCD gain (electrons/DN)"}
string	snoise="0."	{prompt="ccdclip: Sensitivity noise (fraction)"}
real	pclip=-0.5	{prompt="pclip: Percentile clipping parameter"}
real	blank=1.	{prompt="Value if there are no pixels"}
real	grow=3.		{prompt="Radius (pixels) for neighbor rejection",
			 min=0.}
begin
	string	ims, out, temp

	ims = input
	out = output
	temp = mktemp ("tmp$iraf")

	# Check on images to combine.
	coutput (ims, out, temp, rejmask="", plfile="", sigma="", amps=yes,
	    subsets=subsets)

	# Process images.
	ccdproc (ims, output="", bpmasks="", ccdtype=ccdtype, noproc=no,
	    sflatcor=no)

	# Combine the images.
	combine (ims, output=out, headers="", bpmasks="", rejmasks="",
	    nrejmasks="", expmasks="", sigma="", imcmb="$I",
	    combine=combine, reject=reject, ccdtype=ccdtype, amps=yes,
	    subsets=subsets, delete=no, project=no, outtype="real",
	    outlimits="", offsets="none", masktype=masktype,
	    maskvalue=maskvalue, blank=blank, scale=scale, zero="none",
	    weight=no, statsec=statsec, lthreshold=INDEF, hthreshold=INDEF,
	    nlow=nlow, nhigh=nhigh, nkeep=nkeep, mclip=mclip,
	    lsigma=lsigma, hsigma=hsigma, rdnoise=rdnoise, gain=gain,
	    snoise=snoise, sigscale=0.1, pclip=pclip, grow=grow)

	# Set the image type.
	ccdhedit ("@"//temp, "imagetyp", "skyflat", type="string")

	delete (temp, verify-)
end
