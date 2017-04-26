# SFLATCOMBINE -- Process and combine images into a sky flat.

procedure sflatcombine (input)

string	input			{prompt="List of images to combine"}
file	output="Sflat"		{prompt="Output sky flat field root name"}
string	combine="average"	{prompt="Type of combine operation",
				 enum="average|median"}
string	reject="avsigclip"	{prompt="Type of rejection"}
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

struct	*fd

begin
	string	ims, out, temp1, temp2
	real	ccdmean, sigma, lower, upper

	cache	mscextensions

	ims = input
	out = output
	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")

	# Check on images to combine.
	coutput (ims, out, temp1, headers="", bpmasks="", rejmasks="",
	    nrejmasks="", expmasks="", sigma="",
	    ccdtype=ccdtype, amps=yes, subsets=subsets, scale=scale,
	    zero="none", weight=no)

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

	# Set the image type and ccdmean.
	fd = temp1
	while (fscan (fd, out) != EOF) {
	    mscextensions (out, output="file", index="0-", extname="",
		extver="", lindex=yes, lname=no, lver=no, ikparams="", > temp2)
	    if (mscextensions.imext)
		_ccdhedit (out//"[0]", "imagetyp", "skyflat", type="string")
	    _ccdhedit ("@"//temp2, "imagetyp", "skyflat", type="string")
	    if (mscextensions.nimages > 1) {
		hedit (out//"[0],@"//temp2, "ccdmean*", add-, del+, update+,
		    verify-, show-)
		imstat ("@"//temp2, fields="mean", lower=INDEF, upper=INDEF,
		    format=no) | average | scan (ccdmean,sigma)
		lower = ccdmean - sigma
		upper = ccdmean + sigma
		imstat ("@"//temp2, fields="mean", lower=lower, upper=upper,
		    format=no) | average | scan (ccdmean)
		hedit (out//"[0]", "ccdmean", ccdmean, add+, del-, update+,
		    verify-, show-)
	    }
	    delete (temp2, verify-)
	}
	fd = ""; delete (temp1, verify-)
end
