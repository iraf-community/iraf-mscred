# CCDLIST -- List Mosaic CCD images.

procedure ccdlist (images)

string	images		{prompt="CCD images to listed"}
string	ccdtype = ""	{prompt="CCD image type to be listed"}
string	extname = ""	{prompt="Extension name pattern"}
bool	names = no	{prompt="List image names only?"}
bool	long = no	{prompt="Long format listing?"}

begin
	file	inlist

	inlist = mktemp ("tmp$iraf")
	if (extname == "mef" && names && !long) {
	    mscextensions (images, output="file", index="1", extname="",
		extver="", lindex=no, lname=no, lver=no, ikparams="", > inlist)
	    _ccdlist ("@"//inlist, ccdtype=ccdtype, names=names, long=long,
		ccdproc="") | translit ("STDIN", "[", " ", del-) |
		fields ("STDIN", 1, lines="1-")
	} else {
	    mscextensions (images, output="file", index="0-", extname=extname,
		extver="", lindex=no, lname=yes, lver=no, ikparams="", > inlist)
	    _ccdlist ("@"//inlist, ccdtype=ccdtype, names=names, long=long,
		ccdproc="")
	}
	delete (inlist, verify=no)
end
