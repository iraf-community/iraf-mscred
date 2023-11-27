# CCDHEDIT -- Edit Mosaic CCD images.

procedure ccdhedit (images, parameter, value)

string	images		{prompt="CCD images"}
string	parameter	{prompt="Image header parameter"}
string	value		{prompt="Parameter value"}
string	extname = ""	{prompt="Extension name pattern"}
string	type = "string"	{prompt="Parameter type (string|real|integer)",
			 enum="string|real|integer"}

begin
	file	inlist

	inlist = mktemp ("tmp$iraf")
	mscextensions (images, output="file", index="0-", extname=extname,
	    extver="", lindex=no, lname=yes, lver=no, ikparams="", > inlist)
	_ccdhedit ("@"//inlist, parameter, value, type=type)
	delete (inlist, verify=no)
end
