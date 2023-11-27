# MSCGETCATALOG -- Get catalog stars.

procedure mscgetcatalog (input, output)

string	input			{prompt="List of Mosaic files"}
file	output			{prompt="Output file of sources"}
real	magmin = 0.		{prompt="Minimum magnitude"}
real	magmax = 25.		{prompt="Maximum magnitude"}
string	catalog="usnob1@noao"	{prompt="Catalog"}
real	rmin = 21.		{prompt="Minimum radius (arcmin)"}

begin
	file	inlist

	inlist = mktemp ("tmp$iraf")

	mscextensions (input, output="file", index="0-", extname="",
	    extver="", lindex=no, lname=yes, lver=no, dataless=no,
	    ikparams="", > inlist)

	getcatalog ("@"//inlist, output=output, catalog=catalog,
	    magmin=magmin, magmax=magmax, rmin=rmin,
	    radecsys="FK5", equinox=2000.)

	delete (inlist, verify=no)
end
