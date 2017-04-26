# Scale Mosaic images and convert to USHORT.

procedure mquant (images)

string	images		{prompt="List of Mosaic images"}
real	bscale = 0.2	{prompt="FITS bscale parameter to use"}
real	bzero = 500.	{prompt="FITS bzero parameter to use"}
bool	preserve = no	{prompt="Preserve data values?"}
bool	verbose = yes	{prompt="Verbose progress information"}

struct	*infd, *outfd

begin
	file	inlist, extlist, input, output	
	string	names

	inlist = mktemp ("tmp$iraf")
	extlist = mktemp ("tmp$iraf")
	output = mktemp ("mquant")

	files (images, sort=no, > inlist)
	infd = inlist
	while (fscan (infd, input) != EOF) {
	    imextensions (input, output="file", index="", extname="",
		extver="", lindex=yes, lname=no, lver=no, ikparams="", >extlist)
	    imcopy (input//"[0]", output, verbose=verbose)
	    outfd = extlist
	    if (preserve) {
		while (fscan (outfd, ext) != EOF)
		    imexpr ("max(-32768.,min(32757.,(a-"//bzero//")/"//bscale//"))",
			output//"[append,inherit]", ext, intype="auto",
			outtype="short", dims="auto", refim="auto",
			verbose=verbose)
	    } else {
		while (fscan (outfd, ext) != EOF)
		    imexpr ("max(0.,min(65535.,(a-"//bzero//")/"//bscale//"))",
			output//"[append,inherit]", ext, intype="auto",
			outtype="ushort", dims="auto", refim="auto",
			verbose=verbose)
	    }
	    outfd = ""; delete (extlist, verify-)
	    if (preserve) {
		imextensions (output, output="list", index="", extname="",
		    extver="", lindex=yes, lname=no, lver=no, ikparams="") |
		    scan (names)
		hedit (names, "bzero", bzero, add+, update+, verify-,
		    show=verbose)
		hedit (names, "bscale", bscale, add+, update+, verify-,
		    show=verbose)
	    }
	    imdelete (input, verify-)
	    imrename (output, input)
	}
	infd = ""; delete (inlist, verify-)
end
