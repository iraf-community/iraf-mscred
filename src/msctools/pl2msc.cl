# PL2MSC --  Convert a pixel list directory to a mosaic MEF format.

procedure pl2msc (input, output)

file	input			{prompt="Input pixel list directory"}
file	output			{prompt="Output mosaic filename"}
bool	trim=yes		{prompt="Trim data"}
bool	verbose=yes		{prompt="Verbose?"}

struct	*fd

begin
	string	in, out, extname, trimsec
	file	pllist

	in = input // "/"
	out = output
	pllist = mktemp ("tmp$iraf")

	hselect (in // "flat*.pl", "$I,extname,trimsec", yes, > pllist)
	if (!access (pllist))
	    error (1, "Input data not found")

	fd = pllist
	while (fscan (fd, in, extname, trimsec) != EOF) {  
	    if (!trim)
		trimsec = ""
	    else if (nscan() == 2)
		trimsec = ""

	    imcopy (in//trimsec, out//"["//extname//"]", verbose=verbose)
	}
	fd = ""; delete (pllist, verify-)
end
