# MSCRFITS -- Read Mosaic FITS files from tape.

procedure mscrfits (input, output)

string	input		{prompt="Input tape"}
string	output		{prompt="Output file(s)"}
string	tapefiles = "1-"{prompt="Tape file list"}
bool	listonly = no	{prompt="List only?"}
bool	shortlist = yes	{prompt="Short listing?"}
bool	longlist = no	{prompt="Long listing?"}
int	offset = 0	{prompt="Offset for numbering of output disk filenames"}
bool	original = yes	{prompt="Restore original file name?"}

begin
	string	out

	if (listonly)
	    out = ""
	else
	    out = output

	mscred.fitscopy (input, out, listonly=listonly, shortlist=shortlist,
	    longlist=longlist, extn="fits", offset=offset, original=original,
	    intape=yes, outtape=no, tapefiles=tapefiles)
end
