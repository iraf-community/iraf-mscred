# MSCWFITS -- Write Mosaic FITS files to tape.

procedure mscwfits (input, output, newtape)

string	input		{prompt="Mosaic FITS files"}
string	output		{prompt="Output tape"}
bool	newtape		{prompt="Blank tape?"}
bool	shortlist = yes	{prompt="Short listing?"}
bool	longlist = no	{prompt="Long listing?"}

begin
	mscred.fitscopy (input, output, newtape, listonly=no,
	    shortlist=shortlist, longlist=longlist, intape=no, outtape=yes,
	    blocking_factor=10)
end
