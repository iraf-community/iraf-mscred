# MSCHEADER -- Image header listing for multiextension files.

procedure mcsheader (images)

string	images			{prompt="images names"}
bool	longheader = yes	{prompt="print header full header?"} 
bool	ghdr = no		{prompt="print global header separately?"}
string	extnames = ""		{prompt="extension names"}

begin
	struct	cmd

	printf ('"imheader $input long=%b"\n', longheader) | scan (cmd)
	if (ghdr)
	    msccmd (cmd, images, extname=extnames, dataless=yes,
	        ikparams="noinherit", verbose=no)
	else
	    msccmd (cmd, images, extname=extnames, dataless=no,
	        ikparams="", verbose=no)
end
