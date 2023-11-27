# MSCSELECT -- Image header selection on multiextension Mosaic files.

procedure mcsselect (images, fields)

string	images			{prompt="images for selection"}
string	fields			{prompt="fields to be extracted"}
string	expr = "yes"		{prompt="boolean selection expression"}
string	extnames = ""		{prompt="extension names"}

begin
	struct	cmd

	printf ('"hselect $input %s "%s""\n', fields, expr) | scan (cmd)
	msccmd (cmd, images, extname=extnames, verbose=no)
end
