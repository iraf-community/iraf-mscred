# RMFRINGE -- Remove fringe patterns from images.
# The images may be single images or multiextension mosaics.
# This task is a specialized interface to the PATFIT task.

procedure rmfringe (input, output, fringe, masks)

string	input		{prompt="List of input images"}
string	output		{prompt="List of output corrected images"}
string	fringe		{prompt="Fringe or list of fringe patterns"}
string	masks = ""	{prompt="List of object/bad data masks"}
string	fringemasks = "" {prompt="Fringe masks"}
string	background = ""	{prompt="Lisk of input image backgrounds"}

int	ncblk = 5	{prompt="Column smoothing"}
int	nlblk = 5	{prompt="Line smoothing"}
string	extfit = ""	{prompt="Extensions to use in scaling fit"}

file	logfile = ""	{prompt="Logfile"}
bool	verbose = yes	{prompt="Verbose?"}

begin
	patfit (input, output, fringe, weight="", masks=masks,
	    patmasks=fringemasks, background=background, bkgpattern="",
	    bkgweight="", ncblk=ncblk, nlblk=nlblk, extfit=extfit, extout="",
	    outtype="pdiff", logname="RMFRINGE", logfile=logfile,
	    verbose=verbose)
end
