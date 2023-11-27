# RMPUPIL -- Remove pupil patterns from images.
# The images may be single images or multiextension mosaics.
# This task is a specialized interface to the PATFIT task.

procedure rmpupil (input, output, pupil, masks)

string	input		{prompt="List of input images"}
string	output		{prompt="List of output corrected images"}
string	pupil = ""	{prompt="Pupil or list of pupil patterns"}
string	masks = ""	{prompt="List of object/bad data masks"}
string	pupilmasks = ""	{prompt="Pupil masks"}
string	outtype = "sdiff" {prompt="Output type", enum="sdiff|sflat"}

int	ncblk = 5	{prompt="Column smoothing"}
int	nlblk = 5	{prompt="Line smoothing"}
string	extfit = "im[2367]"	{prompt="Extensions to use in scaling fit"}

file	logfile = ""	{prompt="Logfile"}
bool	verbose = yes	{prompt="Verbose?"}

begin
	patfit (input, output, pupil, weight="", masks=masks,
	    patmasks=pupilmasks, background="", bkgpattern="",
	    bkgweight="", ncblk=ncblk, nlblk=nlblk, extfit=extfit,
	    extout="", outtype=outtype, logname="RMPUPIL", logfile=logfile,
	    verbose=verbose)
end
