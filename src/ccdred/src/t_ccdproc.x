include	"ccdred.h"

# T_CCDPROC -- Process CCD images
#
# This is a task entry procedure that get the input image list and then
# calls the main processing task ccdproc1.  The input images are
# processed to a temporary output image which then replaces the
# input image.  Calibration images are also automatically processed.

procedure t_ccdproc ()

int	inlist			# List of input CCD images
int	outlist			# List of output CCD images
int	noilist			# List of uninterpolated images
int	bpmlist			# List of output bad pixel masks
char	selecttype[SZ_FNAME]	# CCD image type

int	i, imtopen(), imtopenp(), ccdtypecl()
errchk	ccdproc1

begin
	# Set the input and output lists and the CCD type.
	inlist = imtopenp ("images")
	outlist = imtopenp ("output")
	noilist = imtopen ("")
	bpmlist = imtopenp ("bpmasks")
	i = ccdtypecl ("ccdtype", selecttype, SZ_FNAME)

	# Process the images.
	call ccdproc (inlist, outlist, noilist, bpmlist, ONERR_ORIG,
	    selecttype, "", YES)

	# Finish up.
	call imtclose (bpmlist)
	call imtclose (noilist)
	call imtclose (outlist)
	call imtclose (inlist)
end
