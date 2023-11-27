include	"ccdred.h"

# T_CCDTOOL -- Process CCD images
#
# This is a task entry procedure that get input and output image lists
# and then calls the main processing task ccdproc.  It does not
# automatically process calibration images.

procedure t_ccdtool ()

int	inlist			# List of input CCD images
int	outlist			# List of output CCD images
int	noilist			# List of output no interplolation images
int	bpmlist			# List of output bad pixel masks
char	selecttype[SZ_FNAME]	# CCD type to select (if not null)
char	proctype[SZ_FNAME]	# CCD processing type (if not null)
int	onerror			# Error action

int	i, calproc, clgwrd(), imtopenp(), imtlen() ccdtypecl(), nowhite()
errchk	ccdproc

begin
	# Set the input and output lists and the CCD type.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	noilist = imtopenp ("nointerp")
	bpmlist = imtopenp ("bpmasks")
	onerror = clgwrd ("onerror", selecttype, SZ_FNAME, ONERROR)
	call clgstr ("calproc", proctype, SZ_FNAME)
	calproc = CALPROC_NO
	if (nowhite(proctype, proctype, SZ_FNAME) == 0)
	    calproc = CALPROC_IGNORE
	i = ccdtypecl ("ccdtype", selecttype, SZ_FNAME)
	i = ccdtypecl ("proctype", proctype, SZ_FNAME)

	if (imtlen (outlist) > 1 && imtlen (outlist) != imtlen (inlist))
	    call error (1, "Input and output image lists do not match")

	# Process the images.
	call ccdproc (inlist, outlist, noilist, bpmlist, onerror, selecttype,
	    proctype, calproc)

	# Finish up.
	call imtclose (bpmlist)
	call imtclose (noilist)
	call imtclose (outlist)
	call imtclose (inlist)
end
