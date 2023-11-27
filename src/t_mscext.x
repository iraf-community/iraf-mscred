# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<imhdr.h>
include	<imset.h>

define	OUTPUTS		"|none|list|file|"
define	NONE		1		# No output
define	LIST		2		# List output
define	FILE		3		# File output

define	SZ_RANGE	100		# Size of range list
define	SZ_LISTOUT	255		# Size of output list


# T_MSCEXTENSIONS -- Expand a template of FITS files into a list of image
# extensions on the standard output and record the number image extensions
# in a parameter.
#
# This differs from IMEXTENSIONS in that extension zero is not returned
# unless it is a simple image and, in that case, the extension is removed.
# Also a parameter is written indicating if the list contains image extensions.

procedure t_mscextensions()

pointer	input			# List of ME file names
int	output			# Output list (none|list|file)
pointer	index			# Range list of extension indexes
pointer	extname			# Patterns for extension names
pointer extver			# Range list of extension versions
int	lindex			# List index number?
int	lname			# List extension name?
int	lver			# List extension version?
int	dataless		# Include dataless global header?
pointer	ikparams		# Image kernel parameters

pointer	sp, image, listout
int	list, nimages, fd, imext
int	clgwrd(), btoi(), xt_extns(), stropen()
int	imtgetim(), imtlen()
bool	clgetb()
errchk	stropen, fprintf, strclose

begin
	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (index, SZ_LINE, TY_CHAR)
	call salloc (extname, SZ_LINE, TY_CHAR)
	call salloc (extver, SZ_LINE, TY_CHAR)
	call salloc (ikparams, SZ_LINE, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Task parameters
	call clgstr ("input", Memc[input], SZ_LINE)
	output = clgwrd ("output", Memc[image], SZ_FNAME, OUTPUTS)
	call clgstr ("index", Memc[index], SZ_LINE)
	call clgstr ("extname", Memc[extname], SZ_LINE)
	call clgstr ("extver", Memc[extver], SZ_LINE)
	lindex = btoi (clgetb ("lindex"))
	lname = btoi (clgetb ("lname"))
	lver = btoi (clgetb ("lver"))
	dataless = btoi (clgetb ("dataless"))
	call clgstr ("ikparams", Memc[ikparams], SZ_LINE)

	# Get the list.
	list = xt_extns (Memc[input], "IMAGE", Memc[index], Memc[extname],
	    Memc[extver], lindex, lname, lver, dataless, Memc[ikparams],
	    NO, imext)

	# Format the output and set the number of images.
	switch (output) {
	case LIST:
	    call salloc (listout, SZ_LISTOUT, TY_CHAR)
	    iferr {
		fd = stropen (Memc[listout], SZ_LISTOUT, WRITE_ONLY)
		nimages = 0
		while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
		    nimages = nimages + 1
		    if (nimages == 1) {
			call fprintf (fd, "%s")
			    call pargstr (Memc[image])
		    } else {
			call fprintf (fd, ",%s")
			    call pargstr (Memc[image])
		    }
		}
		call strclose (fd)
		call printf ("%s\n")
		    call pargstr (Memc[listout])
	    } then {
		call imtclose (list)
		call sfree (sp)
		call error (1, "Output list format is too long")
	    }
	case FILE:
	    while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
		call printf ("%s\n")
		    call pargstr (Memc[image])
	    }
	}
	call clputi ("nimages", imtlen (list))
	call clputb ("imext", (imext==YES))

	call imtclose (list)
	call sfree (sp)
end
