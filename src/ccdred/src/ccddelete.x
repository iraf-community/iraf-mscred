# CCDDELETE -- Delete an image by renaming it to a backup image.
#
# Determine the type of backup; i.e. none, once, or all.  If "none" then
# delete the image without a backup.  Otherwise get a backup root name which
# may be a directory.  Supply a default if needed.  If only backing up "once"
# check if a backup exists and delete the image without making a backup if it
# does.  Otherwise rename the image to the backup name.  If backing up all
# images then rename the image to the backup name.
# 
# The backup image name is formed by prepending the backup prefix to the
# image name.  If a previous backup exists append integers to the backup
# prefix until a nonexistant image name is created.


define	BKUP_TYPES	"|none|once|all|"
define	BKUP_NONE	1
define	BKUP_ONCE	2
define	BKUP_ALL	3

procedure t_ccddelete ()

int	images		# List of images to backup and delete

pointer	sp, image
int	imtopenp(), imtgetim()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	images = imtopenp ("images")
	while (imtgetim (images, Memc[image], SZ_FNAME) != EOF)
	    call ccddelete (Memc[image])

	call imtclose (images)
	call sfree (sp)
end


procedure ccddelete (image)

char	image[ARB]		# Image to delete (backup)

int	i, bkup, clgwrd(), nowhite(), access(), fnldir(), imaccess()
pointer	sp, bkuproot, backup
bool	clgetb()
errchk	imdelete, imrename, fmkdir

begin
	call smark (sp)
	call salloc (bkuproot, SZ_FNAME, TY_CHAR)
	call salloc (backup, SZ_FNAME, TY_CHAR)

	# Get the backup flag.
	bkup = clgwrd ("backup", Memc[backup], SZ_FNAME, BKUP_TYPES)

	# Delete image and return if no backup is desired.
	if (bkup == BKUP_NONE) {
	    call imdelete (image)
	    call sfree (sp)
	    return
	}

	# Get the backup root name and supply a default if needed.
	call clgstr ("bkuproot", Memc[bkuproot], SZ_FNAME)
	if (nowhite (Memc[bkuproot], Memc[bkuproot], SZ_FNAME) == 0)
	    call sprintf (Memc[bkuproot], SZ_FNAME, "Raw/")

	# Create a directory if needed.
	if (fnldir (Memc[bkuproot], Memc[backup], SZ_FNAME) != 0) {
	    if (access (Memc[backup], 0, 0) == NO)
		call fmkdir (Memc[backup])
	}

	# Create a backup name.
	i = 0
	repeat {
	    if (i == 0) {
		call sprintf (Memc[backup], SZ_FNAME, "%s%s")
		    call pargstr (Memc[bkuproot])
		    call pargstr (image)
	    } else {
		call sprintf (Memc[backup], SZ_FNAME, "%s%d%s")
		    call pargstr (Memc[bkuproot])
		    call pargi (i)
		    call pargstr (image)
	    }
	    i = i + 1
	} until (imaccess (Memc[backup], READ_ONLY) == NO)

	# Backup up image if needed.
	switch (bkup) {
	case BKUP_ONCE:
	    if (i == 1) {
		if (clgetb ("verbose")) {
		    call eprintf ("Backup %s to %s\n")
			call pargstr (image)
			call pargstr (Memc[backup])
		}
		call imrename (image, Memc[backup])
	    } else
		call imdelete (image)
	default:
	    if (clgetb ("verbose")) {
		call eprintf ("Backup %s to %s\n")
		    call pargstr (image)
		    call pargstr (Memc[backup])
	    }
	    call imrename (image, Memc[backup])
	}

	call sfree (sp)
end
