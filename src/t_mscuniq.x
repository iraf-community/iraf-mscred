include	<error.h>


# T_MSCUNIQ -- This task is a utility used in the CCDPROC script.
# It takes a list of images and extracts the first occurance of each rootname.

procedure t_mscuniq ()

int	i, n, fin, fout, open(), fscan(), nscan()
pointer	sp, fname, str, list
bool	streq()
errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	iferr {
	    call clgstr ("input", Memc[fname], SZ_FNAME)
	    fin = NULL
	    i = open (Memc[fname], READ_ONLY, TEXT_FILE); fin = i
	    call clgstr ("output", Memc[fname], SZ_FNAME)
	    fout = NULL
	    i = open (Memc[fname], NEW_FILE, TEXT_FILE); fout = i

	    n = 0
	    while (fscan (fin) != EOF) {
		call gargwrd (Memc[fname], SZ_FNAME)
		call gargstr (Memc[str], SZ_LINE)
		if (nscan() < 1)
		    next
		call imgcluster (Memc[fname], Memc[fname], SZ_FNAME)
		do i = 0, n-1 {
		    if (streq (Memc[fname], Memc[Memi[list+i]]))
			break
		}
		if (i < n)
		    next

		if (n == 0)
		    call malloc (list, 10, TY_POINTER)
		else if (mod (n, 10) == 0)
		    call realloc (list, n+10, TY_POINTER)

		call salloc (Memi[list+n], SZ_FNAME, TY_CHAR)
		call strcpy (Memc[fname], Memc[Memi[list+n]], SZ_FNAME)
		n = n + 1

		call fprintf (fout, "%s %s\n")
		    call pargstr (Memc[fname])
		    call pargstr (Memc[str])
	    }
	} then
	    call erract (EA_WARN)

	if (n > 0)
	    call mfree (list, TY_POINTER)
	if (fout != NULL)
	    call close (fout)
	if (fin != NULL)
	    call close (fin)
	call sfree (sp)
end
