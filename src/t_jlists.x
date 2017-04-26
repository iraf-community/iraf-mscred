# T_JLISTS -- Expand and join file or image lists.

procedure t_jlists ()

int	i, j, nlists, lenlists, fd
bool	shortest
pointer	sp, lists, fname, delim, missing, param

bool	clgetb()
int	clgeti(), clgwrd()
int	open()
int	fntopnb(), fntlenb(), fntgfnb()
int	imtopen(), imtlen(), imtgetim()
errchk	open, fntopnb, imtopen

begin
	nlists = clgeti ("$nargs")

	call smark (sp)
	call salloc (lists, nlists, TY_INT)
	call salloc (fname, SZ_LINE, TY_CHAR)
	call salloc (delim, SZ_LINE, TY_CHAR)
	call salloc (missing, SZ_LINE, TY_CHAR)
	call salloc (param, SZ_LINE, TY_CHAR)

	# Get parameters.
	call clgstr ("output", Memc[fname], SZ_LINE)
	call clgstr ("delim", Memc[delim], SZ_FNAME)
	shortest = clgetb ("shortest")
	call clgstr ("missing", Memc[missing], SZ_FNAME)

	# Open output.
	fd = open (Memc[fname], APPEND, TEXT_FILE)

	# Expand lists.
	switch (clgwrd ("type", Memc[fname], SZ_LINE, "|file|image|")) {
	case 1:
	    # Open lists.
	    lenlists = 0
	    do i = 1, nlists {
		call sprintf (Memc[param], SZ_LINE, "list%d")
		    call pargi (i)
		call clgstr (Memc[param], Memc[fname], SZ_LINE)
		Memi[lists+i-1] = fntopnb (Memc[fname], NO)
		if (i == 1)
		    lenlists = fntlenb (Memi[lists+i-1])
		else if (shortest)
		    lenlists = min (fntlenb(Memi[lists+i-1]), lenlists)
		else
		    lenlists = max (fntlenb(Memi[lists+i-1]), lenlists)
	    }
	    # Output lists.
	    do j = 1, lenlists {
		do i = 1, nlists {
		    if (i > 1) {
			call fprintf (fd, "%s")
			    call pargstr (Memc[delim])
		    }
		    if (fntgfnb (Memi[lists+i-1],Memc[fname],SZ_LINE) != EOF) {
			call fprintf (fd, "%s")
			    call pargstr (Memc[fname])
		    } else {
			call fprintf (fd, "%s")
			    call pargstr (Memc[missing])
		    }
		}
		call fprintf (fd, "\n")
	    }
	    # Close lists.
	    do i = 1, nlists
		call fntclsb (Memi[lists+i-1])
	    
	case 2:
	    # Open lists.
	    lenlists = 0
	    do i = 1, nlists {
		call sprintf (Memc[param], SZ_LINE, "list%d")
		    call pargi (i)
		call clgstr (Memc[param], Memc[fname], SZ_LINE)
		Memi[lists+i-1] = imtopen (Memc[fname])
		if (i == 1)
		    lenlists = imtlen (Memi[lists+i-1])
		else if (shortest)
		    lenlists = min (imtlen(Memi[lists+i-1]), lenlists)
		else
		    lenlists = max (imtlen(Memi[lists+i-1]), lenlists)
	    }
	    # Output lists.
	    do j = 1, lenlists {
		do i = 1, nlists {
		    if (i > 1) {
			call fprintf (fd, "%s")
			    call pargstr (Memc[delim])
		    }
		    if (imtgetim (Memi[lists+i-1],Memc[fname],SZ_LINE) != EOF) {
			call fprintf (fd, "%s")
			    call pargstr (Memc[fname])
		    } else {
			call fprintf (fd, "%s")
			    call pargstr (Memc[missing])
		    }
		}
		call fprintf (fd, "\n")
	    }
	    # Close lists.
	    do i = 1, nlists
		call imtclose (Memi[lists+i-1])

	default:
	    call error (1, "Unknown list type")
	}

	# Close output.
	call close (fd)

	call sfree (sp)
end

