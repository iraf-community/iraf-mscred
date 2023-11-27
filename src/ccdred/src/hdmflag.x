define	STEPS	"|overscan|zerocor|flatcor|"
define	O	1
define	Z	2
define	F	3

# HDMFLAG -- Determine if a processing flag is set.  This is less than
# obvious because of the need to use the default value to indicate a
# false flag.

bool procedure hdmflag (im, name)

pointer	im		# IMIO pointer
char	name[ARB]	# Header flag name

int	i, strdic(), stridxs()
bool	flag, strne()
pointer	sp, str1, str2

begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Support both PROCTOOL and CCDPROC conventions.
	call hdmgstr (im, "PROCDONE", Memc[str1], SZ_LINE)
	if (Memc[str1] != EOS) {
	    i = strdic (name, Memc[str2], SZ_LINE, STEPS)
	    switch (i) {
	    case O:
	        flag = (stridxs ("B", Memc[str1]) > 0)
	    case Z:
	        flag = (stridxs ("ZS", Memc[str1]) > 0)
	    case F:
	        flag = (stridxs ("F", Memc[str1]) > 0)
	    default:
	    	flag = false
	    }

	} else {
	    # Get the flag string value and the default value.
	    # The flag is true if the value and the default do not match.

	    call hdmgstr (im, name, Memc[str1], SZ_LINE)
	    call hdmgdef (name, Memc[str2], SZ_LINE)
	    flag = strne (Memc[str1], Memc[str2])
	}

	call sfree (sp)
	return (flag)
end
