include	<ctype.h>


# CCDAMP -- Return the CCD amplifier identifier.
#
# 1. Get the amplifier string and search the record file for the  ID string.
# 2. If the amplifier string is not in the record file define a default ID
#    string based on the first word of the amplifier string.  If the first
#    word is not unique append a integer to the first word until it is
#    unique.
# 3. Add the new amplifier string and identifier to the record file.
# 4. Since the ID string is used to generate image names replace all
#    nonimage name characters with '_'.
# 
# It is an error if the record file cannot be created or written when needed.

procedure ccdamp (im, amp, sz_name)

pointer	im			# Image
char	amp[sz_name]		# CCD amp identifier
int	sz_name			# Size of amp string

bool	streq()
int	i, j, fd, ctowrd(), open(), fscan()
pointer	sp, fname, str1, str2, amp1, amp2, amp3
errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	call salloc (amp1, SZ_LINE, TY_CHAR)
	call salloc (amp2, SZ_LINE, TY_CHAR)
	call salloc (amp3, SZ_LINE, TY_CHAR)

	# Get the amp record file and the amp string.
	call clgstr ("ampfile", Memc[fname], SZ_LINE)
	call hdmgstr (im, "amp", Memc[str1], SZ_LINE)
	if (Memc[str1] == EOS)
	    call hdmgstr (im, "imageid", Memc[str1], SZ_LINE)
	if (Memc[str1] == EOS)
	    call hdmgstr (im, "extname", Memc[str1], SZ_LINE)

	# The default amp identifier is the first word of the amp string.
	i = 1
	i = ctowrd (Memc[str1], i, Memc[amp1], sz_name)

	# A null amp string is ok.  If not null check for conflict
	# with previous amp IDs.
	if (Memc[str1] != EOS) {
	    call strcpy (Memc[amp1], Memc[amp3], sz_name)

	    # Search the amp record file for the same amp string.
	    # If found use the ID string.  If the amp ID has been
	    # used for another amp string then increment an integer
	    # suffix to the default ID and check the list again.

	    i = 1
	    ifnoerr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
		while (fscan (fd) != EOF) {
		    call gargwrd (Memc[str2], SZ_LINE)
		    if (!streq (Memc[str2], "amp"))
			next
		    call gargwrd (Memc[str2], SZ_LINE)
		    call gargwrd (Memc[amp2], SZ_LINE)
		    if (streq (Memc[str1], Memc[str2])) {
			i = 0
			call strcpy (Memc[amp2], Memc[amp1], SZ_LINE)
			break
		    } if (streq (Memc[amp1], Memc[amp2])) {
			j = log10 (real(i))
			Memc[amp3+sz_name-2+j] = EOS
			call sprintf (Memc[amp1], sz_name, "%s_%d")
			    call pargstr (Memc[amp3])
			    call pargi (i)
			i = i + 1
			call seek (fd, BOF)
		    }
		}
		call close (fd)
	    }

	    # If the amp is not in the record file add it.
	    if (i > 0) {
		fd = open (Memc[fname], APPEND, TEXT_FILE)
		call fprintf (fd, "amp\t'%s'\t%s\n")
		    call pargstr (Memc[str1])
		    call pargstr (Memc[amp1])
		call close (fd)
	    }
	}

	# Set the amp ID string and replace magic characters by '_'
	# since the amp ID is used in forming image names.

	call strcpy (Memc[amp1], amp, sz_name)
	for (i=1; amp[i]!=EOS; i=i+1)
	    if (!(IS_ALNUM(amp[i])||amp[i]=='.'))
		amp[i] = '_'

	call sfree (sp)
end


# CCDNAME -- Return the CCD name identifier.
#
# 1. Get the ccd string and search the record file for the  ID string.
# 2. If the ccd string is not in the record file define a default ID
#    string based on the first word of the ccd string.  If the first
#    word is not unique append a integer to the first word until it is
#    unique.
# 3. Add the new ccd string and identifier to the record file.
# 4. Since the ID string is used to generate image names replace all
#    nonimage name characters with '_'.
# 
# It is an error if the record file cannot be created or written when needed.

procedure ccdname (im, ccd, sz_name)

pointer	im			# Image
char	ccd[sz_name]		# CCD identifier
int	sz_name			# Size of ccd string

bool	streq()
int	i, j, fd, ctowrd(), open(), fscan()
pointer	sp, fname, str1, str2, ccd1, ccd2, ccd3
errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	call salloc (ccd1, SZ_LINE, TY_CHAR)
	call salloc (ccd2, SZ_LINE, TY_CHAR)
	call salloc (ccd3, SZ_LINE, TY_CHAR)

	# Get the ccd record file and the ccd string.
	call clgstr ("ampfile", Memc[fname], SZ_LINE)
	call hdmgstr (im, "ccdname", Memc[str1], SZ_LINE)
	if (Memc[str1] == EOS)
	    call hdmgstr (im, "imageid", Memc[str1], SZ_LINE)
	if (Memc[str1] == EOS)
	    call hdmgstr (im, "extname", Memc[str1], SZ_LINE)

	# The default ccd identifier is the first word of the ccd string.
	i = 1
	i = ctowrd (Memc[str1], i, Memc[ccd1], sz_name)

	# A null ccd string is ok.  If not null check for conflict
	# with previous ccd IDs.
	if (Memc[str1] != EOS) {
	    call strcpy (Memc[ccd1], Memc[ccd3], sz_name)

	    # Search the ccd record file for the same ccd string.
	    # If found use the ID string.  If the ccd ID has been
	    # used for another ccd string then increment an integer
	    # suffix to the default ID and check the list again.

	    i = 1
	    ifnoerr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
		while (fscan (fd) != EOF) {
		    call gargwrd (Memc[str2], SZ_LINE)
		    if (!streq (Memc[str2], "ccd"))
			next
		    call gargwrd (Memc[str2], SZ_LINE)
		    call gargwrd (Memc[ccd2], SZ_LINE)
		    if (streq (Memc[str1], Memc[str2])) {
			i = 0
			call strcpy (Memc[ccd2], Memc[ccd1], SZ_LINE)
			break
		    } if (streq (Memc[ccd1], Memc[ccd2])) {
			j = log10 (real(i))
			Memc[ccd3+sz_name-2+j] = EOS
			call sprintf (Memc[ccd1], sz_name, "%s_%d")
			    call pargstr (Memc[ccd3])
			    call pargi (i)
			i = i + 1
			call seek (fd, BOF)
		    }
		}
		call close (fd)
	    }

	    # If the ccd is not in the record file add it.
	    if (i > 0) {
		fd = open (Memc[fname], APPEND, TEXT_FILE)
		call fprintf (fd, "ccd\t'%s'\t%s\n")
		    call pargstr (Memc[str1])
		    call pargstr (Memc[ccd1])
		call close (fd)
	    }
	}

	# Set the ccd ID string and replace magic characters by '_'
	# since the ccd ID is used in forming image names.

	call strcpy (Memc[ccd1], ccd, sz_name)
	for (i=1; ccd[i]!=EOS; i=i+1)
	    if (!(IS_ALNUM(ccd[i])||ccd[i]=='.'))
		ccd[i] = '_'

	call sfree (sp)
end
