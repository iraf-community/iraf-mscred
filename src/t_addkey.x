include	<syserr.h>
include	<fset.h>
include	<imhdr.h>
include	<imio.h>

define  IDB_LENNUMERICRECORD    80              # length of new numeric records
define  IDB_SZFITSKEY           8               # max length FITS keyword


# T_ADDKEY -- Add a keyword with a comment.

procedure t_addkey ()

pointer	image		# Image to edit
pointer	key		# Keyword
pointer	sval		# Keyword string value
bool	bval		# Keyword boolean value
long	lval		# Keyword integer value
double	dval		# Keyword real value
pointer	comment		# Keyword comment
char	type		# Keyword type

char	clgetc()
bool	clgetb()
long	clgetl()
double	clgetd()
int	imaccf()
pointer	sp, im, immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (key, 8, TY_CHAR)
	call salloc (sval, SZ_FNAME, TY_CHAR)
	call salloc (comment, SZ_FNAME, TY_CHAR)

	# Get parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	im = immap (Memc[image], READ_WRITE, 0)

	call clgstr ("keyword", Memc[key], 8)
	type = clgetc ("type")
	switch (type) {
	case 'b':
	    bval = clgetb ("value")
	case 's', 'i', 'l':
	    lval = clgetl ("value")
	case 'r', 'd':
	    dval = clgetd ("value")
	default:
	    call clgstr ("value", Memc[sval], SZ_FNAME)
	}
	call clgstr ("comment", Memc[comment], SZ_FNAME)

	# Initialize keyword with comment.
	if (imaccf (im, Memc[key]) == YES)
	    call imdelf (im, Memc[key])
	call imaddfc (im, Memc[key], type, Memc[comment])

	# Set value.
	switch (type) {
	case 'b':
	    call imaddb (im, Memc[key], bval)
	case 's', 'i', 'l':
	    call imaddl (im, Memc[key], lval)
	case 'r', 'd':
	    call imaddd (im, Memc[key], dval)
	default:
	    call imastr (im, Memc[key], Memc[sval])
	}

	call imunmap (im)
	call sfree (sp)
end



# IMADDFC -- Add a user field to the image header with an optional card
# comment.  It is an error if the named field already exists.

procedure imaddfc (im, key, datatype, comment)

pointer	im			#I image descriptor
char	key[ARB]		#I name of the new parameter
char	datatype[ARB]		#I string permits generalization to domains
char	comment[ARB]		#I comment string

pointer	rp, sp, keyname, ua, ip
int	fd, max_lenuserarea, curlen, buflen, nchars
int	idb_kwlookup(), idb_findrecord()
int	stropen(), strlen(), idb_filstr(), nowhite()
errchk	syserrs, stropen, fprintf, pargstr, pargi

begin
	call smark (sp)
	call salloc (keyname, SZ_FNAME, TY_CHAR)

	# FITS format requires that the keyword name be upper case, not to
	# exceed 8 characters in length.  [Nov97 - This is not entirely 
	# correct, FITS does not require upper case, however we don't want
	# to change this at this time.]

	nchars = idb_filstr (key, Memc[keyname], IDB_SZFITSKEY)
	nchars = nowhite (Memc[keyname], Memc[keyname], IDB_SZFITSKEY)
	call strupr (Memc[keyname])

	# Check for a redefinition.
	if ((idb_kwlookup (key) > 0) || (idb_findrecord (im, key, rp) > 0))
	    call syserrs (SYS_IDBREDEF, key)
	
	# Open the user area string for appending.  'buflen' is the malloc-ed
	# buffer length in struct units; IMU is the struct offset to the user
	# area, i.e., the size of that part of the image descriptor preceding
	# the user area.  If the buffer fills we must allow one extra char for
	# the EOS delimiter; since storage for the image descriptor was
	# allocated in struct units the storage allocator will not have
	# allocated space for the extra EOS char.

	ua = IM_USERAREA(im)
	curlen = strlen (Memc[ua])
	buflen = LEN_IMDES + IM_LENHDRMEM(im)
	max_lenuserarea = (buflen - IMU) * SZ_STRUCT - 1

	# If the user area is not empty the last character must be the newline
	# record delimiter, else the new record we add will be invalid.

	if (curlen > 0 && Memc[ua+curlen-1] != '\n')
	    if (curlen >= max_lenuserarea)
		call syserrs (SYS_IDBOVFL, key)
	    else {
		Memc[ua+curlen] = '\n'
		curlen = curlen + 1
		Memc[ua+curlen] = EOS
	    }

	fd = stropen (Memc[ua+curlen], max_lenuserarea-curlen, APPEND)

	# Append the new record with an uninitialized value field.
	iferr {
	    if (comment[1] == EOS) {
	        call fprintf (fd, "%-8s= %s%*t\n")
		    call pargstr (Memc[keyname])
		    if (datatype[1] == 'c') {
		        call pargstr ("'        '")
		    } else {
		        call pargstr ("")
		    }
	    } else {
	        call fprintf (fd, "%-8s= %s%33t/ %s%*t\n")
		    call pargstr (Memc[keyname])
		    if (datatype[1] == 'c') {
		        call pargstr ("'        '")
		    } else {
		        call pargstr ("")
		    }
		    call pargstr (comment)
	    }
	    call pargi (IDB_LENNUMERICRECORD + 1)

	} then {
	    # Out of space in the user area.  Discard the truncated card at the
	    # end of the buffer by backing up to the last newline and writing
	    # an EOS.

	    call close (fd)
	    for (ip=ua+max_lenuserarea-1;  ip > ua;  ip=ip-1)
		if (Memc[ip] == '\n') {
		    Memc[ip+1] = EOS
		    break
		}
	    call syserrs (SYS_IDBOVFL, key)
	}

	call close (fd)
	call sfree (sp)
end
