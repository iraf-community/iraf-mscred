include	<error.h>
include	<math.h>

# Group type definitions.
define	GROUPS	"|keyword|ccdtype|subset|amplifier|ccdname|ampsubsets|\
		 |position|date|"
define	KEYWORD		1	# Group by keyword
define	CCDTYPE		2	# Group by ccdtype
define	SUBSET		3	# Group by subset
define	AMP		4	# Group by amp
define	CCDNAME		5	# Group by ccdname
define	AMPSUB		6	# Group by amp and subset
define	POSITION	8	# Group by position
define	DATE		9	# Group by position

define	GRP_LEN		200			# Length of strings

define	GRP_GSYMLEN	102			# Length of group symbol
define	GRP_INDEX	Memi[$1]		# Group index
define	GRP_SEQ		Memi[$1+1]		# Group sequence number
define	GRP_ID		Memc[P2C($1+2)]		# Group identification

define	GRP_FSYMLEN	1
define	GRP_NMEMBER	Memi[$1]		# Number of members

define	NALLOC		10	# Allocate memory in this size block
define	SZ_KEYS		(10*SZ_LINE)	# Size of key string


# T_CCDGROUPS -- Group images into files based on parameters with common values.
# The output consists of files containing the image names of images from the
# input image list which have the same group type such as position, date,
# or title.

procedure t_ccdgroups ()

int	images			# List of images
pointer	root			# Output group root name
pointer	list			# Output file containing list of files
pointer	ccdtype			# CCD type to select
int	mingroup		# Minumum number in a group
bool	sequence		# Break into sequences?
int	group			# Group type
int	keywords		# Keywords
real	radius			# Position radius
bool	cluster			# Output only cluster name?
bool	verbose			# Verbose output (package parameter)

int	i, fd, fdlist, npositions
pointer	im, sp, image, keyword, key, keys, output, positions
pointer	stpg, stpf, sym, ptr

bool	clgetb(), strne()
real	clgetr()
int	clgeti(), clpopnu(), clgfil(), imtopenp(), imtlen(), imtgetim()
int	open(), clgwrd(), stridxs(), nowhite()
int	ccdtypecl(), ccdtypes()
pointer	stopen(), sthead(), stnext(), stenter(), stname(), immap()
errchk	open, hdmgstr, ccdtypes, ccdsubset, ccdamp, ccdname
errchk	group_position, group_name

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (list, SZ_FNAME, TY_CHAR)
	call salloc (ccdtype, SZ_LINE, TY_CHAR)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (keys, SZ_KEYS, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	# Get the task parameters.
	images = imtopenp ("images")
	call clgstr ("output", Memc[root], SZ_FNAME)
	call clgstr ("list", Memc[list], SZ_FNAME)
	i = ccdtypecl ("ccdtype", Memc[ccdtype], SZ_LINE)
	mingroup = clgeti ("mingroup")
	sequence = clgetb ("sequence")
	group = clgwrd ("group", Memc[image], SZ_FNAME, GROUPS)
	keywords = clpopnu ("keywords")
	radius = clgetr ("radius")
	call clgstr ("instrument", Memc[image], SZ_FNAME)
	call hdmopen (Memc[image])
	cluster = clgetb ("cluster")
	verbose = clgetb ("verbose")

	# Open file for list of filenames.
	if (nowhite (Memc[list], Memc[list], SZ_FNAME) > 0)
	    fdlist = open (Memc[list], NEW_FILE, TEXT_FILE)
	else
	    fdlist = NULL

	# Setup the groups and files symbol tables.
	i = imtlen (images)
	stpg = stopen ("groups", i, i, i * SZ_LINE)
	stpf = stopen ("files", i, i, i * SZ_LINE)

	# Loop through the images and place them into groups.
	positions = NULL
	npositions = 0
	while (imtgetim (images, Memc[image], SZ_FNAME) != EOF) {

	    # Map the input and check the CCDTYPE.
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    i = ccdtypes (im, Memc[key], SZ_LINE)
	    if (Memc[ccdtype] != EOS && strne (Memc[ccdtype], Memc[key])) {
		call imunmap (im)
		next
	    }

	    iferr {
		# Set the key string.
		call clprew (keywords)
		Memc[keys] = EOS
		while (clgfil (keywords, Memc[keyword], SZ_LINE) != EOF) {
		    ifnoerr (call hdmgstr (im, Memc[keyword], Memc[key],
			SZ_LINE)) {
			call strcat (Memc[key], Memc[keys], SZ_KEYS)
			call strcat (" ", Memc[keys], SZ_KEYS)
		    }
		}

		# Define the output group file.
		switch (group) {
		case KEYWORD:
		    call group_name (stpg, stpf, Memc[keys], "", sequence,
			Memc[root], Memc[output], SZ_FNAME)
		case CCDTYPE:
		    i = ccdtypes (im, Memc[key], SZ_FNAME)
		    call strcat (Memc[key], Memc[keys], SZ_KEYS)
		    call group_name (stpg, stpf, Memc[keys], Memc[key],
			sequence, Memc[root], Memc[output], SZ_FNAME)
		case SUBSET:
		    call ccdsubset (im, Memc[key], SZ_FNAME)
		    call strcat (Memc[key], Memc[keys], SZ_KEYS)
		    call group_name (stpg, stpf, Memc[keys], Memc[key],
			sequence, Memc[root], Memc[output], SZ_FNAME)
		case AMP:
		    call ccdamp (im, Memc[key], SZ_FNAME)
		    call strcat (Memc[key], Memc[keys], SZ_KEYS)
		    call group_name (stpg, stpf, Memc[keys], Memc[key],
			sequence, Memc[root], Memc[output], SZ_FNAME)
		case CCDNAME:
		    call ccdname (im, Memc[key], SZ_FNAME)
		    call strcat (Memc[key], Memc[keys], SZ_KEYS)
		    call group_name (stpg, stpf, Memc[keys], Memc[key],
			sequence, Memc[root], Memc[output], SZ_FNAME)
		case AMPSUB:
		    call ccdamp (im, Memc[key], SZ_FNAME)
		    call ccdsubset (im, Memc[output], SZ_FNAME)
		    call strcat (Memc[output], Memc[key], SZ_FNAME)
		    call strcat (Memc[key], Memc[keys], SZ_KEYS)
		    call group_name (stpg, stpf, Memc[keys], Memc[key],
			sequence, Memc[root], Memc[output], SZ_FNAME)
		case POSITION:
		    call group_position (im, positions, npositions, radius,
			Memc[key], SZ_LINE)
		    call strcat (Memc[key], Memc[keys], SZ_KEYS)
		    call group_name (stpg, stpf, Memc[keys], "",
			sequence, Memc[root], Memc[output], SZ_FNAME)
		case DATE:
		    call hdmgstr (im, "date-obs", Memc[key], SZ_LINE)
		    i = stridxs ("T", Memc[key])
		    if (i > 0)
			Memc[key+i-1] = EOS
		    call strcat (Memc[key], Memc[keys], SZ_KEYS)
		    call group_name (stpg, stpf, Memc[keys], "",
			sequence, Memc[root], Memc[output], SZ_FNAME)
		}

		# Print the operation if verbose.
		if (verbose) {
	            call printf ("%s --> %s\n")
		        call pargstr (Memc[image])
		        call pargstr (Memc[output])
		}

		# Enter the image or cluster in the appropriate group file.
		if (cluster)
		    call imgcluster (Memc[image], Memc[image], SZ_FNAME)
	        fd = open (Memc[output], APPEND, TEXT_FILE)
	        call fprintf (fd, "%s\n")
		    call pargstr (Memc[image])
	        call close (fd)
	    } then
		 call erract (EA_WARN)

	    call imunmap (im)
	}

	# Write list of filenames.  First create a new symbol table in
	# order to reverse the order.
	if (fdlist != NULL) {
	    call stclose (stpg)
	    stpg = stopen ("files", 10, 10, 10 * SZ_LINE)
	    for (sym = sthead (stpf); sym != NULL; sym = stnext (stpf, sym)) {
		output = stname (stpf, sym)
		if (GRP_NMEMBER(sym) < mingroup)
		    call delete (Memc[output])
		else
		    ptr = stenter (stpg, Memc[output], GRP_FSYMLEN)
	    }
	    for (sym = sthead (stpg); sym != NULL; sym = stnext (stpg, sym)) {
		output = stname (stpg, sym)
		call fprintf (fdlist, "%s\n")
		    call pargstr (Memc[output])
	    }
	    call close (fdlist)
	}

	# Finish up.
	call imtclose (images)
	if (positions != NULL)
	    call mfree (positions, TY_REAL)
	call stclose (stpf)
	call stclose (stpg)
	call sfree (sp)
end


# GROUP_NAME -- Group images.

procedure group_name (stpg, stpf, key, id, sequence, root, fname, maxchar)

pointer	stpg			#I Group symbol table
pointer	stpf			#I File symbol table
char	key[ARB]		#U Key string
char	id[ARB]			#I Identification string 
bool	sequence		#I Break into sequences?
char	root[ARB]		#I Root filename
char	fname[ARB]		#O Output filename
int	maxchar			#I Size of output filename
pointer	sym			#O Symbol table pointer

pointer	lastsym, symseq, stfind(), stenter(), sthead()
errchk	hdmgstr, stfind, stenter, sthead

begin
	if (key[1] == EOS)
	    sym = stfind (stpg, "NULL")
	else
	    sym = stfind (stpg, key)
	if (sym == NULL) {
	    lastsym = sthead (stpg)
	    if (key[1] == EOS)
		sym = stenter (stpg, "NULL", GRP_GSYMLEN)
	    else
		sym = stenter (stpg, key, GRP_GSYMLEN)
	    if (lastsym == NULL)
		GRP_INDEX(sym) = 1
	    else
		GRP_INDEX(sym) = GRP_INDEX(lastsym) + 1
	    GRP_SEQ(sym) = 0
	    if (id[1] == EOS) {
		call sprintf (GRP_ID(sym), GRP_LEN, "%03d")
		    call pargi (GRP_INDEX(sym))
	    } else
		call strcpy (id, GRP_ID(sym), GRP_LEN)
	} else if (sym != symseq)
	    GRP_SEQ(sym) = GRP_SEQ(sym) + 1

	if (!sequence || GRP_SEQ(sym) == 0) {
	    call sprintf (fname, maxchar, "%s%s")
		call pargstr (root)
		call pargstr (GRP_ID(sym))
	} else {
	    call sprintf (fname, maxchar, "%s%s.%d")
		call pargstr (root)
		call pargstr (GRP_ID(sym))
		call pargstr (GRP_SEQ(sym))
	}
	symseq = sym

	sym = stfind (stpf, fname)
	if (sym == NULL) {
	    sym = stenter (stpf, fname, GRP_FSYMLEN)
	    GRP_NMEMBER(sym) = 1
	} else
	    GRP_NMEMBER(sym) = GRP_NMEMBER(sym) + 1
end


# GROUP_POSITION -- Group by RA and DEC position.  The RA is in hours and
# the DEC is in degrees.  The radius is in seconds of arc.

procedure group_position (im, positions, npositions, radius, key, maxchar)

pointer	im			# Image
pointer	positions		# Positions
int	npositions		# Number of positions
real	radius			# Matching radius
char	key[ARB]		#O Key
int	maxchar			#I Size of key

real	ra, dec, dra, ddec, r, hdmgetr()
int	i, nalloc
pointer	ptr
errchk	hdmgetr

begin
	ra = hdmgetr (im, "ra")
	dec = hdmgetr (im, "dec")

	for (i=1; i<=npositions; i=i+1) {
	    ptr = positions + 2 * i - 2
	    dra = ra - Memr[ptr]
	    ddec = dec - Memr[ptr+1]
	    if (dra > 12.)
		dra = dra - 24.
	    if (dra < -12.)
		dra = dra + 24.
	    dra = dra * cos (DEGTORAD (dec)) * 15.
	    r = sqrt (dra ** 2 + ddec ** 2) * 3600.
	    if (r < radius)
		break
	}
	if (i > npositions) {
	    if (i == 1) {
		nalloc = NALLOC
		call malloc (positions, nalloc * 2, TY_REAL)
	    } else if (i > nalloc) {
		nalloc = nalloc + NALLOC
		call realloc (positions, nalloc * 2, TY_REAL)
	    }
	    ptr = positions + 2 * i - 2
	    Memr[ptr] = ra
	    Memr[ptr+1] = dec
	    npositions = i
	}

	call sprintf (key, maxchar, "%d")
	    call pargi (i)
end
