# MITEMPLATE -- Expand an image name template and collapse it to a set of
# mosaic image rootnames.

define	LEN_MITSTRUCT	5			# Length of structure.
define	MIT_PTR		Memi[$1]		# String buffer pointer.
define	MIT_LENGTH	Memi[$1+1]		# Length of String buffer.
define	MIT_NIMAGES	Memi[$1+2]		# Number of images.
define	MIT_INDICES	Memi[$1+3]		# Array of string indexes.
define	MIT_INDEX	Memi[$1+4]		# String index of next entry.

define	DEFSTR		SZ_COMMAND		# Default string allocation.
define	DEFIDX		100			# Default index allocation.
define	SPACE		" "			# Space character

pointer	procedure mitopen (input)

char	input[ARB]		#I Image name template.
pointer	mit			#O pointer to rootname list structure.

pointer	sp, image, rootname, previous, buff, idxs, list
int	str_alloc, idx_alloc, nch, nimages, len

pointer	imtopen()
int	imtgetim(), strlen()
bool	streq()

begin
	call smark (sp)
	call salloc (image,    SZ_LINE, TY_CHAR)
	call salloc (rootname, SZ_LINE, TY_CHAR)
	call salloc (previous, SZ_LINE, TY_CHAR)

	# Allocate structure, string buffer and index array
	call malloc (mit,  LEN_MITSTRUCT, TY_STRUCT)
	str_alloc = DEFSTR
	call malloc (buff, str_alloc,     TY_CHAR)
	idx_alloc = DEFIDX
	call malloc (idxs, idx_alloc,     TY_INT)

	nch = 0
	nimages = 0
	Memc[previous] = EOS

	list = imtopen (input)
	while (imtgetim (list, Memc[image], SZ_LINE) != EOF) {
	    call mg_rootname (Memc[image], Memc[rootname], SZ_LINE)	

	    # Skip repeated rootnames.
	    if (streq (Memc[previous], Memc[rootname]))
		next

	    # Check for index and buffer overflow and reallocate if neccesary.
	    nimages = nimages + 1
	    if (nimages > idx_alloc) {
		idx_alloc = idx_alloc + DEFIDX
		call realloc (idxs, idx_alloc, TY_INT)
	    }

	    len = strlen (Memc[rootname])
	    if (nch + len + 2 > str_alloc) {
		str_alloc = str_alloc + DEFSTR
		call realloc (buff, str_alloc, TY_CHAR)
	    }

	    # Index points to start of new string
	    Memi[idxs+nimages-1] = nch

	    call strcpy (Memc[rootname], Memc[buff+nch], len)
	    nch = nch + len

	    call strcpy (Memc[rootname], Memc[previous], SZ_LINE)
	}

	# Reallocate buffer and index array
	call realloc (buff, nch,     TY_CHAR)
	call realloc (idxs, nimages, TY_INT)
	    
	# Save results in structure
	MIT_PTR(mit)     = buff
	MIT_LENGTH(mit)  = nch
	MIT_NIMAGES(mit) = nimages
	MIT_INDICES(mit) = idxs
	MIT_INDEX(mit)   = 1

	return (mit)
end

# MITGETIM -- Get next image from an expanded list of rootnames.

int	procedure mitgetim (mit, image, maxch)

pointer	mit			#I Pointer to template structure.
char	image[ARB]		#O Output image name.
int	maxch			#I maximum length of image.

int	nch			#O Number of characters in image or EOF

int	mitrgetim()

begin
	# Check for null list
	if (mit == NULL)
	    return (EOF)

	nch = mitrgetim (mit, MIT_INDEX(mit), image, maxch)
	return (nch)
end

# MITRGETIM -- Get next image from an expanded list of rootnames.

int	procedure mitrgetim (mit, index, image, maxch)

pointer	mit			#I Pointer to template structure.
int	index			#I List element to be returned.
char	image[ARB]		#O Output image name.
int	maxch			#I maximum length of image.

int	nch			#O Number of characters in image or EOF

int	first, last

begin
	# Check for null list
	if (mit == NULL)
	    return (EOF)

	if (index <= 0 || index > MIT_NIMAGES(mit))
	    return (EOF)

	first = Memi[MIT_INDICES(mit)+index-1]
	if (index == MIT_NIMAGES(mit)) {
	    last = MIT_LENGTH(mit) - 1
	} else {
	    last = Memi[MIT_INDICES(mit)+index] - 1
	}
	nch = min (maxch, last - first + 1)
	call strcpy (Memc[MIT_PTR(mit)+first], image, nch)

	MIT_INDEX(mit) = MIT_INDEX(mit) + 1

	return (nch)

end


# MITLEN -- Get number of images in list of rootnames.
int	procedure mitlen (mit)

pointer	mit			#I Pointer to template structure.

begin
	if (mit != NULL)
	    return (MIT_NIMAGES(mit))
	else 
	    return (0)
end

# MITREWIND -- "rewind" list of rootnames.
procedure mitrewind (mit)

pointer	mit			#I Pointer to template structure.

begin
	if (mit != NULL)
	    MIT_INDEX(mit) = 1
end

# MITCLOSE -- Close list of rootnames.

procedure mitclose (mit)

pointer	mit			#I Pointer to template structure.

begin
	if (mit != NULL) {
	    if (MIT_PTR(mit) != NULL)
		call mfree (MIT_PTR(mit), TY_CHAR)
	    if (MIT_INDICES(mit) != NULL)
		call mfree (MIT_INDICES(mit), TY_INT)
	    call mfree (mit, TY_STRUCT)
	}
end
