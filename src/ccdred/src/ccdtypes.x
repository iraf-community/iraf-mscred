include	"ccdtypes.h"

# CCDTYPE   -- Return the CCD type string and code.
# CCDTYPECL -- Return the CCD type code and string from a CL parameter.
# CCDTYPES  -- Return the CCD type code and string from an image.
# CCDSTR    -- Return the CCD type string from the code.


# CCDTYPE -- Return the CCD type string and code.
# Strip any leading and trailing whitespace from the user string.
# Translate to standard strings if requested.
# The input and output strings may be the same.

int procedure ccdtype (user, translate, ccdstr, maxchar)

char	user[ARB]		#I User CCD type string
int	translate		#I Translate user string?
char	ccdstr[maxchar]		#O Standard CCD type string
int	maxchar			#I Maximum size for CCD type string
int	ccdcode			#O CCD type code

int	nowhite(), strdic()
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, maxchar, TY_CHAR)

	if (nowhite (user, Memc[str], maxchar) == 0) {
	    ccdstr[1] = EOS
	    ccdcode = NONE
	} else {
	    call strcpy (user, Memc[str], maxchar)
	    if (translate == YES) {
		call hdmname (Memc[str], Memc[str], maxchar)
		if (Memc[str] == EOS)
		    call strcpy (user, Memc[str], maxchar)
	    }
	    call strcpy (Memc[str], ccdstr, maxchar)
	    ccdcode = strdic (ccdstr, ccdstr, maxchar, CCDTYPES)
	}

	call sfree (sp)
	return (ccdcode)
end


# CCDTYPECL -- Return the CCD type string and code from a CL parameter.

int procedure ccdtypecl (param, ccdstr, maxchar)

char	param[ARB]		#I CL parameter
char	ccdstr[maxchar]		#O CCD type string
int	maxchar			#I Maximum size for CCD type string

int	ccdtype()

begin
	call clgstr (param, ccdstr, maxchar)
	return (ccdtype (ccdstr, YES, ccdstr, maxchar))
end


# CCDTYPES -- Return the CCD type string and code from an image.

int procedure ccdtypes (im, ccdstr, maxchar)

pointer	im			#I Image pointer
char	ccdstr[maxchar]		#O CCD type string
int	maxchar			#I Maximum size for CCD type string

int	ccdtype()

begin
	call hdmgstr (im, "imagetyp", ccdstr, maxchar)
	return (ccdtype (ccdstr, YES, ccdstr, maxchar))
end


# CCDSTR -- Return the standard CCD type string from the CCD type code.

procedure ccdstr (ccdcode, type, sz_type)

int	ccdcode			#I CCD type code
char	type[sz_type]		#O CCD type string
int	sz_type			#I Maximum size for CCD type string

begin
	switch (ccdcode) {
	case NONE:
	    call strcpy ("none", type, sz_type)
	case OBJECT:
	    call strcpy ("object", type, sz_type)
	case ZERO:
	    call strcpy ("zero", type, sz_type)
	case DARK:
	    call strcpy ("dark", type, sz_type)
	case FLAT:
	    call strcpy ("flat", type, sz_type)
	case SFLAT:
	    call strcpy ("skyflat", type, sz_type)
	case ILLUM:
	    call strcpy ("illum", type, sz_type)
	case FRINGE:
	    call strcpy ("fringe", type, sz_type)
	case OTHER:
	    call strcpy ("other", type, sz_type)
	case COMP:
	    call strcpy ("comp", type, sz_type)
	case MASK:
	    call strcpy ("mask", type, sz_type)
	default:
	    call strcpy ("unknown", type, sz_type)
	}
end
