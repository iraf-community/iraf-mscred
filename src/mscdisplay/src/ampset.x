include <ctotok.h>
include <error.h>

define	DELIM		"|"	# Amplist dictionary delimitor
define	SZ_KEYWRD	8	# Length of valid FITS keyword

procedure ampset()
 
include "ampinfo.com"
 
#pointer	sp, pset, buf1, buf2, token, pp
#int	ip, nch, tok
#
#int	ctotok(), ctowrd()
#pointer	clopset()
#bool	streq()

begin
	call calloc (amplist, SZ_LINE, TY_CHAR)
	call calloc (offset_key, SZ_KEYWRD, TY_CHAR)
	call calloc (gain_key, SZ_KEYWRD, TY_CHAR)
	call calloc (dark_key, SZ_KEYWRD, TY_CHAR)
	namps = 0
 
#	# Allocate stack space
# 	call smark (sp)
# 	call salloc (pset,  SZ_LINE, TY_CHAR)
# 	call salloc (buf1,  SZ_LINE, TY_CHAR)
# 	call salloc (buf2,  SZ_LINE, TY_CHAR)
# 	call salloc (token, SZ_LINE, TY_CHAR)
#
#	# allocate space for string items in /ampinfo/
#	call malloc (amplist,    SZ_LINE,   TY_CHAR)
#	call malloc (offset_key, SZ_KEYWRD, TY_CHAR)
#	call malloc (gain_key,   SZ_KEYWRD, TY_CHAR)
#	call malloc (dark_key,   SZ_KEYWRD, TY_CHAR)
# 
# 	# Open "ampinfo" pset.
# 	call clgstr ("ampinfo", Memc[pset], SZ_LINE)
# 	if (Memc[pset] == EOS)
# 	    call strcpy ("ampinfo", Memc[pset], SZ_LINE)
# 	pp = clopset (Memc[pset])
# 
# 	# Get list of amplifier names.
#	call clgpseta(pp, "amplist", Memc[buf1], SZ_LINE)
#
#	# The amplist parameter may be:
#	#     an image keyword
#	#     one of the special values "image" or "" -->  default keyword
#	#     an explicit amplifier list
#	#if (Memc[buf1] == EOS || streq (Memc[buf1], "image")) {
#	#    call hdmgstr ("amplist", Memc[buf1], SZ_LINE)
#	#} else {
#	#    ip = 1
#	#    nch = ctowrd (Memc[buf1], ip, Memc[buf2], SZ_LINE)
#	#    call hdmgstr (Memc[buf2], Memc[buf2], SZ_LINE)
#	#    if (Memc[buf2] != EOS)
#	#	call strcpy (Memc[buf2], Memc[buf1], SZ_LINE)
#	#}
#
#	# Parse list of amplifier names
#	ip = 1
#	namps = 0
#	call strcpy (DELIM, Memc[amplist], SZ_LINE)
#	repeat { 
#	    tok = ctotok (Memc[buf1], ip, Memc[token], SZ_LINE)
#	    switch (tok) {
#
#	    case TOK_EOS, TOK_NEWLINE:
#		break
#
#	    case TOK_PUNCTUATION, TOK_UNKNOWN:
#		next
#
#	    default:
#		namps = namps + 1
#		call strcat (Memc[token], Memc[amplist], SZ_LINE)
#		call strcat (DELIM,       Memc[amplist], SZ_LINE)
#	    }
#	}
#
#	if (namps == 0) {
#	    Memc[amplist] = EOS
#	    offset = NULL
#	    gain   = NULL
#	    dark   = NULL
#	} else {
#	    call calloc (offset, namps, TY_REAL)
#	    call calloc (dark,   namps, TY_REAL)
#	    call malloc (gain,   namps, TY_REAL)
#	    call amovkr (1.0, Memr[gain], namps)
#	}
#
#	iferr {
#
#	    # Get offset gain and dark keywords, or lists of numeric values
#	    call ampvals (pp, "offset", Memr[offset], namps, Memc[offset_key],
#	    SZ_KEYWRD)
#	    call ampvals (pp, "dark",   Memr[dark],   namps, Memc[dark_key],
#	    SZ_KEYWRD)
#	    call ampvals (pp, "gain",   Memr[gain],   namps, Memc[gain_key],
#	    SZ_KEYWRD)
#
#	} then {
#	    call clcpset (pp)
#	    call sfree (sp)
#	    call ampfree ()
#	    call erract (EA_ERROR)
#	}
#	
##	call eprintf ("namps=%d\n")
##	call pargi (namps)
##
##	do ip = 1, namps {
##	    call eprintf ("amp=%d offset=%f gain=%f dark=%f\n")
##		call pargstr (ip)
##		call pargr (Memr[offset+ip-1])
##		call pargr (Memr[gain+ip-1])
##		call pargr (Memr[dark+ip-1])
##	}
#
#	call clcpset (pp)
#	call sfree (sp)
end

# AMPVALS -- Parse a string returning either a keyword or an array of values
procedure ampvals (pset, param, vals, maxvals, keyword, maxch)

pointer	pset			#I pset pointer.
char	param[ARB]		#I parameter.
real	vals[ARB]		#O Value array.
int	maxvals			#I maximum number of values to return.
char	keyword[ARB]		#O Keyword name.
int	maxch			#I Max chars in keyword

pointer	sp, buffer, token
int	ip, nvals, tok, jp, nch, i
char	errmsg[SZ_LINE]

int	ctotok(), ctor()
bool	streq()

begin
	call smark (sp)
	call salloc (buffer, SZ_LINE, TY_CHAR)
	call salloc (token,  SZ_LINE, TY_CHAR)

	call clgpseta (pset, param, Memc[buffer], SZ_LINE)

	ip = 1
	nvals = 0
	keyword[1] = EOS
	repeat { 
	    tok = ctotok (Memc[buffer], ip, Memc[token], SZ_LINE)
	    switch (tok) {

	    case TOK_EOS, TOK_NEWLINE:
		break

	    case TOK_PUNCTUATION, TOK_UNKNOWN:
		next

	    # Got a number. Interpret as next value.
	    case TOK_NUMBER: 
		jp = 1
		nvals = nvals + 1
		if (nvals > maxvals) 	# Quit if we have as many as we need
		    break

		nch = ctor (Memc[token], jp, vals[nvals])

	    # Got a string. If at beginning, interpret as fits keyword.
	    # Otherwise complain.
	    default:
		if (nvals == 0) {
		    if (streq (Memc[token], "image")) {
			call strcpy (param,       keyword, maxch)
		    } else {
			call strcpy (Memc[token], keyword, maxch)
		    }
		    break

		} else {
		    call sfree (sp)
		    call sprintf (errmsg, SZ_LINE, "Badly formed value for %s")
			 call pargstr (param)
		    call error (0, errmsg)
		}
	    }
	}

	if (nvals == 0) {
	    if (keyword[1] == EOS)
		call strcpy (param,  keyword, maxch)
	} else {
	    # If the list of values is short set the remainder to the last value
	    if (nvals <= maxvals) {
		do i = nvals+1, maxvals 
		    vals[i] = vals[nvals]
	    }
	}

	call sfree (sp)
end

# AMPFREE -- Free memory assigned in  /ampinfo/ common block.

procedure ampfree ()

include "ampinfo.com"

begin
	call mfree (amplist,     TY_CHAR)
	call mfree (offset_key,  TY_CHAR)
	call mfree (gain_key,    TY_CHAR)
	call mfree (dark_key,    TY_CHAR)
	if (namps > 0) {
	    call mfree (offset,  TY_REAL)
	    call mfree (gain,    TY_REAL)
	    call mfree (dark,    TY_REAL)
	}
end

# AMPNULL -- Set null information in /ampinfo/ common block.

procedure ampnull ()

include "ampinfo.com"

begin
	namps = 0
end
