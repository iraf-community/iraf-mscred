# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
.help change
.nf____________________________________________________________________________

This procedure searches for and replaces text patterns in a string. The
text patterns are passed to the procedure as arguments, so this procedure
can be used to perform a variety of text processing tasks. The procedure
has five arguments: a target pattern string (from), a replacement pattern
string (to), an input string (in), an output string (out), and a maximum
length for the output string (maxch). The input and output strings cannot
be the same string. The target and replacement pattern strings contain one
or more patterns separated by a delimeter character, which is taken to be
the first character in the string. Starting at the first character in the
input string, the procedure looks at each pattern in the target string to
see if it matches the input. When the first match is found, the corresponding
string from the replacement pattern is moved to the output string. The
characters that were matched by the target pattern are skipped over and
the procedure searches for a new match with the target pattern. If no match
is found, the character in the input string that was not matched is moved
to the output string and the procedure searches for a new match at the next
character. The syntax for the target and replacement pattern strings largely
follows that used in the substitute command by the Unix text editors `ed'
and `ex'. The pattern consists of a sequence of ordinary characters, which
match themselves, and metacharacters, which match a set of characters.
The delimeter character, which separates patterns, is also treated as a
metacharacter. A metacharacter can be matched as if it were an ordinary
character by preceding it with the escape character, `\'. For example, the
escape character itself is indicated in a pattern by `\\'. The metacharacters
which can be used in the target pattern are:

	beginning of string	^	end of string		$
	white space		#	escape character	\
	ignore case		{	end ignore case		}
	begin character class	[	end character class	]
	not, in char class	^	range, in char class	-
	one character		?	zero or more occurences *
	begin tagged string	\(	end tagged string	\)


A set of characters is indicated in the target string by the character
class construct. For example, punctuation could be indicated by `[,;.!]'.
A range of characters contiguous in the underlying character set can be
abbreviated by the range construct. For example, `[a-z]' matches any lower
case character. The complement of a character set is indicated by making
`^' the first character in a class. For example, `[^0-9]' matches any
non-digit. Repetition of a character or character class is indicated by
the following it with the `*' metacharacter. Thus, zero or more occurences
of a lower case character is indicated by `[a-z]*'. The tagged string
metacharacters have no effect on the match, they only serve to identify
portions of the matched string for the replacement pattern. The metacharacters
which are used in the replacement pattern are the delimeter character and
the following:

	entire string		&	tagged string		\n
	capitalize		\u	upper case		\U
	lower case		\L	end case conversion	\e \E

The ditto metacharacter, `&', indicates that the entire portion of the input
string that was matched by the target pattern is to be moved into the output
string. The tag metacharacter indicates that the n-th tagged string is to
be moved into the output string. For example, `\1' indicates the first tagged
string and `\2' the second. The remaining metacharacters affect the case
of the output string. The capitalization metacharacter only affects the
immediately following metacharacter, but the upper and lower case
metacharacters must be turned off explicitly with `\e' or `\E'. The following
are a few examples of the results that can be obtained with this subroutine:

	from			to			action
	----			--	       		------
	|iraf|IRAF|		|sdas|SDAS|		convert all mentions
							of iraf to sdas
	|==|!=|&&|\|\||!|	|eq|ne|and|or|not|	convert boolean
							operators
	|[a-z][A-Za-z]*|	|\u&|			capitalize all words
	|"\([^"]*\)"|		|'\1'|			convert double quoted
							strings to single
							quoted strings
	|\([^,]*\),\(?*\)|	|\2,\1|			reverse two fields
							separated by commas

.endhelp________________________________________________________________________

include	<ctype.h>
include	<chars.h>
include	"reloperr.h"

# pattern codes used in replacement pattern

define	EOP		-1	# end of pattern
define	DITTO		-2	# substitute matched expression
define	TAG		-3	# substitute tagged part of matched expression
define	CAP		-4	# capitalize next char
define	UCASE		-5	# convert to upper case
define	LCASE		-6	# convert to lower case
define	ENDCASE		-7	# end case conversion

define	PATSEP		-127	# pattern separator

define	CH_DITTO	'&'
define	CH_LTAG		'('
define	CH_RTAG		')'
define	CH_INDEX	'%'

# CHANGE -- Change all instances of one set of patterns into another set
#
# B.Simon	8-Dec-87	First code

procedure change (from, to, in, out, maxch)

char	from[ARB]	# i: Target pattern in input string
char	to[ARB]		# i: Replacement pattern for output string
char	in[ARB]		# i: Input string
char	out[ARB]	# o: Output string
int	maxch		# i: Maximum length of output string
#--
int	ic, nc, oc
pointer	pat, sub, nextpat, nextsub

string	mismatch "Number of to and from patterns must match"

int	patcode(), subcode(), pat_amatch()

errchk	patcode, subcode, pat_amatch, dosub

begin
	# Encode the patterns and check to see that the number of patterns
	# in the set match

	if (patcode (from, pat) != subcode (to, sub))
	    call error (SYNTAX, mismatch)

	# Check each character in the input string for a match

	oc = 1
	for (ic = 1; in[ic] != EOS; ic = ic + nc) {

	    # Check each pattern with an anchored match
	    # Substitute at the first match

	    nc = 0
	    nextsub = sub
	    nextpat = pat
 	    while (Memc[nextpat] != PATSEP) {
		nc = pat_amatch (in, ic, Memc[nextpat])
		if (nc > 0) {
		    call dosub (Memc[nextpat], Memc[nextsub],
				in, ic, ic+nc-1, out, oc, maxch)
		    break
		}

		# Advance pointers to next patterns

		for ( ; Memc[nextpat] != PATSEP; nextpat = nextpat + 1)
			;
		nextpat = nextpat + 1
		for ( ; Memc[nextsub] != PATSEP; nextsub = nextsub + 1)
			;
		nextsub = nextsub + 1
	    }

	    # If no pattern was matched, move a single character to the
	    # output string

	    if (nc == 0) {
		call chdeposit (in[ic], out, maxch, oc)
		nc = 1
	    }
	}
	out[oc] = EOS
	call mfree (pat, TY_CHAR)
	call mfree (sub, TY_CHAR)

end

# PATCODE -- Encode the target pattern set
#
# B.Simon	8-Dec-87	First code

int procedure patcode (from, pat)

char	from[ARB]	# i: String containing target pattern
pointer	pat		# o: Pointer to encoded pattern
#--
char	delim
int	ic, npat, lenpat, maxpat

errchk	addpat

begin
	# Allocate memory for encoded pattern

	maxpat = SZ_LINE
	call malloc (pat, maxpat, TY_CHAR)

	npat = 0
	lenpat = 0

	# Get the delimeter character used between patterns

	delim = from[1]
	if (delim != EOS) {

	    # Encode the next pattern in the from string

	    ic = 2
	    while (from[ic] != EOS) {
		call addpat (delim, from, ic, pat, lenpat, maxpat)
		npat = npat + 1
	    }

	}
	Memc[pat+lenpat] = PATSEP

	# Return the number of patterns found

	return (npat)
end

# SUBCODE -- Encode the replacement pattern set
#
# B.Simon	8-Dec-87	First code

int procedure subcode (to, sub)

char	to[ARB]		# i: String containing replacement pattern
pointer	sub		# o: Pointer to encoded pattern
#--
char	delim
int	ic, nsub, lensub, maxsub

errchk	addsub

begin
	# Allocate memory for encoded pattern

	maxsub = SZ_LINE
	call malloc (sub, maxsub, TY_CHAR)

	nsub = 0
	lensub = 0

	# Get the delimeter character used between patterns

	delim = to[1]
	if (delim != EOS) {

	    # Encode the next pattern in the to string

	    ic = 2
	    while (to[ic] != EOS) {
		call addsub (delim, to, ic, sub, lensub, maxsub)
		nsub = nsub + 1
	    }

	}
	Memc[sub+lensub] = PATSEP

	# Return the number of patterns found

	return (nsub)
end

# ADDPAT -- Add the next encoded pattern to the target pattern set
#
# B.Simon	8-Dec-87	First code

procedure addpat (delim, str, index, pat, lenpat, maxpat)

char	delim		#  i: Delimeter that marks the end of pattern
char	str[ARB]	#  i: String containing pattern
int	index		# io: Index to start of pattern
pointer	pat		# io: Encoded pattern set
int	lenpat		# io: Current length of pattern set
int	maxpat		# io: Maximum length of pattern set
#--
char 	ch
pointer	sp, newpat, newstr
int	jdx,kdx

int	patmake()

errchk	patmake

begin
	# Allocate dynamic memory for temporary strings

	call smark (sp)
	call salloc (newpat, SZ_LINE, TY_CHAR)
	call salloc (newstr, SZ_LINE, TY_CHAR)

	# Translate characters to suit pattern encoder

	jdx = 1
	for ( ; str[index] != delim && str[index] != EOS; index = index + 1) {
	    switch (str[index]) {
	    case ESCAPE:
		if (str[index+1] == CH_LTAG || str[index+1] == CH_RTAG) {
		    ch = CH_INDEX
		    index = index + 1
		} else if (str[index+1] == delim) {
		    ch = delim
		    index = index + 1
		} else {
		    ch = ESCAPE
		}
	    case CH_INDEX:
		ch = ESCAPE
		call chdeposit (ch, Memc[newstr], SZ_LINE, jdx)
		ch = CH_INDEX
	    default:
		ch = str[index]
	    }
	    call chdeposit (ch, Memc[newstr], SZ_LINE, jdx)
	}
	Memc[newstr+jdx-1] = EOS

	# Advance index past delimeter

	if (str[index] != EOS)
	    index = index + 1

	# Encode a single pattern

	kdx = patmake (Memc[newstr], Memc[newpat], SZ_LINE)
	Memc[newpat+kdx] = PATSEP
	kdx = kdx + 1

	# Reallocate memory if new pattern will not fit

	if (lenpat + kdx > maxpat) {
	    maxpat = 2 * maxpat
	    call realloc (pat, maxpat, TY_CHAR)
	}

	# Add new pattern to pattern set

	call amovc (Memc[newpat], Memc[pat+lenpat], kdx)
	lenpat = lenpat + kdx

	call sfree (sp)
end

# ADDSUB -- Add the next encoded pattern to the replacement pattern set
#
# B.Simon	8-Dec-87	First code

procedure addsub (delim, str, index, sub, lensub, maxsub)

char	delim		#  i: Delimeter that marks the end of pattern
char	str[ARB]	#  i: String containing pattern
int	index		# io: Index to start of pattern
pointer	sub		# io: Encoded pattern set
int	lensub		# io: Current length of pattern set
int	maxsub		# io: Maximum length of pattern set
#--
char	ch
pointer	sp, newsub, newstr
int	jdx,kdx

int	submake()

errchk	submake

begin
	# Allocate dynamic memory for temporary strings

	call smark (sp)
	call salloc (newsub, SZ_LINE, TY_CHAR)
	call salloc (newstr, SZ_LINE, TY_CHAR)

	# Translate characters to suit pattern encoder

	jdx = 1
	for ( ; str[index] != delim && str[index] != EOS; index = index + 1) {
	    switch (str[index]) {
	    case ESCAPE:
		if (str[index+1] == delim) {
		    ch = delim
		    index = index + 1
		} else {
		    ch = ESCAPE
		}
	    default:
		ch = str[index]
	    }
	    call chdeposit (ch, Memc[newstr], SZ_LINE, jdx)
	}
	Memc[newstr+jdx-1] = EOS

	# Advance index past delimeter

	if (str[index] != EOS)
	    index = index + 1

	# Encode a single pattern

	kdx = submake (Memc[newstr], Memc[newsub], SZ_LINE)
	Memc[newsub+kdx] = PATSEP
	kdx = kdx + 1

	# Reallocate memory if new pattern will not fit

	if (lensub + kdx > maxsub) {
	    maxsub = 2 * maxsub
	    call realloc (sub, maxsub, TY_CHAR)
	}

	# Add new pattern to pattern set

	call amovc (Memc[newsub], Memc[sub+lensub], kdx)
	lensub = lensub + kdx

	call sfree (sp)
end

# SUBMAKE -- Encode a single replacement pattern
#
# B.Simon	8-Dec-87	First code

int procedure submake (str, buf, maxbuf)

char	str[ARB]	# i: String to be encoded
char	buf[ARB]	# o: Pattern buffer
int	maxbuf		# i: Buffer size
#--
char	ch
int	idx, jdx

int	cctoc()

begin
	jdx = 1
	for (idx = 1; str[idx] != EOS; idx = idx + 1) {
	    switch (str[idx]) {
	    case CH_DITTO:
		ch = DITTO
	    case ESCAPE:
		switch (str[idx+1]) {
		case 'u':
		    ch = CAP
		    idx = idx + 1
		case 'U':
		    ch = UCASE
		    idx = idx + 1
		case 'L':
		    ch = LCASE
		    idx = idx + 1
		case 'e','E':
		    ch = ENDCASE
		    idx = idx + 1
		default:
		    if (IS_DIGIT (str[idx+1])) {
			ch = TAG
			call chdeposit (ch, buf, maxbuf, jdx)
			idx = idx + 1
			ch = TO_INTEG (str[idx])
		    } else if (cctoc (str, idx, ch) == 1) {
			ch = str[idx]
		    } else {
			idx = idx - 1
		    }
		}
	    default:
		ch = str[idx]
	    }
	    call chdeposit (ch, buf, maxbuf, jdx)
	}
	buf[jdx] = EOP

	return (jdx)
end

# DOSUB -- Put the replacement pattern in the output string
#
# B.Simon	8-Dec-87	First code

procedure dosub (pat, sub, in, first, last, out, oc, maxch)

char	pat[ARB]	#  i: Target pattern
char	sub[ARB]	#  i: Replacement pattern
char	in[ARB]		#  i: Input string
int	first		#  i: First character matched in input string
int	last		#  i: Last character matched in input string
char	out[ARB]	# io: Output string
int	oc		# io: Last character in output string
int	maxch		#  i: Maximum length of output string
#--
int	caseflag, ic, index, ltag, rtag

int	patindex()

begin
	caseflag = ENDCASE
	for (ic = 1; sub[ic] != EOP; ic = ic + 1) {
	    switch (sub[ic]) {
	    case ENDCASE:
		caseflag = ENDCASE
	    case LCASE:
		caseflag = LCASE
	    case UCASE:
		caseflag = UCASE
	    case CAP:
		caseflag = CAP
	    case TAG:
		ic = ic + 1
		index = (sub[ic] - 1) * 2 + 1
		ltag = patindex (pat, index)
		rtag = patindex (pat, index+1) - 1
		call movechars (in, ltag, rtag, caseflag, out, oc, maxch)
	    case DITTO:
		call movechars (in, first, last, caseflag, out, oc, maxch)
	    default:
		call movechars (sub, ic, ic, caseflag, out, oc, maxch)
	    }
	}
end

# MOVECHARS -- Move input characters to the output string
#
# B.Simon	8-Dec-87	First code

procedure movechars (str1, first, last, caseflag, str2, len, maxch)

char	str1[ARB]	#  i: Input string
int	first		#  i: First character to be moved
int	last		#  i: Last character to be moved
int	caseflag	# io: Case conversion flag
char	str2[ARB]	# io: Output string
int	len		# io: Length of output string
int	maxch		#  i: Maximum length of output string
#--
char	ch
int	ic

begin                   
	do ic = first, last {
	    switch (caseflag) {
	    case ENDCASE:
		ch = str1[ic]
	    case LCASE:
		ch = str1[ic]
		if (IS_UPPER (ch))
	     	    ch = TO_LOWER (ch)
	    case UCASE,CAP:
		ch = str1[ic]
		if (IS_LOWER (ch))
		    ch = TO_UPPER (ch)
	    default:
		ch = str1[ic]
	    }
	    call chdeposit (ch, str2, maxch, len)
	    if (caseflag == CAP)
		caseflag = ENDCASE
	}
end
