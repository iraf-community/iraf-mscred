# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"reloperr.h"

define	SZ_EXP		1000
define	FROM_STRING	"/\"[^\"]\"/'[^']'/{.eq.}/{.and.}/{.or.}\
/{.gt.}/{.ge.}/{.lt.}/{.le.}/{.not.}/{.ne.}/" 

# SELECT -- Select table rows according to expression
#
# This procedure evaluates a boolean expession for selected rows in a table.
# If the expression is true and does not involve null elements, the index
# of that row is kept in the index array.
#
# B.Simon	 7-Oct-87	First Code
# B.Simon	16-Dec-87	Changed to handle table subsets

procedure select (tp, expr, nindex, index)

pointer	tp		#  i: Table descriptor
char	expr[ARB]	#  i: Algebraic expression used in selection
int	nindex		# io: Number of rows selected
int	index[ARB]	# io: Indices of selected rows
#--
char	ch
pointer	sp, oldexp, newexp, ic, aryptr, nulptr
int	fd, sd, jc, dtype, nary, iary

int	open(), stropen(), stridx()

errchk	open, stropen, tbl_eval

string	badtype	"Expression is not boolean"

string	from	FROM_STRING
string  to	"/&/&/ ==/ \&\&/ ||/ >/ >=/ </ <=/ !/ !=/"

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (oldexp, SZ_EXP, TY_CHAR)
	call salloc (newexp, SZ_EXP, TY_CHAR)

	# Put the two parts of the from string together

	# Check to see if the expression is a file name

	if (expr[1] == '@') {

	    # Copy the file into a string

	    fd = open (expr[2], READ_ONLY, TEXT_FILE)
	    sd = stropen (Memc[oldexp], SZ_EXP, WRITE_ONLY)
	    call fcopyo (fd, sd)
	    call close (fd)
	    call strclose (sd)

	    # Replace the newlines with blanks

	    ic = oldexp
	    ch = '\n'
	    repeat {
		jc = stridx (ch, Memc[ic])
		if (jc == 0)
		    break
		ic = ic + jc
		Memc[ic-1] = ' '
	    }

	    # Convert Fortran relational operators to SPP

	    call change (from, to, Memc[oldexp], Memc[newexp], SZ_EXP)

	} else {

	    # Convert Fortran relational operators to SPP

	    call change (from, to, expr, Memc[newexp], SZ_EXP)
	}
	    
	# Evaluate the expression

	dtype = TY_BOOL
	call tbl_eval (tp, nindex, index, Memc[newexp], dtype, aryptr, nulptr)

	# Check to see if result is boolean

	if (dtype != TY_BOOL) {
	    call mfree (aryptr, dtype)
	    call mfree (nulptr, TY_BOOL)
	    call error (SYNTAX, badtype)
	}

	# Put indices of true, non-null rows in index array

	nary = nindex
	nindex = 0
	do iary = 1, nary

	    if (Memb[aryptr+iary-1] && ! Memb[nulptr+iary-1]) {
		nindex = nindex + 1
		index[nindex] = index[iary]
	    }

	call mfree (aryptr, dtype)
	call mfree (nulptr, TY_BOOL)
	call sfree (sp)
end
