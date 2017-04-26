# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<tbset.h>
include "rfits.h"
# RFT_PUT_TABLE_ROW -- Procedure to fill each column buffer with blanks
# from the last non_character to the buffer length. See also if there are
# null values defined or a scaled column has been found; then copy to a double
# dimension buffer.

procedure rft_put_table_row (tp, colptr, buf, rowlen, ncols, rownum)

pointer tp
int	colptr[SZ_MAXCOL] # i: column pointer descriptor
char	buf[ARB]	# i: input string buffer
int	rowlen		# i: number of chars in buffer
int	ncols		# i: number of columns
int	rownum		# i: actual row number

pointer	sp, pp
int	i, nch, ctor()
int	biof, len, strmatch(), ip
real	rval, value

include	"tab.com"


begin
	call smark (sp)
	call salloc (pp, rowlen+1, TY_CHAR)
	do i = 1, ncols {
	   # get position of first character and length of column
	   biof = tbcol[i]
	   len = tbcw[i]

	   if (tnull[1,i] != EOS) {
	      if (strmatch (buf[biof], tnull[1,i]) != 0) {
		 # if the input buffer has a null value just skip the column,
		 # since the output buffer already has UNDEF on it.
	         next
	      }
	   }
	   # copy the column element to a NULL terminated string
	   call strcpy (buf[biof], Memc[pp], len)
	   # scale data if necessary
	   if (tzero[i] != 0.0 || tscal[i] != 1.0) {
	      ip = 1
	      nch = ctor (Memc[pp], ip, rval)
	      value = rval*tscal[i] + tzero[i]
	      call tbeptr (tp, colptr[i], rownum, value)
	      next
	   } 

	   call tbeptt (tp, colptr[i], rownum, Memc[pp])
	}

	call sfree (sp)
end
