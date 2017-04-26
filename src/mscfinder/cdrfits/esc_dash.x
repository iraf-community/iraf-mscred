# Copyright restrictions apply - see stsdas$copyright.stsdas 


procedure pesc_dash (name)

char name[SZ_FNAME]

pointer sp, pp
int	i,j, np, stridx()
char   dash , plus

begin
	dash = '-'
	np = stridx(dash, name)
	plus = '+'
	if (np == 0)
	   np = stridx(plus, name)

	if (np != 0) {
	   call smark(sp)
	   call salloc(pp,SZ_FNAME,TY_CHAR)
	   j = 0
	   for (i=1; i<= SZ_FNAME ||name[i] == EOS; i=i+1) {

	       if (name[i] != '-' && name[i] != '+')
		  Memc[pp+j] = name[i]
	       else {
		  Memc[pp+j] = '\\'
		  j=j+1
		  Memc[pp+j] = name[i] 
	       }
	       j = j+ 1
	   }
	   call strcpy (Memc[pp], name, SZ_FNAME)
	   call sfree(sp)
	}
end


procedure cesc_dash (name)

char name[SZ_FNAME]

pointer sp, pp, np
int	i,j, stridx()
char   esc 

begin
	esc= '\\'
	np = stridx(esc, name)
 	if (np != 0) {
	   call smark(sp)
	   call salloc(pp,SZ_FNAME,TY_CHAR)
	   j = 0
	   for (i=1; i<= SZ_FNAME ||name[i] == EOS; i=i+1) {

	       if (name[i] != '\\')
		  Memc[pp+j] = name[i]
	       else {
		  if (name[i+1] == '-' || name[i+1] == '+') {
		     Memc[pp+j] = name[i+1]
		     i = i + 1
		  }
	       }
	       j = j+ 1
	   }
	   call strcpy (Memc[pp], name, SZ_FNAME)
	   call sfree(sp)
	}
end
