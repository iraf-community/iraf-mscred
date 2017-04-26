include	<imhdr.h>

# CCDCOPY -- Copy pixels.

procedure ccdcopy (in, out)

pointer	in			#I IMIO pointer
pointer	out			#I IMIO pointer

int	i, nc, nl
pointer	imgl2s(), impl2s(), imgl2r(), impl2r()

begin
	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	switch (IM_PIXTYPE(in)) {
	case TY_SHORT:
	    do i = 1, nl
		call amovs (Mems[imgl2s(in,i)], Mems[impl2s(out,i)], nc)
	default:
	    do i = 1, nl
		call amovr (Memr[imgl2r(in,i)], Memr[impl2r(out,i)], nc)
	}
end
