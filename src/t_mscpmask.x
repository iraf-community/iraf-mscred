include	<imhdr.h>


# T_MSCPMASK -- Convert mscimage mask.
# This is a plio workaround replacement task for imexpr.

procedure t_mscpmask ()

pointer	input			# Input image
pointer	output			# Output image

int	i, j
real	val
pointer	sp, in, out, inbuf, outbuf
pointer	immap(), imgl2r(), impl2i()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	# Get file names.
	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)

	# Map images.  It is assumed the caller has specified
	# a mask output type.
	in = immap (Memc[input], READ_ONLY, 0)
	out = immap (Memc[output], NEW_COPY, in)
	IM_PIXTYPE(out) = TY_INT

	# Copy the pixels.  This assumes 2D images.
	do i = 1, IM_LEN(in,2) {
	    inbuf = imgl2r (in, i)
	    outbuf = impl2i (out, i)
	    do j = 0, IM_LEN(in,1)-1 {
		val = abs (Memr[inbuf+j])
		if (val < 1.)
		    Memi[outbuf+j] = 0
		else
		    Memi[outbuf+j] = val / 10010 + 1
	    }
	}

	# Finish up.
	call imunmap (out)
	call imunmap (in)
	call sfree (sp)
end
