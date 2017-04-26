include	<imhdr.h>
include "mosgeom.h"



# MSCL2 -- Get line of data.

pointer procedure mscl2s (mg, line)

pointer	mg		#I MOSGEOM pointer
int	line		#I Line

int	i, nc, b1, b2, nb
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf

pointer imgl2s()
real	asums()
pointer	imgl2r()
pointer	imgl2i()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgl2s (im, line)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = inbuf

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    inbuf = imgl2s (im, line)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	}

	return (outbuf)
end


# MSCNL -- Get next line of data.

int procedure mscnls (mg, outbuf, v)

pointer	mg		#I MOSGEOM pointer
pointer	outbuf		#I Data buffer
long	v[ARB]		#I Vector

int	i, nc, b1, b2, nb, stat
long	vf[IM_MAXDIM]
real	bias
pointer	im, dzbuf, dfbuf, inbuf

pointer imgnls()
real	asums()
pointer	imgnlr()
pointer	imgnli()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)
	call amovl (v, vf, IM_NDIM(im))

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    stat = imgnls (im, inbuf, v)
	    if (stat == EOF)
		return (stat)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = inbuf

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (stat)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    stat = imgnls (im, inbuf, v)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (stat)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscprocs (Mems[inbuf], Mems[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)
	}

	return (stat)
end


# MSCS2 -- Get section of data.

pointer procedure mscs2s (mg, x1, x2, y1, y2)

pointer	mg		#I MOSGEOM pointer
int	x1, x2, y1, y2	#I Section

int	i, c1, c2, nc, nl, np, b1, b2, nb, line
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf, inptr, outptr

pointer imgs2s(), imgl2i(), imgl2r()
real	asums()

begin
	im = MG_IM(mg)
	if (PROC(mg) == NO) {
	    c1 = x1
	    c2 = x2
	} else {
	    c1 = 1
	    c2 = IM_LEN(im,1)
	}
	nc = x2 - x1 + 1
	nl = y2 - y1 + 1
	np = nc * nl

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgs2s (im, c1, c2, y1, y2)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = inbuf

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, np-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == np) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocs (Mems[inbuf], Mems[outbuf], np, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= c2)
		    bias = asums (Mems[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuprocs (Mems[inptr], Mems[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscuprocs (Mems[inptr], Mems[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuprocs (Mems[inptr], Mems[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscuprocs (Mems[inptr], Mems[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }

	} else {
	    inbuf = imgs2s (im, c1, c2, y1, y2)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= nc)
		    bias = asums (Mems[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscprocs (Mems[inptr], Mems[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscprocs (Mems[inptr], Mems[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscprocs (Mems[inptr], Mems[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscprocs (Mems[inptr], Mems[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }
	}

	return (outbuf)
end


# MSCUPROC - Process unsigned input.

procedure mscuprocs (in, out, n, bias, zero, flat, ccdmean)

short	in[ARB]
short	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call achtss (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end


# MSCPROC - Process input.

procedure mscprocs (in, out, n, bias, zero, flat, ccdmean)

short	in[ARB]
short	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call amovs (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end



# MSCL2 -- Get line of data.

pointer procedure mscl2i (mg, line)

pointer	mg		#I MOSGEOM pointer
int	line		#I Line

int	i, nc, b1, b2, nb
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf

pointer imgl2i()
real	asumi()
pointer	mscbuf(), imgl2s()
real	asums()
pointer	imgl2r()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgl2s (im, line)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (nc, TY_INT)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    inbuf = imgl2i (im, line)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asumi (Memi[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscproci (Memi[inbuf], Memi[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscproci (Memi[inbuf], Memi[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscproci (Memi[inbuf], Memi[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscproci (Memi[inbuf], Memi[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	}

	return (outbuf)
end


# MSCNL -- Get next line of data.

int procedure mscnli (mg, outbuf, v)

pointer	mg		#I MOSGEOM pointer
pointer	outbuf		#I Data buffer
long	v[ARB]		#I Vector

int	i, nc, b1, b2, nb, stat
long	vf[IM_MAXDIM]
real	bias
pointer	im, dzbuf, dfbuf, inbuf

pointer imgnli()
real	asumi()
pointer	mscbuf(), imgnls()
real	asums()
pointer	imgnlr()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)
	call amovl (v, vf, IM_NDIM(im))

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    stat = imgnls (im, inbuf, v)
	    if (stat == EOF)
		return (stat)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (nc, TY_INT)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (stat)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuproci (Mems[inbuf], Memi[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    stat = imgnli (im, inbuf, v)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (stat)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asumi (Memi[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscproci (Memi[inbuf], Memi[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscproci (Memi[inbuf], Memi[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscproci (Memi[inbuf], Memi[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscproci (Memi[inbuf], Memi[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)
	}

	return (stat)
end


# MSCS2 -- Get section of data.

pointer procedure mscs2i (mg, x1, x2, y1, y2)

pointer	mg		#I MOSGEOM pointer
int	x1, x2, y1, y2	#I Section

int	i, c1, c2, nc, nl, np, b1, b2, nb, line
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf, inptr, outptr

pointer imgs2i(), imgl2i(), imgl2r()
real	asumi()
pointer	mscbuf(), imgs2s()
real	asums()

begin
	im = MG_IM(mg)
	if (PROC(mg) == NO) {
	    c1 = x1
	    c2 = x2
	} else {
	    c1 = 1
	    c2 = IM_LEN(im,1)
	}
	nc = x2 - x1 + 1
	nl = y2 - y1 + 1
	np = nc * nl

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgs2s (im, c1, c2, y1, y2)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (np, TY_INT)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, np-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == np) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuproci (Mems[inbuf], Memi[outbuf], np, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= c2)
		    bias = asums (Mems[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuproci (Mems[inptr], Memi[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscuproci (Mems[inptr], Memi[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuproci (Mems[inptr], Memi[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscuproci (Mems[inptr], Memi[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }

	} else {
	    inbuf = imgs2i (im, c1, c2, y1, y2)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= nc)
		    bias = asumi (Memi[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscproci (Memi[inptr], Memi[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscproci (Memi[inptr], Memi[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscproci (Memi[inptr], Memi[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscproci (Memi[inptr], Memi[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }
	}

	return (outbuf)
end


# MSCUPROC - Process unsigned input.

procedure mscuproci (in, out, n, bias, zero, flat, ccdmean)

short	in[ARB]
int	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call achtsi (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end


# MSCPROC - Process input.

procedure mscproci (in, out, n, bias, zero, flat, ccdmean)

int	in[ARB]
int	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call amovi (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end



# MSCL2 -- Get line of data.

pointer procedure mscl2l (mg, line)

pointer	mg		#I MOSGEOM pointer
int	line		#I Line

int	i, nc, b1, b2, nb
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf

pointer imgl2l()
double	asuml()
pointer	mscbuf(), imgl2s()
real	asums()
pointer	imgl2r()
pointer	imgl2i()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgl2s (im, line)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (nc, TY_LONG)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    inbuf = imgl2l (im, line)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asuml (Meml[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscprocl (Meml[inbuf], Meml[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscprocl (Meml[inbuf], Meml[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscprocl (Meml[inbuf], Meml[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscprocl (Meml[inbuf], Meml[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	}

	return (outbuf)
end


# MSCNL -- Get next line of data.

int procedure mscnll (mg, outbuf, v)

pointer	mg		#I MOSGEOM pointer
pointer	outbuf		#I Data buffer
long	v[ARB]		#I Vector

int	i, nc, b1, b2, nb, stat
long	vf[IM_MAXDIM]
real	bias
pointer	im, dzbuf, dfbuf, inbuf

pointer imgnll()
double	asuml()
pointer	mscbuf(), imgnls()
real	asums()
pointer	imgnlr()
pointer	imgnli()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)
	call amovl (v, vf, IM_NDIM(im))

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    stat = imgnls (im, inbuf, v)
	    if (stat == EOF)
		return (stat)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (nc, TY_LONG)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (stat)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuprocl (Mems[inbuf], Meml[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    stat = imgnll (im, inbuf, v)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (stat)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asuml (Meml[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscprocl (Meml[inbuf], Meml[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscprocl (Meml[inbuf], Meml[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscprocl (Meml[inbuf], Meml[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscprocl (Meml[inbuf], Meml[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)
	}

	return (stat)
end


# MSCS2 -- Get section of data.

pointer procedure mscs2l (mg, x1, x2, y1, y2)

pointer	mg		#I MOSGEOM pointer
int	x1, x2, y1, y2	#I Section

int	i, c1, c2, nc, nl, np, b1, b2, nb, line
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf, inptr, outptr

pointer imgs2l(), imgl2i(), imgl2r()
double	asuml()
pointer	mscbuf(), imgs2s()
real	asums()

begin
	im = MG_IM(mg)
	if (PROC(mg) == NO) {
	    c1 = x1
	    c2 = x2
	} else {
	    c1 = 1
	    c2 = IM_LEN(im,1)
	}
	nc = x2 - x1 + 1
	nl = y2 - y1 + 1
	np = nc * nl

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgs2s (im, c1, c2, y1, y2)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (np, TY_LONG)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, np-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == np) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocl (Mems[inbuf], Meml[outbuf], np, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= c2)
		    bias = asums (Mems[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuprocl (Mems[inptr], Meml[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscuprocl (Mems[inptr], Meml[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuprocl (Mems[inptr], Meml[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscuprocl (Mems[inptr], Meml[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }

	} else {
	    inbuf = imgs2l (im, c1, c2, y1, y2)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= nc)
		    bias = asuml (Meml[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscprocl (Meml[inptr], Meml[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscprocl (Meml[inptr], Meml[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscprocl (Meml[inptr], Meml[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscprocl (Meml[inptr], Meml[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }
	}

	return (outbuf)
end


# MSCUPROC - Process unsigned input.

procedure mscuprocl (in, out, n, bias, zero, flat, ccdmean)

short	in[ARB]
long	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call achtsl (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end


# MSCPROC - Process input.

procedure mscprocl (in, out, n, bias, zero, flat, ccdmean)

long	in[ARB]
long	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call amovl (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end



# MSCL2 -- Get line of data.

pointer procedure mscl2r (mg, line)

pointer	mg		#I MOSGEOM pointer
int	line		#I Line

int	i, nc, b1, b2, nb
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf

pointer imgl2r()
real	asumr()
pointer	mscbuf(), imgl2s()
real	asums()
pointer	imgl2i()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgl2s (im, line)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (nc, TY_REAL)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    inbuf = imgl2r (im, line)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asumr (Memr[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscprocr (Memr[inbuf], Memr[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscprocr (Memr[inbuf], Memr[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscprocr (Memr[inbuf], Memr[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscprocr (Memr[inbuf], Memr[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	}

	return (outbuf)
end


# MSCNL -- Get next line of data.

int procedure mscnlr (mg, outbuf, v)

pointer	mg		#I MOSGEOM pointer
pointer	outbuf		#I Data buffer
long	v[ARB]		#I Vector

int	i, nc, b1, b2, nb, stat
long	vf[IM_MAXDIM]
real	bias
pointer	im, dzbuf, dfbuf, inbuf

pointer imgnlr()
real	asumr()
pointer	mscbuf(), imgnls()
real	asums()
pointer	imgnli()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)
	call amovl (v, vf, IM_NDIM(im))

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    stat = imgnls (im, inbuf, v)
	    if (stat == EOF)
		return (stat)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (nc, TY_REAL)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (stat)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuprocr (Mems[inbuf], Memr[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    stat = imgnlr (im, inbuf, v)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (stat)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asumr (Memr[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscprocr (Memr[inbuf], Memr[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscprocr (Memr[inbuf], Memr[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscprocr (Memr[inbuf], Memr[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscprocr (Memr[inbuf], Memr[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)
	}

	return (stat)
end


# MSCS2 -- Get section of data.

pointer procedure mscs2r (mg, x1, x2, y1, y2)

pointer	mg		#I MOSGEOM pointer
int	x1, x2, y1, y2	#I Section

int	i, c1, c2, nc, nl, np, b1, b2, nb, line
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf, inptr, outptr

pointer imgs2r(), imgl2i(), imgl2r()
real	asumr()
pointer	mscbuf(), imgs2s()
real	asums()

begin
	im = MG_IM(mg)
	if (PROC(mg) == NO) {
	    c1 = x1
	    c2 = x2
	} else {
	    c1 = 1
	    c2 = IM_LEN(im,1)
	}
	nc = x2 - x1 + 1
	nl = y2 - y1 + 1
	np = nc * nl

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgs2s (im, c1, c2, y1, y2)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (np, TY_REAL)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, np-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == np) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocr (Mems[inbuf], Memr[outbuf], np, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= c2)
		    bias = asums (Mems[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuprocr (Mems[inptr], Memr[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscuprocr (Mems[inptr], Memr[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuprocr (Mems[inptr], Memr[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscuprocr (Mems[inptr], Memr[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }

	} else {
	    inbuf = imgs2r (im, c1, c2, y1, y2)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= nc)
		    bias = asumr (Memr[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscprocr (Memr[inptr], Memr[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscprocr (Memr[inptr], Memr[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscprocr (Memr[inptr], Memr[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscprocr (Memr[inptr], Memr[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }
	}

	return (outbuf)
end


# MSCUPROC - Process unsigned input.

procedure mscuprocr (in, out, n, bias, zero, flat, ccdmean)

short	in[ARB]
real	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call achtsr (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end


# MSCPROC - Process input.

procedure mscprocr (in, out, n, bias, zero, flat, ccdmean)

real	in[ARB]
real	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call amovr (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end



# MSCL2 -- Get line of data.

pointer procedure mscl2d (mg, line)

pointer	mg		#I MOSGEOM pointer
int	line		#I Line

int	i, nc, b1, b2, nb
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf

pointer imgl2d()
double	asumd()
pointer	mscbuf(), imgl2s()
real	asums()
pointer	imgl2r()
pointer	imgl2i()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgl2s (im, line)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (nc, TY_DOUBLE)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    inbuf = imgl2d (im, line)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asumd (Memd[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		dfbuf = imgl2i (DFIM(mg), line)
		call mscprocd (Memd[inbuf], Memd[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		dzbuf = imgl2r (DZIM(mg), line)
		call mscprocd (Memd[inbuf], Memd[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		dfbuf = imgl2i (DFIM(mg), line)
		call mscprocd (Memd[inbuf], Memd[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscprocd (Memd[inbuf], Memd[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	}

	return (outbuf)
end


# MSCNL -- Get next line of data.

int procedure mscnld (mg, outbuf, v)

pointer	mg		#I MOSGEOM pointer
pointer	outbuf		#I Data buffer
long	v[ARB]		#I Vector

int	i, nc, b1, b2, nb, stat
long	vf[IM_MAXDIM]
real	bias
pointer	im, dzbuf, dfbuf, inbuf

pointer imgnld()
double	asumd()
pointer	mscbuf(), imgnls()
real	asums()
pointer	imgnlr()
pointer	imgnli()

begin
	im = MG_IM(mg)
	nc = IM_LEN(im,1)
	call amovl (v, vf, IM_NDIM(im))

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    stat = imgnls (im, inbuf, v)
	    if (stat == EOF)
		return (stat)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (nc, TY_DOUBLE)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, nc-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == nc) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (stat)
	    }

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asums (Mems[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscuprocd (Mems[inbuf], Memd[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)

	} else {
	    stat = imgnld (im, inbuf, v)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (stat)

	    # Process data.
	    bias = 0.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		if (b1 >= 1 && b2 <= nc) {
		    nb = b2 - b1 + 1
		    bias = asumd (Memd[inbuf+b1-1], nb)/ nb
		}
	    }

	    if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call amovl (v, vf, IM_NDIM(im))
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscprocd (Memd[inbuf], Memd[outbuf], nc, bias,
		    Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
	    } else if (DOZERO(mg) == YES) {
		stat = imgnlr (DZIM(mg), dzbuf, vf)
		call mscprocd (Memd[inbuf], Memd[outbuf], nc, bias,
		    Memr[dzbuf], INDEFI, INDEFR)
	    } else if (DOFLAT(mg) == YES) {
		stat = imgnli (DFIM(mg), dfbuf, vf)
		call mscprocd (Memd[inbuf], Memd[outbuf], nc, bias,
		    INDEFR, Memi[dfbuf], CCDMEAN(mg))
	    } else
		call mscprocd (Memd[inbuf], Memd[outbuf], nc, bias,
		    INDEFR, INDEFI, INDEFR)
	}

	return (stat)
end


# MSCS2 -- Get section of data.

pointer procedure mscs2d (mg, x1, x2, y1, y2)

pointer	mg		#I MOSGEOM pointer
int	x1, x2, y1, y2	#I Section

int	i, c1, c2, nc, nl, np, b1, b2, nb, line
real	bias
pointer	im, dzbuf, dfbuf, inbuf, outbuf, inptr, outptr

pointer imgs2d(), imgl2i(), imgl2r()
double	asumd()
pointer	mscbuf(), imgs2s()
real	asums()

begin
	im = MG_IM(mg)
	if (PROC(mg) == NO) {
	    c1 = x1
	    c2 = x2
	} else {
	    c1 = 1
	    c2 = IM_LEN(im,1)
	}
	nc = x2 - x1 + 1
	nl = y2 - y1 + 1
	np = nc * nl

	if (MG_USHORT(mg) == YES) {
	    IM_PIXTYPE(im) = TY_SHORT
	    inbuf = imgs2s (im, c1, c2, y1, y2)
	    IM_PIXTYPE(im) = TY_USHORT
	    outbuf = mscbuf (np, TY_DOUBLE)

	    if (CKNODATA(mg) == YES) {
		NODATA(mg) = NO
		do i = 0, np-1
		   if (Mems[inbuf+i] != 0)
		       break
		if (i == np) {
		    NODATA(mg) = YES
		    return (outbuf)
		}
	    }

	    if (PROC(mg) == NO) {
		call mscuprocd (Mems[inbuf], Memd[outbuf], np, -32768.,
		    INDEFR, INDEFI, INDEFR)
		return (outbuf)
	    }

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= c2)
		    bias = asums (Mems[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuprocd (Mems[inptr], Memd[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscuprocd (Mems[inptr], Memd[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscuprocd (Mems[inptr], Memd[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscuprocd (Mems[inptr], Memd[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }

	} else {
	    inbuf = imgs2d (im, c1, c2, y1, y2)
	    outbuf = inbuf

	    if (PROC(mg) == NO)
		return (outbuf)

	    # Process data.
	    if (DOBIAS(mg) == YES) {
		b1 = min (BX1(mg), BX2(mg))
		b2 = max (BX1(mg), BX2(mg))
		nb = b2 - b1 + 1
	    } else {
		b1 = 0
		b2 = 0
	    }
	    do line = y1, y2 {
		inptr = inbuf + (line - y1) * (c2 - c1 + 1)
		bias = 0.
		if (b1 >= 1 && b2 <= nc)
		    bias = asumd (Memd[inptr+b1-1], nb)/ nb

		inptr = inbuf + (line - y1) * (c2 - c1 + 1) + x1 - 1
		outptr = outbuf + (line - y1) * nc
		if (DOZERO(mg) == YES && DOFLAT(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscprocd (Memd[inptr], Memd[outptr], nc, bias,
			Memr[dzbuf], Memi[dfbuf], CCDMEAN(mg))
		} else if (DOZERO(mg) == YES) {
		    dzbuf = imgl2r (DZIM(mg), line) + x1 - 1
		    call mscprocd (Memd[inptr], Memd[outptr], nc, bias,
			Memr[dzbuf], INDEFI, INDEFR)
		} else if (DOFLAT(mg) == YES) {
		    dfbuf = imgl2i (DFIM(mg), line) + x1 - 1
		    call mscprocd (Memd[inptr], Memd[outptr], nc, bias,
			INDEFR, Memi[dfbuf], CCDMEAN(mg))
		} else
		    call mscprocd (Memd[inptr], Memd[outptr], nc, bias,
			INDEFR, INDEFI, INDEFR)
	    }
	}

	return (outbuf)
end


# MSCUPROC - Process unsigned input.

procedure mscuprocd (in, out, n, bias, zero, flat, ccdmean)

short	in[ARB]
double	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call achtsd (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end


# MSCPROC - Process input.

procedure mscprocd (in, out, n, bias, zero, flat, ccdmean)

double	in[ARB]
double	out[ARB]
int	n
real	bias
real	zero[ARB]
int	flat[ARB]
real	ccdmean

int	i
real	val

begin
	if (!IS_INDEFR(ccdmean) && !IS_INDEFR(zero[1])) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - zero[i] - bias) / val
	    }

	} else if (!IS_INDEFR(ccdmean)) {
	    do i = 1, n {
		val = flat[i] / ccdmean
		if (val <= 0.)
		    val = 1.
		out[i] = (in[i] - bias) / val
	    }

	} else if (!IS_INDEFR(zero[1])) {
	    do i = 1, n
		out[i] = in[i] - zero[i] - bias

	} else {
	    if (bias == 0.)
		call amovd (in, out, n)
	    else
		do i = 1, n
		    out[i] = in[i] - bias
	}
end




# MSCBUF -- Maintain buffer when data type conversion from IMIO is needed.

pointer procedure mscbuf (buflen, buftype)

int	buflen		#I buffer length
int	buftype		#I buffer type

int	n, type
pointer	buf

data	n/0/, type/0/, buf/NULL/

begin
	if (buflen == n && buftype == type)
	    return (buf)

	if (buftype != type) {
	    call mfree (buf, type)
	    n = 0
	}

	if (n == 0)
	    call malloc (buf, buflen, buftype)
	else
	    call realloc (buf, buflen, buftype)

	n = buflen
	type = buftype
	return (buf)
end
