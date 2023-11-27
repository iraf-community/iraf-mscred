
# PATBLK_INIT --  Initialize block averging buffers.
# Memory is allocated with salloc assuming sfree won't be called until
# done with the block averaging.

procedure pblk_initi (data, map, bufs, sumbuf, nc, nc1, nc2, nlbuf)

pointer	data			#I I/O data pointer
int	map			#I Use mapio?
pointer	bufs			#O Pointer to pointers of data lines
pointer	sumbuf			#O Pointer block summed data
int	nc			#I Number of unblocked columns
int	nc1			#I Column offset
int	nc2			#I Column offset
int	nlbuf			#I Number of lines to sum

int	i, j
pointer	buf, tmpbuf

pointer	imgl2i()

begin
	# If no data is needed then return.
	if (nc1 == nc2 && nlbuf == 1) {
	    bufs = NULL
	    return
	}

	# Allocate and clear block averaging buffers.
	call salloc (bufs, nlbuf, TY_POINTER)
	do i = 1, nlbuf {
	    call salloc (Memi[bufs+i-1], nc, TY_INT)
	    tmpbuf = Memi[bufs+i-1]
	    call aclri (Memi[tmpbuf], nc)
	}
	if (nlbuf > 1) {
	    call salloc (sumbuf, nc, TY_INT)
	    call aclri (Memi[sumbuf], nc)
	} else
	    sumbuf = Memi[bufs]

	# Initialize block average buffers.
	do i = 1, nlbuf-1 {
		buf = imgl2i (data, i) - nc1

	    tmpbuf = Memi[bufs+mod(i,nlbuf)]
	    call aclri (Memi[tmpbuf], nc)
	    do j = nc1, nc2
		call aaddi (Memi[buf+j], Memi[tmpbuf], Memi[tmpbuf], nc)
	    if (nlbuf > 1)
		call aaddi (Memi[tmpbuf], Memi[sumbuf], Memi[sumbuf], nc)
	}
end


# PATBLK -- Return block average with specified end line.
# A 1x1 block is allowed.
# The various input pointers must be initialized by pblk_init and this
# routine must be called sequencially through the lines.

procedure patblki (data, line, map, buf, bufs, sumbuf, nc, nc1, nc2, nlbuf)

pointer	data			#I I/O data pointer
int	line			#I Next line to read
int	map			#I Use mapio?
pointer	buf			#U Pointer to line of data
pointer	bufs			#I Pointer to pointers of data lines
pointer	sumbuf			#I Pointer block summed data
int	nc			#I Number of columns
int	nc1			#I Column offset
int	nc2			#I Column offset
int	nlbuf			#I Number of lines to sum

int	i
pointer	tmpbuf, imgl2i()

begin
	# Get next line.
	    buf = imgl2i (data, line) - nc1

	# Do block averaging if needed.
	if (bufs != NULL) {
	    tmpbuf = Memi[bufs+mod(line,nlbuf)]
	    call asubi (Memi[sumbuf], Memi[tmpbuf], memi[sumbuf], nc)
	    call aclri (Memi[tmpbuf], nc)
	    do i = nc1, nc2
		call aaddi (Memi[buf+i], Memi[tmpbuf], Memi[tmpbuf], nc)
	    if (nlbuf > 1)
		call aaddi (Memi[tmpbuf], Memi[sumbuf], Memi[sumbuf], nc)
	    buf = sumbuf
	}
end

# PATBLK_INIT --  Initialize block averging buffers.
# Memory is allocated with salloc assuming sfree won't be called until
# done with the block averaging.

procedure pblk_initr (data, map, bufs, sumbuf, nc, nc1, nc2, nlbuf)

pointer	data			#I I/O data pointer
int	map			#I Use mapio?
pointer	bufs			#O Pointer to pointers of data lines
pointer	sumbuf			#O Pointer block summed data
int	nc			#I Number of unblocked columns
int	nc1			#I Column offset
int	nc2			#I Column offset
int	nlbuf			#I Number of lines to sum

int	i, j
pointer	buf, tmpbuf

pointer	imgl2r()
pointer	map_glr()

begin
	# If no data is needed then return.
	if (nc1 == nc2 && nlbuf == 1) {
	    bufs = NULL
	    return
	}

	# Allocate and clear block averaging buffers.
	call salloc (bufs, nlbuf, TY_POINTER)
	do i = 1, nlbuf {
	    call salloc (Memi[bufs+i-1], nc, TY_REAL)
	    tmpbuf = Memi[bufs+i-1]
	    call aclrr (Memr[tmpbuf], nc)
	}
	if (nlbuf > 1) {
	    call salloc (sumbuf, nc, TY_REAL)
	    call aclrr (Memr[sumbuf], nc)
	} else
	    sumbuf = Memi[bufs]

	# Initialize block average buffers.
	do i = 1, nlbuf-1 {
	    if (map == YES)
		buf = map_glr (data, i, READ_ONLY) - nc1
	    else
		buf = imgl2r (data, i) - nc1

	    tmpbuf = Memi[bufs+mod(i,nlbuf)]
	    call aclrr (Memr[tmpbuf], nc)
	    do j = nc1, nc2
		call aaddr (Memr[buf+j], Memr[tmpbuf], Memr[tmpbuf], nc)
	    if (nlbuf > 1)
		call aaddr (Memr[tmpbuf], Memr[sumbuf], Memr[sumbuf], nc)
	}
end


# PATBLK -- Return block average with specified end line.
# A 1x1 block is allowed.
# The various input pointers must be initialized by pblk_init and this
# routine must be called sequencially through the lines.

procedure patblkr (data, line, map, buf, bufs, sumbuf, nc, nc1, nc2, nlbuf)

pointer	data			#I I/O data pointer
int	line			#I Next line to read
int	map			#I Use mapio?
pointer	buf			#U Pointer to line of data
pointer	bufs			#I Pointer to pointers of data lines
pointer	sumbuf			#I Pointer block summed data
int	nc			#I Number of columns
int	nc1			#I Column offset
int	nc2			#I Column offset
int	nlbuf			#I Number of lines to sum

int	i
pointer	tmpbuf, imgl2r()
pointer	map_glr()

begin
	# Get next line.
	if (map == YES)
	    buf = map_glr (data, line, READ_ONLY) - nc1
	else
	    buf = imgl2r (data, line) - nc1

	# Do block averaging if needed.
	if (bufs != NULL) {
	    tmpbuf = Memi[bufs+mod(line,nlbuf)]
	    call asubr (Memr[sumbuf], Memr[tmpbuf], memr[sumbuf], nc)
	    call aclrr (Memr[tmpbuf], nc)
	    do i = nc1, nc2
		call aaddr (Memr[buf+i], Memr[tmpbuf], Memr[tmpbuf], nc)
	    if (nlbuf > 1)
		call aaddr (Memr[tmpbuf], Memr[sumbuf], Memr[sumbuf], nc)
	    buf = sumbuf
	}
end

