include	<error.h>
include	<time.h>


define	XLOG_NCOLS	7				# Number of columns
define	XLOG_SZ		99				# String length

define	XLOG_LEN	(XLOG_NCOLS*(XLOG_SZ+1)+1)	# Structure length
define	XLOG_FD		Memi[$1]			# File descriptor
define	XLOG_COL	(P2C($1+($2-1)*(XLOG_SZ+1)+1))	# Pointer to string


# T_XLOG -- CL callable interface to the XLOG routines.

procedure t_xlog ()

char	output[SZ_FNAME]
int	i, j, ncols, nfd, fd[XLOG_NCOLS]
pointer	xlog, col

int	clgeti(), open(), getline()
pointer	xlog_open()
errchk	xlog_open, xlog_send, open

begin
	ncols = clgeti ("$nargs")
	if (ncols == 0)
	    return

	call clgstr ("output", output, SZ_FNAME)
	xlog = xlog_open (output)
	if (xlog == NULL)
	    return

	nfd = 0
	call amovki (NULL, fd, XLOG_NCOLS)

	do i = 1, ncols {
	    col = XLOG_COL(xlog,i)
	    call sprintf (output, SZ_FNAME, "c%d\n")
		call pargi (i)
	    call clgstr (output, Memc[col], XLOG_SZ)
	    if (Memc[col] == '@') {
		fd[i] = open (Memc[col+1], READ_ONLY, TEXT_FILE)
		nfd = nfd + 1
	    }
	}

	if (nfd > 0) {
	    repeat {
		do i = 1, ncols {
		    if (fd[i] == NULL)
			next
		    col = XLOG_COL(xlog,i)
		    j = getline (fd[i], Memc[col])
		    if (j == EOF)
			break
		    if (Memc[col+j-1] == '\n')
			Memc[col+j-1] = EOS
		}
		if (j == EOF)
		    break
		call xlog_send (xlog)
	    }
	    do i = 1, ncols {
		if (fd[i] != NULL)
		    call close (fd[i])
	    }
	} else
	    call xlog_send (xlog)

	call xlog_close (xlog)
end


# XLOG_OPEN -- Open XLOG interface.

pointer procedure xlog_open (output)

char	output[ARB]	#I Log output
pointer	xlog		#U XLOG pointer

int	open()

begin
	if (output[1] == EOS)
	    return (NULL)

	call calloc (xlog, XLOG_LEN, TY_STRUCT)

	iferr (XLOG_FD(xlog) = open (output, APPEND, TEXT_FILE)) {
	    XLOG_FD(xlog) = NULL
	    call erract (EA_ERROR)
	}

	return (xlog)
end


# XLOG_CLOSE -- Close XLOG interface.

procedure xlog_close (xlog)

pointer	xlog		#I XLOG pointer

begin
	if (xlog == NULL)
	    return
	if (XLOG_FD(xlog) != NULL)
	    call close (XLOG_FD(xlog))
	call mfree (xlog, TY_STRUCT)
end


# XLOG_SEND -- Send XLOG data.

procedure xlog_send (xlog)

pointer	xlog		#I XLOG pointer
char	time[SZ_LINE]

int	i, j, tm[LEN_TMSTRUCT]
pointer	col
bool	streq()
long	clktime()

begin
	if (xlog == NULL)
	    return
	if (XLOG_FD(xlog) == NULL)
	    return

	j = 0
	do i = 1, XLOG_NCOLS {
	    col = XLOG_COL(xlog,i)
	    if (Memc[col] == EOS)
		next
	    j = j + 1

	    if (streq (Memc[col], "TIME")) {
		call brktime (clktime(0), tm)
		call dtm_encode_hms (time, SZ_LINE, TM_YEAR(tm), TM_MONTH(tm),
		    TM_MDAY(tm), TM_HOUR(tm), TM_MIN(tm), double(TM_SEC(tm)),
		    0, 0)
		if (j > 1)
		    call fprintf (XLOG_FD(xlog), " ")
		call fprintf (XLOG_FD(xlog),  time)

	    } else {
		if (j > 1)
		    call fprintf (XLOG_FD(xlog), " ")
		call fprintf (XLOG_FD(xlog), Memc[col])
	    }
	}
	if (j > 0)
	    call fprintf (XLOG_FD(xlog), "\n")
	call flush (XLOG_FD(xlog))
end
