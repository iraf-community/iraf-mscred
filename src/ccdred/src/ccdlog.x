include	<imhdr.h>
include	<imset.h>


# CCDLOG - The purpose of these routines is to store up log information
# in a string buffer to be flushed later.


# CCDLOG_OPEN -- Allocate and open log string buffer.

procedure ccdlog_open (maxchar)

int	maxchar		#I Maximum size of string buffer.

int	logfd, logmaxchar, stropen()
pointer	logbuf
common	/ccdlogcom/ logfd, logbuf, logmaxchar

errchk	stropen

begin
	logmaxchar = maxchar
	call calloc (logbuf, logmaxchar, TY_CHAR)
	logfd = stropen (Memc[logbuf], logmaxchar, NEW_FILE)
end


# CCDLOG_CLOSE -- Close and free log string buffer.

procedure ccdlog_close ()

int	logfd, logmaxchar
pointer	logbuf
common	/ccdlogcom/ logfd, logbuf, logmaxchar

begin
	call close (logfd)
	call mfree (logbuf, TY_CHAR)
end


# CCDLOG -- Log information with the image name.

procedure ccdlog (im, str)

pointer	im			# IMIO pointer
char	str[ARB]		# Log string

pointer	sp, fname

int	logfd, logmaxchar
pointer	logbuf
common	/ccdlogcom/ logfd, logbuf, logmaxchar

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)


	call hdmgstr (im, "tmpfname", Memc[fname], SZ_FNAME)
	if (Memc[fname] == EOS)
	    call imstats (im, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	call fprintf (logfd, "%s: %s\n")
	    call pargstr (Memc[fname])
	    call pargstr (str)

	call sfree (sp)
end


# CCDLOG_FLUSH -- Flush output.
#
#   1.  If the package "verbose" parameter is set output to standard error.
#   2.  If the package "logfile" parameter is not null append to the file.

procedure ccdlog_flush ()

int	fd, open(), stropen()
bool	clgetb()
pointer	sp, fname
errchk	open, stropen

int	logfd, logmaxchar
pointer	logbuf
common	/ccdlogcom/ logfd, logbuf, logmaxchar

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Close string buffer.
	call close (logfd)

	# Write verbose output to standard error.
	if (clgetb ("verbose")) {
#	    call eprintf ("%s")
#		call pargstr (Memc[logbuf])
	    call putline (STDERR, Memc[logbuf])
	}

	# Append to the "logfile".
	call clgstr ("logfile", Memc[fname], SZ_FNAME)
	call xt_stripwhite (Memc[fname])
	if (Memc[fname] != EOS) {
	    fd = open (Memc[fname], APPEND, TEXT_FILE)
#	    call fprintf (fd, "%s")
#		call pargstr (Memc[logbuf])
	    call putline (fd, Memc[logbuf])
	    call close (fd)
	}

	# Reopen string buffer at begining.
	logfd = stropen (Memc[logbuf], logmaxchar, NEW_FILE)

	call sfree (sp)
end


# CCDLOG_CLEAR -- Clear log string buffer.

procedure ccdlog_clear ()

int	logfd, logmaxchar, stropen()
pointer	logbuf
common	/ccdlogcom/ logfd, logbuf, logmaxchar

begin
	call close (logfd)
	logfd = stropen (Memc[logbuf], logmaxchar, NEW_FILE)
end
