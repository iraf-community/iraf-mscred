# FIND_SIMPLE -- fix the blocksize mismatch between the apple cdrom
# reader and the GSC cd's.  Scan the file to find the SIMPLE keyword.
# Assumes the SIMPLE offset is either 512, 1024, 1536, or 2048 bytes.

define	CDBUFSIZ	512		# size of the junk blocks IN BYTES
define	CDBUFLIM	4		# maximum number of junk blocks


procedure find_simple (fd)

int     fd				#I file descriptor

pointer	sp, buf
int	i

int	read(), strmatch()

begin
	call smark (sp)
	call salloc (buf, CDBUFSIZ, TY_CHAR)

	do i = 1, CDBUFLIM {

	    # the test includes status == EOF
	    if (read (fd, Memc[buf], CDBUFSIZ / 2) != CDBUFSIZ / 2)
		break

	    call chrupk (Memc[buf], 1, Memc[buf], 1, CDBUFSIZ)

	    if (strmatch (Memc[buf], "^SIMPLE  ") != 0) {
		call seek (fd, 1 + (i-1) * CDBUFSIZ / 2)
		call sfree (sp)
		return
	    }

	}

	# didn't find SIMPLE - let the original code clean up
	call seek (fd, BOFL)
	call sfree (sp)
end
