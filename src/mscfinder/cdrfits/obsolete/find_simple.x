# FIND_SIMPLE -- fix the blocksize mismatch between the apple cdrom
# reader and the GSC cd's.  Scan the file to find the SIMPLE keyword.

int procedure find_simple (fd, card, ncard)

int     fd				#I Input file descriptor
char    card[ARB]			#U FITS card buffer
int     ncard				#I Size of a card

int	idx, stat, len
pointer	sp, buf

int	fillbuf(), strmatch(), fs_stridx()

begin
	call smark (sp)
	call salloc (buf, ncard, TY_CHAR)

	call amovc (card, Memc[buf], ncard)

	repeat {

	    idx = fs_stridx ("S", Memc[buf], ncard)

	    if (idx == 1) {
		# assumes ncard is greater than 8
		stat = strmatch (Memc[buf], "^SIMPLE  ")
		if (stat != 0)
		    break

		# no match, look for another "S" in this card
		idx = fs_stridx ("S", Memc[buf+1], ncard-1)
		if (idx != 0)
		    idx = idx + 1
	    }

	    if (idx == 0) {
		# read another card
		stat = fillbuf (fd, Memc[buf], ncard)

	    } else {
		# shift the buffer and top it off
		len = ncard - idx + 1
		call amovc (Memc[buf+idx-1], Memc[buf], len)
		stat = fillbuf (fd, Memc[buf+len], ncard-len)
	    }

	} until (stat == 0)

	if (stat != 0) {
	    call amovc (Memc[buf], card, ncard)

	    # see fits_rheader to justify this
	    card[ncard+1] = '\n'
	    card[ncard+2] = EOS
	}

	call sfree (sp)
	return (stat)
end


int procedure fillbuf (fd, buf, bufsiz)

int     fd				#I Input file descriptor
char    buf[ARB]			#U FITS card buffer or fraction
int     bufsiz				#I Size of a card or fraction

int	stat, junk

int	rft_read_pixels()

errchk	rft_read_pixels

begin
	iferr (stat = rft_read_pixels (fd, buf, bufsiz, junk, 1))
	    return (0)
	else if (stat != bufsiz)	# catches EOF, too
	    return (0)
	else
	    return (stat)
end



# FS_STRIDX -- Return the index of the first occurrence of a
# character in a non-EOS delimited string.

int procedure fs_stridx (ch, str, len)

char	ch				#I search character
char	str[ARB]			#I string to search
int	len				#I length of str

int	ip

begin
	do ip = 1, len {
	    if (str[ip] == ch)
		return (ip)
	}

	return (0)
end
