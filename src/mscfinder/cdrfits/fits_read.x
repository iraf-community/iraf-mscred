# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include <error.h>
include <imhdr.h>
include <imio.h>
include <imset.h>
include <fio.h>
include <tbset.h>
include	"rfits.h"

# RFT_READ_FITZ -- Convert a FITS file. An EOT is signalled by returning EOF.

int procedure rft_read_fitz (fitsfile, template, iraffile)

char	fitsfile[SZ_FNAME]	# FITS file name
char	iraffile[SZ_FNAME]	# IRAF file name
char	template[SZ_FNAME]	# Template filename

char	root[SZ_FNAME], cluster[SZ_FNAME]
char	nroot[SZ_FNAME]
char	extn[SZ_EXTN], rb, lb
char	tabfile[SZ_FNAME], seqfile[SZ_FNAME]
int	fits_fd, istat, stat, pos1, pos2, ntab, nch, fnroot()
int	rft_read_header(), mtopen(), strlen()
int	tbtopn(), rtb_read_header(), gstrmatch()
int	open(), gi_gstfval()
int	stridx(), id1, id2, tbpsta()
int	nread, fd_usr, ncols

pointer	im, imt, sp, fits, tp, fn

errchk	smark, sfree, salloc, rft_read_header, rft_read_image, mtopen
errchk	close, imunmap, imrename, frename, rft_opnim
	
include	"rfits.com"

define   frmxtn_ 99

begin
	stat = 0
	# Open input FITS data
	fits_fd = mtopen (fitsfile, READ_ONLY, 0)

	# Allocate memory for program data structure
	call smark (sp)
	call salloc (fits, LEN_FITS, TY_STRUCT)
frmxtn_	call salloc (fn, SZ_FNAME, TY_CHAR)

	if (long_header == YES) {
	    call printf ("\n**** FILE: %s\n ")
		call pargstr (iraffile)
	}

	if (short_header == YES) {
	    if (tape == YES) {
		lb = '['
		rb = ']'
		id1 = stridx (lb, fitsfile) + 1
		id2 = stridx (rb, fitsfile) - 1
		call strcpy (fitsfile[id1], Memc[fn], id2-id1+1)
		call printf ("%-5.5s")
		    call pargstr (Memc[fn])
	    } else {
		call printf("%-16.16s ")
		    call pargstr (fitsfile)
	    }
	}

	call imgcluster (iraffile, cluster, SZ_FNAME)
	call iki_parse (cluster, root, extn)

	# 'gkey' can have the following values
	#  DEF_GPB:    Will create a default gp descriptor
	#  NONDEF_GPB: Will read the gp descriptor from a user supplied
	#	       template with the non_default gp keywords.
	#  NON_GPB:    Will not create a gp descriptor in the output SDAS
	#	       file. This value gets setup with the fits keyword
	#	       SDASMGNU, which indicates that the input fits file
	#	       contains a file with an extra dimension for the groups
	#	       and an attached table with the gp values.
	#  TO_MG:      Will create a multigroup Geis file from the input
	#	       FITS file and its attached table. The FITS header
	#	       should have the keyword SDASMGNU and OPSIZE to
	#	       accomplish this.
	#  -1:	       If the output file is 'imh' type.

	if (strlen(template) != 0)
	    gkey = NONDEF_GPB		

	# Open spool file to contain the fits header
	fd_usr = open ("fits_hdr", READ_WRITE, SPOOL_FILE)
	
	iferr {
	    # locate the SIMPLE keyword if junk is prepended from the CDrom
	    call find_simple (fits_fd)

	    nread = rft_read_header (fits_fd, fd_usr, fits)
        } then {
	    call erract (EA_WARN)
	    return
	}

	if (gkey == TO_MG && GCOUNT(fits) == -1)
	    gkey = DEF_GPB 

	# If the user has chosen xdimtogf but PSIZE and GCOUNT are
	# zero then reset to gkey=NON_GPB
	if (gkey == TO_MG && GCOUNT(fits) == 0 && OPSIZE(fits) == 0)
	    gkey = NON_GPB

	if (nread != EOF) {
	    iferr {
		call rft_opnim (template, iraffile, fd_usr, fits,
		    nread, im, imt)
	        call rft_read_image (fits_fd, fits, im)
	    } then {
		call sfree (sp)
	        call erract (EA_WARN)
		return
	    }
	} else {
	    call printf ("End of data\n")
	    call close (fd_usr)
	    call close (fits_fd)
	    call sfree (sp)
	    return (EOF)
	}

	call close (fd_usr)

        if (NAXIS(fits) != 0) {
	
	    if (IM_KERNEL(im) == 2) {
		# Update the wcs values since the STF_WCS routines did not
		# have the CRPIX nor the CRVAL's keywords in the user header.
		if (gkey == DEF_GPB)
		    call update_gpb (im,fits)

		# Because of some bug in stf the value below does
		# not get change to true.
		if (gkey == NON_GPB && gi_gstfval(im, "PCOUNT") == 0)
		    call gi_pstfval (im, "GROUPS", YES)
	    }

	    if (gkey == NON_GPB || gkey == NONDEF_GPB)
		call imunmap (imt)

	    call imunmap (im)

	    call print_header (fits, fitsfile, iraffile)

        } else   # if nxis==0
	    if (short_header == YES ) {
		if (strlen(IRAFNAME(fits)) == 0)
		    call strcpy ("0_len image", IRAFNAME(fits), SZ_FNAME)

		call printf ("%-20.20s")
		    call pargstr (IRAFNAME(fits))

		call printf("0\n")
	    }

	# If the above main header contains an extension flag, then
	# lets see if we find a table.

	if (EXTEND(fits) == YES) {
	    # No ieee for tables in FITS format
	    ieee = NO
	    call tbtext (root, tabfile, SZ_FNAME)
	    call strcpy (tabfile, seqfile, SZ_FNAME)
	    nch = gstrmatch (seqfile, ".tab", pos1, pos2)
	    ntab = 1

	    call sprintf (seqfile[pos1], SZ_FNAME, "%02d")
		call pargi(ntab)

	    call strcat (".tab", seqfile, SZ_FNAME)

	    # look for more than one table in the current file
	    repeat {
		tp = tbtopn (seqfile, NEW_FILE, 0)
	  
		# read FITS table header an user parameters if any,
		# also create the table 'tbtcre'.
		istat = rtb_read_header (fits_fd, im, fits, tp)

		# istat will will have the value below only after the
		# first reading of the header. If the value is not
		# encounter then it will read the whole table header
		# before coming here.

		if (istat == IMAGE_IUE) {
		    call strcpy (seqfile, root, pos1+1)
		    call iki_mkfname (root, extn, iraffile, SZ_FNAME) 
		    goto frmxtn_

		} else if (istat != EOF) {
		    # Now read the fits table data
		    call rtb_read_tfits (fits_fd, fits, tp)
		    ncols = tbpsta(tp, TBL_NCOLS)
		    call tbtclo (tp)
		  
		    # print table information to STDOUT.
		    call prtab_info (fits, seqfile, ncols)

		    # Save previous name in case there is no next table
		    call strcpy (seqfile, Memc[fn], SZ_FNAME)

		    # increase sequence number for table name
		    ntab = ntab + 1
		    call sprintf (seqfile[pos1], SZ_FNAME, "%02d")
			call pargi(ntab)

		    call strcat (".tab", seqfile, SZ_FNAME)

		}

	    } until (istat == EOF)

	    # Get rid of the sequential number if only 
	    # one table is present.
	    if (ntab == 2 && old_name == NO) {
		call frename (Memc[fn], tabfile)

		if (short_header == YES ) {
		    nch = fnroot (tabfile, nroot, SZ_FNAME)
		    call printf("%17t renamed to %s.tab\n")
		    call pargstr(nroot)
		}
	    }
	}

	call close (fits_fd)
	call sfree (sp)
	return (stat)
end


# The following definition is necessary to open the template
# file 'non_gpb.hhh' which has the keywrod "GROUPS = F" 
# plus PCOUNT and GCOUNT to zero also.

define	NONGPB_HDR	"fitsio$non_gpb.hhh"
define	LEN_CARDP1	81

procedure rft_opnim (template, iraffile, fd_usr, fits, nread, im, imt)

char	template[SZ_FNAME]	# template file name
char	iraffile[SZ_FNAME]	# output image name
int	fd_usr			# fits header spool file des.
pointer fits			# fits descriptor
int	nread			# number of header lines in the fits header
pointer	im			# output image descriptor
pointer	imt			# o: template image pointer

pointer	ua
int	i, fd, maxlines, max_lenuser, stropen()
int	immap()
errchk  immap

include "rfits.com"

begin	

	if (NAXIS(fits) == 0)
	    return

	# If template is specified, the user has chosen to create a
	# non_default gpb descriptor. This will reset any previous
	# value, e.g. gkey = NON_GPB if the keyword SDASMGCV was
	# present in the fits header.

	# Create IRAF image header.
	if (gkey == NONDEF_GPB) {
	    imt = immap (template, READ_ONLY, 0)
	    im  = immap (iraffile, NEW_COPY, imt)

	} else if (gkey == NON_GPB) {
	    imt = immap (NONGPB_HDR, READ_ONLY, 0)
	    im  = immap (iraffile, NEW_COPY, imt)

	} else {
	    im = immap (iraffile, NEW_IMAGE, 0)

	}

	# reset the naxis things
	IM_NDIM(im) = NAXIS(fits)

	do i = 1, IM_NDIM(im)
	    IM_LEN(im,i) = NAXISN(fits,i)
	
	# Now copy the fits header lines onto the IM_USERAREA
	maxlines = (ARB - 3700)/LEN_CARD

	if (nread > maxlines) {
	    call printf ("=== %d fits header lines discarded\n")
		call pargi (nread - maxlines)

	    call printf ("Maximun number of lines is: %d\n")
		call pargi (maxlines)

	    nread = maxlines
	} 

	IM_LENHDRMEM(im) = nread*LEN_CARD + LEN_IMHDR
	call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	max_lenuser = (IM_LENHDRMEM(im) + LEN_IMDES - IMU)*SZ_STRUCT
	ua = IM_USERAREA(im) 
	fd = stropen (Memc[ua], max_lenuser, NEW_FILE)
	call seek (fd_usr, BOFL)

	if (gkey == DEF_GPB && IM_KERNEL(im) == 2)
	    call rft_create_gpb (im, fd)

	call fcopyo (fd_usr, fd)
	call close (fd)

	iferr (	call imgstr (im, "OBJECT", IM_TITLE(im), SZ_OBJECT))
	    IM_TITLE(im) = EOS
end


# PRINT_HEADER -- Routine to rename the output file if necessary
# and to print header information.

procedure print_header (fits, fitsfile, iraffile)

pointer fits
char	fitsfile[SZ_FNAME]
char	iraffile[SZ_FNAME]

char	root[SZ_FNAME]
char	nroot[SZ_FNAME], nextn[SZ_EXTN]
char	extn[SZ_EXTN]
int	status, k, strlen(), itab, fnldir(), fnroot(), fnextn()
int	strmatch()
errchk  imrename, stf_rname
pointer sp, bf

include "rfits.com"

begin
	call smark (sp)
	call salloc (bf, SZ_FNAME, TY_CHAR)

	call imgcluster (iraffile, Memc[bf], SZ_FNAME)
	call iki_parse (Memc[bf], root, extn)

	# set itab for tape or disk file
	itab = 16
	if (tape == YES)
	    itab = 5

	if (old_name == YES && strlen (IRAFNAME(fits)) != 0) {
	    ##### At this time we cannot rename old.hhh to new.c0h
	    k = fnldir (iraffile, Memc[bf], SZ_FNAME)
	    call pesc_dash (IRAFNAME(fits))
	    k = fnroot (IRAFNAME(fits), nroot, SZ_FNAME)
	    k = fnextn (IRAFNAME(fits), nextn, SZ_EXTN)

	    if (gkey == TO_MG)
		if (strmatch (nroot, "_cvt") != 0)
		    nroot[strlen(nroot)-3] = EOS 

	    call strcat (nroot, Memc[bf], SZ_FNAME)
	    call iki_mkfname (Memc[bf], nextn, IRAFNAME(fits), SZ_FNAME)
	    call cesc_dash (IRAFNAME(fits))

#	    iferr (call imrename (iraffile, Memc[bf])) {

	    iferr {
		if (gkey < 0)
		    call imrename (iraffile, IRAFNAME(fits))
		else {
		    call stf_rname (root, extn, Memc[bf], nextn, status)

		    # copy in buffer to be use below, in case is > 19 chars
		    call strcpy (IRAFNAME(fits), Memc[bf], SZ_FNAME)
		}

	    } then {
		call printf ("Cannot rename %s to %s\n")
		    call pargstr (iraffile)
		    call pargstr (IRAFNAME(fits))

		call printf ("%*t")
		    call pargi (itab)

	    } else
		if (short_header == YES) {
		    call printf ("%-19.19s ")
			call pargstr (IRAFNAME(fits))
		}

	} else
	    if (short_header == YES) {
		call printf ("%-19.19s ")
		    call pargstr(iraffile)
	    }

	if (short_header == YES) {
	    do k = 1, NAXIS(fits) {
		call printf("%-5.5d")
		    call pargi(NAXISN(fits,k))
	    }

	    if (NAXIS(fits) == 1) 
		call printf("%11t")

	    if (NAXIS(fits) == 2) 
		call printf("%6t")

	    call printf("%-2.2d %-8.8s ") 
		call pargi(BITPIX(fits))
		call pargstr(DATE(fits))

	    if (tape == YES)
		call printf ("%-26.26s")
	    else
		call printf ("%-15.15s")

		    call pargstr (OBJECT(fits))

	    call printf("\n")

	    # See if fitsfile and/or iraffile are too long and put
	    # in the following line
	    do k = 1, 3 {
		if (strlen(fitsfile) > 16*k) {
		    call printf ("%-16.16s ")
			call pargstr (fitsfile[16*k+1])

		    if (strlen(Memc[bf]) > 19*k) {
			call printf ("%-19.19s ")
			    call pargstr (Memc[bf+19*k])
		    }

		    call printf ("\n")

		} else
		    if (strlen(Memc[bf]) > 19*k) {
			call printf ("%*t %-19.19s \n")
			    call pargi (itab)
			    call pargstr (Memc[bf+19*k])
		    }
	    }

	}

	call sfree(sp)
end


# PRTAB_INFO -- Procedure to print table information to STDOUT

define	REGION_KEY	"REGION"
define	LEN_REGION	5

define	SZ_EXTN		3

procedure prtab_info (fits, seqfile, ncols)

pointer	fits			#fits descriptor
char	seqfile[SZ_FNAME]	#sequential table filename
int	ncols			#NUmber of columns in table.

pointer sp, bf, rr, sf, tp
int	len, k, itab, key
char	extn[SZ_EXTN]

int	strlen(), fnldir(), fnroot(), fnextn(), strcmp(), strmatch()
pointer	tbtopn()

include "rfits.com"

begin
	call smark(sp)
	call salloc (bf, SZ_FNAME, TY_CHAR)
	call salloc (sf, SZ_FNAME, TY_CHAR)

	# set tab value for tape or disk file
	itab = 17
	if (tape == YES)
	    itab = 5
	   
	# save 'seqfile' 
	call strcpy (seqfile, Memc[sf], SZ_FNAME)

	if (old_name == YES && strlen (IRAFNAME(fits)) != 0) {
	    call salloc (rr, SZ_FNAME, TY_CHAR)

	    k = fnldir (Memc[sf], Memc[bf], SZ_FNAME)

	    # Escape dash '-' in name if any
	    call pesc_dash(IRAFNAME(fits))
	    k = fnroot (IRAFNAME(fits), Memc[rr], SZ_FNAME)

	    call cesc_dash(Memc[rr])
	    k = fnextn (IRAFNAME(fits), extn, SZ_EXTN)

	    if (strcmp (extn, "trl") == 0) {
		call strcat ("_trl", Memc[rr], SZ_FNAME)
		call strcpy ("tab", extn, SZ_EXTN)
	    }	      	      

	    # Add the .tab extension, if it's missing.  In this case,
	    # also add the GSC region number to the header, should
	    # really parametrize this, but the assumption is that if
	    # both oldirafname is set and IRAFNAME has no extension,
	    # then we're dealing with a GSC file off the CDrom.

	    if (extn[1] == EOS || strmatch (extn, "^#$") != 0) {
		call strcpy ("tab", extn, SZ_EXTN)

		len = strlen (Memc[rr])
		call sscan (Memc[rr+len-LEN_REGION])
		    call gargi (key)

		if (key > 0) {
		    # should really do this somewhere else...
		    tp = tbtopn (seqfile, READ_WRITE, 0)
		    call tbhadi (tp, REGION_KEY, key)
		    call tbtclo (tp)
		}
	    }

	    call strcat (Memc[rr], Memc[bf], SZ_FNAME)
	    call iki_mkfname (Memc[bf], extn, IRAFNAME(fits), SZ_FNAME)

	    iferr (call frename (seqfile, IRAFNAME(fits)) ) {
		call printf ("Cannot rename %s to %s\n")
		    call pargstr (Memc[sf])
		    call pargstr (IRAFNAME(fits))

	    } else if (short_header == YES) {
		call printf ("%*t %-19.19s ")
		    call pargi (itab)
		    call pargstr (IRAFNAME(fits))

		call strcpy (IRAFNAME(fits), Memc[sf], SZ_FNAME)

	    } else {
		# assume that if someone took the time to turn off
		# short_header in these circumstances that they want
		# an explicit list of output file names.

		call printf ("%s\n")
		    call pargstr (IRAFNAME(fits))

	    }

	} else if (short_header == YES) {
		call printf ("%*t %-19.19s ")
		    call pargi (itab)
         	    call pargstr (Memc[sf])
	}

	if (short_header == YES) {
	    call printf ("%-4.4d %-5.5d")
		call pargi (FITS_ROWLEN(fits))
		call pargi (FITS_NROWS(fits))

	    call printf(" Ncols=%3d ")
		call pargi (ncols)

	    if (old_name == NO) {
		call strcpy(IRAFNAME(fits), Memc[bf], LEN_CARD)

		if (Memc[bf] == EOS)
		    call strcpy("  ", Memc[bf], LEN_CARD)

		if (tape == YES)
		    call printf("%-30.30s")
		else
		    call printf("%-22.22s")

			call pargstr(Memc[bf])
	    }

	    call printf("\n")

	    # See if fitsfile and/or iraffile are too long and put
	    # in the following line
	    do k = 1, 3
		if (strlen(Memc[sf]) > 19*k) {
		    call printf ("%*t %-19.19s \n")
			call pargi (itab)
			call pargstr (Memc[sf+19*k])
		}
	}

	call sfree(sp)
end
