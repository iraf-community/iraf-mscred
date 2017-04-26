# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include <error.h>
include <fset.h>
include "rfits.h"

define	MAX_RANGES	100
define	LEN_EXTN	3

# RFITS -- Read FITS format data.  Further documentation given in rfits.hlp

procedure t_rfits()

char	infile[SZ_FNAME]		# fits file
char	outfile[SZ_FNAME]		# IRAF file
char	in_fname[SZ_FNAME]		# input file name
char	out_fname[SZ_FNAME]		# output file name
char	file_list[SZ_LINE]		# list of tape files
char	template[SZ_FNAME]		# template file
char	cluster[SZ_FNAME], tmp[SZ_FNAME]
char    root[SZ_FNAME], extn[LEN_EXTN], extn2[LEN_EXTN]

pointer	list
int	lenlist, junk
int	range[MAX_RANGES*2+1], nfiles, file_number, offset, stat, fits_record

bool	clgetb()
char	clgetc()
int	rft_get_image_type(), clgeti(), mtfile(), strlen(), btoi()
int	rft_read_fitz(), decode_ranges(), get_next_number(), fntgfnb()
int	fntlenb(), envfind(), strncmp()
pointer	fntopnb()
real	clgetr()
int	cl_index, cl_size, xdimtogf
data	fits_record/2880/
include	"rfits.com"

begin
	# Set up the standard output to flush on a newline
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get RFITS parameters.
	call clgstr ("fits_file", infile, SZ_FNAME)
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	len_record = fits_record

	call clgstr ("iraf_file", outfile, SZ_FNAME)
	call clgstr ("template", template, SZ_FNAME)
	data_type = rft_get_image_type (clgetc ("datatype"))
	scale = btoi (clgetb ("scale"))
	blank = clgetr ("blank")
	old_name = btoi (clgetb ("oldirafname"))
	offset = clgeti ("offset")
	xdimtogf = btoi (clgetb ("xdimtogf"))

	
	# Allow only one type of output
	if (short_header == YES)
	   long_header = NO
	# Compute the number of files to be converted
	tape = NO
	if (mtfile (infile) == YES)  {
	    tape = YES
	    list = NULL
	    if (infile[strlen(infile)] != ']')
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
	        call strcpy ("1", file_list, SZ_LINE)
	    if (short_header == YES) {
	       call printf ("FILE# IRAFNAME            Dimensions    ")
	       call printf (" BP   DATE   OBJECT\n")
	    }
	} else {
	    list = fntopnb (infile, YES)
	    lenlist = fntlenb (list)
	    if (lenlist > 0) {
	        call sprintf  (file_list, SZ_LINE, "1-%d")
		    call pargi (lenlist)
	    } else
	        call sprintf  (file_list, SZ_LINE, "0")
	    if (short_header == YES) {
	       call printf ("Fits_file        IRAFNAME           ")
	       call printf (" Dimensions     BP   DATE   OBJECT\n")
	    }
	}

	# Decode the ranges
	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (1, "T_RFITS: Illegal file number list")

	# Read successive FITS files, convert and write into a numbered
	# succession of output IRAF files.

	cl_size = -1
	cl_index = -1
	call imparse (outfile, cluster, SZ_FNAME, tmp, 
	              SZ_FNAME, tmp, SZ_FNAME, cl_index, cl_size)

	call strcpy (cluster, out_fname, SZ_FNAME)
	# Create output filename with multigroup syntax, disable old_name
	# parameter since we cannot rename the output GEIS file to whatever
	# the IRAFNAME FITS keyword has.
	if (cl_size > 1) {
	   old_name = NO
	   call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME, "[1/%d]")
		   call pargi (cl_size)
        }

	# See if there is an extension
	call iki_parse (cluster, root, extn)
	if (extn[1] == EOS) {
	   if (envfind ("imtype", extn, SZ_FNAME) <= 0)
	      # No extension encountered. If there is a template file
	      # get its extension and use that for the output file.
	      if (strlen (template) !=0) {
	         call iki_parse (template, root, extn)
	      } else {
		 # Assume 'hhh' since we are using STSDAS. If the user
		 # has not supplied and output extension he/she will
		 # the above.
		 call strcpy ("hhh", extn, LEN_EXTN)	      
	      }
	   call iki_mkfname (root, extn, cluster, SZ_FNAME)
	   call strcpy (cluster, out_fname, SZ_FNAME)
	}
	
	# Set the type of output file (gkey) for "imh" files.
	if (strncmp (extn, "imh", LEN_EXTN) == 0)
	   gkey = -1
	if (gkey == -1 && xdimtogf == YES)
	   call error (1, "You cannot select the 'imh' extension and xdimtogf")

	file_number = 0
	while (get_next_number (range, file_number) != EOF) {

	    # Set the type of output file.
	    # For the explanation on the values see fits_read.x
	    if (gkey != -1)
	       gkey = DEF_GPB
	    if (xdimtogf == YES)
	       gkey = TO_MG

	    # Get input file name
	    if (list != NULL)
		junk = fntgfnb (list, in_fname, SZ_FNAME)
	    else {
	        call strcpy (infile, in_fname, SZ_FNAME)
	        if (infile[strlen(infile)] != ']') {
		    call sprintf (in_fname[strlen(in_fname)+1], SZ_FNAME,
		        "[%d]")
		        call pargi (file_number)
		}
	    }

	    # Get output file name
            if (cl_index > 1) {
	       template[1] = EOS
	       call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME, "[%d]")
		       call pargi (cl_index)
	    } 
	    if (nfiles > 1 && cl_size == 0) {
	       call iki_parse (out_fname, root, extn2)
	       call sprintf (root[strlen(root)+1], SZ_FNAME, "%03d")
		      call pargi (file_number + offset)
	       call iki_mkfname (root, extn, out_fname, SZ_FNAME)
	    }
	    if (nfiles > 1 && cl_size != 0)
	       cl_index = cl_index + 1

	    # Convert FITS file to the output IRAF file.
	    # If EOT is reached then exit.
	    # If an error is detected then print a warning and continue with
	    # the next file.

	    iferr (stat = rft_read_fitz (in_fname, template, out_fname))
		call erract (EA_FATAL)
	    if (stat == EOF)
		break

	    call strcpy (cluster, out_fname, SZ_FNAME)
	}

	if (list != NULL)
	    call fntclsb (list) 
end


define NTYPES 7
# RFT_GET_IMAGE_TYPE -- Convert a character to and IRAF image type.

int procedure rft_get_image_type (c)

char	c
int	type_codes[NTYPES], i
int	stridx()
string	types "usilrdx"			# supported image data types
data	type_codes /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
		    TY_DOUBLE, TY_COMPLEX/
begin
	i = stridx (c, types)
	if (i == 0)
	    return (ERR)
	else
	    return (type_codes[stridx(c,types)])
end
