include	<imhdr.h>
include	<imio.h>
include	<imset.h>
include	"ccdred.h"

# SET_OUTPUT -- Setup the output images.
# The user may select a pixel datatype with higher precision though not
# lower.

procedure set_output (output, noi, bpmask, ccd, in, out, fdnoi, bpout)

char	output[ARB]		# Image name
char	noi[ARB]		# Uninterpolated image name
char	bpmask[ARB]		# Mask name
pointer	ccd			# CCD pointer
pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer
pointer	fdnoi			# Output nointerpolation pointer
pointer	bpout			# Output BPM pointer

int	i, clscan(), nscan()
char	type[1]
pointer	immap(), ic_pmmap()
errchk	immap, ic_pmmap

begin
	# Workaround to copy any keywords added to input.
	IM_HDRLEN(in) = IM_LENHDRMEM(in)

	# Set output image.
	if (COROUT(ccd) == YES) {
	    out = immap (output, NEW_COPY, in)

	    if (ccd != NULL) {
		OUT_IM(ccd) = out
		IM_LEN(out,1) = TRIM_C2(ccd) - TRIM_C1(ccd) + 1
		IM_LEN(out,2) = TRIM_L2(ccd) - TRIM_L1(ccd) + 1
	    }
	    IM_PIXTYPE(out) = TY_REAL

	    if (clscan ("pixeltype")  != EOF) {
		call gargwrd (type, 1)
		if (nscan() == 1) {
		    i = IM_PIXTYPE(in)
		    IM_PIXTYPE(out) = i
		    switch (type[1]) {
		    case 's':
			if (i == TY_USHORT)
			    IM_PIXTYPE(out) = TY_SHORT
		    case 'u':
			if (i == TY_SHORT)
			    IM_PIXTYPE(out) = TY_USHORT
		    case 'i':
			if (i == TY_SHORT || i == TY_USHORT)
			    IM_PIXTYPE(out) = TY_INT
		    case 'l':
			if (i == TY_SHORT || i == TY_USHORT || i == TY_INT)
			    IM_PIXTYPE(out) = TY_LONG
		    case 'r':
			if (i != TY_DOUBLE)
			    IM_PIXTYPE(out) = TY_REAL
		    case 'd':
			IM_PIXTYPE(out) = TY_DOUBLE
		    default:
			call error (0, "Unknown pixel type")
		    }
		}
	    }
	} else
	    out = NULL

	# Set output bad pixel mask.
	if (CORBPM(ccd) == YES && bpmask[1] != EOS) {
	    if (out != NULL)
		bpout = ic_pmmap (bpmask, NEW_COPY, out)
	    else
		bpout = ic_pmmap (bpmask, NEW_COPY, in)
	} else
	    bpout = NULL

	BPOUT_IM(ccd) = bpout

	# Set output uninterpolated image.
	if ((CORS(ccd,FIXPIX)==YES || CORS(ccd,SATURATE)==YES) && noi[1]!=EOS) {
	    if (out != NULL)
		fdnoi = immap (noi, NEW_COPY, out)
	    else
		fdnoi = immap (noi, NEW_COPY, in)
	} else
	    fdnoi = NULL

	NOIOUT_IM(ccd) = fdnoi
end
