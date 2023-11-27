# MSCBLKAVG -- Block average mosaic exposures
# This task updates RDNOISE, GAIN, CCDSUM, DATASEC, BIASSEC, TRIMSEC, and
# CCDSEC.  It assumes the physical coordinate system corresponds to the CCD
# coordinate sytem.  The LTM and LTV keywords are updated by BLKAVG.  This
# task does not update DETSEC or AMPSEC.  It excludes partial pixels at the
# edges of the data, bias, and trim section.  In this case CCDSEC may change
# slightly and DETSEC and AMPSEC would be wrong.

procedure mcsblkavg (input, output, nc, nl)

string	input		{prompt="Input mosaic exposures"}
string	output		{prompt="Output mosaic exposures"}
int	nc=2		{prompt="Column blocking factor", min=1}
int	nl=2		{prompt="Line blocking factor", min=1}

bool	verbose=no	{prompt="Verbose output"}

struct	*fd, *fd2

begin
	int	nim, nc1, nl1, c1, c2, l1, l2, naxis1, naxis2
	real	x1, x2, y1, y2, rdnoise, gain
	file	list1, list2
	string	in, out, out1
	struct	str

	cache	sections

	list1 = mktemp ("tmp$iraf")
	list2 = mktemp ("tmp$iraf")

	# Expand input and output lists.
	sections (input, option="fullname", > list2)
	nim = sections.nimages
	sections (output, option="fullname") |
	joinlines (list2, "STDIN", output=list1, delim=" ",
	    maxchars=161, verbose-)
	delete (list2, verify-)
	if (sections.nimages != nim) {
	    delete (list1, verify-)
	    error (1, "Input and output lists do not match")
	}

	# Get remaining query parameters.
	nc1 = nc
	nl1 = nl

	# Process list of exposures.
	fd = list1
	while (fscan (fd, in, out) != EOF) {
	    if (in != out && imaccess (out//"[0]")) {
		printf ("WARNING: Output %s exists, skipping input %s\n",
		    out, in)
		next
	    }
	    printf ("blkavg $input $output %d %d option=average\n", nc1, nl1) |
		scan (str)
	    if (verbose)
		printf ("mscblkavg %s %s %d %d\n", in, out, nc1, nl1)

	    # Block average.
	    msccmd (str, in, out, verbose-)

	    # Update header.
	    msccmd ("hselect $input $I,rdnoise,gain, yes", out, verb-, > list2)
	    fd2 = list2
	    while (fscan (fd2, out1, rdnoise, gain) != EOF) {
		if (nscan() != 3)
		    next
		rdnoise = rdnoise / sqrt (nc1*nl1)
		gain = gain / (nc1*nl1)
		hedit (out1, "rdnoise", rdnoise, add+,
		    del-, update+, verify-, show=verbose)
		hedit (out1, "gain", gain, add+,
		    del-, update+, verify-, show=verbose)
	    }
	    fd2 = ""; delete (list2, verify-)
	    msccmd ("hselect $input $I,ccdsum, yes", out, verb-, > list2)
	    fd2 = list2
	    while (fscan (fd2, out1, str) != EOF) {
		if (nscan() != 2)
		    next
		if (fscan (str, c1, l1) != 2)
		    next
		c1 = nc1 * c1
		l1 = nl1 * l1
		printf ("%d %d\n", c1, l1) | scan (str)
		hedit (out1, "ccdsum", str, add+,
		    del-, update+, verify-, show=verbose)
	    }
	    fd2 = ""; delete (list2, verify-)
	    msccmd ("hselect $input $I,naxis1,naxis2,datasec, yes",
		out, verb-, > list2)
	    fd2 = list2
	    while (fscan (fd2, out1, naxis1, naxis2, str) != EOF) {
		if (nscan() != 4)
		    next
		if (fscanf (str, "[%d:%d,%d:%d]", c1, c2, l1, l2) != 4) {
		    c1 = 1
		    l1 = 1
		    c2 = naxis1
		    l2 = naxis2
		} else {
		    c1 = (c1 + nc1 - 2) / nc1 + 1
		    c2 = (c2 - nc1) / nc1 + 1
		    l1 = (l1 + nl1 - 2) / nl1 + 1
		    l2 = (l2 - nl1) / nl1 + 1
		    printf ("[%d:%d,%d:%d]\n", c1, c2, l1, l2) | scan (str)
		    hedit (out1, "datasec", str, add+,
			del-, update+, verify-, show=verbose)
		}
		x1 = c1 - 0.499
		x2 = c2 + 0.499
		y1 = l1 - 0.499
		y2 = l2 + 0.499
		print (x1, y1) | mscctran ("STDIN", "STDOUT", out1,
		    "logical", "physical", formats="", min=7, verbose-) |
		    scan (x1, y1)
		c1 = nint (x1)
		l1 = nint (y1)
		print (x2, y2) | mscctran ("STDIN", "STDOUT", out1,
		    "logical", "physical", formats="", min=7, verbose-) |
		    scan (x2, y2)
		c2 = nint (x2)
		l2 = nint (y2)
		printf ("[%d:%d,%d:%d]\n", c1, c2, l1, l2) | scan (str)
		hedit (out1, "ccdsec", str, add+,
		    del-, update+, verify-, show=verbose)
	    }
	    fd2 = ""; delete (list2, verify-)
	    msccmd ("hselect $input $I,biassec, yes", out, verb-, > list2)
	    fd2 = list2
	    while (fscan (fd2, out1, str) != EOF) {
		if (nscan() != 2)
		    next
		if (fscanf (str, "[%d:%d,%d:%d]", c1, c2, l1, l2) != 4)
		    next
		c1 = (c1 + nc1 - 2) / nc1 + 1
		c2 = (c2 - nc1) / nc1 + 1
		l1 = (l1 + nl1 - 2) / nl1 + 1
		l2 = (l2 - nl1) / nl1 + 1
		printf ("[%d:%d,%d:%d]\n", c1, c2, l1, l2) | scan (str)
		hedit (out1, "biassec", str, add+,
		    del-, update+, verify-, show=verbose)
	    }
	    fd2 = ""; delete (list2, verify-)
	    msccmd ("hselect $input $I,trimsec, yes", out, verb-, > list2)
	    fd2 = list2
	    while (fscan (fd2, out1, str) != EOF) {
		if (nscan() != 2)
		    next
		if (fscanf (str, "[%d:%d,%d:%d]", c1, c2, l1, l2) != 4)
		    next
		c1 = (c1 + nc1 - 2) / nc1 + 1
		c2 = (c2 - nc1) / nc1 + 1
		l1 = (l1 + nl1 - 2) / nl1 + 1
		l2 = (l2 - nl1) / nl1 + 1
		printf ("[%d:%d,%d:%d]\n", c1, c2, l1, l2) | scan (str)
		hedit (out1, "trimsec", str, add+,
		    del-, update+, verify-, show=verbose)
	    }
	    fd2 = ""; delete (list2, verify-)
	}
	fd = ""; delete (list1, verify-)
end
