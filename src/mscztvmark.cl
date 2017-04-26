# MSCZTVMARK -- Mark list of objects given in celestial coordinates.
#
# The mosaic geometry file has lines with the image name, and pixel
# coordinates on the display corresponding to the image limits.
# An optional image may follow to use for the image sizes.  This feature
# is used by MSCZERO.

procedure mscztvmark (coords, frame, mscdisp)

file	coords		{prompt="List of coordinates"}
int	frame		{prompt="Display frame"}
file	mscdisp		{prompt="Mosaic geometry file"}
file	output		{prompt="Output file of pixel coordinates and labels"}
string	fields = "1,2,3" {prompt="Fields for RA, DEC, and ID"}
string	wcs = "world"	{prompt="Coordinate type (logical|physical|world)",
			 enum="logical|physical|world"}
string	mark = "circle"	{prompt="Mark type",
			 enum="point|circle|rectangle|line|plus|cross|none"}
string	radii = "10"	{prompt="Radii of concentric circles"}
string	lengths = "0"	{prompt="Lengths and width of concentric rectangles"}
string	font = "raster"	{prompt="Default font"}
int	color = 204	{prompt="Gray level of marks to be drawn"}
bool	label = no	{prompt="Label the marked coordinates"}
int	nxoffset = 0	{prompt="X offset in display pixels of number"}
int	nyoffset = 0	{prompt="Y offset in display pixels of number"}
int	pointsize = 3	{prompt="Size of mark type point in display pixels"}
int	txsize = 1	{prompt="Size of text and numbers in font units"}

struct	*fd1, *fd2

begin
	string	imroot, imsec, trimsec, str1, str2, str3
	file	crd, mscd, pix1, pix2, pix3, wcsim, im, out
	int	frm, nc, nl, lineno, nwcs
	int	tc1, tc2, tc3, tl1, tl2, tl3
	real	c1, c2, l1, cs, ls, l2, c, l, t, scale, cost, sint
	struct	str

	pix1 = mktemp ("tmp$iraf")
	pix2 = mktemp ("tmp$iraf")
	pix3 = mktemp ("tmp$iraf")

	# Get query parameters.
	crd = coords
	frm = frame
	mscd = mscdisp

	# Extract fields.
	fields (crd, fields, lines="", quit_if_miss=no, print_file_n=no, > pix3)

	# Add ID field if one is not present.
	fd1 = pix3
	lineno = 0
	while (fscan (fd1, str1, str2, str3) != EOF) {
	    if (str1 == "INDEF" || str2 == "INDEF")
	        next
	    lineno = lineno + 1
	    if (nscan() < 3)
		printf ("%s %s %d\n", str1, str2, lineno, >> pix2)
	    else
		printf ("%s %s %s\n", str1, str2, str3, >> pix2)
	}
	delete (pix3, verify-)
	if (access(pix2) == NO) {
	    printf ("No coordinates found (%s)\n", crd)
	    return
	}
	rename (pix2, pix3, field="all")

	# Get output file.
	out = ""
	print (output) | scan (out)
	if (out != "") {
	    fields (pix3, "1,2,3,1,2", quit_if_miss=no, print_file_n=no, > pix2)
	    delete (pix3, verify-)
	    rename (pix2, pix3, field="all")
	}

	fd1 = mscd
	while (fscan (fd1, wcsim, c1, c2, l1, l2, im, nwcs) != EOF) {
	    if (nscan() < 7)
		im = wcsim

	    # Merge any flip and trim.
	    hselect (im, "naxis1,naxis2", yes) | scan (nc, nl)
	    imsec = "[*,*]"
	    hselect (im, "datasec", yes) | scan (imsec)
	    hselect (im, "trimsec", yes) | scan (imsec)
	    trimsec = substr (imsec, 2, stridx(",",imsec)-1)
	    if (fscanf (trimsec, "%d:%d", tc1, tc2) != 2) {
		if (trimsec == "-*") {
		    tc1 = nc
		    tc2 = 1
		} else {
		    tc1 = 1
		    tc2 = nc
		}
	    }
	    tc3 = min (tc1, tc2)
	    tc2 = max (tc1, tc2)
	    tc1 = tc3
	    trimsec = substr (imsec, stridx(",",imsec)+1,1000)
	    if (fscanf (trimsec, "%d:%d", tl1, tl2) != 2) {
		if (trimsec == "-*]") {
		    tl1 = nl
		    tl2 = 1
		} else {
		    tl1 = 1
		    tl2 = nl
		}
	    }
	    tl3 = min (tl1, tl2)
	    tl2 = max (tl1, tl2)
	    tl1 = tl3

	    imsec = "[*,*]"
	    sections (im, option="section") | scan (imsec)
	    trimsec = substr (imsec, 2, stridx(",",imsec)-1)
	    if (trimsec == "-*") {
		tc3 = tc1
		tc1 = tc2
		tc2 = tc3
	    }
	    trimsec = substr (imsec, stridx(",",imsec)+1,1000)
	    if (trimsec == "-*]") {
		tl3 = tl1
		tl1 = tl2
		tl2 = tl3
	    }

	    # Apply merged flip and trim to image name.
	    if (tc1 != 1 || tc2 != nc || tl1 != 1 || tl2 != nl) {
		printf ("[%d:%d,%d:%d]\n", tc1, tc2, tl1, tl2) |
		    scan (trimsec)
		sections (im, option="root") | scan (imroot)
		if (im == wcsim)
		    wcsim = imroot // trimsec
		im = imroot // trimsec
	    }
	    
	    hselect (im, "naxis1,naxis2", yes) | scan (nc, nl)
	    cs = (c2 - c1) / (nc - 1)
	    ls = (l2 - l1) / (nl - 1)
	    mscctran (pix3, pix1, wcsim, wcs, "logical", columns="1 2",
		units="hours native", formats="", min_sigdigit=7, verbose=no)

#	    # Uncomment this to apply a WCS correction to the coordinates.
#	    mscctran (pix1, pix2, wcsim, "logical", "astrometry", columns="1 2",
#		units="native native", formats="", min_sigdigit=9, verbose=no)
#	    delete (pix1, verify-)
#	    t = 0.28
#	    scale = 1.001
#	    cost = scale * cos (t * 3.14159 / 180.)
#	    sint = scale * sin (t * 3.14159 / 180.)
#	    fd2 = pix2
#	    while (fscan (fd2, c, l, str) != EOF) {
#		x = c * cost + l * sint
#		y = -c * sint + l * cost
#		printf ("%g %g %s\n", x, y, str, >> pix1)
#	    }
#	    fd2 = ""; delete (pix2, verify-)
#	    mscctran (pix1, pix2, wcsim, "astrometry", "logical", columns="1 2",
#		units="native native", formats="", min_sigdigit=9, verbose=no)
#	    delete (pix1, verify-)
#	    rename (pix2, pix1, field="all")

	    lineno = 0
	    fd2 = pix1
	    while (fscan (fd2, c, l, str) != EOF) {
		if (nscan() != 3)
		    next
		lineno = lineno + 1
		c = c1 + cs * (c - 1)
		l = l1 + ls * (l - 1)
		if (c < c1 || c > c2 || l < l1 || l > l2)
		    next
		printf ("%.2f %.2f %s %d\n", c, l, str, lineno, >> pix2)
	    }
	    fd2 = ""; delete (pix1, verify-)


	    if (access (pix2)) {
		if (frm > 0)
		    tvmark (frm, pix2, logfile="", autolog=no, outimage="",
			deletions="", commands="", mark=mark, radii=radii,
			lengths=lengths, font=font, color=color, label=label,
			number=no, nxoffset=nxoffset, nyoffset=nyoffset,
			pointsize=pointsize, txsize=txsize, tolerance=1.5,
			interactive=no)

		if (out != "")
		    concatenate (pix2, out, out_type="in_type", append+)
		delete (pix2, verify-)
	    }

	}
	fd1 = ""

	delete (pix3, verify-)
end
