# MSCZERO -- Display, measure coordinates, and adjust WCS zero point.

procedure msczero (input)

string	input			{prompt="List of mosaic exposures"}
string	extname = ""		{prompt="Extension name pattern"}
int	nframes = 2		{prompt="Number of frames to use"}
int	cbox = 11		{prompt="Centering box size (see imcntr)"}
bool	mark = yes		{prompt="Mark display?"}
file	logfile	= "default"	{prompt="Log file for measurements\n\n# MSCTVMARK Parameters"}

file	coords		{prompt="List of coordinates", mode="q"}
string	fields = "1,2,3" {prompt="Fields for RA, DEC, and ID"}
string	wcs = "world"	{prompt="Coordinate type (logical|physical|world)",
			 enum="logical|physical|world"}
string	catalog = "usnob1@noao"	{prompt="Catalog", mode="q"}
string	mtype = "circle" {prompt="Mark type",
			 enum="point|circle|rectangle|line|plus|cross|none"}
string	radii = "20"	{prompt="Radii of concentric circles", mode="q"}
int	color = 204	{prompt="Gray level of marks to be drawn", mode="q"}
bool	label = no	{prompt="Label the marked coordinates", mode="q"}
int	nxoffset = 20	{prompt="X offset in display pixels of number"}
int	nyoffset = 0	{prompt="Y offset in display pixels of number"}
int	pointsize = 3	{prompt="Size of mark type point in display pixels"}
int	txsize = 2	{prompt="Size of text and numbers in font units\n\n# Task query and internal parameters"}

string	ra		{prompt="RA (hours)", mode="q"}
string	dec		{prompt="DEC (degrees)", mode="q"}
string	id		{prompt="Identification", mode="q"}
real	mag		{prompt="Magnitude limit", mode="q"}
bool	update = yes	{prompt="Update WCS zero point?", mode="q"}
bool	updcoord = yes	{prompt="Update coordinate file?", mode="q"}

struct	*fd1, *fd2

begin
	bool	first, upd, cntr, markit, idquery, idlabel
	int	nim, frame, len, nlog, stat, nc, nl, nwcs, nwcs1, wcsver
	int	lineno, linemark
	int	tc1, tc2, tc3, tl1, tl2, tl3
	file	im, im1, wcsim1, crds
	file	mscdisp1, mscdisp2, wcsim, images, markcoord, sed, temp
	real	telra, teldec, crval1, crval2, dx, dy, dist, distmin
	real	wx, wy, wx1, wy1, ax, ay, r, d, c1, c2, l1, l2, cs, ls
	real	cmark, lmark
	string	key, junk, rastr, decstr, logf, id1, ramark, decmark, idmark
	string	imsec, imroot, trimsec, rad, cat, fmtra, fmtdec, fmtboth
	int	clr
	struct	cmd

	mscdisp1 = mktemp ("tmp$iraf")
	mscdisp2 = mktemp ("tmp$iraf")
	wcsim = mktemp ("tmp$iraf") // ".fits"
	images = mktemp ("tmp$iraf")
	markcoord = mktemp ("tmp$iraf")
	sed = mktemp ("tmp$iraf")
	temp = mktemp ("tmp$iraf")

	# Foreign command to edit coordinate list.
	task $msczsed = "$sed -f $(1) $(2) > $(3)"

	# Get task query parameters
	sections (input, option="fullname", > images)

	# Set default marking parameters.
	key = catalog.p_mode; catalog.p_mode = "h"; cat = catalog; catalog.p_mode = key
	key = radii.p_mode; radii.p_mode = "h"; rad = radii; radii.p_mode = key
	key = color.p_mode; color.p_mode = "h"; clr = color; color.p_mode = key

	# Set coordinate print format.
	if (wcs == "world") {
	    fmtra = "%.2H"; fmtdec = "%.1h"; fmtboth = "%.2H %.1h"
	} else {
	    fmtra = "%.2f"; fmtdec = "%.2f"; fmtboth = "%.2f %.2f"
	}

	# Initialize.
	nim = 0
	first = YES
	idquery = NO
	id1 = ""

	# Loop through list of images.
	fd1 = images
	while (fscan (fd1, im) != EOF) {
	    nim = nim + 1

	    # Strip ".fits".
	    len = strlen (im)
	    if (len > 5)
		if (substr (im, len-4, len) == ".fits")
		    im = substr (im, 1, len-5)

	    # Display image if needed.
	    frame = mod (nim - 1, nframes) + 1
	    mscdisplay (im, frame=frame, extname=extname, check+, select+)

	    # Make dummy WCS image for keeping modified WCS.
	    wcsver = 0
	    nwcs = 0
	    nwcs1 = 0
	    fd2 = "uparm$mscdisp" // frame
	    while (fscan (fd2, im1, c1, c2, l1, l2, nwcs1) != EOF) {
		if (nwcs1 > 0)
		    wcsver = 1
		nwcs = nwcs + 1
		printf ("%s[im%d]\n", wcsim, nwcs) | scan (wcsim1)
		printf ("%s %d %d %d %d %s %d\n",
		    wcsim1, c1, c2, l1, l2, im1, nwcs1, >> mscdisp1)

		# Merge any flip and trim.
		hselect (im1, "naxis1,naxis2", yes) | scan (nc, nl)
		imsec = "[*,*]"
		hselect (im1, "datasec", yes) | scan (imsec)
		hselect (im1, "trimsec", yes) | scan (imsec)
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
		sections (im1, option="section") | scan (imsec)
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
		    
		# Extract a small piece for a WCS image.
		if (tc1 <= tc2)
		    tc3 = tc1 + 1
		else
		    tc3 = tc1 - 1
		if (tl1 <= tl2)
		    tl3 = tl1 + 1
		else
		    tl3 = tl1 - 1
		sections (im1, option="root") | scan (imroot)
		printf ("[%d:%d,%d:%d]\n", tc1, tc3, tl1, tl3) | scan (trimsec)
		imcopy (imroot//trimsec, wcsim1, verbose-)

		# Apply merged flip and trim to image name.
		if (tc1 != 1 || tc2 != nc || tl1 != 1 || tl2 != nl) {
		    printf ("[%d:%d,%d:%d]\n", tc1, tc2, tl1, tl2) |
			scan (trimsec)
		    im1 = imroot // trimsec
		}

		hselect (im1, "naxis1,naxis2", yes) | scan (nc, nl)
		printf ("%s %d %d %d %d %s %d %d %d\n",
		    wcsim1, c1, c2, l1, l2, im1, nc, nl, nwcs1, >> mscdisp2)
	    }

	    # Cursor loop.
	    dx = 0.; dy = 0.; nlog = 0 
	    while (fscan (imcur, wx, wy, nwcs, key, cmd) != EOF) {
		if (mod (nwcs, 100) == 0)
		    nwcs += 1

		# Initialize.
		cntr = NO
		markit = NO

		# Quit and/or change image.
		if (key == "n" || key == "p" || key == "q")
		    break

		# List keystrokes.
		if (key == "?") {
		    page ("mscsrc$msczero.key")
		    next
		}

		# Redisplay.
		if (key == "r") {
		    mscdisplay (im, frame=frame, extname=extname, check-)
		    if (access (markcoord))
			delete (markcoord, verify-)
		    next
		}

		# Toggle ID query.
		if (key == "i") {
		    idquery = (!idquery)
		    printf ("Coordinate ID query = %b\n", idquery)
		    next
		}

		# Mark coordinates.
		if (key == "m" || key == "u") {
		    if (access (markcoord))
			delete (markcoord, verify-)
		    if (access (sed) && access (crds)) {
			if (updcoord) {
			    printf ("updating %s ...\n", crds)
			    msczsed (sed, crds, temp)
			    delete (crds, verify-)
			    rename (temp, crds, field="all")
			}
		    }
		    if (access (sed))
			delete (sed, verify-)
		    if (key == "u") {
			cat = catalog
			crds = coords
			if (access (crds))
			    delete (crds, verify+)
			if (!access (crds)) {
			    mscgetcatalog (im, crds, magmin=0., magmax=mag,
				catalog=cat, rmin=0.)
			}
		    } else
			crds = coords
		    if (access (crds)) {
			rad = radii
			clr = color
			mscztvmark (crds, frame, mscdisp1, output=markcoord,
			    fields=fields, wcs=wcs, mark=mtype,
			    radii=rad, lengths="0", color=clr,
			    label=label, nxoffset=nxoffset,
			    nyoffset=nyoffset, pointsize=pointsize,
			    txsize=txsize)
		    } else
			printf ("WARNING: Coordinate list not found (%s)\n",
			    crds)

		    next
		}

		# Select marked object.
		if (key == "s" || key == "e") {
		    if (!access (markcoord)) {
			printf ("No coordinates marked\n")
			next
		    }

		    # Convert to detector coordinates if necessary.
		    if (wcsver == 1) {
			fd2 = mscdisp2
			while (fscan (fd2,wcsim1,c1,c2,l1,l2,im1,nc,nl,
			    nwcs1) != EOF) {
			    if (nwcs != nwcs1)
				next
			    cs = (c2 - c1) / (nc - 1)
			    ls = (l2 - l1) / (nl - 1)
			    wx = (wx - 1) * cs + c1
			    wy = (wy - 1) * ls + l1
			    break
			}
			fd2 = ""
		    }

		    distmin = 1E10
		    fd2 = markcoord
		    while (fscan (fd2,c1,l1,id1,rastr,decstr,lineno) != EOF) {
			dist = sqrt ((c1-wx)**2+(l1-wy)**2)
			if (dist < distmin) {
			    cmark = c1
			    lmark = l1
			    idmark = id1
			    ramark = rastr 
			    decmark = decstr
			    linemark = lineno
			    distmin = dist
			}
		    }
		    if (distmin < 1E10) {
			cmark = c1
			lmark = l1
			id1 = idmark
			ra = ramark
			dec = decmark
			printf ("Selected: %s %s %s\n",
			    ramark, decmark, idmark)
			first = no
		    } else
			id1 = ""

		    if (key == "s")
			next
		}

		# Set centroiding.
		if (key == "c" || key == "x" || key == "z")
		    cntr = YES

		# Select position for edit.
		if (key == "e") {
		    printf ("Select coordinate (c to centroid)\n")
		    if (fscan (imcur, wx, wy, nwcs, key, cmd) == EOF)
			error (1, "No cursor input")
		    if (mod (nwcs, 100) == 0)
			nwcs += 1
		    if (key == "c")
			cntr = YES
		    key = "e"
		}

		# Check log file.
		if (key == "w" || key == "x") {
		    if (fscan (logfile, logf) == 0)
			logf = "Coords." // im
		    if (logf == "default")
			logf = "Coords." // im
		    if (nlog == 0 && access (logf)) {
			printf ("Logfile exists - ")
			delete (logf, verify+)
		    }
		    nlog = nlog + 1
		}

		# Convert to individual extension coordinates.
		wx1 = -1
		wy1 = -1
		fd2 = mscdisp2
		while (fscan (fd2,wcsim1,c1,c2,l1,l2,im1,nc,nl,nwcs1) != EOF) {
		    if (wcsver == 1) {
			if (nwcs == nwcs1) {
			    wx1 = wx
			    wy1 = wy
			    break
			}
		    } else {
			if (wx<(c1-0.5)||wy<(l1-0.5)||wx>(c2+0.5)||wy>(l2+0.5))
			    next
			cs = (c2 - c1) / (nc - 1)
			ls = (l2 - l1) / (nl - 1)
			wx1 = (wx - c1) / cs + 1
			wy1 = (wy - l1) / ls + 1
			break
		    }
		}
		fd2 = ""

		if (wx1 < 0 || wy1 < 0) {
		    print ("Cursor position not in an image") 
		    next
		}

		# Centroid coordinate.
		if (cntr) {
		    imcntr (im1, wx1, wy1, cboxsize=cbox) |
			scan (junk,junk,wx1,junk,wy1)
		}

		# Convert to world coordinates.
		print (wx1, wy1) |
		mscctran ("STDIN", "STDOUT", wcsim1, "logical", "astrometry",
		    columns="1 2", units="", formats="",
		    min_sigdigit=9, verbose=no) | scan (ax, ay)
		print (ax, ay) |
		mscctran ("STDIN", "STDOUT", wcsim1, "astrometry", "world",
		    columns="1 2", units="", formats="", min_sigdigit=9,
		    verbose=no) | scan (wx, wy)

		# Print coordinate at cursor position with no centering.
		if (key == "\\040" || key == "c") {
		    printf (fmtboth//"\n", wx, wy)
		    if (key == "c")
			markit = mark

		# Write coordinate to logfile.
		} else if (key == "w" || key == "x") {
		    if (idquery) {
			id = id1
			id1 = id
		    }
		    printf ("Append to %s: "//fmtboth//" %s\n",
			logf, wx, wy, id1)
		    printf (fmtboth//" %s\n", wx, wy, id1, >> logf)
		    markit = mark

		# Edit coordinate in coordinate list.
		} else if (key == "e") {
		    if (id1 != "")
			printf ("%s: ", id1)
		    printf ("%s %s -> "//fmtboth//"\n", ramark, decmark, wx, wy)
		    printf ("%d,%ds/%s/"//fmtra//"/\n",
			linemark, linemark, ramark, wx, >> sed)
		    printf ("%d,%ds/%s/"//fmtdec//"/\n",
			linemark, linemark, decmark, wy, >> sed)
		    markit = mark

		# Adjust zero point.
		} else if (key == "z") {
		    printf (fmtboth//" %s\n", wx, wy, id1)
		    if (first) {
			printf (fmtra//"\n", wx) | scan (ra)
			printf (fmtdec//"\n", wy) | scan (dec)
			first = no
		    }
		    r = real (ra) * 15.
		    d = real (dec)
		    print (r, d) |
		    mscctran ("STDIN", "STDOUT", wcsim1, "world", "astrometry",
			columns="1 2", units="", formats="", min_sigdigit=9,
			verbose=no) | scan (wx1, wy1)
		    wx1 = wx1 - ax
		    wy1 = wy1 - ay
		    mscwcs (wcsim, ra_shift=wx1, dec_shift=wy1, ra_mag=1.,
			dec_mag=1., ra_rot=0., dec_rot=0., forward+)

		    # Cumulative shift.
		    dx = dx + wx1
		    dy = dy + wy1

		    wx = r
		    wy = d
		    markit = mark
		}

		# Mark on display.
		if (markit) {
		    if (id1 == "")
			idlabel = NO
		    else
			idlabel = YES
		    printf (fmtboth//" %s\n", wx, wy, id1) |
		    mscztvmark ("STDIN", frame, mscdisp1, output="",
			fields="1,2,3", wcs=wcs, mark=mtype,
			radii=rad, lengths="0", color=clr,
			label=idlabel, nxoffset=nxoffset,
			nyoffset=nyoffset, pointsize=pointsize,
			txsize=txsize)
		}

		# Cancel ID.
		id1 = ""
	    }

	    # Update if necessary.
	    if (dx != 0. && dy != 0.) {
		printf ("RA offset = %.2f arcsec", dx)
		printf (", DEC offset = %.2f arcsec\n", dy)
		if (update) {
		    printf ("updating %s ...\n", im)
		    mscwcs (im, ra_shift=dx, dec_shift=dy, ra_mag=1.,
			dec_mag=1., ra_rot=0., dec_rot=0., forward+)
		    printf ("done\n")
		}
		dx = 0.; dy = 0.
	    }
	    if (access (sed) && access (crds)) {
		if (updcoord) {
		    printf ("updating %s ...\n", crds)
		    msczsed (sed, crds, temp)
		    delete (crds, verify-)
		    rename (temp, crds, field="all")
		}
	    }

	    # Delete temporary files.
	    if (access (markcoord))
	        delete (markcoord, verify-)
	    if (access (sed))
		delete (sed, verify-)
	    imdelete (wcsim, verify-)
	    delete (mscdisp1, verify-)
	    delete (mscdisp2, verify-)

	    if (key == "q")
		break
	    if (key == "p") {
		if (nim == 1)
		    next
		fd1 = images
		for (len=1; len<nim-1; len+=1)
		    stat = fscan (fd1, im)
	    }
	}
	fd1 = ""; delete (images, verify-)
end
