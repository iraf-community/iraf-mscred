procedure stats (image, wcs)

string	image				{prompt="Image"}
file	wcs = "ccddb$mosaic/wcs.dat"	{prompt="WCS database"}
file	results = "STDOUT"		{prompt="Output statistics file"}
bool	xystat = no			{prompt="Separate X and Y statistics?"}
file	gaps = "msctools$gaps.dat"	{prompt="Gap data file"}
file	offsets = "msctools$offsets.dat"	{prompt="Offsets data file"}
real	mmscale = 0.015			{prompt="Pixel scale (mm/pixel)"}
bool	verbose = yes			{prompt="Verbose?"}

struct	*fd1, *fd2

begin
	string	name
	struct	time
	file	im, in, out, temp1, temp2
	int	n
	real	val, pixscale, rot, xrot, yrot
	real	mean, sigma, meanx, sigmax, meany, sigmay

	im = image
	in = wcs
	out = results
	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")

	if (verbose) {
	    name = envget ("userid")
	    time | scan (time)
	    printf ("Mosaic statistics prepared by %s on %s\n",
		name, time, >> out)
	    printf ("  WCS database = %s, Image = %s\n", in, im, >> out)
	}
	printf ("\n  WCS statistics:\n", >> out)

	match ("mag", in, stop-, print-, meta-) |
	fields ("STDIN", "2", lines="1-9999", quit-, print-) |
	average ("new_sample") | scan (pixscale, sigma)
	printf ("  Pixel scale (arcsec/pixel):%34t%6.4f (%6.4f)",
	    pixscale, sigma, >> out)
	if (xystat) {
	    match ("xmag", in, stop-, print-, meta-) |
	    fields ("STDIN", "2", lines="1-9999", quit-, print-) |
	    average ("new_sample") | scan (meanx, sigmax)
	    match ("ymag", in, stop-, print-, meta-) |
	    fields ("STDIN", "2", lines="1-9999", quit-, print-) |
	    average ("new_sample") | scan (meany, sigmay)
	    printf (" %6.4f (%6.4f) %6.4f (%6.4f)",
		meanx, sigmax, meany, sigmay, >> out)
	}
	printf ("\n", >> out)

	match ("xrotation", in, stop-, print-, meta-) |
	fields ("STDIN", "2", lines="1-9999", quit-, print-, > temp1)
	xrot = 0
	n = 0
	fd1 = temp1
	while (fscan (fd1, val) != EOF) {
	    if (n == 0)
		rot = val
	    else if (val - rot < -180.)
		val = val + 360.
	    else if (val - rot > 180.)
		val = val - 360.
	    xrot = xrot + val
	    n = n + 1
	}
	fd1 = ""; delete (temp1, verify-)
	xrot = xrot / n

	match ("yrotation", in, stop-, print-, meta-) |
	fields ("STDIN", "2", lines="1-9999", quit-, print-, > temp1)
	yrot = 0
	n = 0
	fd1 = temp1
	while (fscan (fd1, val) != EOF) {
	    if (n == 0)
		rot = val
	    else if (val - rot < -180.)
		val = val + 360.
	    else if (val - rot > 180.)
		val = val - 360.
	    yrot = yrot + val
	    n = n + 1
	}
	fd1 = ""; delete (temp1, verify-)
	yrot = yrot / n

	rot = xrot
	while (rot - yrot < -90.)
	    rot = rot + 180.
	while (rot - yrot > 90.)
	    rot = rot - 180.
	rot = (rot + yrot) / 2.

	printf ("  Field rotation (degrees):%34t%6.2f", rot, >> out)
	if (xystat)
	    printf (" %6.2f %6.2f", xrot, yrot, >> out)
	printf ("\n", >> out)

	match ("begin", in, stop-, print-, meta-) |
	fields ("STDIN", "2", lines="1-9999", quit-, print-, > temp1)
	match ("rotation", in, stop-, print-, meta-) |
	fields ("STDIN", "2", lines="1-9999", quit-, print-, > temp2)

	fd1 = temp1
	fd2 = temp2
	while (fscan (fd1, name) != EOF) {
	    n = fscan (fd2, meanx)
	    n = fscan (fd2, meany)
	    meanx = meanx - rot
	    meany = meany - rot
	    while (meanx < -90.)
		meanx = meanx + 180.
	    while (meanx > 90.)
		meanx = meanx - 180.
	    while (meany < -90.)
		meany = meany + 180.
	    while (meany > 90.)
		meany = meany - 180.
	    mean = (meanx + meany) / 2.
	    if (xystat)
		printf ("  CCD rotation %s (degrees):%34t%6.2f %6.2f %6.2f\n",
		    name, mean, meanx, meany, >> out)
	    else
		printf ("  CCD rotation %s (degrees):%34t%6.2f\n",
		    name, mean, >> out)
	}
	fd1 = ""; fd2 = ""; delete (temp1//","//temp2, verify-)

	gaps (im, results=out, gaps=gaps, pixscale=pixscale, mmscale=mmscale,
	    xrotation=xrot, yrotation=yrot, verbose=no)

	offsets (im, results=out, offsets=offsets, pixscale=pixscale,
	    xrotation=xrot, yrotation=yrot, verbose=no)

	type ("msctools$stats.dat", >> out)
end
