procedure offsets (image)

string	image				{prompt="Image"}
file	results = "STDOUT"		{prompt="Results file"}
file	offsets = "msctools$offsets.dat"	{prompt="Offsets data file"}
real	pixscale = 0.259		{prompt="Pixel scale (arcsec/pixel)"}
real	xrotation = 90.			{prompt="Rotation of ra axis from x"}
real	yrotation = 270.		{prompt="Rotation of dec axis from x"}
bool	verbose = yes			{prompt="Verbose?"}

struct	*fd

begin
	file	im, out, temp
	string	name, ext1, ext2
	struct	time
	int	nc, nl, c1, c2, l1, l2
	real	cx, sx, cy, sy
	real	x1, y1, x2, y2, dx, dy, ra1, dec1, ra2, dec2

	im = image
	out = results
	temp = mktemp ("tmp$iraf")
	cx = cos (xrotation * 3.14159 / 180.)
	sx = sin (xrotation * 3.14159 / 180.)
	cy = cos (yrotation * 3.14159 / 180.)
	sy = sin (yrotation * 3.14159 / 180.)

	if (verbose) {
	    name = envget ("userid")
	    time | scan (time)
	    printf ("Mosaic statistics prepared by %s on %s\n", name, time, >> out)
	    printf ("  Image = %s, pixel scale = %.4g, xrotation = %.4g, yrotation = %.4g\n",
		im, pixscale, xrotation, yrotation, >> out)
	}
	printf ("\n  Offsets:%44t pixel  pixel detsec\n", >> out)

	dx = 100000
	dy = 100000
	fd = offsets
	while (fscan (fd, ext1, x1, y1, ext2, x2, y2, nc, nl) != EOF) {
	    print (x1, y1) |
	    mscctran ("STDIN", "STDOUT", im//"["//ext1//"]", yes,
		xcolumn=1, ycolumn=2, lngformat="", latformat="",
		min_sigdigit=7) | scan (ra1, dec1)
	    print (x2, y2) |
	    mscctran ("STDIN", "STDOUT", im//"["//ext2//"]", yes,
		xcolumn=1, ycolumn=2, lngformat="", latformat="",
		min_sigdigit=7) | scan (ra2, dec2)
	    ra1 = 3600. * 15. * (ra1-ra2) * cos ((dec1+dec2)*3.14159/360.)
	    dec1 = 3600. * (dec1-dec2)
	    ra2 = (ra1 * cx + dec1 * sy) / pixscale
	    dec2 = (-ra1 * sx + dec1 * cy) / pixscale
	    print (ext1, " ", x1, y1, ext2, " ", x2, y2, nc, nl,
		ra2, dec2, >> temp)
	    dx = min (dx, ra2)
	    dy = min (dy, dec2)
	}
	
	fd = temp
	while (fscan (fd, ext1, x1, y1, ext2, x2, y2, nc, nl, ra2, dec2)!=EOF) {
	    ra2 = abs (ra2 - dx)
	    dec2 = abs (dec2 - dy)
	    c1 = 1 + nint (ra2)
	    c2 = c1 + nc - 1
	    l1 = 1 + nint (dec2)
	    l2 = l1 + nl - 1
	    printf ("  %s[%d,%d] - %s[%d,%d]: %44t%6.1f %6.1f [%d:%d,%d:%d]\n",
		ext1, x1, y1, ext2, x2, y2, ra2, dec2, c1, c2, l1, l2, >> out)
	}
	fd = ""; delete (temp, verify-)
end
