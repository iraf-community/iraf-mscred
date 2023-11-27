procedure gaps (image)

string	image				{prompt="Image"}
file	results = "STDOUT"		{prompt="Results file"}
file	gaps = "msctools$gaps.dat"	{prompt="Gaps data file"}
real	pixscale = 0.259		{prompt="Pixel scale (arcsec/pixel)"}
real	mmscale = 0.015			{prompt="Physical scale (mm/pixel)"}
real	xrotation = 90.			{prompt="Rotation of ra axis from x"}
real	yrotation = 270.		{prompt="Rotation of dec axis from x"}
bool	verbose = yes			{prompt="Verbose?"}

struct	*fd

begin
	file	im, out
	string	name, ext1, ext2
	struct	time
	real	cx, sx, cy, sy
	real	x1, y1, x2, y2, ra1, dec1, ra2, dec2
	real	secgap, pixgap, mmgap

	im = image
	out = results
	cx = cos (xrotation * 3.14159 / 180.)
	sx = sin (xrotation * 3.14159 / 180.)
	cy = cos (yrotation * 3.14159 / 180.)
	sy = sin (yrotation * 3.14159 / 180.)

	if (verbose) {
	    name = envget ("userid")
	    time | scan (time)
	    printf ("Mosaic statistics prepared by %s on %s\n",
		name, time, >> out)
	    printf ("  Image = %s, pixel scale = %.4g, xrotation = %.4g, yrotation = %.4g\n",
		im, pixscale, xrotation, yrotation, >> out)
	}
	printf ("\n  Gaps:%44tarcsec  pixel     mm\n", >> out)

	fd = gaps
	while (fscan (fd, ext1, x1, y1, ext2, x2, y2) != EOF) {
	    print (x1, y1) |
	    mscctran ("STDIN", "STDOUT", im//"["//ext1//"]", yes,
		xcolumn=1, ycolumn=2, lngformat="", latformat="",
		min_sigdigit=7) | scan (ra1, dec1)
	    print (x2, y2) |
	    mscctran ("STDIN", "STDOUT", im//"["//ext2//"]", yes,
		xcolumn=1, ycolumn=2, lngformat="", latformat="",
		min_sigdigit=7) | scan (ra2, dec2)
	    ra1 = 3600. * 15. * (ra2-ra1) * cos ((dec1+dec2)*3.14159/360.)
	    dec1 = 3600. * (dec2-dec1)
	    ra2 = ra1 * cx + dec1 * sy
	    dec2 = -ra1 * sx + dec1 * cy
	    secgap = max (abs (ra2), abs (dec2))
	    pixgap = secgap / pixscale
	    mmgap = secgap / pixscale * mmscale
	    printf ("  %s[%d,%d] - %s[%d,%d]: %44t%6.1f %6.1f %6.2f\n",
		ext1, x1, y1, ext2, x2, y2, secgap, pixgap, mmgap, >> out)
	}
	fd = ""
end
