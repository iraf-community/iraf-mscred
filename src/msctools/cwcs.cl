# CWCS -- Modify coordinates with a shift, scale, and rotation.

procedure cwcs (input, output, image, rashift, decshift, scale, rotation)

file	input		{prompt="Input coordinates"}
file	output		{prompt="Output coordinates"}
file	image		{prompt="Image with WCS"}
real	rashift = 0.	{prompt="RA shift (arcsec)"}
real	decshift = 0.	{prompt="DEC shift (arcsec)"}
real	scale = 1.	{prompt="Scale"}
real	rotation = 0.	{prompt="Rotation (deg)"}

struct	*fd

begin
	file	in, out, im, temp
	real	ras, decs, scl, rot, cost, sint, x1, x2, y1, y2
	struct	str

	temp = mktemp ("tmp$iraf")

	# Get query parameters.
	in = input
	out = output
	im = image
	ras = rashift
	decs = decshift
	scl = scale
	rot = rotation

	cost = scl * cos (rot * 3.14159 / 180.)
	sint = scl * sin (rot * 3.14159 / 180.)

	mscctran (in, temp, im, "world", "astrometry", columns="1 2",
	    units="hours native", formats="", min_sigdigit=9, verbose=no)

	fd = temp
	while (fscan (fd, x1, y1, str) != EOF) {
	    x2 = x1 * cost + y1 * sint - ras
	    y2 = -x1 * sint + y1 * cost - decs
	    printf ("%g %g %s\n", x2, y2, str, >> out)
	}
	fd = ""; delete (temp, verify-)

	rename (out, temp, field="all")
	mscctran (temp, out, im, "astrometry", "world", columns="1 2",
	    units="native native", formats="%.2H %.h", min_sigdigit=9,
	    verbose=no)

	delete (temp, verify-)
end
