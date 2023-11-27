procedure mscxreg (input, output)

string	input		{prompt="Input data file"}
string	output		{prompt="Output data file"}

string	correlation = "discrete" {prompt="Cross-correlation function",
			enum="|discrete|fourier|difference|file|"}
int	window = 11	{prompt="Width of correlation window\n", min=3}

string	function = "centroid" {prompt="Correlation peak centering function",
			enum="|none|centroid|sawtooth|parabola|mark|"}
int	box = 5		{prompt="Box width for centering correlation peak\n",
			min=3}
real	xreject = 0.1	{prompt="Reject x shifts less than this value"}
real	yreject = 0.	{prompt="Reject y shifts less than this value\n"}

bool	verbose = no	{prompt="Verbose output?"}
bool	graph = no	{prompt="Graph correlations?"}
bool	display = no	{prompt="Display regions?"}
bool	interactive = no	{prompt="Interactive correlation fitting?"}
file	gcommands = "mscsrc$mscxreg.dat" {prompt="Graphics cursor commands"}
bool	accept = yes	{prompt="Accept shift?", mode="q"}

struct	*fd

begin
	file	temp
	string	in, ref, reg1, reg2, junk
	real	wx, wy, x1, y1, x2, y2, xlag, ylag, xshift, yshift
	bool	contour

	temp = mktemp ("tmp$iraf")

	if (verbose) {
	    printf ("MSCXREG:\n")
	    printf ("  correlation = %s, window = %d\n", correlation, window)
	    printf ("  function = %s, box = %d\n", function, box)
	}
	fd = input
	while (fscan (fd, wx, wy, in, x1, y1, reg1, ref, x2, y2, reg2) != EOF) {
	    if (nscan() != 10)
		next
	    contour = (interactive || graph)
	    if (display) {
		display (in//reg1, 1, zscale+, > "dev$null")
		display (ref//reg2, 2, zscale+, > "dev$null")
	    }
	    xlag = x1 - x2
	    ylag = y1 - y2
	    xreg (in, ref, reg2, temp, output="", databasefmt=no,
		append=yes, records="", coords="", xlag=xlag, ylag=ylag,
		dxlag=0, dylag=0, background="none", border=INDEF,
		loreject=INDEF, hireject=INDEF, apodize=0., filter="none",
		correlation=correlation, xwindow=window, ywindow=window,
		function=function, xcbox=box, ycbox=box,
		interactive=contour, verbose=no, gcommands=gcommands,
		> "dev$null")
	    type (temp) | scan (junk, xshift, yshift)
	    delete (temp, verify-)

	    if (abs (xlag) > 3. && abs (xshift) < xreject) {
		if (verbose) {
		    printf ("    Rejected because magnitude of x shift ")
		    printf ("is less than %g\n", xreject)
		}
		next
	    }
	    if (abs (ylag) > 3. && abs (yshift) < yreject) {
		if (verbose)
		    printf ("    Rejected because magnitude of y shift ")
		    printf ("is less than %g\n", yreject)
		next
	    }

	    xshift = xshift + xlag
	    yshift = yshift + ylag
	    x1 = x1 + xshift
	    y1 = y1 + yshift

	    if (verbose || interactive)
		printf ("  %s%s: x shift = %.2f, y shift = %.2f\n",
		    in, reg1, xshift, yshift)
	    if (interactive) {
		if (accept)
		    printf ("%s %g %g %g %g\n", in, wx, wy, x1, y1, >> output)
	    } else
		printf ("%s %g %g %g %g\n", in, wx, wy, x1, y1, >> output)
	}
	fd = ""
end
