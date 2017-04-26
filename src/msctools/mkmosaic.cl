procedure mkmosaic (mosaic, template)

string	mosaic			{prompt="Mosaic filename"}
file	template		{prompt="Template image"}
int	xsize = 100		{prompt="CCD size"}
int	ysize = 200		{prompt="CCD size"}
int	nxccd = 4		{prompt="Number of CCDs in X"}
int	nyccd = 2		{prompt="Number of CCDs in Y"}
int	namp = 1		{prompt="Number of amps per CCD"}
int	gap = 5			{prompt="Gap width"}
int	noverscan = 32		{prompt="Overscan columns (-=left, +=right)"}
int	xdither = 0		{prompt="X dither"}
int	ydither = 0		{prompt="Y dither"}

int	seed = 1		{prompt="Random number seed"}
real	bias = 500.		{prompt="Bias levels"}

bool	verbose = yes		{prompt="Verbose?"}

begin
	file	mim, tim, im
	int	i, j, k, l
	int	xs, ys, xoffset, yoffset, xccdoffset, yccdoffset, noscan
	real	rand1, rand2

	mim = mosaic
	tim = template
	xs = xsize / namp
	ys = ysize
	xccdoffset = 0
	yccdoffset = 0

	k = 0
	for (j=1; j<=nyccd; j+=1) {
	    xoffset = -(xs + gap)
	    yoffset = (j - 1) * (ys + gap) 
	    for (i=1; i<=namp*nxccd; i+=1) {
		k = k + 1
		l = (k + 1) / namp
		printf ("%s[im%d,append]\n", mim, k) | scan (im)
	
		if (namp == 1) {
		    xoffset = xoffset + xs + gap
		    noscan = noverscan * (2 * mod (j, 2) - 1)
		} else if (mod(i,namp) == 1) {
		    xoffset = xoffset + xs + gap
		    xccdoffset = 0
		    noscan = noverscan
		} else {
		    xoffset = xoffset + xs
		    xccdoffset = xs
		    noscan = -noverscan
		}

		urand (k, 2, ndigits=4, scale=2.0, seed=seed) |
		    fields ("STDIN", "1-2", lines=k) | scan (rand1, rand2)
		rand1 = bias * (1 + 0.2 * (rand1 - 1.))
		rand2 = 1 + 0.2 * (rand2 - 1.)
		mkccd (im, tim, xsize=xs, ysize=ys,
		    xoffset=xoffset, yoffset=yoffset,
		    xccdoffset=xccdoffset, yccdoffset=yccdoffset,
		    xdither=xdither, ydither=ydither,
		    noverscan=noscan, bias=rand1, flat=rand2,
		    imageid=k, ccdname="ccd"//l, ampname="amp"//k,
		    verbose=verbose)
		flpr
	    }
	}
end
