# PIXCTRAN -- Convert world coordinates to pixel coordinates.
# The input is a list of world coordinates and the output is a list
# of pixel coordinates limited to the image region.

procedure pixctran (input, output, image)

file	input		{prompt="Input coordinate file"}
file	output		{prompt="Output coordinate file"}
file	image		{prompt="Input image"}
bool	wcssol = yes	{prompt="Use WCS plate solution?"}

struct	*fd

begin
	file	in, out, im, temp
	real	nc, nl, xpos, ypos

	in = input
	out = output
	im = image
	temp = mktemp ("tmp$iraf")

	mscctran (in, temp, im, no, xcolumn=1, ycolumn=2, min_sigdigit=9,
	    wcssol=wcssol)

	hselect (im, "naxis1,naxis2", yes) | scan (nc, nl)

	fd = temp
	while (fscan (fd, xpos, ypos) != EOF) {
	    if (xpos < 0.5 || xpos > nc+0.5 || ypos < 0.5 || ypos > nl+0.5)
		next
	    print (xpos, ypos, >> out)
	}

	delete (temp, verify-)
end
