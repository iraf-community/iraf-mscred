# MKBPM -- Make bad pixel masks.

procedure mkbpm (input, output, template)

file	input			{prompt="Input mosaic flat field ratio"}
file	output			{prompt="Output bad pixel mask"}
file	template		{prompt="Template raw mosaic exposure"}

bool	display			{prompt="Display result?", mode="q"}

struct	*fd

begin
	file	in, bpm, tmplt, extlist, tempa, tempb, tempc, tempd
	string	extname, trimsec
	int	naxis1, naxis2

	# Temporary files.
	tempa = mktemp ("temp")
	tempb = mktemp ("temp")
	tempc = mktemp ("temp")
	tempd = mktemp ("temp") // ".pl"

	# Query parameters.
	flat = input
	out = output

	# Create output file (really a directory of pl files)
	mkdir (out)

	# Expand template.
	imextensions (template, output="file", index="1-", extname="",
	    extver="", lindex+, lname-, lver-, ikparams="", > extlist)
	fd = extlist
	while (fscan (fd, tmplt) != EOF) {
	    hselect (tmplt, "extname,naxis1,naxis2,trimsec", yes) |
		scan (extname, naxis1, naxis2, trimsec)

	    in = flat // "[" // extname // "]"
	    bpm= out // "/bpm_" // extname // ".pl"
	    printf ("mkbpm: %s -> %s\n", in, bpm)

	    imsurfit (in, tempb, 2, 2)
	    imexpr ("abs(a)", tempa, tempb, verbose-)
	    imdelete (tempb, verify-)
	    boxcar (tempa, tempb, 3, 2)
	    boxcar (tempa, tempc, 1, 9)
	    imexpr ("(abs(a)>1 || abs(b)>1 || abs(c)>0.5) ? 1 : 0", tempd,
		tempa, tempb, tempc, verbose-)
	    imdelete (tempa, verify-)
	    imdelete (tempb, verify-)
	    imdelete (tempc, verify-)

	    hselect (tmplt, "naxis1,naxis2,trimsec", yes) |
		scan (naxis1, naxis2, trimsec)
	    mkpattern (tempa, output="", pattern="constant", option="replace",
		v1=0., v2=1., size=1, title="", pixtype="short", ndim=2,
		ncols=naxis1, nlines=naxis2, header=tmplt)
	    imcopy (tempd, tempa//trimsec, verbose=no)
	    imdelete (tempd, verify=no)
	    imcopy (tempa, bpm, verbose=no)
	    imdelete (tempa, verify=no)
	    hedit (bpm, "obstype", "mask", verify-, show-, update+)

	    if (display) {
		display (in, 1)
		display (in, 2, bpdisplay="overlay", overlay=bpm)
		printf ("Entering IMEXAM (quit with 'q')...\n") 
		imexamine
	    }
	}
	fd = ""; delete (extlist, verify=no)
end
