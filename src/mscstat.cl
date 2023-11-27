# MSCSTAT -- Image statistcs on multiextension Mosaic files.

procedure mcsstat (images)

string	images		{prompt="Images"}
string	extname = ""	{prompt="Extension name selection\n"}

bool	usemask = no	{prompt="Use mask in BPM keyword?"}
bool	gmode = no	{prompt="Global mode statistics?"}
string	fields = "image,npix,mean,stddev,min,max" {prompt="Fields to be printed"}
real	lower = INDEF	{prompt="Lower cutoff for pixel values"}
real	upper = INDEF	{prompt="Upper cutoff for pixel values"}
int	nclip = 0	{prompt="Number of clipping iterations"}
real	lsigma = 3.	{prompt="Lower clipping factor in sigma"}
real	usigma = 3.	{prompt="Upper clipping factor in sigma"}
real	binwidth = 0.1	{prompt="Bin width of histogram in sigma"}
bool	format = yes	{prompt="Format output and print column labels?\n"}

struct	*fd1, *fd2

begin
	int	i, nmode
	real	bmode[20], cmode
	file	image, mask, extn
	file	list1, list2
	bool	fmt

	list1 = mktemp ("tmp$iraf")
	list2 = mktemp ("tmp$iraf")

	sections (images, option="fullname", > list1)
	fd1 = list1

	if (gmode) {
	    while (fscan (fd1, image) != EOF) {
		printf ("%s ", image)

		mscextensions (image, output="file", index="0-",
		    extname=extname, extver="", lindex=no, lname=yes,
		    lver=no, ikparams="", >> list2)

		cmode = 0.0
		nmode = 0
		fd2 = list2
		while (fscan (fd2, extn) != EOF) {
		    nmode = nmode+1
		    if (usemask)
		        hselect (extn, "BPM", yes) | scan (mask)
		    else
		        mask = ""
		    mimstat (extn, imasks=mask, omasks="", fields="mode",
		        lower=lower, upper=upper, nclip=nclip, lsigma=lsigma,
			usigma=usigma, binwidth=binwidth, format=no) |
			scan(bmode[nmode])
		    cmode = cmode + bmode[nmode]
		}
		fd2 = ""; delete (list2, verify-)

		cmode = cmode / nmode
		printf ("%10.3f", cmode)
		for (i=1; i<=nmode; i+=1)
		    printf (" %6.3f", bmode[i]/cmode)
		printf ("\n")
	    }

	} else {
	    fmt = format
	    while (fscan (fd1, image) != EOF) {
		mscextensions (image, output="file", index="0-",
		    extname=extname, extver="", lindex=no, lname=yes,
		    lver=no, ikparams="", >> list2)

		fd2 = list2
		while (fscan (fd2, extn) != EOF) {
		    if (usemask)
		        hselect (extn, "BPM", yes) | scan (mask)
		    else
		        mask = ""
		    mimstat (extn, imasks=mask, omasks="", fields=fields,
		        lower=lower, upper=upper, nclip=nclip, lsigma=lsigma,
			usigma=usigma, binwidth=binwidth, format=fmt)
		    fmt = no
		}
		fd2 = ""; delete (list2, verify-)
	    }
	}

	fd1 = ""; delete (list1, verify-)
end
