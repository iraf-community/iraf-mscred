# MSCFINDGAIN - calculate the gain and readnoise given two flats and two
# bias frames.  Algorithm (method of Janesick) courtesy Phil Massey.
#
#	flatdif = flat1 - flat2
#	biasdif = bias1 - bias2
#
#	e_per_adu = ((mean(flat1)+mean(flat2)) - (mean(bias1)+mean(bias2))) /
#		    ((rms(flatdif))**2 - (rms(biasdif))**2)
#
#	readnoise = e_per_adu * rms(biasdif) / sqrt(2)
#
# In our implementation, `mean' may actually be any of `mean',
# `midpt', or `mode' as in the IMSTATISTICS task.

procedure mscfindgain (flat1, flat2, zero1, zero2)

string	flat1			{prompt="First flat frame"}
string	flat2			{prompt="Second flat frame"}
string	zero1			{prompt="First zero frame"}
string	zero2			{prompt="Second zero frame"}

string	extname = ""		{prompt="Select extension names"}
string	mask = "BPM"		{prompt="Bad pixel mask"}
string	section = ""		{prompt="Selected image section"}
string	center = "mean"		{prompt="Central statistical measure",
				    enum="mean|midpt|mode"}
int	nclip = 3		{prompt="Number of clipping iterations"}
real	lclip = 4		{prompt="Lower clipping sigma factor"}
real	uclip = 4		{prompt="Upper clipping sigma factor"}
real	binwidth = 0.1		{prompt="Bin width of histogram in sigma"}
bool	verbose = yes		{prompt="Verbose output?"}

string	*fd1, *fd2

begin
	bool	first, err
	file	f1, f2, z1, z2, lf1, lf2, lz1, lz2
	file	f1list, flatdiff, zerodiff, statsfile
	real	e_per_adu, readnoise, m_f1, m_f2, m_b1, m_b2, s_fd, s_bd, junk
	struct	images,ext

	# Temporary files.
	f1list = mktemp ("tmp$iraf")
	flatdif = mktemp ("tmp$iraf")
	zerodif = mktemp ("tmp$iraf")
	statsfile = mktemp ("tmp$iraf")

	# Query parameters.
	f1	= flat1
	f2	= flat2
	z1	= zero1
	z2	= zero2

	# Expand first flat as the reference.
	mscextensions (f1, output="file", index="", extname=extname, extver="",
	    lindex=no, lname=yes, lver=no, ikparams="", > f1list)

	first = YES

	# For each f1 get the extension for all the other images.
	fd1 = f1list
	while (fscan (fd1, f1) != EOF) {
	    hselect (f1, "extname", yes) | scan (ext)
	    ext = "[" // ext // "]"

	    lf1 = f1 // section
	    lf2 = f2 // ext // section
	    lz1 = z1 // ext // section
	    lz2 = z2 // ext // section

	    imarith (lf1, "-", lf2, flatdif)
	    imarith (lz1, "-", lz2, zerodif)

	    printf ("%s,%s,%s,%s,%s,%s\n",
		lf1, lf2, lz1, lz2, flatdif, zerodif) | scan (images)
	    ximstat (images, mask="^"//mask, fields=center//",stddev",
		lower=INDEF, upper=INDEF, nclip=nclip, lclip=lclip, uclip=uclip,
		binwidth=binwidth, format-, > statsfile)
	    imdelete (flatdif, verify-)
	    imdelete (zerodif, verify-)

	    fd2 = statsfile
	    err = NO
	    if (fscan (fd2, m_f1, junk) != 2) {
		printf ("WARNING: Failed to compute statisics for %s\n", lf1)
		err = YES
	    }
	    if (fscan (fd2, m_f2, junk) != 2) {
		printf ("WARNING: Failed to compute statisics for %s\n", lf2)
		err = YES
	    }
	    if (fscan (fd2, m_b1, junk) != 2) {
		printf ("WARNING: Failed to compute statisics for %s\n", lz1)
		err = YES
	    }
	    if (fscan (fd2, m_b2, junk) != 2) {
		printf ("WARNING: Failed to compute statisics for %s\n", lz1)
		err = YES
	    }
	    if (fscan (fd2, junk, s_fd) != 2) {
		printf ("WARNING: Failed to compute statisics for %s - %s\n",
		    lf1, lf2)
		err = YES
	    }
	    if (fscan (fd2, junk, s_bd) != 2) {
		printf ("WARNING: Failed to compute statisics for %s - %s\n",
		    lz1, lz2)
		err = YES
	    }
	    fd2 = ""; delete (statsfile, verify-)

	    if (err == YES)
		next

	    e_per_adu = ((m_f1 + m_f2) - (m_b1 + m_b2)) / (s_fd**2 - s_bd**2)
	    readnoise = e_per_adu * s_bd / sqrt(2)

	    # round to three decimal places
	    e_per_adu = real (nint (e_per_adu * 1000.)) / 1000.
	    readnoise = real (nint (readnoise * 1000.)) / 1000.

	    # print results
	    if (verbose) {
		if (first) {
		    printf ("MSCFINDGAIN:\n")
		    printf ("  mask = %s, center = %s, binwidth = %g\n",
			mask, center, binwidth)
		    printf ("  nclip = %d, lclip = %g, uclip = %g\n",
			nclip, lclip, uclip)
		    first = NO
		}
		printf ("\n  Flats      = %s  &  %s\n", lf1, lf2)
		printf ("  Zeros      = %s  &  %s\n", lz1, lz2)
		printf ("  Gain       = %5.2f electrons per ADU\n", e_per_adu)
		printf ("  Read noise = %5.2f electrons\n", readnoise)
	    } else
		printf ("%s\t%5.2f\t%5.2f\n", ext, e_per_adu, readnoise)

	}
	fd1 = ""; delete (f1list, verify-)
end
