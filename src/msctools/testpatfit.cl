procedure testpatfit ()

string	weights = '"" weights'
string	bkgs = '"" 0 500 sky'
string	bkgpats = '"" 0 0.1 zero'
string	bkgwts = '"" 0 0.1 zero'
string	masks = 'objmask ""'
string	patmasks = 'patmask ""'
string	outtypes = 'none fit diff pfit pdiff sfit sdiff'
file	logfile = ""
bool	verbose = yes

begin
	file	im, tmp
	string	outtype, bkg, bkgpat, bkgwt, weight, mask, patmask
	struct	aouttypes, abkgs, abkgpats, abkgwts, aweights, amasks, apatmasks

	tmp = mktemp ("tmp")

	# Make data.

	im = "galfield"
	if (!imaccess(im)) {
	    gallist (tmp, 1000, interactive=no, spatial="uniform", xmin=1.,
		xmax=512., ymin=1., ymax=512., xcenter=INDEF,
		ycenter=INDEF, core_radius=50., base=0., sseed=1,
		luminosity="powlaw", minmag=-7., maxmag=0.,
		mzero=15., power=0.45, alpha=-1.24, mstar=-21.41,
		lseed=1, egalmix=0.4, ar=0.7, eradius=10., sradius=1.,
		absorption=1.2, z=0.05, sfile="", nssample=100, sorder=10,
		lfile="", nlsample=100, lorder=10, rbinsize=10.,
		mbinsize=0.5, dbinsize=0.5, ebinsize=0.1, pbinsize=20.,
		graphics="stdgraph", cursor="")

	    starlist (tmp, 100, "", "", interactive=no, spatial="uniform",
		xmin=1., xmax=512., ymin=1., ymax=512., xcenter=INDEF,
		ycenter=INDEF, core_radius=30., base=0., sseed=2,
		luminosity="powlaw", minmag=-7., maxmag=0., mzero=-4.,
		power=0.6, alpha=0.74, beta=0.04, delta=0.294, mstar=1.28,
		lseed=2, nssample=100, sorder=10, nlsample=100, lorder=10,
		rbinsize=10., mbinsize=0.5, graphics="stdgraph", cursor="")

	    mkobjects (im, output="", ncols=512, nlines=512,
		title="Example artificial galaxy field",
		header="", background=0.,
		objects=tmp, xoffset=0., yoffset=0., star="moffat",
		radius=1.0, beta=2.5, ar=1., distance=1.,
		exptime=1., magzero=5.5, gain=5., rdnoise=0., poisson=no,
		seed=3, comments=no)

	    delete (tmp, verify=no)
	}

	im = "objmask"
	if (!imaccess(im))
	    imcopy ("galfield", im//".pl", verbose-)

	im = "sky"
	if (!imaccess(im))
	    mkpattern (im, output="", pattern="slope", option="replace",
		v1=490., v2=510., size=1, title="Sky", pixtype="real",
		ndim=2, ncols=512, nlines=512, n3=1, n4=1, n5=1, n6=1,
		n7=1, header="")

	im = "zero"
	if (!imaccess(im))
	    mkpattern (im, output="", pattern="constant", option="replace",
		v1=0., v2=1., size=1, title="", pixtype="real",
		ndim=2, ncols=512, nlines=512, n3=1, n4=1, n5=1, n6=1,
		n7=1, header="")

	im = "pattern"
	if (!imaccess(im)) {
	    if (imaccess("patmask"))
	        imdelete ("patmask", verify-)
	    imexpr ("sqrt((I)**2+(J)**2)<200?0 : 1", "patmask.pl",
	        dims="512,512", outtype="int", verbose-)
	    mkpattern (tmp, output="", pattern="square", option="replace",
		v1=1., v2=3., size=1, title="Sky", pixtype="real",
		ndim=2, ncols=512, nlines=512, n3=1, n4=1, n5=1, n6=1,
		n7=1, header="")
	    imexpr ("b==0?a : 0", im, tmp, "patmask", verbose-)
	    imdelete (tmp, verify-)
	}

	im = "weights"
	if (!imaccess(im))
	    imcopy ("pattern", im, verbose-)

	im = "noise"
	if (!imaccess(im))
	    mknoise (im, output="", title="", ncols=512, nlines=512,
	        header="", background=0., gain=1., rdnoise=10., poisson=no,
		seed=1, cosrays="", ncosrays=0, energy=30000., radius=0.5,
		ar=1., comments=yes)

	im = "test"
	if (!imaccess(im))
	    imexpr ("a+b+c+2*d", im, "galfield", "sky", "noise",
	        "pattern", verbose-)

	# Test various modes.
	if (access("logfile"))
	    delete ("logfile", verify-)
	abkgwts = bkgwts
	while (fscan (abkgwts, bkgwt, abkgwts) > 0) {
	abkgs = bkgs
	while (fscan (abkgs, bkg, abkgs) > 0) {
	abkgpats = bkgpats
	while (fscan (abkgpats, bkgpat, abkgpats) > 0) {
	aweights = weights
	while (fscan (aweights, weight, aweights) > 0) {
	apatmasks = patmasks
	while (fscan (apatmasks, patmask, apatmasks) > 0) {
	amasks = masks
	while (fscan (amasks, mask, amasks) > 0) {
	aouttypes = outtypes
	while (fscan (aouttypes, outtype, aouttypes) > 0) {
	    im = "testout"
	    if (imaccess(im))
		imdelete (im, verify-)
	    patfit ("test", im, "pattern", weight, background=bkg,
		bkgpattern=bkgpat, bkgweight=bkgwt, masks=mask,
		patmasks=patmask, extfit="", extout="", outtype=outtype,
		logfile=logfile, verbose=verbose)
	}
	}
	}
	}
	}
	}
	}
end
