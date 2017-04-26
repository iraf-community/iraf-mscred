procedure mscdither

begin
	int	i, j
	string	in, out, outlist, temp

	in = input
	out = output
	temp = mktemp ("tmp$iraf")
	outlist = mktemp ("tmp$iraf")

	i = stridx (".", out)
	j = strlen (out)
	if (i > 0 && j > 3) {
	    if (substr (out, i, j) == ".fits")
		out = substr (out, 1, i-1)
	    else if (substr (out, i, j) == ".fit")
		out = substr (out, 1, i-1)
	}
	sections (in, option="fullname", > temp)
	i = 0
	fd = temp
	while (fscan (fd) != EOF) {
	    i = i + 1
	    printf ("%s_%02d\n", out, i, >> outlist)
	}
	fd = ""; delete (temp, verify-)

	msccoordfit (in, coords, database="mscwcs.dat", nfit=nfit, rms=rms,
	    maxshift=maxshift, fitgeometry=fittype, update=yes,
	    interactive=no, verbose=yes)

	mscimage (in, "@"//outlist, reference="", pixmask=pixmask,
	    wcssol=wcssol, interactive=no, nx=nx, ny=ny,
	    fitgeometry=fitgeometry, function=function, xxorder=xxorder,
	    xyorder=xyorder, xxterms=xxterms, yxorder=yxorder,
	    yyorder=yyorder, yxterms=yxterms, verbose=verbose,
	    interpolant="linear",nxblock=nxblock,nyblock=nyblock,
	    fluxconserve=fluxconserve, ntrim=ntrim)

	mscstack ("@"//outlist, out, plfile="", combine=combine, reject="none",
	    masktype="goodvalue", maskvalue=0, scale=scale, zero=zero,
	    weight="none", statsec=statsec, lthreshold=lthreshold,
	    hthreshold=hthreshold)

	delete (outlist, verify-)
end
