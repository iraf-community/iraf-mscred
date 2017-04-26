# MSCAGETCAT -- Interface to ASTCAT.AGETCAT.

procedure mscagetcat (output, catalog, ra, dec, rawidth, decwidth)

file	output			{prompt="Output list"}
string	catalog			{prompt="Catalog"}
string	ra			{prompt="RA[J2000] (hours)"}
string	dec			{prompt="DEC[J2000] (degrees)"}
real	rawidth			{prompt="RA width (minutes)"}
real	decwidth		{prompt="DEC width (minutes)"}
string	fields = "ra,dec,mag1,mag2,mag3" {prompt="Catalog fields"}
file	catdb = "mscsrc$catdb.dat"	{prompt="Catalog database"}

begin
	agetcat ("pars", output, catalogs=catalog, standard=no,
	    update=no, verbose=no, catdb=catdb,
	    aregpars="", rcra=ra, rcdec=dec, rrawidth=rawidth,
	    rdecwidth=decwidth, rcsystem="J2000", rcraunits="hours",
	    rcdecunits="degrees", filter=yes, afiltpars="", fsort="",
	    freverse=no, fexpr="yes", fields=fields, fnames="",
	    fntypes="", fnunits="", fnformats="", fosystem="J2000",
	    fira="ra", fidec="dec", foraunits="hours",
	    fodecunits="degrees", foraformat="%.3h", fodecformat="%.2h",
	    fixp="xp", fiyp="yp", fixc="xc", fiyc="yc", foxformat="%10.3f",
	    foyformat="%10.3f")
end
