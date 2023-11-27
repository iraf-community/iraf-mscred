procedure tastrom (table, output)

string	table			{prompt="Input table name"}
string	output			{prompt="Output root name"}
pset	catpars	= ""		{prompt="Catalog description pset\n"}

real	epoch = 2000.		{prompt="Report epoch\n"}

real	ra_tan = INDEF		{prompt="RA of the tangent point"}
real	dec_tan = INDEF		{prompt="Dec of the tangent point"}
real	tepoch = INDEF		{prompt="Epoch for the tangent coordinates\n"}

string	*list

begin
	string	ltable, loutput, tmp1, tmp2, buf
	real	cepoch, ltepoch
	int	ra_hrs, ra_min, dec_deg, dec_min, dec_sec, dec_sign
	real	ra_sec

	tmp1 = mktemp ("tmp$tmp")
	tmp2 = mktemp ("tmp$tmp")

	ltable = table
	loutput = output

	print (epoch, >> tmp1)
	print ("ASTR", >> tmp1)

	ltepoch = tepoch	# to avoid generating two prompts

	ra_hrs = int (ra_tan)
	ra_min = int (60. * (ra_tan - ra_hrs))
	ra_sec = 60. * (60. * (ra_tan - ra_hrs) - ra_min)

	if (dec_tan < 0.) {
	    dec_tan = - dec_tan
	    dec_sign = -1
	} else {
	    dec_sign = 1
	}

	dec_deg = int (dec_tan)
	dec_min = int (60. * (dec_tan - dec_deg))
	dec_sec = int (60. * (60. * (dec_tan - dec_deg) - dec_min))

	if (dec_sign == -1)
	    dec_deg = - dec_deg

	printf ("%2d %02d %04.1f", ra_hrs, ra_min, ra_sec, >> tmp1)
	printf ("  %3d %02d %02d", dec_deg, dec_min, dec_sec, >> tmp1)
	printf ("  %8.4f %8.4f\n", ltepoch, ltepoch, >> tmp1)

	buf =	catpars.sub_col // " == 1 && " //
		catpars.cen_col // " == 1 && " //
		catpars.obj_col // " == 0"

	tselect (ltable, tmp2, buf)

	tcalc (tmp2, "RA_HRS", "RA_DEG / 15.", datatype="real",
	    colunits="", colfmt="%13.3h")

	tchcol (tmp2, "DEC_DEG", "", "%12.2h@", "", verbose=no)
#	tchcol (tmp2, "REGION", "", "%5d\n", "", verbose=no)
	tchcol (tmp2, "X_CENTER", "", "%8.2f", "", verbose=no)
	tchcol (tmp2, "Y_CENTER", "", "%8.2f #", "", verbose=no)
	tchcol (tmp2, "GSC_ID", "", "%5d\n", "", verbose=no)

#	tprint (tmp2, columns="ra_hrs,dec_deg,region,x_center,y_center,gsc_id",
#	    prparam-, prdata+, pwidth=80, plength=0, showrow-, showhdr-,
#	    rows="-", option="plain", sp_col="", lgroup=0, >> tmp1)

	tprint (tmp2, columns="ra_hrs,dec_deg,gsc_id,x_center,y_center,region",
	    prparam-, prdata+, pwidth=80, plength=0, showrow-, showhdr-,
	    rows="-", option="plain", sp_col="", lgroup=0, >> tmp1)

	tdelete (tmp2, ver-, >& "dev$null")

	print ("*", >> tmp1)
	print ("0.0 0.0 * JUNK", >> tmp1)

	buf =	catpars.sub_col // " == 1 && " //
		catpars.cen_col // " == 1 && " //
		catpars.obj_col // " == 1"

	tselect (ltable, tmp2, buf)

	tchcol (tmp2, "Y_CENTER", "", "%8.2f *", "", verbose=no)
	tchcol (tmp2, "GSC_ID", "", "%5d", "", verbose=no)

	tprint (tmp2, columns="x_center,y_center,gsc_id",
	    prparam-, prdata+, pwidth=80, plength=0, showrow-, showhdr-,
	    rows="-", option="plain", sp_col="", lgroup=0, >> tmp1)

	tdelete (tmp2, ver-, >& "dev$null")

	cepoch = catpars.cat_epoch

	print ("s/:/\ /g", > tmp2)
	print ("s/@/\ 0.0\ 0.0\ ", cepoch, "\ *\ /", >> tmp2)
	print ("s/#/\ *\ /", >> tmp2)

	print ("!sed -f ", osfn(tmp2), " ", osfn(tmp1),
	    " > ", loutput, ".in") | cl

	delete (tmp2, ver-, >& "dev$null")
	delete (tmp1, ver-, >& "dev$null")

	astrom (osfn(loutput//".in"), > loutput // ".out")

	rename ("astrom.lis", loutput // ".ast")
end
