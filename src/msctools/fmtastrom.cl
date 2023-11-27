# FMTASTROM -- Format data for KTM from WCS of astrometry images.

procedure fmtastrom (input)

string	input			{prompt="List of mosaic exposures"}
string	field			{prompt="Astrometry field"}
string	author			{prompt="Author of astrometry solution"}
string	date			{prompt="Date of solution"}

struct	*fd

begin
	file	in, inlist
	struct	obsid, filter, extname, telescop, value
	string	key

	inlist = mktemp ("tmp$iraf")

	imextensions (input, output="file", index="1-", extname="",
	    extver="", lindex+, lname-, lver-, ikparams="", > inlist)

	fd = inlist
	while (fscan (fd, in) != EOF) {
	    hselect (in, "obsid", yes) | scan (obsid)
	    hselect (in, "filter", yes) | scan (filter)
	    #hselect (in, "extname", yes) | scan (extname)
	    hselect (in, "imageid", yes) | scan (extname)
	    extname = "ccd" // extname
	    telescop = substr (obsid, 1, stridx(".",obsid)-1)
	    print (filter) |
		translit ("STDIN", "^0-9a-zA-Z\n", "_", delete-, collapse-) |
		scan (value)
	    printf ("%s,%s,%s\n", telescop, value, extname) | scan (key)

	    printf ("set WCSASTRM(%s) \\\n          {%s (%s %s) by %s %s}\n",
		key, obsid, field, filter, author, date)
	    hselect (in, "CTYPE1", yes) | scan (value)
	    printf ("set CTYPE1(%s) %s\n", key, value)
	    hselect (in, "CTYPE2", yes) | scan (value)
	    printf ("set CTYPE2(%s) %s\n", key, value)
	    hselect (in, "CRPIX1", yes) | scan (value)
	    printf ("set CRPIX1(%s) %s\n", key, value)
	    hselect (in, "CRPIX2", yes) | scan (value)
	    printf ("set CRPIX2(%s) %s\n", key, value)
	    hselect (in, "CD1_1", yes) | scan (value)
	    printf ("set CD1_1(%s) %s\n", key, value)
	    hselect (in, "CD2_2", yes) | scan (value)
	    printf ("set CD2_2(%s) %s\n", key, value)
	    hselect (in, "CD1_2", yes) | scan (value)
	    printf ("set CD1_2(%s) %s\n", key, value)
	    hselect (in, "CD2_1", yes) | scan (value)
	    printf ("set CD2_1(%s) %s\n", key, value)
	    hselect (in, "WAT0_001", yes) | scan (value)
	    printf ("set WAT01(%s) {%s}\n", key, value)
	    hselect (in, "WAT1_001", yes) | scan (value)
	    printf ("set WAT11(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT1_002", yes) | scan (value)
	    printf ("set WAT12(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT1_003", yes) | scan (value)
	    printf ("set WAT13(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT1_004", yes) | scan (value)
	    printf ("set WAT14(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT1_005", yes) | scan (value)
	    printf ("set WAT15(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT1_006", yes) | scan (value)
	    if (value != "")
		printf ("set WAT16(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT2_001", yes) | scan (value)
	    printf ("set WAT21(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT2_002", yes) | scan (value)
	    printf ("set WAT22(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT2_003", yes) | scan (value)
	    printf ("set WAT23(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT2_004", yes) | scan (value)
	    printf ("set WAT24(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT2_005", yes) | scan (value)
	    printf ("set WAT25(%s) \\\n          {%-68s}\n", key, value)
	    hselect (in, "WAT2_006", yes) | scan (value)
	    if (value != "")
		printf ("set WAT26(%s) \\\n          {%-68s}\n", key, value)

	    printf ("\n")
	}
	fd = ""; delete (inlist, verify-)
end
