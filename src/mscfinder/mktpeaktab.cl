# MKTPEAKTAB -- Make a table for TPEAK.
# The input is a list of pixel coordinates and celestial coordinates.
# An optional fifth column is a integer identification number.

procedure mktpeaktab (input, output)

file	input			{prompt="Input list (x y ra dec [id])"}
file	output			{prompt="Output table for TPEAK"}
bool	centered = yes		{prompt="Are (x,y) centered coordinates?"}

struct	*fd

begin
	file	temp1, temp2
	real	xpos, ypos, ra, dec
	int	id, cen_flag

	if (access (input) == NO)
	    error (1, "No input list of coordinates to make TPEAK table")

	# Load table tools.
	nttools

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")

	# Make the table column description file.
	print (catpars.id_col // " i", > temp1)
	print (catpars.ra_col // " d h12.1 degrees", >> temp1)
	print (catpars.dec_col // " d h12.1 degrees", >> temp1)
	print (catpars.xpred_col // " r", >> temp1)
	print (catpars.ypred_col // " r", >> temp1)
	print (catpars.xcen_col // " r", >> temp1)
	print (catpars.ycen_col // " r", >> temp1)
	print (catpars.cerr_col // " r", >> temp1)
	print (catpars.cen_col // " i", >> temp1)
	print (catpars.sub_col // " i", >> temp1)
	print (catpars.obj_col // " i", >> temp1)
	print (catpars.region_col // " i", >> temp1)

	# Make the table data file.
	if (centered)
	    cen_flag = 1
	else
	    cen_flag = 0
	id = 0
	fd = input
	while (fscan (fd, xpos, ypos, ra, dec, id) != EOF) {
	    if (nscan () < 4)
		next
	    else if (nscan() == 4)
		id = id + 1
	    ra = ra * 15
	    print (id, ra, dec, xpos, ypos, xpos, ypos, 0., cen_flag, 0, 0, 0,
		>> temp2)
	}
	fd = ""

	# Create the table.
	tcreate (output, temp1, temp2, uparfile="", nskip=0, nlines=0, nrows=0,
	    hist=yes, extrapar=5, tbltype="default", extracol=0)

	delete (temp1, verify-)
	delete (temp2, verify-)
end
