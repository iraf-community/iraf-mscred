# MSCTVMARK -- Mark list of objects given in celestial coordinates.
#
# The mosaic geometry file is that produced by the last MSCDISPLAY.

procedure msctvmark (coords, frame)

file	coords		{prompt="List of coordinates"}
int	frame		{prompt="Display frame"}
file	output		{prompt="Output file of pixel coordinates and labels"}
string	fields = "1,2,3" {prompt="Fields for RA, DEC, and ID"}
string	wcs = "world"	{prompt="Coordinate type (logical|physical|world)",
			 enum="logical|physical|world"}
string	mark = "circle"	{prompt="Mark type",
			 enum="point|circle|rectangle|line|plus|cross|none"}
string	radii = "10"	{prompt="Radii of concentric circles"}
string	lengths = "0"	{prompt="Lengths and width of concentric rectangles"}
string	font = "raster"	{prompt="Default font"}
int	color = 204	{prompt="Gray level of marks to be drawn"}
bool	label = no	{prompt="Label the marked coordinates"}
int	nxoffset = 0	{prompt="X offset in display pixels of number"}
int	nyoffset = 0	{prompt="Y offset in display pixels of number"}
int	pointsize = 3	{prompt="Size of mark type point in display pixels"}
int	txsize = 1	{prompt="Size of text and numbers in font units"}

begin
	file	crd
	int	frm

	# Query parameters
	crd = coords
	frm = frame

	mscztvmark (crd, frm, "uparm$mscdisp"//frm, output=output,
	    fields=fields, wcs=wcs, mark=mark, radii=radii,
	    lengths=lengths, font=font, color=color, label=label,
	    nxoffset=nxoffset, nyoffset=nyoffset, pointsize=pointsize,
	    txsize=txsize)
end
