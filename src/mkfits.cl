# MKFITS -- Make Mosaic FITS file from a set of images.
#
# This is a template program that can be expanded as needed.
# For example the header data file could contain other information.
# The "imtype" variable must be set to "fits" as is the case for the
# MSCRED package.

procedure mkfits (images, mosaic)

string	images = ""		{prompt="Images to be converted"}
string	mosaic = ""		{prompt="Mosaic file to be created"}
file	header = ""		{prompt="Header data"}
bool	delete = no		{prompt="Delete converted images?"}

struct	*fd1, *fd2

begin
	file	imlist
	string	mos, image, ext, detsec

	imlist = mktemp ("tmp$iraf")

	# Set input.
	sections (images, option="root", > imlist)
	mos = mosaic
	fd2 = header

	# Check the mosaic does not exist.
	if (imaccess(mos))
	    error (1, "Mosaic file already exists ("//mos//")")

	# Read through input list appending the images.
	fd1 = imlist
	while (fscan (fd1, image) != EOF) {
	    if (fscan (fd2, ext, detsec) != 2)
		error (2, "Bad format in header file "//header)

	    imcopy (image, mos//"["//ext//",append,inherit]", verbose=verbose)
	    hedit (mos//"["//ext//"]", "detsec", detsec, add+, del-, update+,
		verify-, show-)

	    if (delete)
		imdelete (image, verify-)
	}
	fd1 = ""; delete (imlist, verify=no)
	fd2 = ""
end
