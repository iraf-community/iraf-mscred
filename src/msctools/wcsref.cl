# WCSREF -- Create or modify a WCS reference image.

procedure wcsref (image, ra, dec, scale, rotation)

file	image			{prompt="Reference image"}
real	ra			{prompt="RA (hours)"}
real	dec			{prompt="DEC (degrees)"}
real	scale			{prompt="Scale (arcsec/pixel)"}
real	rotation = 90		{prompt="Rotation of DEC from N to E (degrees)"}

begin
	file	im
	real	crval1, crval2, cd11, cd22, rot

	# Get query parameters.
	im = image
	crval1 = ra
	crval2 = dec
	cd11 = -scale
	cd22 = -cd11
	rot = rotation

	# Make the template image if necessary.
	if (!imaccess (im))
	    mkpattern (im, output="", pattern="constant", option="replace",
		v1=0., v2=0., size=1, title="", pixtype="short", ndim=2,
		ncols=1, nlines=1, n3=1, n4=1, n5=1, n6=1, n7=1, header="")

	# Set the WCS.
	ccsetwcs (im, "", "", xref=INDEF, yref=INDEF, xmag=cd11, ymag=cd22,
	    xrotation=rot, yrotation=rot, lngref=crval1, latref=crval2,
	    lngunits="hours", latunits="degrees", transpose=no,
	    projection="tan", coosystem="j2000", update=yes,
	    pixsystem="physical", verbose=yes)
end
