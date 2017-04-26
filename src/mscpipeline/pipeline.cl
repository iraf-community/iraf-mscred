# PIPELINE - Run pipeline on a set of images.

procedure pipeline (input)

string	input			{prompt="List of input Mosaic files"}
file	caldb = ""		{prompt="Calibration database"}
string	date = "!DATE-OBS"	{prompt="Calibration date\n"}
file	procrecipe		{prompt="Processing recipe"}
file	stackrecipe		{prompt="Stack recipe\n"}

struct	*fd

begin
	file	inpt, inlist, in, curdir
	file	inlist1, groups, temp
	int	len
	struct	str

	# Set current directory and pipeline log with full pathname.
	pathnames (".") | scan (curdir)
	pathnames (pipelog) | scan (temp)
	pipelog = temp

	# Expand input list.
	inpt = input
	inlist = mktemp ("tmp$iraf")
	inlist1 = mktemp ("tmp$iraf")
	sections (inpt, option="fullname", > inlist)

	# Run single exposure recipes.
	fd = inlist
	while (fscan (fd, in) != EOF) {
	    # Trim any ".fits extension.
	    len = strlen (in)
	    if (substr (in, len-4, len) == ".fits")
		in = substr (in, 1, len-5)

	    # Check working directory and image.
	    cd (curdir)
	    if (!access (in)) {
		if (!imaccess (in//"[0]")) {
		    xlog ("*1*", "TIME", "error", in, "pipeline",
			"WARNING: File not found", output=pipelog)
		    next
		}
		mkdir (in)
		imrename (in, in, verbose-)
	    }
	    cd (in)
	    if (!imaccess (in//"[0]")) {
		xlog ("*1*", "TIME", "error", in, "pipeline",
		    "WARNING: File not found", output=pipelog)
		next
	    }

	    # Add image to list of available images for stack processing.
	    printf ("%s/%s[1]\n", in, in, >> inlist1)

	    # Check for completed processing.
	    if (access (pipelog)) {
		xlog ("done", in, "pipeline", output="STDOUT") | scan (str)
		match (str, pipelog, stop-) | count ("STDIN") | scan (i)
		if (i > 0)
		    next
	    }
	    xlog ("*1*", "TIME", "run ", in, "pipeline", output=pipelog)

	    # Set processing parameters and recipe.
	    logfile = in // ".html"
	    plotfile = in // ".mc"
	    set stdvdm = (plotfile)
	    caldb.input = in
	    caldb.caldb = caldb
	    caldb.date = date
	    pipestep ("caldb", in, "", "CALDB")

	    # Run the recipe.
	    if (access (procrecipe)) {
		printf ("redefine $procrecipe = %s; procrecipe %s\n",
		    procrecipe, in) | cl
		xlog ("*1*", "TIME", "done", in, "pipeline", output=pipelog)
	    } else
		printf ("WARNING: No recipe found for %s\n", in)

	}
	fd = ""
	cd (curdir)

#	# Run stack recipe.
#	if (access (inlist1) && access (stackrecipe)) {
#	    redefine $stackrecipe = (stackrecipe)
#
#	    groups = mktemp ("tmp$iraf")
#
#	    # Group using the first extension header.
#	    ccdgroups ("@"//inlist1, groups, list=groups)
#
#	    # Process each group.
#	    fd = groups
#	    while (fscan (fd, in) != EOF)
#		stackrecipe ("@"//in)
#	    fd = ""
#
#	    delete ("@"//groups, verify-)
#	    delete (groups, verify-)
#	}
	
	if (access (inlist1))
	    delete (inlist1, verify-)
	delete (inlist, verify-)
end
