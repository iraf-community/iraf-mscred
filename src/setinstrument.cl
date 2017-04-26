# SETINSTRUMENT -- Set up instrument parameters for the CCD reduction tasks.
#
# This task sets default parameters based on an instrument ID.

procedure setinstrument (site, telescope, instrument)

char	site			{prompt="Site (? for menu)"}
char	telescope		{prompt="Telescope (? for menu)"}
char	instrument		{prompt="Instrument (? for a list)"}

char	directory="mscdb$noao/"	{prompt="Instrument directory"}
bool	review=yes		{prompt="Review instrument parameters?"}
char	query_site		{prompt="Site (? for menu or q to quit)",
				 mode="q"}
char	query_tel		{prompt="Telescope (? for menu or q to quit)",
				 mode="q"}
char	query_inst		{prompt="Instrument (? for menu or q to quit)",
				 mode="q"}

begin
	string	obs, tel, inst, instdir, men, dir, instfile

	# Define instrument directory.
	instdir = directory
	dir = directory

	# Define site.
	men = instdir // "sites.men"
	obs = site
	dir = instdir // obs
	while (obs != "" && !access (dir)) {
	    if (access (men)) {
		print ("\nSites:\n")
		type (men)
	    } else if (obs == "?")
		print ("Site menu ", men, " not found")
	    else
	        print ("Site ", tel, " not found")
	    print ("")
	    obs = query_site
	    if (obs == "q")
		return
	    site = obs
	    dir = instdir // obs
	}
	if (obs != "")
	    instdir = instdir // obs // "/"

	# Define telescope.
	men = instdir // "telescopes.men"
	tel = telescope
	dir = instdir // tel
	while (tel != "" && !access (dir)) {
	    if (access (men)) {
		print ("\nTelescopes:\n")
		type (men)
	    } else if (tel == "?")
		print ("Telescope menu ", men, " not found")
	    else
	        print ("Telescope ", tel, " not found")
	    print ("")
	    tel = query_tel
	    if (tel == "q")
		return
	    telescope = tel
	    dir = instdir // tel
	}
	if (tel != "")
	    instdir = instdir // tel // "/"

	# Define instrument.
	men = instdir // "instruments.men"
	inst = instrument
	instfile = instdir // inst // ".dat"
	while (inst != "" && !access (instfile)) {
	    if (access (men)) {
		print ("\nInstruments:\n")
		type (men)
	    } else if (inst == "?")
		print ("Instrument menu ", men, " not found")
	    else
	        print ("Instrument file ", instfile, " not found")
	    print ("")
	    inst = query_inst
	    if (inst == "q")
		return
	    instrument = inst
	    instfile = instdir // inst // ".dat"
	}

	# Set instrument parameter.
	if (access (instfile))
	    mscred.instrument = instfile
	else
	    mscred.instrument = ""

	# Run instrument setup script.
	instfile = instdir // inst // ".cl"
	if (access (instfile))
	    cl (< instfile)

	# Review parameters if desired.
	if (review) {
	    eparam ("mscred")
	    eparam ("ccdproc")
	}
end
