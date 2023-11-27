# Create an MEF file from a dump of the headers.
# Input header listsing is "hdrs" and output MEF is "mef".

procedure mkmef (mef, hdrs)

file	mef			{prompt="MEF file to create"}
file	hdrs			{prompt="Input header listings"} 
file	global = ""	{prompt="Global header (optional)\n\n# MKPATTERN"}
string	pattern = "constant"	{prompt="Data pattern"}
real	v1 = 0.			{prompt="Data value 1"}
real	v2 = 1.			{prompt="Data value 2"}
int	size = 1		{prompt="Pattern size"}

begin
	file	in, out, tmp
	int	ncols, nlines
	struct	newline

	out = mef
	in = hdrs
	tmp = mktemp ("tmp$iraf")

	# Delete if already exists.
	if (imaccess (out//"[0]"))
	    imdelete (out, verify-)

	# Create global header.
	printf ("Creating global header...\n")
	mkpattern (out, output="", pattern="constant",
	    option="multiply", v1=0., v2=1., size=1, title="", pixtype="real",
	    ndim=0, ncols=512, nlines=512, header=global)

	# Create extensions.
	list = in
	line = ""
	while (fscan (list, newline) != EOF) {
	    if (substr(newline,1,13) == "No bad pixels") {
		if (access (tmp)) {
		    printf ("Creating extension...%d x %d\n", ncols, nlines)
		    mkpattern (out//"[dummy,append,inherit]", output="",
			pattern=pattern, option="multiply", v1=v1, v2=v2,
			size=size, title="", pixtype="real", ndim=2,
			ncols=ncols, nlines=nlines, header=tmp)
		    delete (tmp, verify-)
		}

		# Get next extensions size.
		line = substr(line, stridx("[",line)+1,1000)
		line = substr(line, stridx("[",line)+1,1000)
		print (line) | scanf ("%d,%d", ncols, nlines)
	    } else if (line != "")
		print (line, >> tmp)
	    line = newline
	}
	if (line != "")
	    print (line, >> tmp)
	if (access (tmp)) {
	    printf ("Creating extension...%d x %d\n", ncols, nlines)
	    mkpattern (out//"[dummy,append,inherit]", output="",
		pattern=pattern, option="multiply", v1=v1, v2=v2,
		size=size, title="", pixtype="real", ndim=2, ncols=ncols,
		nlines=nlines, header=tmp)
	    delete (tmp, verify-)
	}
	list = ""
end

