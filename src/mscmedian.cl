# MSCMEDIAN -- Mosiac MEDIAN with outtype option.

procedure mscmedian ()

begin
	file	jlist, in, out
	int	xwin, ywin

	jlist = mktemp ("tmp$iraf")

	joinlists (input, output, output=jlist, delim=" ", shortest+)
	xwin = xwindow
	ywin = ywindow

	msctmp1.xwindow = xwin
	msctmp1.ywindow = ywin
	msctmp1.zloreject = zloreject
	msctmp1.zhireject = zhireject
	msctmp1.boundary = "reflect"
	msctmp1.verbose = no

	msctmp1.fmedian = fmedian
	msctmp1.hmin = hmin
	msctmp1.hmax = hmax
	msctmp1.zmin = zmin
	msctmp1.zmax = zmax
	msctmp1.unmap = yes

	fd = jlist
	while (fscan (fd, in, out) != EOF) {
	    if (outtype == "median") {
		if (verbose) {
		    if (fmedian)
			printf ("fmedian %s %s %d %d\n", in, out, xwin, ywin)
		    else
			printf ("median %s %s %d %d\n", in, out, xwin, ywin)
		}
		msccmd ("msctmp1 $input $output", in, out, extname="",
		    alist=no, flist=yes, verbose=no, exec=yes)
	    } else {
		if (verbose) {
		    if (fmedian)
			printf ("fmedian %s %s %d %d\n", in, out, xwin, ywin)
		    else
			printf ("median %s %s %d %d\n", in, out, xwin, ywin)
		}
		msccmd ("msctmp1 $input $output", in, out, extname="",
		    alist=no, flist=yes, verbose=no, exec=yes)
		if (verbose)
		    printf ("imarith %s - %s %s\n", in, out, out)
		mscarith (in, "-", out, out, extname="", title="",
		    divzero=0., hparams="", pixtype="", calctype="",
		    verbose=no, noact=no)
	    }
	}
	fd = ""; delete (jlist, verify-)
end
