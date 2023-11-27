if (imaccess ("median"))
    imdel median
if (imaccess ("absresid"))
    imdel median

list = "list"
while (fscan (list, s1, s2) != EOF) {
display (s1, 2, > "dev$null")
#median (s1, "median", 9, 5, zloreject=INDEF, zhireject=INDEF,
#    boundary="reflect", constant=0., verbose=no)
#display median 1 > "dev$null"
#imexpr ("abs(a-b)", "absresid", s1, "median")
#display absresid 1 > "dev$null"
#boxcar ("absresid", "absresid", 1, 20, boundary="reflect")
#display absresid 1 > "dev$null"
#imstat ("absresid", fields="midpt,stddev", lower=INDEF, upper=INDEF, format-) |
#    scan (x, y)
#z = x + 10 * y
#imstat ("absresid", fields="midpt,stddev", lower=INDEF, upper=z, format-) |
#    scan (x, y)
#z = x + 8 * y
#printf ("a > %g ? 1 : 0\n", z) | scan (line)
#if (access (s2))
#    imdel (s2)
#imexpr (line, s2, "absresid")
#display (s1, 1, overlay=s2, > "dev$null")
#crgrow (s2, s2, radius=1.5, inval=INDEF, outval=INDEF)
#convolve (s2, s2, "", "0.5 1 0.5", "1",
#    bilinear=yes, radsym=no, boundary="nearest", constant=0., row_delimite=";")
#convolve (s2, s2, "", "1", "0.5 0.5 0 0 0 1 0 0 0 0.5 0.5",
#    bilinear=yes, radsym=no, boundary="nearest", constant=0., row_delimite=";")
#display (s1, 1, overlay=s2, > "dev$null")
#convolve (s2, s2, "", "1", "0.5 0.5 0 0 0 1 0 0 0 0.5 0.5",
#    bilinear=yes, radsym=no, boundary="nearest", constant=0., row_delimite=";")
#display (s1, 1, overlay=s2, > "dev$null")
#convolve (s2, s2, "", "1", "0.5 0.5 0 0 0 1 0 0 0 0.5 0.5",
#    bilinear=yes, radsym=no, boundary="nearest", constant=0., row_delimite=";")
#display (s1, 1, overlay=s2, > "dev$null")
imedit (s2, "", cursor="", logfile="", display=yes, autodisplay=yes,
    autosurface=no, aperture="square", radius=1., search=0., buffer=1.,
    width=2., xorder=2, yorder=2, value=0., sigma=INDEF, angh=-33., angv=25.,
    command="display $image 1 erase=$erase fill=no order=0 >& dev$null",
    graphics="stdgraph", default="b", fixpix=no)
#imdel median,absresid
}
list = ""
