real	x1, y1, scale, zero

if (access ("stars.dat"))
    delete ("stars.dat", verify-)
else ;
starlist ("stars.dat", 20, "", "", interactive=no, spatial="uniform", xmin=1.,
xmax=512., ymin=1., ymax=512., xcenter=INDEF, ycenter=INDEF,
core_radius=30., base=0., sseed=1, luminosity="powlaw", minmag=-7.,
maxmag=0., mzero=-4., power=0.6, alpha=0.74, beta=0.04, delta=0.294,
mstar=1.28, lseed=1, nssample=100, sorder=10, nlsample=100, lorder=10,
rbinsize=10., mbinsize=0.5, graphics="stdgraph", cursor="")

i = 0
list="imatch.dat"
while (fscan (list, s1, x, y) != EOF) {
    i = i + 1

    if (i == 1) {
	x1 = x
	y1 = y
    }
    scale = x * x1
    zero = y1 - y
    printf ("%s %8.2f %8.1f %8.2f %8.1f\n", s1, x, y, scale, zero) |
	tee ("logfile")
    if (imaccess (s1) == YES)
	imdelete (s1, verify-)
    z = 1 / x
    mkobjects (s1, output="", title="", ncols=512, nlines=512, header="",
    background=y, objects="stars.dat", xoffset=0., yoffset=0.,
    star="moffat", radius=1., beta=2.5, ar=1., pa=0., distance=1.,
    exptime=z, magzero=8., gain=1., rdnoise=1., poisson=yes, seed=i,
    comments=yes)
}
list = ""

if (access ("stars1.dat"))
    delete ("stars1.dat", verify-)
else ;
mscctran ("stars.dat", "stars1.dat", "test1", "logical", "world",
columns="1 2", units="", formats="%H %h", min_sigdigit=7, verbose=no)

mscimatch ("test*", "stars1.dat", no, scale=yes, zero=yes, box1=9, box2=13,
lower=INDEF, upper=INDEF, niterate=3, sigma=3., interactive=no,
verbose=no) | tee ("logfile")
