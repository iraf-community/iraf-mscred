# Make CCD Mosaic data.

ccdred.instrument = "ccddb$kpno/mosaic.dat"

s1 = mktemp ("tmp") // ".fits"
s2 = mktemp ("tmp") // ".fits"
j = 0	# Number of table extensions
k = 8	# Number of image extensions

if (access ("Zero.fits") == no) {
    print ("Making zero image `Zero' ...")
    artobs (s2, 0., "zero", filter="V", skyrate=0., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no, title="M51", header="demos$mosaic/mosaic1.dat")
    mkpattern (s1, output="", title="M51", pixtype="short", ndim=0,
	header="demos$mosaic/mosaic0a.dat")
    if (j > 0)
	concatenate ("demos$mosaic/atable.dat", s1, out_type="binary",
	    append=yes)
    if (j > 1)
	concatenate ("demos$mosaic/btable.dat", s1, out_type="binary",
	    append=yes)
    for (i=1; i<=k; i+=1)
	imcopy (s2, s1//"[inherit]", verbose=no)
    for (i=k; i>=0; i-=1) {
	if (i == 0)
	    s3 = s1 // "[0]"
	else
	    s3 = s1 // "[" // i+j // "]"
	mkheader (s3, "demos$mosaic/mosaic"//i//".dat", append=no, verbose=no)
	ccdhedit (s3, "imagetyp", "zero", type="string")
	ccdhedit (s3, "exptime", 0., type="real")
	ccdhedit (s3, "darktime", 0., type="real")
	ccdhedit (s3, "subset", "V", type="string")
    }
    imrename (s1, "Zero")
    imdelete (s2, verify=no)
} else
    ;

if (access ("Dark.fits") == no) {
    print ("Making dark count image `Dark' ...")
    artobs (s2, 1000., "dark", filter="V", skyrate=0., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no, title="M51", header="demos$mosaic/mosaic1.dat")
    mkpattern (s1, output="", title="M51", pixtype="short", ndim=0,
	header="demos$mosaic/mosaic0a.dat")
    if (j > 0)
	concatenate ("demos$mosaic/atable.dat", s1, out_type="binary",
	    append=yes)
    if (j > 1)
	concatenate ("demos$mosaic/btable.dat", s1, out_type="binary",
	    append=yes)
    for (i=1; i<=k; i+=1)
	imcopy (s2, s1//"[inherit]", verbose=no)
    for (i=k; i>=0; i-=1) {
	if (i == 0)
	    s3 = s1 // "[0]"
	else
	    s3 = s1 // "[" // i+j // "]"
	mkheader (s3, "demos$mosaic/mosaic"//i//".dat", append=no, verbose=no)
	ccdhedit (s3, "imagetyp", "dark", type="string")
	ccdhedit (s3, "exptime", 1000., type="real")
	ccdhedit (s3, "darktime", 1000., type="real")
	ccdhedit (s3, "subset", "V", type="string")
    }
    imrename (s1, "Dark")
    imdelete (s2, verify=no)
} else
    ;

if (access ("FlatV.fits") == no) {
    print ("Making flat field image `FlatV' ...")
    artobs (s2, 1., "flat", filter="V", skyrate=2000., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no, title="M51", header="demos$mosaic/mosaic1.dat")
    mkpattern (s1, output="", title="M51", pixtype="short", ndim=0,
	header="demos$mosaic/mosaic0a.dat")
    if (j > 0)
	concatenate ("demos$mosaic/atable.dat", s1, out_type="binary",
	    append=yes)
    if (j > 1)
	concatenate ("demos$mosaic/btable.dat", s1, out_type="binary",
	    append=yes)
    for (i=1; i<=k; i+=1)
	imcopy (s2, s1//"[inherit]", verbose=no)
    for (i=k; i>=0; i-=1) {
	if (i == 0)
	    s3 = s1 // "[0]"
	else
	    s3 = s1 // "[" // i+j // "]"
	mkheader (s3, "demos$mosaic/mosaic"//i//".dat", append=no, verbose=no)
	ccdhedit (s3, "imagetyp", "flat", type="string")
	ccdhedit (s3, "exptime", 1., type="real")
	ccdhedit (s3, "darktime", 1., type="real")
	ccdhedit (s3, "subset", "V", type="string")
    }
    imrename (s1, "FlatV")
    imdelete (s2, verify=no)
} else
    ;

if (access ("FlatB.fits") == no) {
    print ("Making flat field image `FlatB' ...")
    artobs (s2, 2., "flat", filter="B", skyrate=1000., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no, title="M51", header="demos$mosaic/mosaic1.dat")
    mkpattern (s1, output="", title="M51", pixtype="short", ndim=0,
	header="demos$mosaic/mosaic0a.dat")
    if (j > 0)
	concatenate ("demos$mosaic/atable.dat", s1, out_type="binary",
	    append=yes)
    if (j > 1)
	concatenate ("demos$mosaic/btable.dat", s1, out_type="binary",
	    append=yes)
    for (i=1; i<=k; i+=1)
	imcopy (s2, s1//"[inherit]", verbose=no)
    for (i=k; i>=0; i-=1) {
	if (i == 0)
	    s3 = s1 // "[0]"
	else
	    s3 = s1 // "[" // i+j // "]"
	mkheader (s3, "demos$mosaic/mosaic"//i//".dat", append=no, verbose=no)
	ccdhedit (s3, "imagetyp", "flat", type="string")
	ccdhedit (s3, "exptime", 2., type="real")
	ccdhedit (s3, "darktime", 2., type="real")
	ccdhedit (s3, "subset", "B", type="string")
    }
    imrename (s1, "FlatB")
    imdelete (s2, verify=no)
} else
    ;

if (access ("obs001.fits") == no) {
    print ("Making object image `obs001' ...")
    artobs (s2, 10., "object", filter="V", skyrate=200., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no, title="M51", header="demos$mosaic/mosaic1.dat")
    mkpattern (s1, output="", title="M51", pixtype="short", ndim=0,
	header="demos$mosaic/mosaic0a.dat")
    if (j > 0)
	concatenate ("demos$mosaic/atable.dat", s1, out_type="binary",
	    append=yes)
    if (j > 1)
	concatenate ("demos$mosaic/btable.dat", s1, out_type="binary",
	    append=yes)
    for (i=1; i<=k; i+=1)
	imcopy (s2, s1//"[inherit]", verbose=no)
    for (i=k; i>=0; i-=1) {
	if (i == 0)
	    s3 = s1 // "[0]"
	else
	    s3 = s1 // "[" // i+j // "]"
	mkheader (s3, "demos$mosaic/mosaic"//i//".dat", append=no, verbose=no)
	ccdhedit (s3, "imagetyp", "object", type="string")
	ccdhedit (s3, "exptime", 10., type="real")
	ccdhedit (s3, "darktime", 10., type="real")
	ccdhedit (s3, "subset", "V", type="string")
    }
    imrename (s1, "obs001")
    imdelete (s2, verify=no)
} else
    ;

if (access ("obs002.fits") == no) {
    print ("Making object image `obs002' ...")
    artobs (s2, 20., "object", filter="B", skyrate=100., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no, title="M51", header="demos$mosaic/mosaic1.dat")
    mkpattern (s1, output="", title="M51", pixtype="short", ndim=0,
	header="demos$mosaic/mosaic0a.dat")
    if (j > 0)
	concatenate ("demos$mosaic/atable.dat", s1, out_type="binary",
	    append=yes)
    if (j > 1)
	concatenate ("demos$mosaic/btable.dat", s1, out_type="binary",
	    append=yes)
    for (i=1; i<=k; i+=1)
	imcopy (s2, s1//"[inherit]", verbose=no)
    for (i=k; i>=0; i-=1) {
	if (i == 0)
	    s3 = s1 // "[0]"
	else
	    s3 = s1 // "[" // i+j // "]"
	mkheader (s3, "demos$mosaic/mosaic"//i//".dat", append=no, verbose=no)
	ccdhedit (s3, "imagetyp", "object", type="string")
	ccdhedit (s3, "exptime", 20., type="real")
	ccdhedit (s3, "darktime", 20., type="real")
	ccdhedit (s3, "subset", "B", type="string")
    }
    imrename (s1, "obs002")
    imdelete (s2, verify=no)
} else
    ;
