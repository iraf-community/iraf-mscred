# Make some data.

s1 = "."  //  envget ("imtype")
if (access ("Zero" // s1) == no) {
    print ("Making zero image `Zero' ...")
    artobs ("Zero", 0., "zero", filter="V", skyrate=0., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no)
} else
    ;
if (access ("Dark" // s1) == no) {
    print ("Making dark count image `Dark' ...")
    artobs ("Dark", 1000., "dark", filter="V", skyrate=0., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no)
} else
    ;
if (access ("FlatV" // s1) == no) {
    print ("Making flat field image `FlatV' ...")
    artobs ("FlatV", 1., "flat", filter="V", skyrate=2000., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no)
} else
    ;
if (access ("FlatB" // s1) == no) {
    print ("Making flat field image `FlatB' ...")
    artobs ("FlatB", 1., "flat", filter="B", skyrate=1000., imdata="",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no)
} else
    ;
if (access ("obs001" // s1) == no) {
    print ("Making object image `obs001' ...")
    artobs ("obs001", 10., "object", filter="V", skyrate=100., imdata="dev$pix",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no)
} else
    ;
if (access ("obs002" // s1) == no) {
    print ("Making object image `obs002' ...")
    artobs ("obs002", 30., "object", filter="B", skyrate=100., imdata="dev$pix",
	ncols=132, nlines=100, datasec="[1:100,1:100]", trimsec="[3:98,3:98]",
	biassec="[103:130,*]", badpix="demos$badpix.dat", biasval=500.,
	badval=500., zeroval=100., darkrate=1., zeroslope=0.01,
	darkslope=0.002, flatslope=3.0000000000000E-4, sigma=5., seed=0,
	overwrite=no)
} else
    ;
