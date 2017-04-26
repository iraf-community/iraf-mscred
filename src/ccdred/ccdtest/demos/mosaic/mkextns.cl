# Make data file of ASCII and binary table extensions using FTOOLS package.
# The data files are created as appendable extensions since users
# may not have FTOOLS installed.

ftools > dev$null
futils > dev$null

fcreate ("acols.dat", "adata.dat", "atable.fits", headfile="ahdr.dat",
    tbltype="ASCII", nskip=0, nrows=0, history=yes, morehdr=0,
    extname="atable", anull=" ", inull=0, clobber=yes)
!tail +2881c atable.fits > atable.dat
delete ("atable.fits", verify=no)

fcreate ("bcols.dat", "bdata.dat", "btable.fits", headfile="bhdr.dat",
    tbltype="binary", nskip=0, nrows=0, history=yes, morehdr=0,
    extname="btable", anull=" ", inull=0, clobber=yes)
!tail +2881c btable.fits > btable.dat
delete ("btable.fits", verify=no)
