#{ MSCRED -- Mosaic CCD Reduction Package

# Load dependent packages.
if (deftask ("fitsutil"))
    fitsutil
else
    ;
nproto
astutil
digiphot
apphot
photcal
if (deftask ("astcat"))
    astcat
else
    ;

# This package requires FITS image type and various kernel parameters.
reset imtype = "fits"
if (defvar ("fkinit"))
    set fkinit = envget ("fkinit") // ",append,padlines=10,cachesize=60"
else
    set fkinit = "append,padlines=10,cachesize=60"


cl < "mscred$lib/zzsetenv.def"
package mscred, bin = mscbin$

# Logical directories.
set	xtcoeff		= "mscred$lib/xtcoeff/"
set	mccdred		= "mscsrc$ccdred/"
set	combine		= "mscsrc$ccdred/src/combine/"

# Tasks.

task	mscmedian	= mscsrc$mscmedian.cl
task	msctmp1		= mscsrc$msctmp1.cl
task	mscfindgain	= mscsrc$mscfindgain.cl
task	mscsplit	= mscsrc$mscsplit.cl
task	mscjoin		= mscsrc$mscjoin.cl
task	mscwfits	= mscsrc$mscwfits.cl
task	mscrfits	= mscsrc$mscrfits.cl
task	msctoshort	= mscsrc$msctoshort.cl
task	dispsnap	= mscsrc$dispsnap.cl

task	ccdproc		= mscsrc$ccdproc.cl
task	calproc		= mscsrc$calproc.cl
task	ccdhedit	= mscsrc$ccdhedit.cl
task	ccdlist		= mscsrc$ccdlist.cl
task	setinstrument	= mscsrc$setinstrument.cl

task	_ccdhedit,
	_ccdlist,
	_ccdtool	= mscsrc$x_ccdred.e

task	ccddelete,
	ccdgroups	= mccdred$x_ccdred.e
hidetask ccddelete, ccdgroups

task	combine,
	coutput,
	mergeamps	= combine$x_combine.e
hidetask coutput, mergeamps

task	darkcombine	= mccdred$darkcombine.cl
task	flatcombine	= mccdred$flatcombine.cl
task	sflatcombine	= mscsrc$sflatcombine.cl
task	zerocombine	= mccdred$zerocombine.cl

task	mscgetcatalog	= mscsrc$mscgetcatalog.cl
task	mscagetcat	= mscsrc$mscagetcat.cl
task	mscsetwcs	= mscsrc$mscsetwcs.cl
task	msczero		= mscsrc$msczero.cl
task	mscxreg		= mscsrc$mscxreg.cl
task	mscimage	= mscsrc$mscimage.cl
task	mscoimage	= mscsrc$mscoimage.cl
task	mscstack	= mscsrc$mscstack.cl
#task	mscdither	= mscsrc$mscdither.cl
task	msccmd		= mscsrc$msccmd.cl
task	mscarith	= mscsrc$mscarith.cl
task	mscstat		= mscsrc$mscstat.cl
#task	mscimatch	= mscsrc$mscimatch.cl
#task	ffpupilcor	= mscsrc$ffpupilcor.cl
task	rmfringe	= mscsrc$rmfringe.cl
task	rmpupil		= mscsrc$rmpupil.cl
task	irmfringe	= mscsrc$irmfringe.cl
task	irmpupil	= mscsrc$irmpupil.cl
task	mscpupil	= mscsrc$mscpupil.cl
task	mscblkavg	= mscsrc$mscblkavg.cl
task	mscpixarea	= mscsrc$mscpixarea.cl
#task	xtalkcor	= mscsrc$xtalkcor.cl
task	mscqphot	= mscsrc$mscqphot.cl
task	msccntr		= mscsrc$msccntr.cl
task	mscshutcor	= mscsrc$mscshutcor.cl
task	mscselect	= mscsrc$mscselect.cl

task	addkey,
	fitscopy,
	getcatalog,
	joinlists,
	mkmsc,
	msccmatch,
	mscctran,
	mscextensions,
	mscgmask,
	mscimatch,
	mscpmask,
	mscskysub,
	msctemplate,
	mscwtemplate,
	mscwcs,
	mscuniq,
	patfit,
	pixarea,
	pupilfit,
	toshort,
	ximstat,
	xlog,
	xtalkcor,
	xtcoeff		= mscsrc$x_mscred.e

# Photometry parameters.
#task	msccpars	= mscsrc$msccpars.par
#task	mscdpars	= mscsrc$mscdpars.par
#task	mscppars	= mscsrc$mscppars.par
#task	mscspars	= mscsrc$mscspars.par

hidetask ximstat, joinlists, mscoimage, msccntr
hidetask addkey, fitscopy, calproc, getcatalog
hidetask mscgmask, mscpmask, msctemplate, mscwtemplate
hidetask mscxreg, mscuniq, mscextensions
hidetask patfit, pupilfit, toshort, xlog
#hidetask msccpars, mscdpars, mscppars, mscspars
hidetask dispsnap, mscqphot, pixarea, msctmp1

# Special version of utilities.curfit
task	msccurfit	= "mscsrc$curfit/x_mscred.e"
hidetask msccurfit

# Display stuff.

#task	newdisplay = "mscsrc$display/x_display.e"

task	msctvmark	= "mscsrc$msctvmark.cl"
task	mscztvmark	= "mscsrc$mscztvmark.cl"

set	mscdisplay	= "mscsrc$mscdisplay/"
set	mosexam		= "mscdisplay$src/imexam/"
set	starfocus	= "mscdisplay$src/starfocus/"

task	mscstarfocus	= starfocus$x_mscdisplay.e; hidetask mscstarfocus
task	mscfocus	= starfocus$mscfocus.cl

task	mscdisplay,
	mscrtdisplay	= mscdisplay$x_mscdisplay.e
task	mimpars		= mscdisplay$mimpars.par

hidetask mscrtdisplay, mscztvmark

task    mscexamine    = "mosexam$x_mscexam.e"

task    cimexam2 = mosexam$cimexam2.par;    hidetask cimexam2
task    eimexam2 = mosexam$eimexam2.par;    hidetask eimexam2
task    himexam2 = mosexam$himexam2.par;    hidetask himexam2
task    jimexam2 = mosexam$jimexam2.par;    hidetask jimexam2
task    kimexam2 = mosexam$kimexam2.par;    hidetask kimexam2
task    limexam2 = mosexam$limexam2.par;    hidetask limexam2
task    rimexam2 = mosexam$rimexam2.par;    hidetask rimexam2
task    simexam2 = mosexam$simexam2.par;    hidetask simexam2
task    vimexam2 = mosexam$vimexam2.par;    hidetask vimexam2

task	mscotfflat	= "mscdisplay$mscotfflat.cl"
task	flatcompress	= "mscdisplay$flatcompress.cl"
hidetask flatcompress

# Stuff for PICREAD data.
#task	mkfits		= mscsrc$picread/mkfits.cl
#task	mosfocus	= mscsrc$picread/mosfocus.cl

# Subpackages

set	mscfinder	= "mscsrc$mscfinder/"
task	$mscfinder	= mscfinder$mscfinder.cl

set	mscpipeline	= "mscsrc$mscpipeline/"
task	mscpipeline	= mscpipeline$mscpipeline.cl
hidetask mscpipeline

set	msctest		= "mscsrc$msctest/"
task	$msctest	= msctest$msctest.cl; 	 hidetask msctest

set	msctools	= "mscsrc$msctools/"
#task	$msctools	= msctools$msctools.cl

clbye()
