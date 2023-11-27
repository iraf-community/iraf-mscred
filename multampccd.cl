#{ MULTAMPCCD -- Multiamp CCD Reduction Package

# This package requires FITS image type and various kernel parameters.
reset imtype = "fits"
if (defvar ("fkinit"))
    set fkinit = envget ("fkinit") // ",append,padlines=10"
else
    set fkinit = "append,padlines=10"

cl < "mscred$lib/zzsetenv.def"
package multampccd, bin = mscbin$

set	ccdred		= "mscsrc$ccdred/"

task	mscwfits	= mscsrc$mscwfits.cl
task	mscrfits	= mscsrc$mscrfits.cl

task	ccdproc		= mscsrc$ccdproc.cl
task	calproc		= mscsrc$calproc.cl
task	ccdhedit	= mscsrc$ccdhedit.cl
task	ccdlist		= mscsrc$ccdlist.cl
task	setinstrument	= mscsrc$setinstrument.cl

task	_ccdhedit,
	_ccdlist,
	_ccdtool	= mscsrc$x_ccdred.e

task	ccddelete,
	ccdgroups,
	combine,
	coutput,
	mergeamps	= ccdred$x_ccdred.e
hidetask ccddelete, ccdgroups, coutput, mergeamps

task	darkcombine	= ccdred$darkcombine.cl
task	flatcombine	= ccdred$flatcombine.cl
task	sflatcombine	= mscsrc$sflatcombine.cl
task	zerocombine	= ccdred$zerocombine.cl

task	msccmd		= mscsrc$msccmd.cl
task	mscarith	= mscsrc$mscarith.cl
task	mscstat		= mscsrc$mscstat.cl
task	mscblkavg	= mscsrc$mscblkavg.cl

task	fitscopy,
	mscuniq		= mscsrc$x_mscred.e

hidetask fitscopy, calproc, mscuniq

# Display stuff.

set	mscdisplay	= "mscsrc$mscdisplay/"
set	mosexam		= "mscdisplay$src/imexam/"
set	starfocus	= "mscdisplay$src/starfocus/"

task	mscstarfocus	= starfocus$x_mscdisplay.e; hidetask mscstarfocus
task	mscfocus	= starfocus$mscfocus.cl

task	mscdisplay,
	mscrtdisplay	= mscdisplay$x_mscdisplay.e
task	mimpars		= mscdisplay$mimpars.par
hidetask mscrtdisplay

task    mscexamine    = "mosexam$x_mscexam.e"

task    cimexam2 = mosexam$cimexam2.par;    hidetask cimexam2
task    eimexam2 = mosexam$eimexam2.par;    hidetask eimexam2
task    himexam2 = mosexam$himexam2.par;    hidetask himexam2
task    jimexam2 = mosexam$jimexam2.par;    hidetask jimexam2
task    limexam2 = mosexam$limexam2.par;    hidetask limexam2
task    rimexam2 = mosexam$rimexam2.par;    hidetask rimexam2
task    simexam2 = mosexam$simexam2.par;    hidetask simexam2
task    vimexam2 = mosexam$vimexam2.par;    hidetask vimexam2

task	mscotfflat	= "mscdisplay$mscotfflat.cl"
task	flatcompress	= "mscdisplay$flatcompress.cl"
hidetask flatcompress

clbye()
