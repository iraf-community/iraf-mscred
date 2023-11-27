#{ CCDRED -- CCD Reduction Package

set	ccddb	= "ccdred$ccddb/"
set	ccdtest	= "ccdred$ccdtest/"

package ccdred

task	$ccdtest	= ccdtest$ccdtest.cl

task	ccdgroups,
	ccdhedit,
	ccdinstrument,
	ccdlist,
	ccdmask,
	ccdproc,
	ccdtool,
	combine,
	cosmicrays,
	coutput,
	mkfringecor,
	mkillumcor,
	mkillumflat,
	mkskycor,
	mkskyflat	= ccdred$xx_ccdred.e

task	darkcombine	= ccdred$darkcombine.cl
task	flatcombine	= ccdred$flatcombine.cl
task	sflatcombine	= ccdred$sflatcombine.cl
task	setinstrument	= ccdred$setinstrument.cl
task	zerocombine	= ccdred$zerocombine.cl

clbye()
