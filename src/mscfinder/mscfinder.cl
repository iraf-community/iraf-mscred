#{ MSCFINDER.CL -- Script to set up tasks in the MSCFINDER package

proto
immatch

package	mscfinder

task	msctpeak	= mscfinder$msctpeak.cl
task	mktpeaktab	= mscfinder$mktpeaktab.cl
hidetask mktpeaktab

task	tpltsol		= mscfinder$tpltsol.cl
task	logfile		= mscfinder$logfile.cl
task	tvmark_		= mscfinder$tvmark_.cl
task	catpars		= mscfinder$catpars.par
task	selectpars	= mscfinder$selectpars.par
task	_qpars		= mscfinder$_qpars.par

task	tpeak		= mscfinder$x_finder.e

hidetask mktpeaktab, tpeak, tpltsol
hidetask logfile, tvmark_, catpars, selectpars, _qpars


clbye()
