#{ MSCPIPELINE -- Mosaic CCD Reduction Pipeline Package

package mscpipeline

task	pipeline = mscpipeline$pipeline.cl
task	caldb = mscpipeline$caldb.cl
task	pipestep = mscpipeline$pipestep.cl

task	$procrecipe = mscpipeline$procrecipe.cl
task	$stackrecipe = mscpipeline$stackrecipe.cl

struct	*fd1, *fd2
struct	*fdstack

if (access (pipeuser))
    cl (< pipeuser)
else
    ;

clbye()
