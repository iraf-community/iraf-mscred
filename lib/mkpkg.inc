# Global MKPKG definitions for the package.

$set XFLAGS	= "$(XFLAGS) -p mscred"
$set XVFLAGS	= "$(XVFLAGS) -p mscred"
$set LFLAGS	= "$(LFLAGS) -p mscred"

$ifeq (MACH, ssun) then
    $include "mscred$lib/mkpkg.sf.SSUN"
#$else $ifeq (MACH, redhat) then
#    $include "mscred$lib/mkpkg.sf.REDHAT"
#$else $ifeq (MACH, sparc) then
#    $include "mscred$lib/mkpkg.sf.SUN4"
$end
