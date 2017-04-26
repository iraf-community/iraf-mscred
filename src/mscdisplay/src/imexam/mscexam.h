include	"../mosim.h"
include "../mosgeom.h"

define	IM_TITLE	Memc[MI_RNAME($1)]
define	MI_LEN1		NX(MI_CMG($1))
define	MI_LEN2		NY(MI_CMG($1))
define	IM_LEN		MI_LEN$2($1)

define	immap		mimap
define	imunmap		miunmap
define	imgs2r		migs2r
#define	imtopen		mitopen
#define	imtrgetim	mitrgetim
#define	imtlen		mitlen
#define	imtclose	mitclose
define	dsunmap		imunmap
define	mw_openim	msc_openim
define	mw_close	msc_close
define	mw_sctran	msc_sctran
define	mw_ctranr	msc_ctranr
