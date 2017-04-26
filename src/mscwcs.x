include	<error.h>

define	WCS_NCT		4		# Number of transformations
define	LEN_MSCWCS	(3+WCS_NCT*3)

define	WCS_MW		Memi[$1]	# Image MWCS pointer
define	WCS_MWP		Memi[$1+1]	# Projection MWCS pointer

define	WCS_DIR		Memi[$1+$2*3]	# Direction of transformation
define	WCS_CT		Memi[$1+$2*3+1]	# Transformation
define	WCS_CTP		Memi[$1+$2*3+2]	# Projection transformation

define	WCS_TYPES	"|logical|physical|astrometry|world|"
define	L2P		12	# logical to physical
define	L2A		13	# logical to astrometry
define	L2W		14	# logical to world
define	P2L		21	# physical to logical
define	P2A		23	# physical to astrometry
define	P2W		24	# physical to world
define	A2L		31	# world to logical
define	A2P		32	# world to physical
define	A2W		34	# world to astrometry
define	W2L		41	# world to logical
define	W2P		42	# world to physical
define	W2A		43	# world to astrometry


# MSC_OPENIM -- Open Mosaic WCS.

pointer procedure msc_openim (im, wcs)

pointer	im		#I Image
pointer	wcs		#O WCS

pointer	sp, attrib, database, solution, mw

int	nscan()
pointer	ccsetwcs(), mw_openim()

errchk	ccsetwcs, mw_openim, msc_open

begin
	call smark (sp)
	call salloc (attrib, SZ_LINE, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (solution, SZ_FNAME, TY_CHAR)

	iferr {
	    ifnoerr (call imgstr (im, "wcssol", Memc[attrib], SZ_LINE)) {
		call sscan (Memc[attrib])
		call gargwrd (Memc[database], SZ_FNAME)
		call gargwrd (Memc[solution], SZ_FNAME)
		if (nscan() != 2)
		    call error (1, "Invalid WCSSOL keyword")
		mw = ccsetwcs (im, Memc[database], Memc[solution])
	    } else
		mw = mw_openim (im)

	    call msc_open (mw, wcs)
	} then {
	    call msc_close (wcs)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
	return (WCS_MW(wcs))
end


# MSC_OPEN -- Open Mosaic WCS.

procedure msc_open (mw, wcs)

pointer	mw		#I MWCS pointer
pointer	wcs		#O WCS

int	axes[2]
double	r[2], w[2], cd[2,2]
pointer	sp, attrib, mwp

bool	streq()
pointer	mw_open()

data	axes/1,2/

begin
	call smark (sp)
	call salloc (attrib, SZ_LINE, TY_CHAR)

	iferr {
	    call calloc (wcs, LEN_MSCWCS, TY_STRUCT)

	    # Set projection MWCS for computing "astrometry" coordinates.
	    mwp = mw_open (NULL, 2)
	    call mw_gsystem (mw, Memc[attrib], SZ_LINE)
	    if (!streq (Memc[attrib], "physical")) {
		iferr {
		    call mw_newsystem (mwp, Memc[attrib], 2)
		    call mw_swtype (mwp, axes, 2, "tan", "")
		    call mw_gwattrs (mw, 1, "axtype", Memc[attrib], SZ_LINE)
		    call mw_swattrs (mwp, 1, "axtype", Memc[attrib])
		    call mw_gwattrs (mw, 2, "axtype", Memc[attrib], SZ_LINE)
		    call mw_swattrs (mwp, 2, "axtype", Memc[attrib])
		    call mw_gwtermd (mw, r, w, cd, 2)
		    r[1] = 0; r[2] = 0
		    cd[1,1] = 1; cd[1,2] = 0; cd[2,1] = 0; cd[2,2] = 1
		    call mw_swtermd (mwp, r, w, cd, 2)
		} then {
		    call erract (EA_WARN)
		    call mw_close (mwp)
		    mwp = mw_open (NULL, 2)
		}
	    }

	    WCS_MW(wcs) = mw
	    WCS_MWP(wcs) = mwp
	} then {
	    call msc_close (wcs)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
end


# MSC_CLOSE - Close Mosaic WCS.

procedure msc_close (wcs)

pointer	wcs		#I WCS

int	ct

begin
	if (wcs == NULL)
	    return

	do ct = 1, WCS_NCT {
	    if (WCS_CT(wcs,ct) != NULL)
		call mw_ctfree (WCS_CT(wcs,ct))
	    if (WCS_CTP(wcs,ct) != NULL)
		call mw_ctfree (WCS_CTP(wcs,ct))
	}
	if (WCS_MW(wcs) != NULL)
	    call mw_close (WCS_MW(wcs))
	if (WCS_MWP(wcs) != NULL)
	    call mw_close (WCS_MWP(wcs))
	call mfree (wcs, TY_STRUCT)
end


# MSC_SCTRAN -- Set up the coordinate transformation.
# The default is to copy the input coordinates to the output coordinates.
#
# Add new system "astrometry".

pointer procedure msc_sctran (wcs, ct,  system1, system2, ax)

pointer	wcs		#I WCS structure
int	ct		#I Coordinate system index
char	system1[ARB]	#I Input system
char	system2[ARB]	#I Output system
int	ax		#I Axes

char	sys1[10], sys2[10]
bool	streq()
int	strdic()
pointer	mw_sctran()

begin
	if (ct < 1 || ct > WCS_NCT)
	    call error (1, "Coordinate system index out of bounds")

	if (WCS_CT(wcs,ct) != NULL)
	    call mw_ctfree (WCS_CT(wcs,ct))
	if (WCS_CTP(wcs,ct) != NULL)
	    call mw_ctfree (WCS_CTP(wcs,ct))

	WCS_DIR(wcs,ct) = 10 * strdic (system1, sys1, 10, WCS_TYPES) +
	    strdic (system2, sys2, 10, WCS_TYPES)
	if (streq (sys1, "astrometry"))
	    call strcpy ("world", sys1, 10)
	if (streq (sys2, "astrometry"))
	    call strcpy ("world", sys2, 10)

	WCS_CT(wcs,ct) = mw_sctran (WCS_MW(wcs), sys1, sys2, ax)
	switch (WCS_DIR(wcs,ct)) {
	case L2A, P2A, W2A:
	    WCS_CTP(wcs,ct) = mw_sctran (WCS_MWP(wcs),"world","physical",ax)
	case A2L, A2P, A2W:
	    WCS_CTP(wcs,ct) = mw_sctran (WCS_MWP(wcs),"physical","world",ax)
	}
	return (WCS_CT(wcs,ct))
end


procedure msc_ctrand (wcs, ct, in, out, ndim)

pointer	wcs			#I WCS pointer
int	ct			#I Coordinate system index
double	in[ARB]			#I Input coordinate
double	out[ARB]		#O Output coordinate
int	ndim			#I Dimensionality of coordinate

begin
	if (ct < 1 || ct > WCS_NCT)
	    call error (1, "Coordinate system index out of bounds")

	if (ndim != 2)
	    call error (1, "MSC_CTRAND - WCS dimensionality not supported")

	call msc_c2trand (wcs, ct, in[1], in[2], out[1], out[2])
end


procedure msc_ctranr (wcs, ct, in, out, ndim)

pointer	wcs			#I WCS pointer
int	ct			#I Coordinate system index
real	in[ARB]			#I Input coordinate
real	out[ARB]		#O Output coordinate
int	ndim			#I Dimensionality of coordinate

begin
	if (ct < 1 || ct > WCS_NCT)
	    call error (1, "Coordinate system index out of bounds")

	if (ndim != 2)
	    call error (1, "MSC_CTRAND - WCS dimensionality not supported")

	call amovr (in, out, ndim)
	call msc_c2tranr (wcs, ct, in[1], in[2], out[1], out[2])
end

procedure msc_c2tranr (wcs, ct, x, y, xt, yt)

pointer	wcs			#I WCS pointer
int	ct			#I Coordinate system index
real	x, y                    #I initial position
real	xt, yt                  #O transformed position

double	dxt, dyt

begin
	if (ct < 1 || ct > WCS_NCT)
	    call error (1, "Coordinate system index out of bounds")

	call msc_c2trand (wcs, ct, double(x), double(y), dxt, dyt)
	xt = dxt
	yt = dyt
end


# MSC_C2TRAND -- Evaluate the coordinates using the full transformation.

procedure msc_c2trand (wcs, ct, x, y, xt, yt)

pointer	wcs			#I WCS pointer
int	ct			#I Coordinate system index
double  x, y                    #I initial position
double  xt, yt                  #O transformed position

double	x1, y1, x2, y2

begin
	if (ct < 1 || ct > WCS_NCT)
	    call error (1, "Coordinate system index out of bounds")

	x2 = x
	y2 = y

	switch (WCS_DIR(wcs,ct)) {
	case L2A, P2A:
	    call mw_c2trand (WCS_CT(wcs,ct), x2, y2, x1, y1)
	    call mw_c2trand (WCS_CTP(wcs,ct), x1, y1, xt, yt)
	    xt = xt * 3600.
	    yt = yt * 3600.
	case A2L, A2P:
	    x1 = x2 / 3600.
	    y1 = y2 / 3600.
	    call mw_c2trand (WCS_CTP(wcs,ct), x1, y1, x1, y1)
	    call mw_c2trand (WCS_CT(wcs,ct), x1, y1, xt, yt)
	case W2A:
	    call mw_c2trand (WCS_CTP(wcs,ct), x2, y2, xt, yt)
	    xt = xt * 3600.
	    yt = yt * 3600.
	case A2W:
	    x1 = x2 / 3600.
	    y1 = y2 / 3600.
	    call mw_c2trand (WCS_CTP(wcs,ct), x1, y1, xt, yt)
	default:
	    call mw_c2trand (WCS_CT(wcs,ct), x2, y2, xt, yt)
	}
end


int procedure msc_wcsstati (wcs, param)

pointer	wcs		#I WCS pointer
char	param[ARB]	#I Parameter

bool	streq()

begin
	if (streq (param, "mw"))
	    return (WCS_MW(wcs))
	else if (streq (param, "projection"))
	    return (WCS_MWP(wcs))
	else
	    call error (1, "msc_wcsstati: unknown parameter")
end


double procedure msc_wcsstatd (wcs, param)

pointer	wcs		#I WCS pointer
char	param[ARB]	#I Parameter

double	r[2], w[2], cd[2,2], scale
bool	streq()

begin
	if (streq (param, "scale")) {
	    call mw_gwtermd (WCS_MW(wcs), r, w, cd, 2)
	    scale = sqrt((cd[1,1]**2+cd[2,1]**2+cd[1,2]**2+cd[2,2]**2)/2.)
	    return (scale * 3600.)
	} else if (streq (param, "crpix1")) {
	    call mw_gwtermd (WCS_MW(wcs), r, w, cd, 2)
	    return (r[1])
	} else if (streq (param, "crpix2")) {
	    call mw_gwtermd (WCS_MW(wcs), r, w, cd, 2)
	    return (r[2])
	} else
	    call error (1, "msc_wcsstatd: unknown parameter")
end
