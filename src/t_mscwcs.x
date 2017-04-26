include	<error.h>
include	<syserr.h>
include	<math.h>
include	<imhdr.h>
include	<imset.h>

# T_MSCWCS -- Adjust the WCS for shift, scale, and rotation.

procedure t_mscwcs ()

pointer	images			# List of Mosaic images
double	ra_shift		# RA shift (arc sec)
double	dec_shift		# Dec shift (arc sec)
double	ra_mag			# RA magnification
double	dec_mag			# Dec magnification
double	ra_rot			# RA rotation change
double	dec_rot			# Dec rotation change
int	fwd			# Forward transformation?
int	ngrid			# Number of grid points
int	xxo, xyo, yxo, yyo	# CCMAP orders

int	extlist
pointer	sp, image
pointer	mw

bool	clgetb()
int	clgeti(), btoi(), errcode()
double	clgetd()
int	xt_imextns(), imtgetim()
pointer	wcs_trans()
errchk	wcs_trans, wcs_adjust

begin
	call smark (sp)
	call salloc (images, SZ_LINE, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	call clgstr ("images", Memc[images], SZ_LINE)
	ra_shift = clgetd ("ra_shift")
	dec_shift = clgetd ("dec_shift")
	ra_mag = clgetd ("ra_mag")
	dec_mag = clgetd ("dec_mag")
	ra_rot = clgetd ("ra_rot")
	dec_rot = clgetd ("dec_rot")
	fwd = btoi (clgetb ("forward"))

	ngrid = clgeti ("ngrid")
	xxo = clgeti ("xxorder")
	xyo = clgeti ("xyorder")
	yxo = clgeti ("yxorder")
	yyo = clgeti ("yyorder")

	mw = wcs_trans (ra_shift, dec_shift, ra_mag, dec_mag, ra_rot, dec_rot)

	extlist = xt_imextns (Memc[images], "0-", "", "", YES, NO, NO, "", NO)
	while (imtgetim (extlist, Memc[image], SZ_FNAME) != EOF) {
	    iferr (call wcs_adjust (Memc[image], mw, fwd, ngrid,
		xxo, xyo, yxo, yyo)) {
		switch (errcode()) {
		case SYS_MWNDIM:
		    ;
		default:
		    call erract (EA_WARN)
		}
	    }
	}
	call imtclose (extlist)

	if (mw != NULL)
	    call mw_close (mw)

	call sfree (sp)
end


# WCS_TRANS - Compute transformation to astrometry coordinates consisting of
# a shift, magnification, and rotation.

pointer procedure wcs_trans (ra_shift, dec_shift, ra_mag, dec_mag,
	ra_rot, dec_rot)

double	ra_shift		#I RA shift (arc sec)
double	dec_shift		#I Dec shift (arc sec)
double	ra_mag			#I RA magnification
double	dec_mag			#I Dec magnification
double	ra_rot			#I RA rotation (deg)
double	dec_rot			#I Dec rotation (deg)

double	r[2], w[2], cd[2,2]
pointer	mw, mw_open()

begin
	r[1] = 0.
	r[2] = 0.
	w[1] = ra_shift
	w[2] = dec_shift
	cd[1,1] = ra_mag * cos (DEGTORAD(ra_rot))
	cd[2,1] = dec_mag * sin (DEGTORAD(dec_rot))
	cd[1,2] = -ra_mag * sin (DEGTORAD(ra_rot))
	cd[2,2] = dec_mag * cos (DEGTORAD(dec_rot))

	mw = mw_open (NULL, 2)
	call mw_newsystem (mw, "world", 2)
	call mw_swtype (mw, 1, 1, "linear", "")
	call mw_swtype (mw, 2, 1, "linear", "")
	call mw_swtermd (mw, r, w, cd, 2)

	return (mw)
end


# WCS_ADJUST - Create a database by sampling the image WCS and calling CCMAP.

procedure wcs_adjust (image, mw, fwd, ngrid, xxo, xyo, yxo, yyo)

char	image[ARB]		#I Image to adjust
pointer	mw			#I Astrometry coordinate transformation
int	fwd			#I Forward transformation?
int	ngrid			#I Number of grid points
int	xxo, xyo, yxo, yyo	#I CCMAP orders

int	i, j, nxgrid, nygrid, fd
double	r[2], w[2], cd[2,2], shift[2], a[2]
pointer	sp, fname, pname, proj, cmd
pointer	im, mw1, wcs, ct

bool	fp_equald()
int	open(), stropen()
pointer	immap(), mw_openim(), msc_openim(), msc_sctran(), mw_sctran()
errchk	immap, mw_openim, msc_openim, open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (pname, SZ_FNAME, TY_CHAR)
	call salloc (proj, SZ_FNAME, TY_CHAR)
	call salloc (cmd, 2*SZ_LINE, TY_CHAR)

	# Check for simple shift.
	call mw_gwtermd (mw, r, shift, cd, 2)
	if (fp_equald(cd[1,1],1D0) && fp_equald(cd[2,1],0D0) &&
	    fp_equald(cd[1,2],0D0) && fp_equald(cd[1,1],1D0)) {
	    im = immap (image, READ_WRITE, 0)
	    mw1 = mw_openim (im)
	    call mw_gwtermd (mw1, r, w, cd, 2)
	    if (fwd == YES) {
		w[1] = w[1] + shift[1] / 3600. / cos (DEGTORAD(w[2]))
		w[2] = w[2] + shift[2] / 3600.
	    } else {
		w[2] = w[2] - shift[2] / 3600.
		w[1] = w[1] - shift[1] / 3600. / cos (DEGTORAD(w[2]))
	    }
	    call mw_swtermd (mw1, r, w, cd, 2)
	    call mw_saveim (mw1, im)
	    call msc_close (wcs)
	    call imunmap (im)
	    call sfree (sp)
	    return
	}

	im = immap (image, READ_WRITE, 0)
	mw1 = msc_openim (im, wcs)
	ct = msc_sctran (wcs, 1, "logical", "physical", 3)
	ct = msc_sctran (wcs, 2, "physical", "astrometry", 3)
	ct = msc_sctran (wcs, 3, "astrometry", "world", 3)
	if (fwd == YES)
	    ct = mw_sctran (mw, "physical", "world", 3)
	else
	    ct = mw_sctran (mw, "world", "physical", 3)

	if (IM_LEN(im,1) < IM_LEN(im,2)) {
	    nxgrid = max(1,
		nint (sqrt(real(ngrid)*IM_LEN(im,1)/IM_LEN(im,2))+0.5))
	    nygrid = max (1, nxgrid * IM_LEN(im,2) / IM_LEN(im,1))
	} else {
	    nygrid = max(1,
		nint (sqrt(real(ngrid)*IM_LEN(im,2)/IM_LEN(im,1))+0.5))
	    nxgrid = max (1, nygrid * IM_LEN(im,1) / IM_LEN(im,2))
	}
	nxgrid = max (xxo+1, yxo+1, nxgrid)
	nygrid = max (xyo+1, yyo+1, nygrid)

	call mktemp ("tmp$iraf", Memc[fname], SZ_FNAME)
	fd = open (Memc[fname], NEW_FILE, TEXT_FILE)
	do j = 1, nygrid {
	    do i = 1, nxgrid {
		r[1] = nint (1. + (i - 1) * (IM_LEN(im,1) - 1.) / (nxgrid - 1.))
		r[2] = nint (1. + (j - 1) * (IM_LEN(im,2) - 1.) / (nygrid - 1.))
		call msc_c2trand (wcs, 1, r[1], r[2], r[1], r[2])
		call msc_c2trand (wcs, 2, r[1], r[2], a[1], a[2])
		call mw_c2trand (ct, a[1], a[2], w[1], w[2])
		call msc_c2trand (wcs, 3, w[1], w[2], w[1], w[2])
		call fprintf (fd, "%g %g %g %g\n")
		    call pargd (r[1])
		    call pargd (r[2])
		    call pargd (w[1])
		    call pargd (w[2])
	    }
	}
	call close (fd)

	call mw_gwattrs (mw1, 1, "wtype", Memc[proj], SZ_FNAME)
	if (Memc[proj] == 'z') {
	    call mktemp (Memc[proj], Memc[pname], SZ_FNAME)
	    fd = open (Memc[pname], NEW_FILE, TEXT_FILE)
	    call fprintf (fd, "%s\n")
	        call pargstr (Memc[proj])
	    do i = 0, 9 {
		call sprintf (Memc[cmd], SZ_LINE, "projp%d")
		    call pargi (i)
		ifnoerr (call mw_gwattrs (mw1, 1, Memc[cmd],
		    Memc[proj], SZ_FNAME)) {
		    call fprintf (fd, "%s %s\n")
			call pargstr (Memc[cmd])
			call pargstr (Memc[proj])
		}
	    }
	    call close (fd)
	    call strcpy (Memc[pname], Memc[proj], SZ_FNAME)
	} else
	    Memc[pname] = EOS

	call mw_gwtermd (mw1, r, w, cd, 2)
	call mw_c2trand (ct, 0D0, 0D0, w[1], w[2])
	call mw_ctfree (ct)
	call msc_c2trand (wcs, 3, w[1], w[2], w[1], w[2])
	call msc_close (wcs)
	iferr (call imdelf (im, "wcssol"))
	    ;
	call imunmap (im)

	fd = stropen (Memc[cmd], 2*SZ_LINE, NEW_FILE)
	call fprintf (fd,
	    "ccmap input=%s database='dev$null' sol='' im=%s results=''")
	    call pargstr (Memc[fname])
	    call pargstr (image)
	call fprintf (fd, " xc=1 yc=2 lngc=3 latc=4")
	call fprintf (fd, " xmin=INDEF xmax=INDEF ymin=INDEF ymax=INDEF")
	call fprintf (fd, " lngu=degrees latu=degrees insys=j2000")
	call fprintf (fd,
	    " refp=user lngref=%g latref=%g refsys=INDEF lngrefu='' latrefu=''")
	    call pargd (w[1])
	    call pargd (w[2])
	call fprintf (fd, " proj=%s fitg=general func=polynomial")
	    call pargstr (Memc[proj])
	call fprintf (fd,
	    " xxorder=%d xyorder=%d yxorder=%d yyorder=%d xxt=%s yxt=%s")
	    call pargi (xxo)
	    call pargi (xyo)
	    call pargi (yxo)
	    call pargi (yyo)
	    call pargstr ("half")
	    call pargstr ("half")
	call fprintf (fd,
	    " rej=INDEF upd=yes pixsys=physical verb=no inter=no")
	call close (fd)

	call clcmdw (Memc[cmd])

	call delete (Memc[fname])
	if (Memc[pname] != EOS)
	    call delete (Memc[pname])

	call sfree (sp)
end
