include <error.h>
include <imhdr.h>
include <fset.h>
include <imset.h>
include <mach.h>
include "mosim.h"
include "mosgeom.h"
include "mosproc.h"
include "display.h"
include "gwindow.h"

# ZCOMBINE options.
define  ZC_DICT         "|none|auto|minmax|average|median|"
define  ZC_NONE         1
define  ZC_AUTO         2
define  ZC_MINMAX       3
define  ZC_AVERAGE      4
define  ZC_MEDIAN       5

# ADU to/from e- options.
define  UNCORRECT       0
define  CORRECT         1


# T_MSCRTDISPLAY -- Quick-look real-time mosaic display task.
# This version checks for lines with no data and iterates waiting
# for more data to appear.

procedure t_mscrtdisplay ()

bool    zmap, firsttime
int     i, j, k, wcsver, frame, ninput, nproc, zcom, select
int	nx[2], ny[2]
int	nit, sleep, y1last, y2last, ndone, stalled
real    xc, yc, xs[2], ys[2], a, b, c, d, tx, ty, zz1, zz2
pointer sp, image, image1, title, procstr, imtitle, wcs, y1, y2, done
pointer	z1, z2, wdes, mi, mg, cmg, ds, im, wipix, wdpix, wnwin, wdwin

bool    clgetb(), fp_equalr(), streq(), hdmflag()
int     clgeti(), clgwrd(), btoi(), access(), imstati(), imd_wcsver()
int	imtlen(), imtgetim()
real    clgetr(), ahivr(), alovr(), asumr(), amedr()
pointer mimap(), immap(), imd_mapframe1(), iw_open(), imtopenp()

include "mosproc.com"

begin
	call tsleep (clgeti ("wait"))

        call smark (sp)
        call salloc (image, SZ_LINE, TY_CHAR)
        call salloc (image1, SZ_LINE, TY_CHAR)
        call salloc (title, SZ_LINE, TY_CHAR)
        call salloc (procstr, SZ_LINE, TY_CHAR)
        call salloc (imtitle, SZ_LINE, TY_CHAR)
        call salloc (wcs, SZ_LINE, TY_CHAR)

        # Set instrument, amplifier, and process information.
        #call clgstr ("instrument",  Memc[image1],  SZ_LINE)
	Memc[image1] = EOS
        call hdmopen (Memc[image1])
        call ampset()
        call procset()

	# Initialize multiple mappings.
	wcsver = imd_wcsver()

	# Get image.
	im = imtopenp ("image")
	if (imtlen (im) != 1)
	    call error (1, "Only one image may be displayed")
	i = imtgetim (im, Memc[image], SZ_LINE)
	call imtclose (im)

        frame = clgeti ("frame")
	if ((wcsver == 0 && frame > 4) || frame > 16)
	    call error (1, "Frame number too large for display server")
	select = btoi (clgetb ("select_frame"))
	nit = clgeti ("niterate")
	sleep = clgeti ("sleep")

	call sprintf (Memc[wcs], SZ_LINE, "uparm$mscdisp%d")
	    call pargi (frame)

	# Check if already loaded.
	if (clgetb ("check")) {
	    if (access (Memc[wcs], 0, 0) == YES) {
		ds = imd_mapframe1 (frame, READ_ONLY, select, NO)
		im = iw_open (ds, frame, Memc[image1], SZ_LINE, i)
		call iw_close (im)
		call imunmap (ds)
		if (i != ERR) {
		    if (streq (Memc[image], Memc[image1])) {
			call miunmap (mi)
			call hdmclose ()
			call ampfree()
			call sfree (sp)
			return
		    }
		}
	    }
	}

        # Map mosaic image.
        iferr (mi = mimap (Memc[image], READ_ONLY, 0)) {
            call sfree (sp)
            call hdmclose()
            call erract (EA_ERROR)
        }
        ninput = MI_NIMS(mi)
        cmg    = MI_CMG(mi)

	# Set real time data checking.
	do i = 1, ninput
	    CKNODATA(MI_MG(mi,i)) = YES

        # Allocate memory for each input image.
        call salloc (done, ninput, TY_INT)
        call salloc (y1, ninput, TY_INT)
        call salloc (y2, ninput, TY_INT)
        call salloc (z1, ninput, TY_REAL)
        call salloc (z2, ninput, TY_REAL)
        call salloc (wdes, ninput+1, TY_POINTER)
        do i = 0, ninput {
            call salloc (Memi[wdes+i], LEN_WDES, TY_STRUCT)
            call aclri (Memi[Memi[wdes+i]], LEN_WDES)
        }

        # Convert default z values given in e- to uncorrected (input) ADU.
        call amovkr (clgetr ("z1"), Memr[z1], ninput)
        call amovkr (clgetr ("z2"), Memr[z2], ninput)
        call zproc (Memi[MI_MGS(mi)], Memr[z1], Memr[z2], ninput, proc,
            UNCORRECT)

        # Open display without erasing yet.
        ds = imd_mapframe1 (frame, READ_ONLY, NO, NO)

        # Determine parameters for the output display.
	if (clgetb ("fill"))
	    call mos_params (NULL, ds, Memi[wdes], 1, NX(cmg), 1, 1, NY(cmg), 1,
	        0.5, 0.5, 1.0, 1.0, 0., 0.)
	else {
	    mg = MI_MG(mi,1)
	    xs[1] = real (NX(cmg)) / abs (DX(mg)) / IM_LEN(ds,1)
	    ys[1] = real (NY(cmg)) / abs (DY(mg)) / IM_LEN(ds,2)
	    i = max (1000 - int (1000. - xs[1]), 1000 - int (1000. - ys[1]))
	    xs[1] = xs[1] / i
	    ys[1] = ys[1] / i
	    call mos_params (NULL, ds, Memi[wdes], 1, NX(cmg), 1, 1, NY(cmg), 1,
		0.5, 0.5, xs, ys, 0., 0.)
	}

	firsttime = true
	stalled = 0
	repeat {
	    # Determine parameters for each input image.
	    nproc = 0
	    do i = 1, ninput {
		mg = MI_MG(mi,i)
		im = MI_IM(mi,i)

		# Set display window for image.
		call mos_comap (Memi[wdes], mg, cmg, xc, yc, xs, ys)

		# Set display parameters for image.
		call mos_params (mg, ds, Memi[wdes+i], DX1(mg), DX2(mg),
		    abs(DX(mg)), DY1(mg), DY2(mg), abs(DY(mg)),
		    xc, yc, xs, ys, Memr[z1+i-1], Memr[z2+i-1])

		if (PROC(mg) == YES)
		    nproc = nproc + 1

		# Initialize to display the whole image.
		Memi[y1+i-1] = INDEFI
		Memi[y2+i-1] = INDEFI
		Memi[done+i-1] = NO
	    }

	    # Combine the z levels if needed.  Do things in e-.
	    call zproc (Memi[MI_MGS(mi)], Memr[z1], Memr[z2], ninput, proc,
		CORRECT)
	    zz1 = asumr (Memr[z1], ninput) / ninput
	    zz2 = asumr (Memr[z2], ninput) / ninput
	    zmap = false
	    do i = 1, ninput
		if (!fp_equalr(Memr[z1+i-1],zz1) ||
		    !fp_equalr(Memr[z2+i-1],zz2)) {
		    zmap = true
		    break
		}
	    if (zmap) {
		zcom = clgwrd ("zcombine", Memc[image1], SZ_LINE, ZC_DICT)
		if (zcom == ZC_AUTO) {
		    zcom = ZC_MINMAX
		    do i = 1, ninput {
			mg = MI_MG(mi,i)
			im = MI_IM(mi,i)
			if (!hdmflag (im, "flatcor")) {
			    if (PROC(mg) == NO ||
				(PROC(mg) == YES && DOFLAT(mg) == NO)) {
				zcom = ZC_NONE
				break
			    }
			}
		    }
		}
		if (zcom != ZC_NONE) {
		    switch (zcom) {
		    case ZC_MINMAX:
			zz1 = alovr (Memr[z1], ninput)
			zz2 = ahivr (Memr[z2], ninput)
		    case ZC_AVERAGE:
			zz1 = asumr (Memr[z1], ninput) / ninput
			zz2 = asumr (Memr[z2], ninput) / ninput
		    case ZC_MEDIAN:
			zz1 = amedr (Memr[z1], ninput)
			zz2 = amedr (Memr[z2], ninput)
		    }

		    # Now set the values for display.
		    call zproc (Memi[MI_MGS(mi)], zz1, zz2, 1, proc, UNCORRECT)
		    do i = 1, ninput {
			wdwin = W_WC(Memi[wdes+i],W_DWIN)
			W_ZS(wdwin) = zz1
			W_ZE(wdwin) = zz2
		    }
		    call zproc (Memi[MI_MGS(mi)], zz1, zz2, 1, proc, CORRECT)
		}
	    }

	    # Print out some useful information
	    call printf ("\n\n")
	    call mos_info (mi, wdes, zcom, Memr[z1], Memr[z2], STDOUT)

	    if (firsttime) {
		# Print tile information.
		call msctile (mi, Memc[wcs], frame, wcsver)

		# Now we're ready to write to the display.
		call imunmap (ds)
		ds = imd_mapframe1 (frame, READ_WRITE, select,
		    btoi (clgetb ("erase")))

		# Set WCS.
		do k = 1, ninput+1 {
		    i = mod (k, ninput+1)
		    j = max (i, 1)

		    wipix = W_WC(Memi[wdes+i],W_IPIX)
		    wdpix = W_WC(Memi[wdes+i],W_DPIX)
		    wnwin = W_WC(Memi[wdes+i],W_NWIN)
		    wdwin = W_WC(Memi[wdes+i],W_DWIN)

		    # Define mapping from image pixels to display pixels.
		    xs[1] = W_XS(wipix)
		    ys[1] = W_YS(wipix)
		    nx[1] = W_XE(wipix) - W_XS(wipix) + 1
		    ny[1] = W_YE(wipix) - W_YS(wipix) + 1

		    xs[2] = W_XS(wdpix)
		    ys[2] = W_YS(wdpix)
		    nx[2] = W_XE(wdpix) - W_XS(wdpix) + 1
		    ny[2] = W_YE(wdpix) - W_YS(wdpix) + 1

		    if (i > 0) {
			call imstats (MI_IM(mi,i), IM_IMAGENAME, Memc[image1],
			    SZ_LINE)
			iferr (call imgstr (MI_IM(mi,i), "EXTNAME", Memc[wcs],
			    SZ_LINE)) {
			    call sprintf (Memc[wcs], SZ_LINE, "%d")
				call pargi (i)
			}
		    } else {
			call strcpy (Memc[MI_RNAME(mi)], Memc[image1], SZ_LINE)
			call strcpy ("mosaic", Memc[wcs], SZ_LINE)
		    }
		    call fpathname (Memc[image1], Memc[image1], SZ_LINE)
		    call imd_setmapping (Memc[wcs], xs[1], ys[1], nx[1], ny[1],
			nint(xs[2]), nint(ys[2]), nx[2], ny[2], Memc[image1])

		    # Define linear pixel WCS.
		    a  = (W_XE(wdwin)-W_XS(wdwin))/((W_XE(wnwin)-W_XS(wnwin))*
			IM_LEN(ds,1))
		    b  = 0.0
		    c  = 0.0
		    d  = (W_YE(wdwin)-W_YS(wdwin))/((W_YE(wnwin)-W_YS(wnwin))*
			IM_LEN(ds,2))
		    tx = W_XS(wdwin) - a * (W_XS(wnwin) * IM_LEN(ds,1))
		    ty = W_YS(wdwin) - d * (W_YS(wnwin) * IM_LEN(ds,2))

		    # Y-flip (origin at upper left in display window).
		    d = -d
		    ty = W_YE(wdwin) - d * ((1.0 - W_YE(wnwin)) * IM_LEN(ds,2))

		    # Translate screen corner to the center of the screen pixel.
		    tx = tx + 0.5 * a
		    ty = ty + 0.5 * d

		    # Set origin to mosaic coordinates.
		    tx = tx + CX1(cmg) - 1
		    ty = ty + CY1(cmg) - 1

		    # Set the title and WCS.
		    if (nproc > 0) {
			mg = MI_MG(mi,j)
			if (DOBIAS(mg)==YES || DOZERO(mg)==YES ||
			    DOFLAT(mg)==YES) {
			    Memc[title] = EOS
			    if (DOBIAS(mg) == YES) {
				call sprintf (Memc[procstr], SZ_LINE, ",bias")
				call strcat (Memc[procstr], Memc[title],
				    SZ_LINE)
			    }
			    if (DOZERO(mg) == YES) {
				call sprintf (Memc[procstr], SZ_LINE,
				    ",zero=%s")
				    call pargstr (ZERONAME(mg))
				call strcat (Memc[procstr], Memc[title],
				    SZ_LINE)
			    }
			    if (DOFLAT(mg) == YES) {
				call sprintf (Memc[procstr], SZ_LINE,
				    ",flat=%s")
				    call pargstr (FLATNAME(mg))
				call strcat (Memc[procstr], Memc[title],
				    SZ_LINE)
			    }
			    call sprintf (Memc[procstr], SZ_LINE, "] %s")
				call pargstr (IM_TITLE(MI_IM(mi,j)))
			    call strcat (Memc[procstr], Memc[title], SZ_LINE)
			    Memc[title] = '['
			} else {
			    call sprintf (Memc[title], SZ_LINE, "[process] %s")
				call pargstr (IM_TITLE(MI_IM(mi,j)))
			}
		    } else
			call strcpy (IM_TITLE(MI_IM(mi,j)), Memc[title],
			    SZ_LINE)
		    call imd_putwcs (ds, frame, Memc[MI_RNAME(mi)], Memc[title],
			a, b, c, d, tx, ty, zz1, zz2,
			W_ZT(W_WC(Memi[wdes+j],W_DWIN)))
		}

		firsttime = false
	    }

	    # Now display the images.
	    for (j=1; j<=nit; j=j+1) {
		ndone = 0
		do i = 1, ninput {
		    if (Memi[done+i-1] == NO) {
			mg = MI_MG(mi,i)
			im = MI_IM(mi,i)
			#call imstats (im, IM_IMAGENAME, Memc[image1], SZ_LINE)
			#call ds_setwcs (im, ds, Memi[wdes+i], Memc[image1],
			#    frame)
			y1last = Memi[y1+i-1]
			y2last = Memi[y2+i-1]
			call msc_rtload_display (mg, ds, Memi[wdes+i],
			    Memi[y1+i-1], Memi[y2+i-1], Memi[done+i-1])
			if (Memi[y1+i-1] == y1last && Memi[y2+i-1] == y2last)
			    stalled = stalled + 1
			else
			    stalled = 0
		    }
		    if (Memi[done+i-1] == YES)
			ndone = ndone + 1
		    else {
			call imseti (im, IM_CANCEL, 0)
			call fseti (imstati (im, IM_PIXFD), F_CANCEL, 0)
		    }
		}
		if (ndone == ninput)
		    break
		if (stalled / ninput > 0) {
		    if (stalled / ninput > 10)
			break
		    call tsleep (stalled / ninput)
		} else if (ndone == 0) {
		    if (j < nit)
			call tsleep (max(1,sleep))
		} else {
		    call tsleep (1)
		    j = j - 1
		}
	    }
	    if (ndone == ninput || stalled / ninput > 0)
		break
	}

	# Check the title and try several times if there is no title.
	if (Memc[imtitle] == EOS) {
	    call sprintf (Memc[image1], SZ_LINE, "%s[1]")
		call pargstr (Memc[image])
	    do i = 0, 3 {
		call tsleep (i)
		call imunmap (MI_IM(mi,1))
		MI_IM(mi,1) = immap (Memc[image1], READ_ONLY, 0)
		call strcpy (IM_TITLE(MI_IM(mi,1)), Memc[imtitle], SZ_LINE)
		if (Memc[imtitle] != EOS)
		    break
	    }
	    if (nproc > 0) {
		mg = MI_MG(mi,1)
		if (DOBIAS(mg) == YES && DOFLAT(mg) == YES) {
		    call sprintf (Memc[title], SZ_LINE,
			"[bias,flat=%s] %s")
			call pargstr (FLATNAME(mg))
			call pargstr (Memc[imtitle])
		} else if (DOBIAS(mg) == YES) {
		    call sprintf (Memc[title], SZ_LINE, "[bias] %s")
			call pargstr (Memc[imtitle])
		} else if (DOFLAT(mg) == YES) {
		    call sprintf (Memc[title], SZ_LINE, "[flat=%s] %s")
			call pargstr (FLATNAME(mg))
			call pargstr (Memc[imtitle])
		} else {
		    call sprintf (Memc[title], SZ_LINE, "[process] %s")
			call pargstr (Memc[imtitle])
		}
	    } else
		call strcpy (Memc[imtitle], Memc[title], SZ_LINE)
	    call imd_putwcs (ds, frame, Memc[MI_RNAME(mi)], Memc[title],
		a, b, c, d, tx, ty, zz1, zz2, W_ZT(W_WC(Memi[wdes+1],1)))
	}

        # Tidy up
        do i = 0, ninput {
            do j = 0, W_MAXWC
                if (W_UPTR(W_WC(Memi[wdes+i],j)) != NULL)
                    call ds_ulutfree (W_UPTR(W_WC(Memi[wdes+i],j)))
        }
        call miunmap (mi)
        call imunmap (ds)
        call hdmclose ()
        call ampfree()
        call sfree (sp)
end


# MSC_LOAD_DISPLAY -- Map an image into the display window.  In general this
#   involves independent linear transformations in the X, Y, and Z (greyscale)
#   dimensions.  If a spatial dimension is larger than the display window then
#   the image is block averaged.  If a spatial dimension or a block averaged
#   dimension is smaller than the display window then linear interpolation is
#   used to expand the image.  Both the input image and the output device appear
#   to us as images, accessed via IMIO.  All spatial scaling is 
#   handled by the "scaled input" package, i.e., SIGM2[SR].  Our task is to
#   get lines from the scaled input image, transform the greyscale if necessary,
#   and write the lines to the output device.

procedure msc_rtload_display (mg, ds, wdes, y1, y2, done)

pointer	mg			# input image
pointer	ds			# output image
pointer	wdes			# graphics window descriptor
int	y1, y2			# last data displayed
int	done			# done?

real	z1, z2, dz1, dz2, px1, px2, py1, py2
int	i, order, zt, wx1, wx2, wy1, wy2, wy, nx, ny, xblk, yblk
pointer	wdwin, wipix, wdpix, ovrly, bpm, pm, uptr
pointer	im, in, out, si, si_ovrly, si_bpovrly, ocolors, bpcolors, rtemp
pointer	sp, fname
bool	unitary_greyscale_transformation
short	lut1, lut2, dz1_s, dz2_s, z1_s, z2_s

real	logerrfcn()
bool	fp_equalr()
int	imstati(), maskcolor()
pointer	yt_pmmap(), imps2s(), imps2r()
pointer	yigm2s(), zigm2_setup(), zigm2s(), zigm2r()
errchk	yt_pmmap, imps2s, imps2r, yigm2s, zigm2_setup, zigm2s, zigm2r

extern	logerrfcn

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	im = MG_IM(mg)
	wdwin = W_WC(wdes,W_DWIN)
	wipix = W_WC(wdes,W_IPIX)
	wdpix = W_WC(wdes,W_DPIX)

	# Set image and display pixels.
	px1 = nint (W_XS(wipix))
	px2 = nint (W_XE(wipix))
	py1 = nint (W_YS(wipix))
	py2 = nint (W_YE(wipix))
	wx1 = nint (W_XS(wdpix))
	wx2 = nint (W_XE(wdpix))
	wy1 = nint (W_YS(wdpix))
	wy2 = nint (W_YE(wdpix))
	if (IS_INDEFI(y1)) {
	    y2 = wy1 - 1
	    y1 = wy2 + 1
	}

	z1 = W_ZS(wdwin)
	z2 = W_ZE(wdwin)
	zt = W_ZT(wdwin)
	uptr = W_UPTR(wdwin)
	order = max (W_XT(wdwin), W_YT(wdwin))

	# Setup scaled input and masks.
	si = NULL
	si_ovrly = NULL
	si_bpovrly = NULL
	nx = wx2 - wx1 + 1
	ny = wy2 - wy1 + 1
	xblk = INDEFI
	yblk = INDEFI

	ocolors = W_OCOLORS(wdes)
	iferr (ovrly = yt_pmmap (W_OVRLY(wdes), im, Memc[fname], SZ_FNAME)) {
	    call erract (EA_WARN)
	    ovrly = NULL
	}
	if (ovrly != NULL) {
	    xblk = INDEFI
	    yblk = INDEFI
	    si_ovrly = zigm2_setup (ovrly, NULL, px1,px2,nx,xblk,
		py1,py2,ny,yblk, -1)
	}

	bpcolors = W_BPCOLORS(wdes)
	switch (W_BPDISP(wdes)) {
	case BPDNONE:
	    si = zigm2_setup (im, NULL, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	case BPDOVRLY:
	    si = zigm2_setup (im, NULL, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	    iferr (bpm = yt_pmmap (W_BPM(wdes), im, Memc[fname], SZ_FNAME))
		bpm = NULL
	    if (bpm != NULL)
		si_bpovrly = zigm2_setup (bpm, NULL, px1,px2,nx,xblk,
		    py1,py2,ny,yblk, -1)
	case BPDINTERP:
	    iferr (bpm = yt_pmmap (W_BPM(wdes), im, Memc[fname], SZ_FNAME))
		bpm = NULL
	    if (bpm != NULL)
		pm = imstati (bpm, IM_PMDES)
	    else
		pm = NULL
	    si = zigm2_setup (im, pm, px1,px2,nx,xblk, py1,py2,ny,yblk, order)
	}

	# The device IM_MIN and IM_MAX parameters define the acceptable range
	# of greyscale values for the output device (e.g., 0-255 for most 8-bit
	# display devices).  Values Z1 and Z2 are mapped linearly or
	# logarithmically into IM_MIN and IM_MAX.

	dz1 = IM_MIN(ds)
	dz2 = IM_MAX(ds)
	if (fp_equalr (z1, z2)) {
	    z1 = z1 - 1
	    z2 = z2 + 1
	}

	# If the user specifies the transfer function, verify that the
	# intensity and greyscale are in range.

	if (zt == W_USER) {
	    call alims (Mems[uptr], U_MAXPTS, lut1, lut2)
	    dz1_s = short (dz1)
	    dz2_s = short (dz2)
	    if (lut2 < dz1_s || lut1 > dz2_s)
		call eprintf ("User specified greyscales out of range\n")
	    if (z2 < IM_MIN(im) || z1 > IM_MAX(im))
		call eprintf ("User specified intensities out of range\n")
	}

	# Type short pixels are treated as a special case to minimize vector
	# operations for such images (which are common).  If the image pixels
	# are either short or real then only the ALTR (greyscale transformation)
	# vector operation is required.  The ALTR operator linearly maps
	# greylevels in the range Z1:Z2 to DZ1:DZ2, and does a floor ceiling
	# of DZ1:DZ2 on all pixels outside the range.  If unity mapping is
	# employed the data is simply copied, i.e., floor ceiling constraints
	# are not applied.  This is very fast and will produce a contoured
	# image on the display which will be adequate for some applications.

	if (zt == W_UNITARY) {
	    unitary_greyscale_transformation = true
	} else if (zt == W_LINEAR) {
	    unitary_greyscale_transformation =
		(fp_equalr(z1,dz1) && fp_equalr(z2,dz2))
	} else
	    unitary_greyscale_transformation = false

	if (y2 >= wy1 || y1 > wy2) {		# Display from the first line
	    if (IM_PIXTYPE(im) == TY_SHORT && PROC(mg) == NO && zt != W_LOG) {
		z1_s = z1;  z2_s = z2
		if (z1_s == z2_s) {
		    z1_s = z1_s - 1
		    z2_s = z2_s + 1
		}

		for (wy=y2+1;  wy <= wy2;  wy=wy+1) {
		    in  = zigm2s (mg, si, wy - wy1 + 1)
		    if (NODATA(mg) == YES)
			break
		    out = imps2s (ds, wx1, wx2, wy, wy)

		    if (unitary_greyscale_transformation) {
			call amovs (Mems[in], Mems[out], nx)
		    } else if (zt == W_USER) {
			dz1_s = U_Z1; dz2_s = U_Z2
			call amaps (Mems[in],Mems[out],nx, z1_s,z2_s,
			     dz1_s, dz2_s)
			call aluts (Mems[out], Mems[out], nx, Mems[uptr])
		    } else {
			dz1_s = dz1; dz2_s = dz2
			call amaps (Mems[in],Mems[out],nx, z1_s,z2_s,
			    dz1_s, dz2_s)
		    }

		    if (si_ovrly != NULL) {
			in = yigm2s (si_ovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Mems[out+i] = maskcolor (ocolors,
				    int(Mems[in+i]))
			}
		    }
		    if (si_bpovrly != NULL) {
			in = yigm2s (si_bpovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Mems[out+i] = maskcolor (bpcolors,
				    int(Mems[in+i]))
			}
		    }
		}

	    } else if (zt == W_USER) {
		call salloc (rtemp, nx, TY_REAL)

		for (wy=y2+1;  wy <= wy2;  wy=wy+1) {
		    in  = zigm2r (mg, si, wy - wy1 + 1)
		    if (NODATA(mg) == YES)
			break
		    out = imps2s (ds, wx1, wx2, wy, wy)

		    call amapr (Memr[in], Memr[rtemp], nx, z1, z2, 
			real(U_Z1), real(U_Z2))
		    call achtrs (Memr[rtemp], Mems[out], nx)
		    call aluts (Mems[out], Mems[out], nx, Mems[uptr])

		    if (si_ovrly != NULL) {
			in = yigm2s (si_ovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Mems[out+i] = maskcolor (ocolors,
				     int(Mems[in+i]))
			}
		    }
		    if (si_bpovrly != NULL) {
			in = yigm2s (si_bpovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Mems[out+i] = maskcolor (bpcolors,
				    int(Mems[in+i]))
			}
		    }
		}

	    } else {
		for (wy=y2+1;  wy <= wy2;  wy=wy+1) {
		    in  = zigm2r (mg, si, wy - wy1 + 1)
		    if (NODATA(mg) == YES)
			break
		    out = imps2r (ds, wx1, wx2, wy, wy)

		    if (unitary_greyscale_transformation)  {
			call amovr (Memr[in], Memr[out], nx)
		    } else if (zt == W_LOG) {
			call amapr (Memr[in], Memr[out], nx,
			    z1, z2, 1.0, 10.0 ** MAXLOG)
			call alogr (Memr[out], Memr[out], nx, logerrfcn)
			call amapr (Memr[out], Memr[out], nx,
			    0.0, real(MAXLOG), dz1, dz2)
		    } else
			call amapr (Memr[in], Memr[out], nx, z1, z2, dz1, dz2)

		    if (si_ovrly != NULL) {
			in = yigm2s (si_ovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Memr[out+i] = maskcolor (ocolors,
				    int(Mems[in+i]))
			}
		    }
		    if (si_bpovrly != NULL) {
			in = yigm2s (si_bpovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Memr[out+i] = maskcolor (bpcolors,
				    int(Mems[in+i]))
			}
		    }
		}
	    }
	} else
	    wy = wy1

	# If no data was found at the beginning of the image try the end.
	if (wy == wy1) {
	    if (IM_PIXTYPE(im) == TY_SHORT && zt != W_LOG) {
		z1_s = z1;  z2_s = z2
		if (z1_s == z2_s) {
		    z1_s = z1_s - 1
		    z2_s = z2_s + 1
		}

		for (wy=y1-1;  wy >= wy1;  wy=wy-1) {
		    in  = zigm2s (mg, si, wy - wy1 + 1)
		    if (NODATA(mg) == YES)
			break
		    out = imps2s (ds, wx1, wx2, wy, wy)

		    if (unitary_greyscale_transformation) {
			call amovs (Mems[in], Mems[out], nx)
		    } else if (zt == W_USER) {
			dz1_s = U_Z1; dz2_s = U_Z2
			call amaps (Mems[in],Mems[out],nx, z1_s,z2_s,
			    dz1_s,dz2_s)
			call aluts (Mems[out], Mems[out], nx, Mems[uptr])
		    } else {
			dz1_s = dz1; dz2_s = dz2
			call amaps (Mems[in],Mems[out],nx, z1_s,z2_s,
			    dz1_s,dz2_s)
		    }

		    if (si_ovrly != NULL) {
			in = yigm2s (si_ovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Mems[out+i] = maskcolor (ocolors,
				    int(Mems[in+i]))
			}
		    }
		    if (si_bpovrly != NULL) {
			in = yigm2s (si_bpovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Mems[out+i] = maskcolor (bpcolors,
				    int(Mems[in+i]))
			}
		    }
		}

	    } else if (zt == W_USER) {

		for (wy=y1-1;  wy >= wy1;  wy=wy-1) {
		    in  = zigm2r (mg, si, wy - wy1 + 1)
		    if (NODATA(mg) == YES)
			break
		    out = imps2s (ds, wx1, wx2, wy, wy)

		    call amapr (Memr[in], Memr[rtemp], nx, z1, z2, 
			real(U_Z1), real(U_Z2))
		    call achtrs (Memr[rtemp], Mems[out], nx)
		    call aluts (Mems[out], Mems[out], nx, Mems[uptr])

		    if (si_ovrly != NULL) {
			in = yigm2s (si_ovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Mems[out+i] = maskcolor (ocolors,
				    int(Mems[in+i]))
			}
		    }
		    if (si_bpovrly != NULL) {
			in = yigm2s (si_bpovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Mems[out+i] = maskcolor (bpcolors,
				    int(Mems[in+i]))
			}
		    }
		}

	    } else {
		for (wy=y1-1;  wy >= wy1;  wy=wy-1) {
		    in  = zigm2r (mg, si, wy - wy1 + 1)
		    if (NODATA(mg) == YES)
			break
		    out = imps2r (ds, wx1, wx2, wy, wy)

		    if (unitary_greyscale_transformation)  {
			call amovr (Memr[in], Memr[out], nx)
		    } else if (zt == W_LOG) {
			call amapr (Memr[in], Memr[out], nx,
			    z1, z2, 1.0, 10.0 ** MAXLOG)
			call alogr (Memr[out], Memr[out], nx, logerrfcn)
			call amapr (Memr[out], Memr[out], nx,
			    0.0, real(MAXLOG), dz1, dz2)
		    } else
			call amapr (Memr[in], Memr[out], nx, z1, z2, dz1, dz2)

		    if (si_ovrly != NULL) {
			in = yigm2s (si_ovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Memr[out+i] = maskcolor (ocolors,
				    int(Mems[in+i]))
			}
		    }
		    if (si_bpovrly != NULL) {
			in = yigm2s (si_bpovrly, wy - wy1 + 1)
			do i = 0, nx-1 {
			    if (Mems[in+i] != 0)
				Memr[out+i] = maskcolor (bpcolors,
				    int(Mems[in+i]))
			}
		    }
		}
	    }
	    y1 = min (wy + 25, wy2)
	    if (wy < wy1)
		done = YES
	    else
		done = NO
	} else {
	    y2 = max (wy - 25, wy1)
	    if (wy > wy2)
		done = YES
	    else
		done = NO
	}

	call sigm2_free (si)
	if (si_ovrly != NULL)
	    call sigm2_free (si_ovrly)
	if (si_bpovrly != NULL)
	    call sigm2_free (si_bpovrly)
	if (ovrly != NULL)
	    call imunmap (ovrly)
	if (bpm != NULL)
	    call imunmap (bpm)
	call sfree (sp)
end
