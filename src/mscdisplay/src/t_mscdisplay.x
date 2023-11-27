include <error.h>
include <imhdr.h>
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


# T_MSCDISPLAY -- Quick-look mosaic display task.

procedure t_mscdisplay ()

bool    zmap
int     i, j, k, wcsver, frame, ninput, nproc, zcom, select, bufsize
int	nx[2], ny[2]
real    xc, yc, xs[2], ys[2], a, b, c, d, tx, ty, zz1, zz2
pointer sp, image, image1, title, procstr, wcs
pointer	z1, z2, wdes, mi, mg, cmg, ds, im, wipix, wdpix, wnwin, wdwin, dsbuf

bool    clgetb(), fp_equalr(), streq(), hdmflag()
int     clgeti(), clgwrd(), btoi(), access(), imd_wcsver(), imtlen(), imtgetim()
real    clgetr(), ahivr(), alovr(), asumr(), amedr()
pointer mimap(), imd_mapframe1(), iw_open(), imps2s(), imtopenp()
errchk	imps2s, msc_load_display

include "mosproc.com"

begin
        call smark (sp)
        call salloc (image, SZ_LINE, TY_CHAR)
        call salloc (image1, SZ_LINE, TY_CHAR)
        call salloc (title, SZ_LINE, TY_CHAR)
        call salloc (procstr, SZ_LINE, TY_CHAR)
        call salloc (wcs, SZ_LINE, TY_CHAR)

        # Set buffer, instrument, amplifier, and process information.
        #bufsize = max (1024., 1000000. * clgetr ("im_bufsize"))
        bufsize = 65000.
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

        # Allocate memory for each input image.
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

	# Determine parameters for each input image.
	nproc = 0
	do i = 1, ninput {
	    mg = MI_MG(mi,i)
	    im = MI_IM(mi,i)

	    # Set display window for image.
	    call mos_comap (Memi[wdes], mg, cmg, xc, yc, xs, ys)

	    # Set display parameters for image.
	    call mos_params (mg, ds, Memi[wdes+i], DX1(mg), DX2(mg),
		abs(DX(mg)), DY1(mg), DY2(mg), abs(DY(mg)), xc, yc,
		xs, ys, Memr[z1+i-1], Memr[z2+i-1])
	    call imseti (im, IM_CANCEL, 0)
	    if (PROC(mg) == YES)
		nproc = nproc + 1
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
	call mos_info (mi, wdes, zcom, Memr[z1], Memr[z2], STDOUT)

	# Print tile information.
	call msctile (mi, Memc[wcs], frame, wcsver)

	# Now we're ready to write to the display.
	call imunmap (ds)
        ds = imd_mapframe1 (frame, WRITE_ONLY, select, btoi (clgetb ("erase")))

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
		call imstats (MI_IM(mi,i), IM_IMAGENAME, Memc[image1], SZ_LINE)
		iferr (call imgstr (MI_IM(mi,i),"EXTNAME",Memc[wcs],SZ_LINE)) {
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

	    # Allow for the Y-flip (origin at upper left in display window).
	    d = -d
	    ty = W_YE(wdwin) - d * ((1.0 - W_YE(wnwin)) * IM_LEN(ds,2))

	    # Translate the screen corner to the center of the screen pixel.
	    tx = tx + 0.5 * a
	    ty = ty + 0.5 * d

	    # Set origin to mosaic coordinates.
	    tx = tx + CX1(cmg) - 1
	    ty = ty + CY1(cmg) - 1

	    # Set the title and WCS.
	    if (nproc > 0) {
		mg = MI_MG(mi,j)
		if (DOBIAS(mg)==YES || DOZERO(mg)==YES || DOFLAT(mg)==YES) {
		    Memc[title] = EOS
		    if (DOBIAS(mg) == YES) {
		        call sprintf (Memc[procstr], SZ_LINE, ",bias")
			call strcat (Memc[procstr], Memc[title], SZ_LINE)
		    }
		    if (DOZERO(mg) == YES) {
		        call sprintf (Memc[procstr], SZ_LINE, ",zero=%s")
			    call pargstr (ZERONAME(mg))
			call strcat (Memc[procstr], Memc[title], SZ_LINE)
		    }
		    if (DOFLAT(mg) == YES) {
		        call sprintf (Memc[procstr], SZ_LINE, ",flat=%s")
			    call pargstr (FLATNAME(mg))
			call strcat (Memc[procstr], Memc[title], SZ_LINE)
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
		call strcpy (IM_TITLE(MI_IM(mi,j)), Memc[title], SZ_LINE)
	    call imd_putwcs (ds, frame, Memc[MI_RNAME(mi)], Memc[title],
		a, b, c, d, tx, ty, zz1, zz2, W_ZT(W_WC(Memi[wdes+j],W_DWIN)))
	}

	# Now display the images.
	dsbuf = NULL
	if (clgetb ("onepass")) {
	    iferr {
		dsbuf = imps2s (ds, 1, IM_LEN(ds,1), 1, IM_LEN(ds,2))
		call aclrs (Mems[dsbuf], IM_LEN(ds,1)*IM_LEN(ds,2))
	    } then
		dsbuf = NULL
	}
	if (dsbuf == NULL) {
	    call imunmap (ds)
	    ds = imd_mapframe1 (frame, READ_WRITE, select, NO)
	}

	do i = 1, ninput {
	    mg = MI_MG(mi,i)
	    im = MI_IM(mi,i)
            #call imseti (im, IM_BUFSIZE, bufsize)
	    call msc_load_display (mg, ds, dsbuf, Memi[wdes+i])
	    #call imseti (im, IM_CANCEL, 0)
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


# MSCTILE -- Print tile information.

procedure msctile (mi, fname, frame, wcsver)

pointer	mi		#I MOSIM structure
char	fname[ARB]	#I File name for output
int	frame		#I Display frame
int	wcsver		#I WCS version

int     i, fd, nowhite(), access(), open()
pointer sp, image, cmg, mg

begin
        call smark (sp)
        call salloc (image, SZ_LINE, TY_CHAR)

	if (nowhite (fname, Memc[image], SZ_LINE) == 0) {
	    call sfree (sp)
	    return
	}

	if (access (Memc[image], 0, 0) == YES)
	    call delete (Memc[image])
	fd = open (Memc[image], NEW_FILE, TEXT_FILE)

	cmg = MI_CMG(mi)
	do i = 1, MI_NIMS(mi) {
	    mg = MI_MG(mi,i)

	    call imstats (MI_IM(mi,i), IM_IMAGENAME, Memc[image], SZ_LINE)
	    if (wcsver == 0) {
		call fprintf (fd, "%s %d %d %d %d\n")
		    call pargstr (Memc[image])
		    call pargi (CX1(mg))
		    call pargi (CX2(mg))
		    call pargi (CY1(mg))
		    call pargi (CY2(mg))
	    } else {
		call fprintf (fd, "%s %d %d %d %d %d%02d\n")
		    call pargstr (Memc[image])
		    call pargi (CX1(mg))
		    call pargi (CX2(mg))
		    call pargi (CY1(mg))
		    call pargi (CY2(mg))
		    call pargi (frame)
		    call pargi (i)
	    }
	}

	call close (fd)
        call sfree (sp)
end


# MOS_COMAP -- Compute NDC parameters for a mosaic tile.

procedure mos_comap (wdes, img, omg, xc, yc, xs, ys)

pointer wdes            #I Output window descriptor
pointer img             #I Mosgeom structure for input image
pointer omg             #I Mosgeom structure for tiled image
real    xc, yc          #O Center of tile in x and y in NDC 
real    xs, ys          #O Size of tile in x and y in NDC 

real    x1, x2, y1, y2, xscale, yscale
pointer wnwin, wdwin

begin
        wnwin = W_WC(wdes,W_NWIN)
        wdwin = W_WC(wdes,W_DWIN)

	x1 = CX1(img) - CX1(omg) + 1
	x2 = CX2(img) - CX1(omg) + 1
	y1 = CY1(img) - CY1(omg) + 1
	y2 = CY2(img) - CY1(omg) + 1

        xscale = (W_XE(wnwin) - W_XS(wnwin)) / (W_XE(wdwin) - W_XS(wdwin))
        yscale = (W_YE(wnwin) - W_YS(wnwin)) / (W_YE(wdwin) - W_YS(wdwin))
        xc = W_XS(wnwin) + xscale * (0.5 * (x1 + x2) - W_XS(wdwin))
        yc = W_YS(wnwin) + yscale * (0.5 * (y1 + y2) - W_YS(wdwin))
        xs = min (xscale * abs (x2 - x1 + 1), 1.0)
        ys = min (yscale * abs (y2 - y1 + 1), 1.0)
end


# MOS_INFO -- Print information about Z values.

procedure mos_info (mi, wdes, zcom, z1, z2, fd)

pointer mi              #I Mosim structure pointer.
pointer wdes            #I Display structure pointer.
int	zcom		#I Zcombine option
real    z1[ARB]         #I Corrected Z1 values.
real    z2[ARB]         #I Corrected Z2 values.
int	fd		#I File descriptor

int	i
pointer mg, wdwin

begin
	call fprintf (fd, " Image:%17tIndividual%34tDisplay (zcombine=%s)\n")
	switch (zcom) {
	case ZC_MINMAX:
	    call pargstr ("minmax")
	case ZC_AVERAGE:
	    call pargstr ("average")
	case ZC_MEDIAN:
	    call pargstr ("median")
	default:
	    call pargstr ("none")
	}

        do i = 1, MI_NIMS(mi) {
            mg = MI_MG(mi,i)
	    wdwin = W_WC(Memi[wdes+i],W_DWIN)

	    if (AMPID(mg) == NULL) {
		call fprintf (fd, "   im%d:")
		    call pargi (i)
	    } else {
		call fprintf (fd, "%6s:")
		    call pargstr (Memc[AMPID(mg)])
	    }

	    call fprintf (fd, "%5t%8.1f %8.1f%30t%8.1f %8.1f\n")
		call pargr (z1[i])
		call pargr (z2[i])
		call pargr (W_ZS(wdwin))
		call pargr (W_ZE(wdwin))
        }

        call flush (fd)
end


# MOS_PARAMS -- Get the parameters controlling how the image is mapped
# into the display frame.  Set up the transformations and save in the graphics
# descriptor file.

procedure mos_params (mg, ds, wdes, x1, x2, dx, y1, y2, dy, xc, yc, xs, ys,
	z1, z2)

pointer mg, ds, wdes            #I Image, display, and graphics descriptors
int     x1, x2, y1, y2          #I Image section
int	dx, dy			#I Pixel summing factors
real    xc, yc                  #I Center of display window in NDC
real    xs, ys                  #I Size of display window in NDC
real    z1, z2                  #U Default and final values

bool    fill, zscale_flag, zrange_flag, zmap_flag
real    xcenter, ycenter, xsize, ysize
real    xmag, ymag, xscale, yscale, pxsize, pysize
real    contrast
int     ncols, nlines, nsample
pointer im, wnwin, wdwin, wwwin, wipix, wdpix, zpm, bpm
pointer sp, str, ztrans, lutfile

int     clgeti(), clgwrd(),nowhite()
real    clgetr()
pointer xmaskcolor_map(), yt_pmmap(), zsc_pmsection()
pointer ds_ulutalloc()
bool    streq(), clgetb()
errchk	xmaskcolor_map, yt_pmmap, zsc_pmsection, msc_mzscale

begin
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)
        call salloc (ztrans, SZ_FNAME, TY_CHAR)

	if (mg != NULL)
	    im = MG_IM(mg)
	else
	    im = NULL

        if (im != NULL) {
	    # Get overlay mask and colors.
	    call clgstr ("overlay", W_OVRLY(wdes), W_SZSTRING)
	    call clgstr ("ocolors", Memc[str], SZ_LINE)
	    W_OCOLORS(wdes) = xmaskcolor_map (Memc[str])

	    # Get bad pixel mask.
	    call clgstr ("bpmask", W_BPM(wdes), W_SZSTRING)
	    W_BPDISP(wdes) = clgwrd ("bpdisplay", Memc[str], SZ_LINE, BPDISPLAY)
	    call clgstr ("bpcolors", Memc[str], SZ_LINE)
	    W_BPCOLORS(wdes) = xmaskcolor_map (Memc[str])
        }

        # Determine the display window into which the image is to be mapped
        # in normalized device coordinates.

        #xcenter = max(0.0, min(1.0, clgetr ("xcenter")))
        #ycenter = max(0.0, min(1.0, clgetr ("ycenter")))
        #xsize   = max(0.0, min(1.0, clgetr ("xsize")))
        #ysize   = max(0.0, min(1.0, clgetr ("ysize")))
	xcenter = xc
	ycenter = yc
	xsize = xs
	ysize = ys

        # Set up a new graphics descriptor structure defining the coordinate
        # transformation used to map the image into the display frame.

        wnwin = W_WC(wdes,W_NWIN)
        wdwin = W_WC(wdes,W_DWIN)
        wwwin = W_WC(wdes,W_WWIN)
        wipix = W_WC(wdes,W_IPIX)
        wdpix = W_WC(wdes,W_DPIX)

        # Determine X and Y scaling ratios required to map the image into the
        # normalized display window.  If spatial scaling is not desired filling
        # must be disabled and the XMAG and YMAG are adjusted to the nearest
	# integer size.

        #ncols  = IM_LEN(im,1)
        #nlines = IM_LEN(im,2)
	ncols = x2 - x1 + 1
	nlines = y2 - y1 + 1
        #fill = clgetb ("fill")
	fill = true
        if (fill) {
            # Compute scale in units of window coords per data pixel required
            # to scale image to fit window.

            xmag = (IM_LEN(ds,1) * xsize) / ncols
            ymag = (IM_LEN(ds,2) * ysize) / nlines

            if (xmag > ymag)
                xmag = ymag
            else
                ymag = xmag

	    xmag = xmag * dx / min (dx, dy)
	    ymag = ymag * dy / min (dx, dy)

        } else {
            # Compute scale required to provide block averaging only.

            xmag = (IM_LEN(ds,1) * xsize) / ncols
            ymag = (IM_LEN(ds,2) * ysize) / nlines

            if (xmag > ymag)
                xmag = ymag
            else
                ymag = xmag

	    xmag = xmag * dx / min (dx, dy)
	    ymag = ymag * dy / min (dx, dy)

	    xmag = 1. / int (1. / xmag + 0.999)
	    ymag = 1. / int (1. / ymag + 0.999)

        }

        xscale = 1.0 / (IM_LEN(ds,1) / xmag)
        yscale = 1.0 / (IM_LEN(ds,2) / ymag)

        # Set device window limits in normalized device coordinates.
        # World coord system 0 is used for the device window.

        W_XS(wnwin) = xcenter - xsize / 2.0
        W_XE(wnwin) = xcenter + xsize / 2.0
        W_YS(wnwin) = ycenter - ysize / 2.0
        W_YE(wnwin) = ycenter + ysize / 2.0

        # Set pixel coordinates of window.
        # If the image is too large to fit in the window given the scaling
        # factors XSCALE and YSCALE, the following will set starting and ending
        # pixel coordinates in the interior of the image.  If the image is too
        # small to fill the window then the pixel coords will reference beyond
        # the bounds of the image.  Note that the 0.5 is because NDC has
        # the screen corner at 0 while screen pixels have the corner at 0.5.

        pxsize = xsize / xscale
        pysize = ysize / yscale

        W_XS(wdwin) = (ncols / 2.0) - (pxsize / 2.0) + 0.5
        W_XE(wdwin) = W_XS(wdwin) + pxsize
        W_YS(wdwin) = (nlines / 2.0) - (pysize / 2.0) + 0.5
        W_YE(wdwin) = W_YS(wdwin) + pysize

        # Compute X and Y magnification ratios required to map image into
        # the device window in device pixel units.

        xmag = (W_XE(wnwin)-W_XS(wnwin))*IM_LEN(ds,1)/(W_XE(wdwin)-W_XS(wdwin))
        ymag = (W_YE(wnwin)-W_YS(wnwin))*IM_LEN(ds,2)/(W_YE(wdwin)-W_YS(wdwin))

        # Compute the coordinates of the image section to be displayed.
        # Round down if upper pixel is exactly at one-half.

        W_XS(wipix) = max (1, nint(W_XS(wdwin))) + x1 - 1
        #W_XE(wipix) = min (ncols, nint(W_XE(wdwin)-1.01)) + x1 - 1
        W_XE(wipix) = min (ncols, nint(W_XE(wdwin)-0.01)) + x1 - 1
        W_YS(wipix) = max (1, nint(W_YS(wdwin))) + y1 - 1
        #W_YE(wipix) = min (nlines, nint(W_YE(wdwin)-1.01)) + y1 - 1
        W_YE(wipix) = min (nlines, nint(W_YE(wdwin)-0.01)) + y1 - 1

        # Now compute the image and display pixels to be used.
        # The image may be truncated to fit in the display window.
        # These are integer coordinates at the pixel centers.

        pxsize = W_XE(wipix) - W_XS(wipix) + 1
        pysize = W_YE(wipix) - W_YS(wipix) + 1
        xcenter = (W_XE(wnwin) + W_XS(wnwin)) / 2.0 * IM_LEN(ds,1) + 0.5
        ycenter = (W_YE(wnwin) + W_YS(wnwin)) / 2.0 * IM_LEN(ds,2) + 0.5

        W_XS(wdpix) = max (1, nint (xcenter - (pxsize/2.0*xmag) + 0.5))
        #W_XE(wdpix) = min (IM_LEN(ds,1), nint (W_XS(wdpix)+pxsize*xmag - 1.01))
        W_XE(wdpix) = min (IM_LEN(ds,1), nint (W_XS(wdpix)+pxsize*xmag - 0.01))
        W_YS(wdpix) = max (1, nint (ycenter - (pysize/2.0*ymag) + 0.5))
        #W_YE(wdpix) = min (IM_LEN(ds,2), nint (W_YS(wdpix)+pysize*ymag - 1.01))
        W_YE(wdpix) = min (IM_LEN(ds,2), nint (W_YS(wdpix)+pysize*ymag - 0.01))

	# I don't remember why the changes indicated by the commented code
	# above were done.  So the following is a hack for a specific
	# case.
	if (nint((W_XE(wdpix)-W_XS(wdpix))-(W_XE(wipix)-W_XS(wipix))) == 1)
	    W_XE(wdpix) = W_XE(wdpix) - 1
	if (nint((W_YE(wdpix)-W_YS(wdpix))-(W_YE(wipix)-W_YS(wipix))) == 1)
	    W_YE(wdpix) = W_YE(wdpix) - 1

        # Now adjust the display window to be consistent with the image and
        # display pixels to be used.

        W_XS(wdwin) = W_XS(wnwin) * IM_LEN(ds,1) + 0.5
        W_XE(wdwin) = W_XE(wnwin) * IM_LEN(ds,1) + 0.5
        W_YS(wdwin) = W_YS(wnwin) * IM_LEN(ds,2) + 0.5
        W_YE(wdwin) = W_YE(wnwin) * IM_LEN(ds,2) + 0.5
        W_XS(wdwin) = (W_XS(wipix)-0.5) + (W_XS(wdwin)-(W_XS(wdpix)-0.5))/xmag
        W_XE(wdwin) = (W_XS(wipix)-0.5) + (W_XE(wdwin)-(W_XS(wdpix)-0.5))/xmag
        W_YS(wdwin) = (W_YS(wipix)-0.5) + (W_YS(wdwin)-(W_YS(wdpix)-0.5))/ymag
        W_YE(wdwin) = (W_YS(wipix)-0.5) + (W_YE(wdwin)-(W_YS(wdpix)-0.5))/ymag

        if (im != NULL) {
            # Order of interpolator used for spatial transformation.
            W_XT(wdwin) = max(0, min(1, clgeti ("order")))
            W_YT(wdwin) = W_XT(wdwin)

            # Determine the greyscale transformation.
            call clgstr ("ztrans", Memc[ztrans], SZ_FNAME)
            if (streq (Memc[ztrans], "log"))
                W_ZT(wdwin) = W_LOG
            else if (streq (Memc[ztrans], "linear"))
                W_ZT(wdwin) = W_LINEAR
            else if (streq (Memc[ztrans], "none"))
                W_ZT(wdwin) = W_UNITARY
            else if (streq (Memc[ztrans], "user")) {
                W_ZT(wdwin) = W_USER
                call salloc (lutfile, SZ_FNAME, TY_CHAR)
                call clgstr ("lutfile", Memc[lutfile], SZ_FNAME)
                W_UPTR(wdwin) = ds_ulutalloc (Memc[lutfile], z1, z2)
            } else {
                call eprintf ("Bad greylevel transformation '%s'\n")
                    call pargstr (Memc[ztrans])
                W_ZT(wdwin) = W_LINEAR
            }

            # The zscale, and zrange parameters determine the algorithms for
            # determining Z1 and Z2, the range of input z values to be mapped
            # into the fixed range of display greylevels.  If sampling and no
            # sample mask is given then create one as a subsampled image section.
            # If greyscale mapping is disabled the zscale and zrange options are
            # disabled.  Greyscale mapping can also be disabled by turning off
            # zscale and zrange and setting Z1 and Z2 to the device greyscale min
            # and max values, producing a unitary transformation.

            if (W_ZT(wdwin) == W_UNITARY || W_ZT(wdwin) == W_USER) {
                zscale_flag = false
                zrange_flag = false
                zmap_flag   = false
            } else {
                zmap_flag = true
                zscale_flag = clgetb ("zscale")
                if (!zscale_flag)
                    zrange_flag = clgetb ("zrange")
            }

            if (zscale_flag || (zrange_flag && IM_LIMTIME(im) < IM_MTIME(im))) {
		call clgstr ("zmask", W_ZPM(wdes), W_SZSTRING)
		nsample = max (100, clgeti ("nsample"))
		if (nowhite (W_ZPM(wdes), W_ZPM(wdes), W_SZSTRING) > 0) {
		    if (W_ZPM(wdes) == '[')
			zpm = zsc_pmsection (W_ZPM(wdes), im)
		    else
			zpm = yt_pmmap (W_ZPM(wdes), im, Memc[str], SZ_LINE)
		} else
		    zpm = NULL
		iferr (bpm = yt_pmmap (W_BPM(wdes), im, Memc[str], SZ_LINE)) {
		    call erract (EA_WARN)
		    bpm = NULL
		}
            }

            if (zscale_flag) {
                # Autoscaling is desired.  Compute Z1 and Z2 which straddle the
                # median computed by sampling a portion of the image.

                contrast = clgetr ("contrast")
                call msc_mzscale (mg, zpm, bpm, contrast, nsample, z1, z2)
                if (zpm != NULL)
                    call imunmap (zpm)
                if (bpm != NULL)
                    call imunmap (bpm)

            } else if (zrange_flag) {
                # Use the limits in the header if current otherwise get the
                # minimum and maximum of the sample mask.
                if (IM_LIMTIME(im) >= IM_MTIME(im)) {
                    z1 = IM_MIN(im)
                    z2 = IM_MAX(im)
                } else {
                    call msc_mzscale (mg, zpm, bpm, 0., nsample, z1, z2)
                    if (zpm != NULL)
                        call imunmap (zpm)
                    if (bpm != NULL)
                        call imunmap (bpm)
                }

            } else if (zmap_flag) {
                #z1 = clgetr ("z1")
                #z2 = clgetr ("z2")
            } else {
                z1 = IM_MIN(ds)
                z2 = IM_MAX(ds)
            }

            W_ZS(wdwin) = z1
            W_ZE(wdwin) = z2

            #call printf ("z1=%g z2=%g\n")
            #    call pargr (W_ZS(wdwin))
            #    call pargr (W_ZE(wdwin))
            #call flush (STDOUT)
        }

        # The user world coordinate system should be set from the CTRAN
        # structure in the image header, but for now we just make it equal
        # to the pixel coordinate system.

        call amovi (Memi[wdwin], Memi[wwwin], LEN_WC)
        W_UPTR(wwwin) = NULL            # should not copy pointers!!
        call sfree (sp)
end


# MZSCALE -- Sample an image with pixel masks and compute greyscale limits.
# The image is sampled through a pixel mask.  If no pixel mask is given
# a uniform sample mask is generated.  If a bad pixel mask is given
# bad pixels in the sample are eliminated.  Once the sample is obtained
# the greyscale limits are obtained using the ZSC_ZLIMITS algorithm.

procedure msc_mzscale (mg, zpm, bpm, contrast, maxpix, z1, z2)

pointer	mg			#I image to be sampled
pointer	zpm			#I pixel mask for sampling
pointer	bpm			#I bad pixel mask
real	contrast		#I contrast parameter
int	maxpix			#I maximum number of pixels in sample
real	z1, z2			#O output min and max greyscale values

int	i, ndim, nc, nl, dx1, dx2, dy1, dy2, npix, nbp, imstati()
pointer	sp, section, v, sample, zmask, bp, zim, pmz, pmb, buf, im
pointer	zsc_pmsection(), mscnlr()
bool	pm_linenotempty()
errchk	zsc_pmsection, zsc_zlimits

begin
	call smark (sp)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call salloc (sample, maxpix, TY_REAL)
	zmask = NULL
	bp = NULL

	im = MG_IM(mg)
	ndim = min (2, IM_NDIM(im))
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	dx1 = DX1(mg)
	dx2 = DX2(mg)
	dy1 = DY1(mg)
	dy2 = DY2(mg)

	# Generate a uniform sample mask if none is given.
	if (zpm == NULL) {
	    switch (IM_NDIM(im)) {
	    case 1:
		call sprintf (Memc[section], SZ_FNAME, "[%d:%d]")
		    call pargi (dx1)
		    call pargi (dx2)
	    default:
		i = max (1., sqrt ((dx2-dx1)*(dy2-dy1) / real (maxpix)))
		call sprintf (Memc[section], SZ_FNAME, "[%d:%d:%d,%d:%d:%d]")
		    call pargi (dx1+i/2)
		    call pargi (dx2-i/2)
		    call pargi (i)
		    call pargi (dy1+i/2)
		    call pargi (dy2-i/2)
		    call pargi (i)
	    }
	    zim = zsc_pmsection (Memc[section], im)
	    pmz = imstati (zim, IM_PMDES)
	} else
	    pmz = imstati (zpm, IM_PMDES)

	# Set bad pixel mask.
	if (bpm != NULL)
	    pmb = imstati (bpm, IM_PMDES)
	else
	    pmb = NULL

	# Get the sample up to maxpix pixels.
	npix = 0
	nbp = 0
	call amovkl (long(1), Memi[v], IM_MAXDIM)
	repeat {
	    if (pm_linenotempty (pmz, Meml[v])) {
		if (zmask == NULL)
		    call salloc (zmask, nc, TY_INT)
		call pmglpi (pmz, Meml[v], Memi[zmask], 0, nc, 0)
		if (pmb != NULL) {
		    if (pm_linenotempty (pmb, Meml[v])) {
			if (bp == NULL)
			    call salloc (bp, nc, TY_INT)
			call pmglpi (pmb, Meml[v], Memi[bp], 0, nc, 0)
			nbp = nc
		    } else
			nbp = 0

		}
		if (mscnlr (mg, buf, Meml[v]) == EOF)
		    break
		if (NODATA(mg) == YES)
		    break
		do i = 0, nc-1 {
		    if (Memi[zmask+i] == 0)
			next
		    if (nbp > 0)
			if (Memi[bp+i] != 0)
			    next
		    Memr[sample+npix] = Memr[buf+i]
		    npix = npix + 1
		    if (npix == maxpix)
			break
		}
		if (npix == maxpix)
		    break
	    } else {
		do i = 2, ndim {
		    Meml[v+i-1] = Meml[v+i-1] + 1
		    if (Meml[v+i-1] <= IM_LEN(im,i))
			break
		    else if (i < ndim)
			Meml[v+i-1] = 1
		}
	    }
        } until (Meml[v+ndim-1] > IM_LEN(im,ndim))

	if (npix == 0) {
	    Memi[v+1] = IM_LEN(im,2)
	    repeat {
		if (pm_linenotempty (pmz, Meml[v])) {
		    if (zmask == NULL)
			call salloc (zmask, nc, TY_INT)
		    call pmglpi (pmz, Meml[v], Memi[zmask], 0, nc, 0)
		    if (pmb != NULL) {
			if (pm_linenotempty (pmb, Meml[v])) {
			    if (bp == NULL)
				call salloc (bp, nc, TY_INT)
			    call pmglpi (pmb, Meml[v], Memi[bp], 0, nc, 0)
			    nbp = nc
			} else
			    nbp = 0

		    }
		    if (mscnlr (mg, buf, Meml[v]) == EOF)
			break
		    Meml[v+1] = Meml[v+1] - 2
		    if (NODATA(mg) == YES)
			break
		    do i = 0, nc-1 {
			if (Memi[zmask+i] == 0)
			    next
			if (nbp > 0)
			    if (Memi[bp+i] != 0)
				next
			Memr[sample+npix] = Memr[buf+i]
			npix = npix + 1
			if (npix == maxpix)
			    break
		    }
		    if (npix == maxpix)
			break
		} else {
		    Meml[v+1] = Meml[v+1] - 1
		}
	    } until (Meml[v+1] == 0)
	}

	if (zpm == NULL)
	    call imunmap (zim)

	# Compute greyscale limits.
	call zsc_zlimits (Memr[sample], npix, contrast, z1, z2)

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

procedure msc_load_display (mg, ds, dsbuf, wdes)

pointer	mg			# input image
pointer	ds			# output image
pointer	dsbuf			# display buffer
pointer	wdes			# graphics window descriptor

real	z1, z2, dz1, dz2, px1, px2, py1, py2
int	i, order, zt, wx1, wx2, wy1, wy2, wy, nx, ny, xblk, yblk, nxds, nyds
int	color
pointer	wdwin, wipix, wdpix, ovrly, bpm, pm, uptr
pointer	im, in, out, si, si_ovrly, si_bpovrly, ocolors, bpcolors, rtemp
pointer	sp, fname
bool	unitary_greyscale_transformation
short	lut1, lut2, dz1_s, dz2_s, z1_s, z2_s

real	logerrfcn()
bool	fp_equalr()
int	imstati(), xmaskcolor()
pointer	yt_pmmap(), imps2s(), imps2r()
pointer	yigm2i(), zigm2_setup(), zigm2s(), zigm2r()
errchk	yt_pmmap, imps2s, imps2r, yigm2i, zigm2_setup, zigm2s, zigm2r
errchk	xmaskexprn

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

	nxds = IM_LEN(ds,1)
	nyds = IM_LEN(ds,2)
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

	if (IM_PIXTYPE(im) == TY_SHORT && PROC(mg) == NO && zt != W_LOG) {
	    z1_s = z1;  z2_s = z2
	    if (z1_s == z2_s) {
		z1_s = z1_s - 1
		z2_s = z2_s + 1
	    }

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = zigm2s (mg, si, wy - wy1 + 1)
		if (dsbuf == NULL)
		    out = imps2s (ds, wx1, wx2, wy, wy)
		else
		    out = dsbuf + (wy - 1) * nxds + wx1 - 1

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
		    in = yigm2i (si_ovrly, wy - wy1 + 1)
		    call xmaskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = xmaskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Mems[out+i] = color
			}
		    }
		}
		if (si_bpovrly != NULL) {
		    in = yigm2i (si_bpovrly, wy - wy1 + 1)
		    call xmaskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = xmaskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Mems[out+i] = color
			}
		    }
		}
	    }

	} else if (zt == W_USER) {
	    call salloc (rtemp, nx, TY_REAL)

	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = zigm2r (mg, si, wy - wy1 + 1)
		if (dsbuf == NULL)
		    out = imps2s (ds, wx1, wx2, wy, wy)
		else
		    out = dsbuf + (wy - 1) * nxds + wx1 - 1

		call amapr (Memr[in], Memr[rtemp], nx, z1, z2, 
		    real(U_Z1), real(U_Z2))
		call achtrs (Memr[rtemp], Mems[out], nx)
		call aluts (Mems[out], Mems[out], nx, Mems[uptr])

		if (si_ovrly != NULL) {
		    in = yigm2i (si_ovrly, wy - wy1 + 1)
		    call xmaskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = xmaskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Mems[out+i] = color
			}
		    }
		}
		if (si_bpovrly != NULL) {
		    in = yigm2i (si_bpovrly, wy - wy1 + 1)
		    call xmaskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = xmaskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Mems[out+i] = color
			}
		    }
		}
	    }

	} else {
	    if (dsbuf != NULL)
		call malloc (out, nx, TY_REAL)
	    for (wy=wy1;  wy <= wy2;  wy=wy+1) {
		in  = zigm2r (mg, si, wy - wy1 + 1)
		if (dsbuf == NULL)
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
		    in = yigm2i (si_ovrly, wy - wy1 + 1)
		    call xmaskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = xmaskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Memr[out+i] = color
			}
		    }
		}
		if (si_bpovrly != NULL) {
		    in = yigm2i (si_bpovrly, wy - wy1 + 1)
		    call xmaskexprn (ocolors, in, nx)
		    do i = 0, nx-1 {
			if (Memi[in+i] != 0) {
			    color = xmaskcolor (ocolors, Memi[in+i])
			    if (color >= 0)
				Memr[out+i] = color
			}
		    }
		}

		if (dsbuf != NULL)
		    call achtrs (Memr[out], Mems[dsbuf+(wy-1)*nxds+wx1-1], nx)
	    }
	    if (dsbuf != NULL)
		call mfree (out, TY_REAL)
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


# LOGERRFCN -- Error function value for out of range input.

real procedure logerrfcn (x)

real	x

begin
	return (-MAX_REAL)
end
