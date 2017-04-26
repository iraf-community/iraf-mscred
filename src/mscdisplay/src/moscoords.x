# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# MOSCOORDS -- Convert display coordinates to mosaic coordinates.

procedure moscoords (wcs, x, y)

int	wcs			#U Display WCS
real	x			#U X
real	y			#U Y

int	i, snx, sny, dx, dy, dnx, dny
real	sx, sy
pointer	sp, reg, objref
int	imd_wcsver(), imd_query_map()

int	wcsver
data	wcsver/-1/

begin
	if (wcsver == -1)
	    wcsver = imd_wcsver()

	if (wcsver == 0)
	    return

	call smark (sp)
	call salloc (reg, SZ_FNAME, TY_CHAR)
	call salloc (objref, SZ_FNAME, TY_CHAR)

	if (imd_query_map (wcs, Memc[reg], sx, sy, snx, sny, dx, dy,
	    dnx, dny, Memc[objref]) == ERR) {
	    call sfree (sp)
	    return
	}

	x = (x - sx) * (dnx - 1.) / (snx - 1.) + dx
	y = (y - sy) * (dny - 1.) / (sny - 1.) + dy

	do i = wcs+1, ARB
	    if (imd_query_map (i, Memc[reg], sx, sy, snx, sny, dx, dy,
		dnx, dny, Memc[objref]) == ERR)
		break

	x = (x - dx) * (snx - 1.) / (dnx - 1.) + sx
	y = (y - dy) * (sny - 1.) / (dny - 1.) + sy
	wcs = i - 1

	call sfree (sp)
end
