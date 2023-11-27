# Table column descriptors

char	colname[SZ_COLNAME]	# Column name
char	colunits[SZ_COLUNITS]	# Column units
char	colfmt[SZ_COLFMT]	# Column print format
int	tbcol[SZ_MAXCOL]	# starting position for each table column
int	tbcw[SZ_MAXCOL]		# width field in character (from TBFORM)
int	datat, lendata, coln
char	tnull[SZ_COLUNITS, SZ_MAXCOL]	# null value for each column
real	tzero[SZ_MAXCOL]	# offset value for each column
real	tscal[SZ_MAXCOL]	# scale value for each column

common /ctables/ colname, colunits, colfmt, tbcol, tbcw, datat, lendata, coln,
		 tnull, tzero, tscal
