# TPEAK -- interactively center a list of sources in an image, given
# their approximate coordinates.  Various options for shifting, rotating,
# and 2D linear fits are provided.  The output is suitable for input to
# a plate solving routine.

# NOTE:  This is a prototype routine that is coded as an SPP `script'
# using clcmd to call other IRAF tasks to do much of the work.  This
# method allows access to facilities such as SPP data structures while
# not requiring complicated algorithms to be recoded.  The expense is
# in the dependence on the current CL / task configuration, in longer
# execution times, and in more complicated plumbing.  We even use disk
# based table access exclusively for reading the catalogs.  No copy is
# kept in memory for internal use.

include	<fset.h>
include	<mach.h>
include <imhdr.h>
include	<error.h>
include	<tbset.h>


# Could make this at runtime from the commands below, but why bother?
define	TASKDICT	"|display|tvmark_|selectpars|imcentroid|tpltsol|"

define	DISPCMD	"display %s %d fill=%b >& dev$null"
define	MARKCMD	"tvmark_ %d %s mark=%s color=%s"
define	CENTCMD	"imcentroid %s reference='' coords=%s shifts=%s box=%d verb+ >& %s"
define	LOGCMD	"logfile %s %s append+ >& dev$null"

#define	FITCMD	"tpltsol %s %s %s imupdate=%s tabupdate+ refitcat=%s dssheader+ ra_ref=%s dec_ref=%s eq_ref=%g"

define	FITCMD	"tpltsol %s %s %s imupdate=%s tabupdate+ refitcat=%s dssheader- ra_ref='%s' dec_ref='%s' eq_ref=%g"

define	CMDDICT	"|autodisplay|boxsize|badcolor|goodcolor|eparam|marker|omarker|replace|rotate|scale|shift|show|subsample|update"

define	KEYHELP		"mscfinder$tpeak.key"

define	AUTODISPLAY	1
define	BOXSIZE		2
define	BADCOLOR	3
define	GOODCOLOR	4
define	EPARAM		5
define	MARKER		6
define	OMARKER		7
define	REPLACE		8
define	ROTATE		9
define	SCALE		10
define	SHIFT		11
define	SHOW		12
define	SUBSAMPLE	13
define	UPDATE		14

define	SZ_NAME		10			# for markers and colors

define	MARKDICT	"|point|circle|cross|plus|rectangle"
define	COLORDICT	"|black|white|red|green|blue|yellow"

define	BLACK		1
define	WHITE		2
define	RED		3
define	GREEN		4
define	BLUE		5
define	YELLOW		6

define	BLACKCODE	202
define	WHITECODE	203
define	REDCODE		204
define	GREENCODE	205
define	BLUECODE	206
define	YELLOWCODE	207


# an integer value that is guaranteed not to be YES or NO
define	ISOBJ		(abs (YES) + abs (NO) + 1)


# tables definitions
define	NUM_COLS	9			# number of columns we need

define	XPRED		1
define	YPRED		2
define	XCEN		3
define	YCEN		4
define	CERR		5
define	SUBSET		6
define	CENTER		7
define	OBJECT		8

define	ID		9

define  PI              3.14159265358979

procedure t_tpeak ()

char	mark[SZ_NAME], ovmark[SZ_NAME]
char	gcolor[SZ_NAME], bcolor[SZ_NAME], ocolor[SZ_NAME]

pointer	sp, image, table, objects, tp, cmd, buf, flags, cp[NUM_COLS]
pointer	database, ra_ref, dec_ref
int	frame, boxsize, nstars, key, junk, index, ob, subsample
bool	update, autodisplay, autocenter, fill, interactive
bool	redisplay, every_source
real	pangle, eq_ref

# window, catalog, and peaked coords
real	wx, wy, cx, cy, px, py, xshift, yshift, xscale, yscale
int	ixshift, iyshift

int	clgeti(), clgcur(), opentab(), center_one(), nearest()
int	strmatch(), open(), getline(), nscan()
real	clgetr()
bool	clgetb(), tp_colon()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (objects, SZ_FNAME, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (ra_ref, SZ_FNAME, TY_CHAR)
	call salloc (dec_ref, SZ_FNAME, TY_CHAR)

	# query parameters
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("table", Memc[table], SZ_FNAME)

	# the rest of the parameters

	call clgstr ("ra_ref", Memc[ra_ref], SZ_LINE)
	call clgstr ("dec_ref", Memc[dec_ref], SZ_LINE)
	eq_ref = clgetr ("eq_ref")

	call clgstr ("database", Memc[database], SZ_FNAME)

	update = clgetb ("update")
	interactive = clgetb ("interactive")

	if (interactive)
	    autocenter = clgetb ("autocenter")
	else
	    autocenter = true

	autodisplay = clgetb ("autodisplay")
	fill = clgetb ("fill")

	boxsize = clgeti ("boxsize")
	pangle = clgetr ("rotate")
	xscale = clgetr ("xscale")
	yscale = clgetr ("yscale")
	ixshift = clgeti ("xshift")
	iyshift = clgeti ("yshift")
	frame = clgeti ("frame")

	subsample = clgeti ("subsample")

	call clgstr ("marker", mark, SZ_NAME)
	call clgstr ("omarker", ovmark, SZ_NAME)
	call clgstr ("goodcolor", gcolor, SZ_NAME)
	call clgstr ("badcolor", bcolor, SZ_NAME)
	call clgstr ("objcolor", ocolor, SZ_NAME)

	nstars = opentab (Memc[table], tp, cp)

	if (clgetb ("reselect"))
	    call tp_select (tp, cp)

	call clgstr ("objects", Memc[objects], SZ_FNAME)
	if (Memc[objects] != EOS && strmatch (Memc[objects], "^#$") == 0) {

	    iferr (ob = open (Memc[objects], READ_ONLY, TEXT_FILE)) {
		call eprintf ("Warning:  problem opening `%s'\n")
		    call pargstr (Memc[objects])
		call flush (STDERR)

	    } else {
		while (getline (ob, Memc[buf]) != EOF)
		    if (strmatch (Memc[buf], "^#\#") == 0)  {	# comment?
			call sscan (Memc[buf])
			    call gargr (cx)
			    call gargr (cy)
			    call gargi (index)

			if (nscan () == 3)
			    if (! IS_INDEFR (cx) && ! IS_INDEFR (cy))
				call newobject (tp, cp, nstars, cx, cy, index)
		    }
	    }

	}


	if (! interactive) {
	    if (autocenter)
		call center (Memc[image], boxsize, tp, cp, nstars, 0.,0.,false)

	    call tbtclo (tp)
	    call sfree (sp)
	    return
	}

	call salloc (flags, nstars, TY_INT)

	# This assumes that the centered coords have been initialized
	# to the predicted coords.  Allows reentering the task...

	call display (Memc[image], frame, fill)

	# display the uncentered sources first so user can see the
	# improvement from autocentering
	call overlay (frame, tp, cp, XCEN, YCEN, nstars,
	    subsample, mark, bcolor, NO)

	if (autocenter)
	    call center (Memc[image], boxsize, tp, cp, nstars, 0.,0.,false)
	call overlay (frame, tp, cp, XCEN, YCEN, nstars,
	    subsample, mark, gcolor, YES)

	call overlay (frame, tp, cp, XCEN, YCEN, nstars,
	    subsample, mark, ocolor, ISOBJ)

	redisplay = false
	every_source = false

	while (clgcur ("imcur", wx, wy, junk, key, Memc[cmd], SZ_LINE) != EOF) {

	    # Find the catalog source nearest the cursor (current subset)
	    index = nearest (wx, wy, tp, cp, nstars, cx, cy)

	    switch (key) {

	    # one-time toggle between single/all object(s)
	    # applies to the 'c', 'd', 'i', 'j', 'k', 'l', and 'u' keys
	    case 'a':
		if (every_source) {
		    every_source = false
		    call printf ("... `a' mode canceled\n")
		    call flush (STDOUT)
		} else {
		    every_source = true
		    call printf ("Press one of c,d,i,j,k,l,p,u\n")
		    call flush (STDOUT)
		}
		next

	    # redisplay only the bad object list
	    case 'b':
		call display (Memc[image], frame, fill)
		call overlay (frame, tp, cp, XCEN, YCEN, nstars,
		    subsample, mark, bcolor, NO)

	    # recenter object(s) relative to current catalog coordinates
	    case 'c':
		if (every_source) {
		    call center (Memc[image], boxsize, tp, cp, nstars,
			0., 0., true)

		    if (autodisplay)
			redisplay = true
		    else
			call overlay (frame, tp, cp, XCEN, YCEN, nstars,
			    subsample, mark, gcolor, YES)

		    every_source = false

		} else if (center_one (Memc[image],
		    boxsize, cx, cy, px, py) == OK) {

		    call newpoint (tp, cp, nstars, index, px, py)
		    call overlay_one (frame, px, py, mark, gcolor)

		} else {
		    call badpoint (tp, cp, nstars, index)
		    call overlay_one (frame, cx, cy, mark, bcolor)

		    call eprintf ("Centering failed at (%7.2f,%7.2f).\n")
			call pargr (cx)
			call pargr (cy)
		    call flush (STDERR)
		}

	    # delete object(s)
	    case 'd':
		if (every_source) {
		    call amovki (NO, Memi[flags], nstars)
		    call tbcpti (tp, cp[CENTER], Memi[flags], 1, nstars)
		    call tbtflu (tp)

		    if (autodisplay)
			redisplay = true
		    else
			call overlay (frame, tp, cp, XCEN, YCEN, nstars,
			    subsample, mark, bcolor, NO)

		    every_source = false

		} else {
		    call badpoint (tp, cp, nstars, index)
		    call overlay_one (frame, cx, cy, mark, bcolor)
		}

	    # fit the current catalog sample
	    case 'f':
		call tbtclo (tp)
		call tpltsol (Memc[image], Memc[table], Memc[database], update,
		    true, Memc[ra_ref], Memc[dec_ref], eq_ref)
		nstars = opentab (Memc[table], tp, cp)
		if (clgetb ("reselect"))
		    call tp_select (tp, cp)
		if (autodisplay)
		    redisplay = true

	    # redisplay only the centered object list
	    case 'g':
		call display (Memc[image], frame, fill)
		call overlay (frame, tp, cp, XCEN, YCEN, nstars,
		    subsample, mark, gcolor, YES)

	    # start over:  reinitialize to the raw position(s)
	    case 'i':
		if (every_source) {
		    call printf ("Reinitialize X,Y coords for ALL sources?  ")
		    if (clgetb ("go_ahead")) {
			call reinit (tp, cp, nstars)
			redisplay = true
			every_source = false
		    }
		} else {
		    call reinit_one (tp, cp, nstars, index, cx, cy)
		    call overlay_one (frame, cx, cy, mark, bcolor)
		}

	    # center object(s) relative to current catalog coordinates
	    case 'j':
		if (every_source) {
		    call center (Memc[image], boxsize, tp, cp, nstars,
			0., 0., false)

		    if (autodisplay)
			redisplay = true
		    else
			call overlay (frame, tp, cp, XCEN, YCEN, nstars,
			    subsample, mark, gcolor, YES)

		    every_source = false

		} else if (center_one (Memc[image],
		    boxsize, cx, cy, px, py) == OK) {

		    call newpoint (tp, cp, nstars, index, px, py)
		    call overlay_one (frame, px, py, mark, gcolor)

		} else {
		    call badpoint (tp, cp, nstars, index)
		    call overlay_one (frame, cx, cy, mark, bcolor)

		    call eprintf ("Centering failed at (%7.2f,%7.2f).\n")
			call pargr (cx)
			call pargr (cy)
		    call flush (STDERR)
		}

	    # center object(s), first shifting to current cursor coordinates
	    case 'k':
		if (center_one (Memc[image], boxsize, wx, wy, px, py) == OK) {
		    if (every_source) {
			xshift = cx - px
			yshift = cy - py
			call center (Memc[image], boxsize, tp, cp, nstars,
			    xshift, yshift, false)

			if (autodisplay)
			    redisplay = true
			else
			    call overlay (frame, tp, cp, XCEN, YCEN, nstars,
				subsample, mark, gcolor, YES)

			every_source = false

		    } else {
			call newpoint (tp, cp, nstars, index, px, py)
			call overlay_one (frame, px, py, mark, gcolor)
		    }

		} else {
		    call badpoint (tp, cp, nstars, index)
		    call overlay_one (frame, cx, cy, mark, bcolor)

		    call eprintf ("Centering failed at (%7.2f,%7.2f).\n")
			call pargr (cx)
			call pargr (cy)
		    call flush (STDERR)
		}

	    # center object(s), selecting the source explicitly
	    case 'l':
		call printf ("Move cursor to matching source, type `l':\n")
		call flush (STDOUT)
		junk = clgcur ("imcur", wx, wy, junk, key, Memc[cmd], SZ_LINE)

		if (key != 'l') {
		    call printf ("`l' cancelled, continue with next command.\n")
		    call flush (STDOUT)
		    next
		}

		if (center_one (Memc[image], boxsize, wx, wy, px, py) == OK) {
		    if (every_source) {
			xshift = cx - px
			yshift = cy - py
			call center (Memc[image], boxsize, tp, cp, nstars,
			    xshift, yshift, false)

			if (autodisplay)
			    redisplay = true
			else
			    call overlay (frame, tp, cp, XCEN, YCEN, nstars,
				subsample, mark, gcolor, YES)

			every_source = false

		    } else {
			call newpoint (tp, cp, nstars, index, px, py)
			call overlay_one (frame, px, py, mark, gcolor)
		    }

		} else {
		    call badpoint (tp, cp, nstars, index)
		    call overlay_one (frame, cx, cy, mark, bcolor)

		    call eprintf ("Centering failed at (%7.2f,%7.2f).\n")
			call pargr (cx)
			call pargr (cy)
		    call flush (STDERR)
		}

	    # overlay the raw coordinates
	    case 'o':
		call overlay (frame, tp, cp, XPRED, YPRED, nstars,
		    subsample, ovmark, bcolor, YES)
		call overlay (frame, tp, cp, XPRED, YPRED, nstars,
		    subsample, ovmark, bcolor, NO)

#	    # recenter program objects, first shifting to current cursor coords
#	    case 'p':
#		if (center_one (Memc[image], boxsize, wx, wy, px, py) == OK) {
#		    if (every_source) {
#			xshift = cx - px
#			yshift = cy - py
#			call pcenter (Memc[image], boxsize, tp, cp, nstars,
#			    xshift, yshift, true)
#
#			if (autodisplay)
#			    redisplay = true
#			else
#			    call overlay (frame, tp, cp, XCEN, YCEN, nstars,
#				subsample, mark, ocolor, ISOBJ)
#
#			every_source = false
#
#		    } else {
#			call newpoint (tp, cp, nstars, index, px, py)
#			call overlay_one (frame, px, py, mark, gcolor)
#		    }
#
#		} else {
#		    call badpoint (tp, cp, nstars, index)
#		    call overlay_one (frame, cx, cy, mark, bcolor)
#
#		    call eprintf ("Centering failed at (%7.2f,%7.2f).\n")
#			call pargr (cx)
#			call pargr (cy)
#		    call flush (STDERR)
#		}


	    # exit the program (EOF also works)
	    case 'q':
		break

	    # redisplay both the centered and bad lists
	    case 'r':
		redisplay = true

	    # undelete object(s)
	    case 'u':
		if (every_source) {
		    call amovki (YES, Memi[flags], nstars)
		    call tbcpti (tp, cp[CENTER], Memi[flags], 1, nstars)
		    call tbtflu (tp)
		    call overlay (frame, tp, cp, XCEN, YCEN, nstars,
			subsample, mark, gcolor, YES)
		    every_source = false

		} else {
		    call goodpoint (tp, cp, nstars, index)
		    call overlay_one (frame, cx, cy, mark, gcolor)
		}

#	    # add an extra (program) object (won't participate in the fit)
#	    case 'x':
#		# point and prompt for a name
#		if (center_one (Memc[image], boxsize, wx, wy, px, py) == OK) {
#		    call newobject (tp, cp, nstars, px, py, nstars+1)
#		    call overlay_one (frame, px, py, mark, ocolor)
#
#		} else {
#		    call eprintf ("Couldn't center object!\n")
#		    call flush (STDERR)
#		}

	    # page keystroke helpfile
	    case '?':
		call pagefiles (KEYHELP)

	    # colon commands
	    case ':':
		redisplay = tp_colon (Memc[image], Memc[table], nstars, tp, cp,
		    Memc[cmd], update, autodisplay, boxsize, subsample, pangle,
		    xscale, yscale, ixshift, iyshift, bcolor, gcolor,
		    mark, ovmark)

	    # beep and mention '?' help
	    default:
		call printf ("\007unknown command, type `?' for help\n")
		call flush (STDOUT)

	    }

	    if (every_source) {
		every_source = false
		call printf ("... `a' mode canceled\n")
		call flush (STDOUT)
	    }

	    if (redisplay) {
		call display (Memc[image], frame, fill)

		call overlay (frame, tp, cp, XCEN, YCEN, nstars,
		    subsample, mark, gcolor, YES)
		call overlay (frame, tp, cp, XCEN, YCEN, nstars,
		    subsample, mark, bcolor, NO)
		call overlay (frame, tp, cp, XCEN, YCEN, nstars,
		    subsample, mark, ocolor, ISOBJ)

		redisplay = false
	    }
	}

	call tbtclo (tp)
	call sfree (sp)
end


# TP_COLON -- do the colon commands.

bool procedure tp_colon (image, table, nstars, tp, cp, command, update,
    autodisplay, boxsize, subsample, pangle, xscale, yscale, xshift, yshift,
    bcolor, gcolor, mark, ovmark)

char	image[ARB]		#I image name
char	table[ARB]		#I table name
int	nstars			#I number of entries in the table
pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I array of column pointers
char	command[ARB]		#I command line (after `:')
bool	update			#U update image header wcs?
bool	autodisplay		#U redisplay following all source toggle?
int	boxsize			#U centering boxsize (will be odd)
int	subsample		#U subsample factor for catalog objects
real	pangle			#U relative position angle
real	xscale			#U relative X scale, percent
real	yscale			#U relative Y scale, percent
int	xshift			#U relative X axis shift
int	yshift			#U relative Y axis shift
char	bcolor[SZ_NAME]		#U bad color
char	gcolor[SZ_NAME]		#U good color
char	mark[SZ_NAME]		#U marker type
char	ovmark[SZ_NAME]		#U overlay (raw) marker type

char	cmd[SZ_NAME]
char	arg[SZ_NAME]
char	arg2[SZ_NAME]

bool	redisplay, ad, upd
pointer	sp, sp1, buf, longarg, longarg2, logfile
int	bs, ss, xs, ys
real	pa, xsc, ysc

int	strdic(), strcmp(), nscan()
bool	clgetb(), fp_equalr()

begin
	call smark (sp)
	call salloc (longarg, SZ_FNAME, TY_CHAR)
	call salloc (longarg2, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	redisplay = false

	call sscan (command)
	    call gargwrd (cmd, SZ_NAME)
	    call gargwrd (Memc[longarg], SZ_FNAME)
	    call gargwrd (Memc[longarg2], SZ_FNAME)

	call strcpy (Memc[longarg], arg, SZ_NAME)
	call strcpy (Memc[longarg2], arg2, SZ_NAME)

	switch (strdic (cmd, cmd, SZ_NAME, CMDDICT)) {
	case AUTODISPLAY:
	    if (nscan () == 2) {
		call sscan (arg)
		    call gargb (ad)

		if (nscan () != 1) {
		    call printf ("error reading autodisplay `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    autodisplay = ad

		}

	    } else {
		call printf ("autodisplay %b\n")
		    call pargb (autodisplay)
		call flush (STDOUT)

	    }

	case BOXSIZE:
	    if (nscan () == 2) {
		call sscan (arg)
		    call gargi (bs)

		if (nscan () != 1) {
		    call printf ("error reading boxsize `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else if (mod (bs, 2) != 1) {
		    boxsize = bs + 1
		    call printf ("boxsize must be odd, using boxsize = %d\n")
			call pargi (boxsize)
		    call flush (STDOUT)

		} else {
		    boxsize = bs

		}

	    } else {
		call printf ("boxsize = %d\n")
		    call pargi (boxsize)
		call flush (STDOUT)

	    }

	case BADCOLOR:
	    if (nscan () == 2) {
		if (strdic (arg, arg, SZ_NAME, COLORDICT) == 0) {
		    call printf ("unknown color `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    call strcpy (arg, bcolor, SZ_NAME)

		}

	    } else {
		call printf ("badcolor = %s\n")
		    call pargstr (bcolor)
		call flush (STDOUT)

	    }

	case GOODCOLOR:
	    if (nscan () == 2) {
		if (strdic (arg, arg, SZ_NAME, COLORDICT) == 0) {
		    call printf ("unknown color `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    call strcpy (arg, gcolor, SZ_NAME)

		}

	    } else {
		call printf ("goodcolor = %s\n")
		    call pargstr (gcolor)
		call flush (STDOUT)

	    }

	case EPARAM:
	    if (nscan () != 2) {
		call printf ("command needs an argument (%s|)\n")
		    call pargstr (TASKDICT)
		call flush (STDOUT)

	    } else if (strdic (arg, arg, SZ_NAME, TASKDICT) == 0) {
		call printf ("invalid parameter set `%s'\n")
		    call pargstr (arg)
		call flush (STDOUT)

	    } else {
		call smark (sp1)
		call salloc (buf, SZ_FNAME, TY_CHAR)
		call sprintf (Memc[buf], SZ_FNAME, "eparam %s")
		    call pargstr (arg)

		call clcmdw (Memc[buf])

		call sfree (sp1)

		if (strcmp (arg, "selectpars") == 0) {
		    call tp_select (tp, cp)
		    redisplay = true
		}
	    }

	case MARKER:
	    if (nscan () == 2) {
		if (strdic (arg, arg, SZ_NAME, MARKDICT) == 0) {
		    call printf ("unknown marker type `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    call strcpy (arg, mark, SZ_NAME)

		}

	    } else {
		call printf ("marker = %s\n")
		    call pargstr (mark)
		call flush (STDOUT)

	    }

	case OMARKER:
	    if (nscan () == 2) {
		if (strdic (arg, arg, SZ_NAME, MARKDICT) == 0) {
		    call printf ("unknown marker type `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    call strcpy (arg, ovmark, SZ_NAME)

		}

	    } else {
		call printf ("omarker = %s\n")
		    call pargstr (ovmark)
		call flush (STDOUT)

	    }

	case REPLACE:
	    call printf ("Replace predicted X,Y coords for all sources?  ")
	    if (clgetb ("go_ahead")) {
		call replace (tp, cp, nstars)
		redisplay = false
		call printf ("Done!\n")

	    } else {
		call printf ("Predicted coordinates not replaced.\n")

	    }

	case ROTATE:
	    if (nscan () == 2) {
		call sscan (arg)
		    call gargr (pa)

		if (nscan () != 1) {
		    call printf ("error reading rotate argument `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		call printf ("Reposition ALL catalog sources %.4g degrees?  ")
			call pargr (pa)
		    call flush (STDOUT)

		    if (clgetb ("go_ahead")) {
			pangle = pa
			call rotate (image, tp, cp, nstars, pangle)
			redisplay = true

		    } else {
			call printf ("rotate unchanged\n")

		    }

		}

	    } else {
		call printf ("latest rotate argument = %.4g degrees\n")
		    call pargr (pangle)
		call flush (STDOUT)

	    }

	case SCALE:
	    if (nscan () == 3) {
		call sscan (arg)
		    call gargr (xsc)

		if (nscan () != 1) {
		    call printf ("error reading X scale factor `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    call sscan (arg2)
			call gargr (ysc)

		    if (nscan () != 1) {
			call printf ("error reading Y scale factor `%s'\n")
			    call pargstr (arg2)
			call flush (STDOUT)

		    } else {
		call printf ("Scale ALL catalog sources (%.2g,%.2g) percent?  ")
			    call pargr (xsc)
			    call pargr (ysc)
			call flush (STDOUT)

			if (clgetb ("go_ahead")) {
			    xscale = xsc
			    yscale = ysc
			    call scale (image, tp, cp, nstars, xscale, yscale)
			    redisplay = true

			} else {
			    call printf ("scale unchanged\n")

			}
		    }
		}

	    } else if (nscan () == 2) {
		call sscan (arg)
		    call gargr (xsc)

		if (nscan () != 1) {
		    call printf ("error reading X scale factor `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    ysc = xsc

		    call printf ("Scale ALL catalog sources %.2g percent?  ")
			call pargr (xsc)
		    call flush (STDOUT)

		    if (clgetb ("go_ahead")) {
			xscale = xsc
			yscale = ysc
			call scale (image, tp, cp, nstars, xscale, yscale)
			redisplay = true

		    } else {
			call printf ("scale unchanged\n")

		    }
		}

	    } else {
		if (fp_equalr (xscale, yscale)) {
		    call printf ("latest scale factor = %.2g percent\n")
			call pargr (xscale)

		} else {
		    call printf ("latest scale factor = (%.2g,%.2g) percent\n")
			call pargr (xscale)
			call pargr (yscale)

		}

		call flush (STDOUT)

	    }

	case SHIFT:
	    if (nscan () == 3) {
		call sscan (arg)
		    call gargi (xs)

		if (nscan () != 1) {
		    call printf ("error reading X shift `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    call sscan (arg2)
			call gargi (ys)

		    if (nscan () != 1) {
			call printf ("error reading Y shift `%s'\n")
			    call pargstr (arg2)
			call flush (STDOUT)

		    } else {
		    call printf ("Shift ALL catalog sources (%d,%d) pixels?  ")
			    call pargi (xs)
			    call pargi (ys)
			call flush (STDOUT)

			if (clgetb ("go_ahead")) {
			    xshift = xs
			    yshift = ys
			    call shift (image, tp, cp, nstars, xshift, yshift)
			    redisplay = true

			} else {
			    call printf ("shift unchanged\n")

			}
		    }
		}

	    } else if (nscan () == 2) {
	    call printf ("error with `%s %s', both X&Y shifts are required\n")
		    call pargstr (arg)
		    call pargstr (arg2)
		call flush (STDOUT)

	    } else {
		call printf ("latest shift = (%d,%d) pixels\n")
		    call pargi (xshift)
		    call pargi (yshift)
		call flush (STDOUT)

	    }

	case SHOW:
	    if (nscan () == 2) {
		call strcpy (Memc[longarg], Memc[logfile], SZ_FNAME)
		call printlog (table, Memc[logfile], false)

	    } else {
		call printlog (table, "", true)

	    }

	case SUBSAMPLE:
	    if (nscan () == 2) {
		call sscan (arg)
		    call gargi (ss)

		if (nscan () != 1) {
		    call printf ("error reading subsample factor `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else if (ss < 1) {
		    subsample = 1
		    redisplay = true
		    call printf (
			"subsampling must be >= 1, using subsample = %d\n")
			call pargi (subsample)
		    call flush (STDOUT)

		} else {
		    subsample = ss
		    redisplay = true

		}

	    } else {
		call printf ("subsample = %d\n")
		    call pargi (subsample)
		call flush (STDOUT)

	    }

	case UPDATE:
	    if (nscan () == 2) {
		call sscan (arg)
		    call gargb (upd)

		if (nscan () != 1) {
		    call printf ("error reading autodisplay `%s'\n")
			call pargstr (arg)
		    call flush (STDOUT)

		} else {
		    update = upd

		}

	    } else {
		call printf ("update %b\n")
		    call pargb (update)
		call flush (STDOUT)

	    }

	default:
	    call printf ("\007unknown colon command, `%s', type `?' for help\n")
		call pargstr (cmd)
	    call flush (STDOUT)
	}

	call sfree (sp)
	return (redisplay)
end


# OPENTAB -- open the catalog table and locate the columns.

int procedure opentab (table, tp, cp)

char	table[ARB]		#I input (raw) table name
pointer	tp			#O table pointer
pointer	cp[NUM_COLS]		#O array of column pointers

pointer	sp, buf, pp
char	colname[SZ_COLNAME, NUM_COLS]

pointer	clopset(), tbtopn()
int	tbpsta()

errchk	tbtopn, tbcfnd

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	call fseti (CLIN, F_CANCEL, OK)

	call clgstr ("catpars", Memc[buf], SZ_FNAME)
	pp = clopset (Memc[buf])

	call sfree (sp)

	call clgpset (pp, "xpred_col", colname[1,1], SZ_COLNAME)
	call clgpset (pp, "ypred_col", colname[1,2], SZ_COLNAME)
	call clgpset (pp, "xcen_col", colname[1,3], SZ_COLNAME)
	call clgpset (pp, "ycen_col", colname[1,4], SZ_COLNAME)
	call clgpset (pp, "cerr_col", colname[1,5], SZ_COLNAME)
	call clgpset (pp, "sub_col", colname[1,6], SZ_COLNAME)
	call clgpset (pp, "cen_col", colname[1,7], SZ_COLNAME)
	call clgpset (pp, "obj_col", colname[1,8], SZ_COLNAME)

	call clgpset (pp, "id_col", colname[1,9], SZ_COLNAME)

	call clcpset (pp)

	# open the tables, find the columns, return the number of rows
	tp = tbtopn (table, READ_WRITE, 0)
	call tbcfnd (tp, colname, cp, NUM_COLS)
	return (tbpsta (tp, TBL_NROWS))
end


# NEAREST -- Find the catalog object nearest the pixel (cursor) position
# by minimizing the squared cartesian distance.  Returns the catalog index.
# Only examines the sources in the current subset.

int procedure nearest (wx, wy, tp, cp, n, cx, cy)

real	wx, wy			#I cursor coordinates
pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	n			#I total number of entries in the table
real	cx, cy			#O catalog coordinates of selected object

pointer	sp, x, y, s, xf, yf, sf
real	dx, dy, r2, r2min
int	index, i

begin
	call smark (sp)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (s, n, TY_INT)
	call salloc (xf, n, TY_BOOL)
	call salloc (yf, n, TY_BOOL)
	call salloc (sf, n, TY_BOOL)

	call tbcgtr (tp, cp[XCEN], Memr[x], Memb[xf], 1, n)
	call tbcgtr (tp, cp[YCEN], Memr[y], Memb[yf], 1, n)
	call tbcgti (tp, cp[SUBSET], Memi[s], Memb[sf], 1, n)

	r2min = MAX_REAL
	index  = 1		# should never need this default

	do i = 1, n {
	    if (Memb[xf+i-1] || Memb[yf+i-1] || Memb[sf+i-1])
		next

	    if (Memi[s+i-1] == YES) {
		dx = wx - Memr[x+i-1]
		dy = wy - Memr[y+i-1]

		r2 = dx ** 2 + dy ** 2

		if (r2 < r2min) {
		    r2min = r2
		    index = i
		}
	    }
	}

	cx = Memr[x+index-1]
	cy = Memr[y+index-1]

	call sfree (sp)
	return (index)
end


# DISPLAY -- construct a command line and display an image.

procedure display (image, frame, fill)

char	image[ARB]		#I image name to display
int	frame			#I frame number to display it in
bool	fill			#I fill the frame

pointer	sp, cmdline

begin
	call smark (sp)
	call salloc (cmdline, SZ_LINE, TY_CHAR)

	call sprintf (Memc[cmdline], SZ_LINE, DISPCMD)
	    call pargstr (image)
	    call pargi (frame)
	    call pargb (fill)

	# currently error checking doesn't work
	call clcmdw (Memc[cmdline])

	call sfree (sp)
end


# TPLTSOL -- fit the currently centered catalog sources.

procedure tpltsol (image, table, database, update, refitcat, ra, dec, eq)

char	image[ARB]		#I image name
char	table[ARB]		#I table name
char	database[ARB]		#I output database name
bool	update			#I update image header WCS?
bool	refitcat		#I update uncentered catalog source XY's?
char	ra[ARB]			#I plate center RA
char	dec[ARB]		#I plate center Dec
real	eq			#I plate center equinox

pointer	sp, cmdline

begin
	call smark (sp)
	call salloc (cmdline, SZ_LINE, TY_CHAR)

	if (update) {
	    if (refitcat) {
		call sprintf (Memc[cmdline], SZ_LINE, FITCMD)
		    call pargstr (image)
		    call pargstr (table)
		    call pargstr (database)
		    call pargstr ("yes")
		    call pargstr ("yes")
		    call pargstr (ra)
		    call pargstr (dec)
		    call pargr (eq)
	    } else {
		call sprintf (Memc[cmdline], SZ_LINE, FITCMD)
		    call pargstr (image)
		    call pargstr (table)
		    call pargstr (database)
		    call pargstr ("yes")
		    call pargstr ("no")
		    call pargstr (ra)
		    call pargstr (dec)
		    call pargr (eq)
	    }

	} else {
	    if (refitcat) {
		call sprintf (Memc[cmdline], SZ_LINE, FITCMD)
		    call pargstr (image)
		    call pargstr (table)
		    call pargstr (database)
		    call pargstr ("no")
		    call pargstr ("yes")
		    call pargstr (ra)
		    call pargstr (dec)
		    call pargr (eq)
	    } else {
		call sprintf (Memc[cmdline], SZ_LINE, FITCMD)
		    call pargstr (image)
		    call pargstr (table)
		    call pargstr (database)
		    call pargstr ("no")
		    call pargstr ("no")
		    call pargstr (ra)
		    call pargstr (dec)
		    call pargr (eq)
	    }

	}

	call clcmdw (Memc[cmdline])

	call sfree (sp)
end


# PRINTLOG -- dump table log information into a file

procedure printlog (table, logfile, pageit)

char	table[ARB]		#I table name
char	logfile[ARB]		#I logfile name
bool	pageit			#I page output from scratch file?

pointer	sp, cmdline, tmpfile, rewrite

int	access(), strcmp()

begin
	call smark (sp)
	call salloc (cmdline, SZ_LINE, TY_CHAR)
	call salloc (tmpfile, SZ_FNAME, TY_CHAR)
	call salloc (rewrite, SZ_FNAME, TY_CHAR)

	if (pageit) {
	    call mktemp ("tmp$JUNK", Memc[tmpfile], SZ_FNAME)

	} else {
	    if (access (logfile, 0, 0) == YES) {
		call printf ("File `%s' exists")
		    call pargstr (logfile)
		call flush (STDOUT)

		call clgstr ("_qpars.rewrite", Memc[rewrite], SZ_FNAME)

		if (strcmp (Memc[rewrite], "cancel") == 0)
		    return
		else if (strcmp (Memc[rewrite], "replace") == 0)
		    call delete (logfile)
	    }

	    call strcpy (logfile, Memc[tmpfile], SZ_FNAME)
	}

	call sprintf (Memc[cmdline], SZ_LINE, LOGCMD)
	    call pargstr (table)
	    call pargstr (Memc[tmpfile])

	call clcmdw (Memc[cmdline])

	if (pageit) {
	    call pagefiles (Memc[tmpfile])
	    if (access (Memc[tmpfile], 0, 0) == YES)
		call delete (Memc[tmpfile])
	}

	call sfree (sp)
end


# OVERLAY -- construct a command line and overlay marks on the display.

procedure overlay (frame, tp, cp, xcol, ycol, n, subsample,
    marktype, color, valid)

int	frame			#I frame number to display it in
pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	xcol, ycol		#I indices into the column ptr array
int	n			#I total number of entries in the table
int	subsample		#I subsample factor for catalog objects
char	marktype[ARB]		#I type of mark to draw
char	color[ARB]		#I color of mark to draw
int	valid			#I mark valid (or invalid) sources?

pointer	sp, cmdline, tmp, x, y, s, c, o, xf, yf, sf, cf, of
int	colorcode, i, fd, nsample, nsubsample

int	open(), strdic()

begin
	call smark (sp)
	call salloc (cmdline, SZ_LINE, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)

	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (s, n, TY_INT)
	call salloc (c, n, TY_INT)
	call salloc (o, n, TY_INT)

	call salloc (xf, n, TY_BOOL)
	call salloc (yf, n, TY_BOOL)
	call salloc (sf, n, TY_BOOL)
	call salloc (cf, n, TY_BOOL)
	call salloc (of, n, TY_BOOL)

	if (valid == YES)
	    call printf ("\nMarking centered catalog sources in ")
	else if (valid == NO)
	    call printf ("\nMarking uncentered catalog sources in ")
	else if (valid == ISOBJ)
	    call printf ("\nMarking objects in ")

	switch (strdic (color, Memc[tmp], SZ_FNAME, COLORDICT)) {
	case BLACK:
	    colorcode = BLACKCODE
	    call printf ("black...\n")
	case WHITE:
	    colorcode = WHITECODE
	    call printf ("white...\n")
	case RED:
	    colorcode = REDCODE
	    call printf ("red...\n")
	case GREEN:
	    colorcode = GREENCODE
	    call printf ("green...\n")
	case BLUE:
	    colorcode = BLUECODE
	    call printf ("blue...\n")
	case YELLOW:
	    colorcode = YELLOWCODE
	    call printf ("yellow...\n")
	}

	call flush (STDOUT)

	call tbcgtr (tp, cp[xcol], Memr[x], Memb[xf], 1, n)
	call tbcgtr (tp, cp[ycol], Memr[y], Memb[yf], 1, n)
	call tbcgti (tp, cp[SUBSET], Memi[s], Memb[sf], 1, n)
	call tbcgti (tp, cp[CENTER], Memi[c], Memb[cf], 1, n)
	call tbcgti (tp, cp[OBJECT], Memi[o], Memb[of], 1, n)

	# dump valid or invalid X,Y pairs from the table to the tmp file
	call mktemp ("tmp$JUNK", Memc[tmp], SZ_FNAME)
	fd = open (Memc[tmp], NEW_FILE, TEXT_FILE)

	nsample = 0
	nsubsample = 0

	do i = 1, n {
	    if (Memb[xf+i-1] || Memb[yf+i-1] || Memb[sf+i-1] ||
		Memb[cf+i-1] || Memb[of+i-1]) {

		next
	    }

	    # not sure what tbcgti returns if the sf flag (above) was set
	    if (Memi[s+i-1] == NO)
		next

	    # hard to phrase this elegantly
	    if (Memi[c+i-1] == valid && Memi[o+i-1] == NO)
		;
	    else if (valid == ISOBJ && Memi[o+i-1] == YES)
		;
	    else
		next

	    nsample = nsample + 1

	    # always include the first source in the subsample
	    if (valid != ISOBJ && subsample > 1) {
		if (mod (nsample, subsample) != 1)
		    next
	    }

	    nsubsample = nsubsample + 1

	    call fprintf (fd, "%g %g\n")
		call pargr (Memr[x+i-1])
		call pargr (Memr[y+i-1])
	}

	call close (fd)

	if (valid == ISOBJ) {
	    if (nsample <= 0) {
		call printf ("  no objects to mark\n")
	    } else {
		call printf ("  all %d objects will be marked...")
		    call pargi (nsample)
	    }

	} else if (subsample > 1) {
	    if (nsample <= 0) {
		call printf ("  no sources to mark\n")
	    } else if (nsubsample <= 0) {
		call printf("  no sources remain to mark out of %d total")
		    call pargi (nsample)
		call printf(" (SUBSAMPLED 1:%d)\n")
		    call pargi (subsample)
	    } else {
		call printf ("  marking %d sources out of %d total")
		    call pargi (nsubsample)
		    call pargi (nsample)
		call printf(" (SUBSAMPLED 1:%d)...")
		    call pargi (subsample)
	    }

	} else {
	    if (nsample <= 0) {
		call printf ("  no sources to mark\n")
	    } else {
		call printf ("  all %d sources will be marked...")
		    call pargi (nsample)
	    }
	}

	call flush (STDOUT)

	if (nsubsample > 0) {
	    call sprintf (Memc[cmdline], SZ_LINE, MARKCMD)
		call pargi (frame)
		call pargstr (Memc[tmp])
		call pargstr (marktype)
		call pargi (colorcode)

	    call clcmdw (Memc[cmdline])

	    call printf ("done\n")
	    call flush (STDOUT)
	}

	call delete (Memc[tmp])
	call sfree (sp)
end


# CENTER -- center the sources.

procedure center (image, boxsize, tp, cp, n, xshift, yshift, update)

char	image[ARB]		#I image name
int	boxsize			#I centering box fullwidth
pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	n			#I total number of entries in the table
real	xshift, yshift		#I X and Y shift to add before centering
bool	update			#I update centered or uncentered sources?

pointer	sp, cmdline, coords, shifts, tmp, x, y, c, s, o, xf, yf, cf, sf, of
int	cfd, sfd, tfd, junk, i, index
real	xout, yout

int	open(), getline(), fscan(), nscan()

begin
	call smark (sp)
	call salloc (cmdline, SZ_LINE, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (shifts, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (c, n, TY_INT)
	call salloc (s, n, TY_INT)
	call salloc (o, n, TY_INT)
	call salloc (xf, n, TY_BOOL)
	call salloc (yf, n, TY_BOOL)
	call salloc (cf, n, TY_BOOL)
	call salloc (sf, n, TY_BOOL)
	call salloc (of, n, TY_BOOL)

	call printf ("\nCentering catalog sources...")
	call flush (STDOUT)

	call tbcgtr (tp, cp[XCEN], Memr[x], Memb[xf], 1, n)
	call tbcgtr (tp, cp[YCEN], Memr[y], Memb[yf], 1, n)
	call tbcgti (tp, cp[CENTER], Memi[c], Memb[cf], 1, n)
	call tbcgti (tp, cp[SUBSET], Memi[s], Memb[cf], 1, n)
	call tbcgti (tp, cp[OBJECT], Memi[o], Memb[of], 1, n)

	call mktemp ("tmp$JUNK", Memc[coords], SZ_FNAME)
	call mktemp ("tmp$JUNK", Memc[shifts], SZ_FNAME)
	call mktemp ("tmp$JUNK", Memc[tmp], SZ_FNAME)

	cfd = open (Memc[coords], NEW_FILE, TEXT_FILE)

# need to handle undefined coordinates

	do i = 1, n {
	    call fprintf (cfd, "%g %g\n")
		call pargr (Memr[x+i-1])
		call pargr (Memr[y+i-1])
	}

	call close (cfd)

	sfd = open (Memc[shifts], NEW_FILE, TEXT_FILE)

	call fprintf (sfd, "%g %g\n")
	    call pargr (xshift)
	    call pargr (yshift)

	call close (sfd)

	call sprintf (Memc[cmdline], SZ_LINE, CENTCMD)
	    call pargstr (image)
	    call pargstr (Memc[coords])
	    call pargstr (Memc[shifts])
	    call pargi (boxsize)
	    call pargstr (Memc[tmp])

	call clcmdw (Memc[cmdline])

	call delete (Memc[coords])
	call delete (Memc[shifts])

	tfd = open (Memc[tmp], READ_ONLY, TEXT_FILE)
	junk = getline (tfd, Memc[shifts])   # eat the comment, recycle shifts

	do index = 1, n {
	    # only update the current subset
	    if (Memi[s+index-1] == NO)
		next

	    # don't update the program objects!
	    if (Memi[o+index-1] == YES)
		next

	    # want to shift sources for which the centering failed
	    if (update == (Memi[c+index-1] == YES)) {
		Memr[x+index-1] = Memr[x+index-1] - xshift
		Memr[y+index-1] = Memr[y+index-1] - yshift
	    }
	}

	index = 1		# in case first read fails
	for (i=1; i <= n && fscan (tfd) != EOF; i=i+1) {
	    call gargwrd (Memc[shifts], SZ_LINE)
	    call gargr (xout)
	    call gargwrd (Memc[coords], SZ_LINE)	# ignore it
	    call gargr (yout)
	    call gargwrd (Memc[coords], SZ_LINE)	# ignore it
	    call gargi (index)

	    # look for `Warning: failed to converge near ...',  don't worry
	    # about previously centered sources that flunked this time
	    if (Memc[shifts] != 'W' && nscan () == 6) {

		# only update the current subset
		if (Memi[s+index-1] == NO)
		    next

		# don't update the program objects!
		if (Memi[o+index-1] == YES)
		    next

		# either only update centered sources or
		# only update previously uncentered sources
	        if (update == (Memi[c+index-1] == YES)) {
		    Memr[x+index-1] = xout
		    Memr[y+index-1] = yout
		    Memi[c+index-1] = YES

		}

	    }
	}

	call tbcptr (tp, cp[XCEN], Memr[x], 1, n)
	call tbcptr (tp, cp[YCEN], Memr[y], 1, n)
	call tbcpti (tp, cp[CENTER], Memi[c], 1, n)
	call tbtflu (tp)

	call close (tfd)
	call delete (Memc[tmp])
	call sfree (sp)

	call printf ("done\n")
	call flush (STDOUT)
end


# PCENTER -- center the program sources.

procedure pcenter (image, boxsize, tp, cp, n, xshift, yshift, update)

char	image[ARB]		#I image name
int	boxsize			#I centering box fullwidth
pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	n			#I total number of entries in the table
real	xshift, yshift		#I X and Y shift to add before centering
bool	update			#I update centered or uncentered sources?

pointer	sp, cmdline, coords, shifts, tmp, x, y, c, s, o, xf, yf, cf, sf, of
int	cfd, sfd, tfd, junk, i, index
real	xout, yout

int	open(), getline(), fscan(), nscan()

begin
	call smark (sp)
	call salloc (cmdline, SZ_LINE, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (shifts, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (c, n, TY_INT)
	call salloc (s, n, TY_INT)
	call salloc (o, n, TY_INT)
	call salloc (xf, n, TY_BOOL)
	call salloc (yf, n, TY_BOOL)
	call salloc (cf, n, TY_BOOL)
	call salloc (sf, n, TY_BOOL)
	call salloc (of, n, TY_BOOL)

	call tbcgtr (tp, cp[XCEN], Memr[x], Memb[xf], 1, n)
	call tbcgtr (tp, cp[YCEN], Memr[y], Memb[yf], 1, n)
	call tbcgti (tp, cp[CENTER], Memi[c], Memb[cf], 1, n)
	call tbcgti (tp, cp[SUBSET], Memi[s], Memb[cf], 1, n)
	call tbcgti (tp, cp[OBJECT], Memi[o], Memb[of], 1, n)

	call mktemp ("tmp$JUNK", Memc[coords], SZ_FNAME)
	call mktemp ("tmp$JUNK", Memc[shifts], SZ_FNAME)
	call mktemp ("tmp$JUNK", Memc[tmp], SZ_FNAME)

	cfd = open (Memc[coords], NEW_FILE, TEXT_FILE)

# need to handle undefined coordinates

	do i = 1, n {
	    call fprintf (cfd, "%g %g\n")
		call pargr (Memr[x+i-1])
		call pargr (Memr[y+i-1])
	}

	call close (cfd)

	sfd = open (Memc[shifts], NEW_FILE, TEXT_FILE)

	call fprintf (sfd, "%g %g\n")
	    call pargr (xshift)
	    call pargr (yshift)

	call close (sfd)

	call sprintf (Memc[cmdline], SZ_LINE, CENTCMD)
	    call pargstr (image)
	    call pargstr (Memc[coords])
	    call pargstr (Memc[shifts])
	    call pargi (boxsize)
	    call pargstr (Memc[tmp])

	call clcmdw (Memc[cmdline])

	call delete (Memc[coords])
	call delete (Memc[shifts])

	tfd = open (Memc[tmp], READ_ONLY, TEXT_FILE)
	junk = getline (tfd, Memc[shifts])   # eat the comment, recycle shifts

	do index = 1, n {
	    # only update the current subset
	    if (Memi[s+index-1] == NO)
		next

	    # don't update the reference objects!
	    if (Memi[o+index-1] == NO)
		next

	    # want to shift sources for which the centering failed
	    if (update == (Memi[c+index-1] == YES)) {
		Memr[x+index-1] = Memr[x+index-1] - xshift
		Memr[y+index-1] = Memr[y+index-1] - yshift
	    }
	}

	index = 1		# in case first read fails
	for (i=1; i <= n && fscan (tfd) != EOF; i=i+1) {
	    call gargwrd (Memc[shifts], SZ_LINE)
	    call gargr (xout)
	    call gargwrd (Memc[coords], SZ_LINE)	# ignore it
	    call gargr (yout)
	    call gargwrd (Memc[coords], SZ_LINE)	# ignore it
	    call gargi (index)

	    # look for `Warning: failed to converge near ...',  don't worry
	    # about previously centered sources that flunked this time
	    if (Memc[shifts] != 'W' && nscan () == 6) {

		# only update the current subset
		if (Memi[s+index-1] == NO)
		    next

		# don't update the reference objects!
		if (Memi[o+index-1] == NO)
		    next

		# either only update centered sources or
		# only update previously uncentered sources
	        if (update == (Memi[c+index-1] == YES)) {
		    Memr[x+index-1] = xout
		    Memr[y+index-1] = yout
		    Memi[c+index-1] = YES

		}

	    }
	}

	call tbcptr (tp, cp[XCEN], Memr[x], 1, n)
	call tbcptr (tp, cp[YCEN], Memr[y], 1, n)
	call tbcpti (tp, cp[CENTER], Memi[c], 1, n)
	call tbtflu (tp)

	call close (tfd)
	call delete (Memc[tmp])
	call sfree (sp)
end


# ROTATE -- rotate the catalog sources, mark as uncentered.

procedure rotate (image, tp, cp, n, pangle)

char	image[ARB]		#I image name
pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	n			#I total number of entries in the table
real	pangle			#I relative position angle

pointer	sp, x, y, c, o, xf, yf, cf, of, im
int	index
real	xx, yy, xmiddle, ymiddle, rpangle

pointer	immap()

errchk immap, imunmap

begin
	call smark (sp)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (c, n, TY_INT)
	call salloc (o, n, TY_INT)
	call salloc (xf, n, TY_BOOL)
	call salloc (yf, n, TY_BOOL)
	call salloc (cf, n, TY_BOOL)
	call salloc (of, n, TY_BOOL)

	if (pangle > 0) {
	    call printf ("\nRotating catalog sources by %.4g degrees (CCW) ...")
		call pargr (pangle)
	} else {
	    call printf ("\nRotating catalog sources by %.4g degrees (CW) ...")
		call pargr (pangle)
	}

	call flush (STDOUT)

	# invert to match sense of tfields task position angle
	rpangle = - (PI * pangle) / 180.

	im = immap (image, READ_ONLY, 0)
	xmiddle = real (IM_LEN(im,1)) / 2.
	ymiddle = real (IM_LEN(im,2)) / 2.
	call imunmap (im)

	call tbcgtr (tp, cp[XCEN], Memr[x], Memb[xf], 1, n)
	call tbcgtr (tp, cp[YCEN], Memr[y], Memb[yf], 1, n)
	call tbcgti (tp, cp[CENTER], Memi[c], Memb[cf], 1, n)
	call tbcgti (tp, cp[OBJECT], Memi[o], Memb[of], 1, n)

	# rotate all catalog sources, even if not currently in subset
	# rotated sources (not program objects) are also marked uncentered
	do index = 1, n {
	    # don't update the program objects!
	    if (Memi[o+index-1] == YES)
		next

	    xx = Memr[x+index-1] - xmiddle
	    yy = Memr[y+index-1] - ymiddle

	    Memr[x+index-1] =   xx*cos(rpangle) + yy*sin(rpangle) + xmiddle
	    Memr[y+index-1] = - xx*sin(rpangle) + yy*cos(rpangle) + ymiddle
	    Memi[c+index-1] = NO
	}

	call tbcptr (tp, cp[XCEN], Memr[x], 1, n)
	call tbcptr (tp, cp[YCEN], Memr[y], 1, n)
	call tbcpti (tp, cp[CENTER], Memi[c], 1, n)
	call tbtflu (tp)

	call printf ("done\n")
	call flush (STDOUT)

	call sfree (sp)
end


# SCALE -- scale the catalog sources (about image center), mark as uncentered.

procedure scale (image, tp, cp, n, xscale, yscale)

char	image[ARB]		#I image name
pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	n			#I total number of entries in the table
real	xscale			#I relative X scale factor, percent
real	yscale			#I relative Y scale factor, percent

pointer	sp, x, y, c, o, xf, yf, cf, of, im
int	index
real	xx, yy, xmiddle, ymiddle

pointer	immap()
bool	fp_equalr()

errchk immap, imunmap

begin
	call smark (sp)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (c, n, TY_INT)
	call salloc (o, n, TY_INT)
	call salloc (xf, n, TY_BOOL)
	call salloc (yf, n, TY_BOOL)
	call salloc (cf, n, TY_BOOL)
	call salloc (of, n, TY_BOOL)

	if (fp_equalr (xscale, yscale)) {
	    call printf ("\nScaling catalog sources by %.1g percent...")
		call pargr (xscale)

	} else {
	    call printf ("\nScaling catalog sources by (%.1g,%.1g) percent...")
		call pargr (xscale)
		call pargr (yscale)

	}

	call flush (STDOUT)

	im = immap (image, READ_ONLY, 0)
	xmiddle = real (IM_LEN(im,1)) / 2.
	ymiddle = real (IM_LEN(im,2)) / 2.
	call imunmap (im)

	call tbcgtr (tp, cp[XCEN], Memr[x], Memb[xf], 1, n)
	call tbcgtr (tp, cp[YCEN], Memr[y], Memb[yf], 1, n)
	call tbcgti (tp, cp[CENTER], Memi[c], Memb[cf], 1, n)
	call tbcgti (tp, cp[OBJECT], Memi[o], Memb[of], 1, n)

	# scale all catalog sources, even if not currently in subset
	# scaled sources (not program objects) are also marked uncentered
	do index = 1, n {
	    # don't update the program objects!
	    if (Memi[o+index-1] == YES)
		next

	    xx = Memr[x+index-1] - xmiddle
	    yy = Memr[y+index-1] - ymiddle

	    Memr[x+index-1] = xx*(xscale/100.0) + xmiddle
	    Memr[y+index-1] = yy*(yscale/100.0) + ymiddle
	    Memi[c+index-1] = NO
	}

	call tbcptr (tp, cp[XCEN], Memr[x], 1, n)
	call tbcptr (tp, cp[YCEN], Memr[y], 1, n)
	call tbcpti (tp, cp[CENTER], Memi[c], 1, n)
	call tbtflu (tp)

	call printf ("done\n")
	call flush (STDOUT)

	call sfree (sp)
end


# SHIFT -- shift the catalog sources, mark as uncentered.

procedure shift (image, tp, cp, n, xshift, yshift)

char	image[ARB]		#I image name
pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	n			#I total number of entries in the table
int	xshift			#I X axis shift to apply
int	yshift			#I Y axis shift to apply

pointer	sp, x, y, c, o, xf, yf, cf, of
int	index

begin
	call smark (sp)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (c, n, TY_INT)
	call salloc (o, n, TY_INT)
	call salloc (xf, n, TY_BOOL)
	call salloc (yf, n, TY_BOOL)
	call salloc (cf, n, TY_BOOL)
	call salloc (of, n, TY_BOOL)

	call printf ("\nShifting catalog sources by (%d,%d)...")
	    call pargi (xshift)
	    call pargi (yshift)
	call flush (STDOUT)

	call tbcgtr (tp, cp[XCEN], Memr[x], Memb[xf], 1, n)
	call tbcgtr (tp, cp[YCEN], Memr[y], Memb[yf], 1, n)
	call tbcgti (tp, cp[CENTER], Memi[c], Memb[cf], 1, n)
	call tbcgti (tp, cp[OBJECT], Memi[o], Memb[of], 1, n)

	# shift all catalog sources, even if not currently in subset
	# shifted sources (not program objects) are also marked uncentered
	do index = 1, n {
	    # don't update the program objects!
	    if (Memi[o+index-1] == YES)
		next

	    Memr[x+index-1] = Memr[x+index-1] + xshift
	    Memr[y+index-1] = Memr[y+index-1] + yshift
	    Memi[c+index-1] = NO
	}

	call tbcptr (tp, cp[XCEN], Memr[x], 1, n)
	call tbcptr (tp, cp[YCEN], Memr[y], 1, n)
	call tbcpti (tp, cp[CENTER], Memi[c], 1, n)
	call tbtflu (tp)

	call printf ("done\n")
	call flush (STDOUT)

	call sfree (sp)
end


# OVERLAY_ONE -- mark a single object on the display.

procedure overlay_one (frame, x, y, marktype, color)

int	frame			#I frame number to display it in
real	x, y			#I coordinates for the mark
char	marktype[ARB]		#I type of mark to draw
char	color[ARB]		#I color of mark to draw

pointer	sp, cmdline, tmp
int	colorcode, fd

int	open(), strdic()

begin
	call smark (sp)
	call salloc (cmdline, SZ_LINE, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)

	switch (strdic (color, Memc[tmp], SZ_FNAME, COLORDICT)) {
	case BLACK:
	    colorcode = BLACKCODE
	case WHITE:
	    colorcode = WHITECODE
	case RED:
	    colorcode = REDCODE
	case GREEN:
	    colorcode = GREENCODE
	case BLUE:
	    colorcode = BLUECODE
	case YELLOW:
	    colorcode = YELLOWCODE
	}

	# dump valid or invalid X,Y pairs from the table to the tmp file
	call mktemp ("tmp$JUNK", Memc[tmp], SZ_FNAME)
	fd = open (Memc[tmp], NEW_FILE, TEXT_FILE)

	call fprintf (fd, "%g %g\n")
	    call pargr (x)
	    call pargr (y)

	call close (fd)

	call sprintf (Memc[cmdline], SZ_LINE, MARKCMD)
	    call pargi (frame)
	    call pargstr (Memc[tmp])
	    call pargstr (marktype)
	    call pargi (colorcode)

	call clcmdw (Memc[cmdline])

	call delete (Memc[tmp])
	call sfree (sp)
end


# CENTER_ONE -- center a single source.  This is overkill, but want to
# use the same algorithm whether centering a list or a single source.

int procedure center_one (image, boxsize, xin, yin, xout, yout)

char	image[ARB]		#I image name
int	boxsize			#I centering box fullwidth
real	xin, yin		#I initial coordinates
real	xout, yout		#O centered coordinates (or INDEF)

pointer	sp, cmdline, coords, tmp, buf
int	cfd, tfd, nchar

int	open(), getline(), nscan()

begin
	call smark (sp)
	call salloc (cmdline, SZ_LINE, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call mktemp ("tmp$JUNK", Memc[coords], SZ_FNAME)
	call mktemp ("tmp$JUNK", Memc[tmp], SZ_FNAME)

	cfd = open (Memc[coords], WRITE_ONLY, TEXT_FILE)

	call fprintf (cfd, "%g %g\n")
	    call pargr (xin)
	    call pargr (yin)

	call close (cfd)

	call sprintf (Memc[cmdline], SZ_LINE, CENTCMD)
	    call pargstr (image)
	    call pargstr (Memc[coords])
	    call pargstr ("''")
	    call pargi (boxsize)
	    call pargstr (Memc[tmp])

	call clcmdw (Memc[cmdline])

	tfd = open (Memc[tmp], READ_ONLY, TEXT_FILE)

	nchar = getline (tfd, Memc[buf])	# eat the comment
	nchar = getline (tfd, Memc[buf])

	call close (tfd)

	call delete (Memc[coords])
	call delete (Memc[tmp])

	# look for `Warning: failed to converge near ...'
	if (nchar == EOF || nchar <= 0 || Memc[buf] == 'W') {
	    call sfree (sp)
	    xout = INDEFR
	    yout = INDEFR
	    return (ERR)
	}

	call sscan (Memc[buf])
	    call gargwrd (Memc[tmp], SZ_FNAME)
	    call gargr (xout)
	    call gargwrd (Memc[tmp], SZ_FNAME)
	    call gargr (yout)

	if (nscan () != 4) {
	    call sfree (sp)
	    xout = INDEFR
	    yout = INDEFR
	    return (ERR)
	}

	call sfree (sp)
	return (OK)
end


# NEWPOINT -- mark an object as well centered.  Note that the centering
# could have converged on the wrong location, though.

procedure newpoint (tp, cp, n, index, newx, newy)

pointer	tp			#I catalog descriptor
pointer	cp[NUM_COLS]		#I column descriptor
int	n			#I number of rows
int	index			#I catalog row index
real	newx, newy		#I newly measured coordinates

begin
	if (index < 1 || index > n) {
	    call eprintf ("Warning: indexed outside of catalog\n")
	    call eprintf ("Catalog not updated!\n")
	    call flush (STDERR)
	    return
	}

	call tbrptr (tp, cp[XCEN], newx, 1, index)
	call tbrptr (tp, cp[YCEN], newy, 1, index)

	call tbrpti (tp, cp[CENTER], YES, 1, index)
	call tbtflu (tp)
end


# NEWOBJECT -- add an additional (program) object.

procedure newobject (tp, cp, n, newx, newy, id)

pointer	tp			#I catalog descriptor
pointer	cp[NUM_COLS]		#I column descriptor
int	n			#U number of rows
real	newx, newy		#I newly measured coordinates
int	id			#I id number for the new entry

begin
	n = n + 1

	call tbrptr (tp, cp[XPRED], newx, 1, n)
	call tbrptr (tp, cp[YPRED], newy, 1, n)
	call tbrptr (tp, cp[XCEN], newx, 1, n)
	call tbrptr (tp, cp[YCEN], newy, 1, n)
	call tbrptr (tp, cp[CERR], INDEFR, 1, n)

	call tbrpti (tp, cp[SUBSET], YES, 1, n)
	call tbrpti (tp, cp[CENTER], YES, 1, n)
	call tbrpti (tp, cp[OBJECT], YES, 1, n)

	call tbrpti (tp, cp[ID], id, 1, n)
	call tbtflu (tp)
end


# GOODPOINT -- mark an object as centered.

procedure goodpoint (tp, cp, n, index)

pointer	tp			#I catalog descriptor
pointer	cp[NUM_COLS]		#I column descriptor
int	n			#I number of rows
int	index			#I catalog row index

begin
	if (index < 1 || index > n) {
	    call eprintf ("Warning: indexed outside of catalog\n")
	    call eprintf ("Catalog not updated!\n")
	    call flush (STDERR)
	    return
	}

	call tbrpti (tp, cp[CENTER], YES, 1, index)
	call tbtflu (tp)
end


# BADPOINT -- mark an object as not centered.

procedure badpoint (tp, cp, n, index)

pointer	tp			#I catalog descriptor
pointer	cp[NUM_COLS]		#I column descriptor
int	n			#I number of rows
int	index			#I catalog row index

begin
	if (index < 1 || index > n) {
	    call eprintf ("Warning: indexed outside of catalog\n")
	    call eprintf ("Catalog not updated!\n")
	    call flush (STDERR)
	    return
	}

	call tbrpti (tp, cp[CENTER], NO, 1, index)
	call tbtflu (tp)
end


# TP_SELECT -- update the SUB_FLAG parameter from the selectpars pset.

procedure tp_select (tp, cp)

pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I array of column pointers

pointer	sp, buf, phrase, expr, pp, index, flags
int	i, idx, nindex, nentries
bool	disjunction, firsttime

pointer	clopset()
bool	clgpsetb()
int	strmatch()

begin
	call allrows (tp, nindex, index)
	nentries = nindex

	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (phrase, SZ_LINE, TY_CHAR)
	call salloc (expr, SZ_LINE, TY_CHAR)
	call salloc (flags, nentries, TY_INT)

# cache isn't updated if pset is a parameter
#	call clgstr ("selectpars", Memc[buf], SZ_FNAME)
#	pp = clopset (Memc[buf])

	pp = clopset ("selectpars")

disjunction = clgpsetb (pp, "disjunction")

	call clgpset (pp, "explicit", Memc[expr], SZ_LINE)

	if (Memc[expr] == EOS || strmatch (Memc[expr], "^#$") != 0) {
	    call strcpy ("(", Memc[expr], SZ_LINE)

#	    disjunction = clgpsetb (pp, "disjunction")
	    firsttime = true

	    # just use brute force
	    # note that buffer overflow is silently ignored

	    call clgpset (pp, "column1", Memc[buf], SZ_LINE)
	    if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
		call strcpy ("(", Memc[phrase], SZ_LINE)
		call strcat (Memc[buf], Memc[phrase], SZ_LINE)

		call clgpset (pp, "boolop1", Memc[buf], SZ_LINE)
		if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
		    call strcat (Memc[buf], Memc[phrase], SZ_LINE)

		    call clgpset (pp, "value1", Memc[buf], SZ_LINE)
		    if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
			call strcat (Memc[buf], Memc[phrase], SZ_LINE)
			call strcat (")", Memc[phrase], SZ_LINE)

			if (! firsttime) {
			    if (disjunction)
				call strcat (" || ", Memc[expr], SZ_LINE)
			    else
				call strcat (" && ", Memc[expr], SZ_LINE)
			} else
			    firsttime = false

			call strcat (Memc[phrase], Memc[expr], SZ_LINE)
		    }
		}
	    }

	    call clgpset (pp, "column2", Memc[buf], SZ_LINE)
	    if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
		call strcpy ("(", Memc[phrase], SZ_LINE)
		call strcat (Memc[buf], Memc[phrase], SZ_LINE)

		call clgpset (pp, "boolop2", Memc[buf], SZ_LINE)
		if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
		    call strcat (Memc[buf], Memc[phrase], SZ_LINE)

		    call clgpset (pp, "value2", Memc[buf], SZ_LINE)
		    if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
			call strcat (Memc[buf], Memc[phrase], SZ_LINE)
			call strcat (")", Memc[phrase], SZ_LINE)

			if (! firsttime) {
			    if (disjunction)
				call strcat (" || ", Memc[expr], SZ_LINE)
			    else
				call strcat (" && ", Memc[expr], SZ_LINE)
			} else
			    firsttime = false

			call strcat (Memc[phrase], Memc[expr], SZ_LINE)
		    }
		}
	    }

	    call clgpset (pp, "column3", Memc[buf], SZ_LINE)
	    if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
		call strcpy ("(", Memc[phrase], SZ_LINE)
		call strcat (Memc[buf], Memc[phrase], SZ_LINE)

		call clgpset (pp, "boolop3", Memc[buf], SZ_LINE)
		if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
		    call strcat (Memc[buf], Memc[phrase], SZ_LINE)

		    call clgpset (pp, "value3", Memc[buf], SZ_LINE)
		    if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
			call strcat (Memc[buf], Memc[phrase], SZ_LINE)
			call strcat (")", Memc[phrase], SZ_LINE)

			if (! firsttime) {
			    if (disjunction)
				call strcat (" || ", Memc[expr], SZ_LINE)
			    else
				call strcat (" && ", Memc[expr], SZ_LINE)
			} else
			    firsttime = false

			call strcat (Memc[phrase], Memc[expr], SZ_LINE)
		    }
		}
	    }

	    call clgpset (pp, "column4", Memc[buf], SZ_LINE)
	    if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
		call strcpy ("(", Memc[phrase], SZ_LINE)
		call strcat (Memc[buf], Memc[phrase], SZ_LINE)

		call clgpset (pp, "boolop4", Memc[buf], SZ_LINE)
		if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
		    call strcat (Memc[buf], Memc[phrase], SZ_LINE)

		    call clgpset (pp, "value4", Memc[buf], SZ_LINE)
		    if (Memc[buf] != EOS && strmatch (Memc[buf], "^#$") == 0) {
			call strcat (Memc[buf], Memc[phrase], SZ_LINE)
			call strcat (")", Memc[phrase], SZ_LINE)

			if (! firsttime) {
			    if (disjunction)
				call strcat (" || ", Memc[expr], SZ_LINE)
			    else
				call strcat (" && ", Memc[expr], SZ_LINE)
			} else
			    firsttime = false

			call strcat (Memc[phrase], Memc[expr], SZ_LINE)
		    }
		}
	    }

	    # apparently want to select all catalog entries
	    if (firsttime) {
		call amovki (YES, Memi[flags], nentries)
		call tbcpti (tp, cp[SUBSET], Memi[flags], 1, nentries)
		call tbtflu (tp)
		return
	    }

	    call strcat (")", Memc[expr], SZ_LINE)

	}

	call clcpset (pp)

	call select (tp, Memc[expr], nindex, Memi[index])

	# not in the form needed for vops$alut.gx
	call amovki (NO, Memi[flags], nentries)
	do idx = 1, nindex {
	    i = Memi[index+idx-1]
	    Memi[flags+i-1] = YES
	}

	# put back the object listing
	call allrows (tp, nindex, index)
	call select (tp, "(OBJ_FLAG == 1)", nindex, Memi[index])
	do idx = 1, nindex {
	    i = Memi[index+idx-1]
	    Memi[flags+i-1] = YES
	}

	call tbcpti (tp, cp[SUBSET], Memi[flags], 1, nentries)
	call tbtflu (tp)

	call mfree (index, TY_INT)
	call sfree (sp)
end


# REINIT -- reinitialize the centered coordinate columns to the values
# from the predicted coordinate columns.  All sources are affected.

procedure reinit (tp, cp, n)

pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	n			#I total number of entries in the table

pointer	sp, x, y, rbuf, ibuf, junk

begin
	call smark (sp)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (rbuf, n, TY_REAL)
	call salloc (ibuf, n, TY_INT)
	call salloc (junk, n, TY_BOOL)

	# should check the flags...
	call tbcgtr (tp, cp[XPRED], Memr[x], Memb[junk], 1, n)
	call tbcgtr (tp, cp[YPRED], Memr[y], Memb[junk], 1, n)
	call amovkr (INDEFR, Memr[rbuf], n)
	call amovki (NO, Memi[ibuf], n)

	call tbcptr (tp, cp[XCEN], Memr[x], 1, n)
	call tbcptr (tp, cp[YCEN], Memr[y], 1, n)
	call tbcptr (tp, cp[CERR], Memr[rbuf], 1, n)
	call tbcpti (tp, cp[CENTER], Memi[ibuf], 1, n)
	call tbtflu (tp)

	call sfree (sp)
end


# REINIT_ONE -- reinitialize the centered coordinate columns for a
# single source to the values from the predicted coordinate columns.

procedure reinit_one (tp, cp, n, index, x, y)

pointer	tp			#I catalog descriptor
pointer	cp[NUM_COLS]		#I column descriptor
int	n			#I number of rows
int	index			#I catalog row index
real	x, y			#O initialized x and y coordinates

bool	junk

begin
	if (index < 1 || index > n) {
	    call eprintf ("Warning: indexed outside of catalog\n")
	    call eprintf ("Catalog not updated!\n")
	    call flush (STDERR)
	    return
	}

	# should check the flags...
	call tbrgtr (tp, cp[XPRED], x, junk, 1, index)
	call tbrgtr (tp, cp[YPRED], y, junk, 1, index)

	call tbrptr (tp, cp[XCEN], x, 1, index)
	call tbrptr (tp, cp[YCEN], y, 1, index)
	call tbrptr (tp, cp[CERR], INDEFR, 1, n)
	call tbrpti (tp, cp[CENTER], NO, 1, index)
	call tbtflu (tp)
end


# REPLACE -- reinitialize the predicted coordinate columns to the current
# values from the centered coordinate columns.  All sources are affected.

procedure replace (tp, cp, n)

pointer	tp			#I table pointer
pointer	cp[NUM_COLS]		#I column pointers for x, y, and valid flag
int	n			#I total number of entries in the table

pointer	sp, x, y, junk

begin
	call smark (sp)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)
	call salloc (junk, n, TY_BOOL)

	# should check the flags...
	call tbcgtr (tp, cp[XCEN], Memr[x], Memb[junk], 1, n)
	call tbcgtr (tp, cp[YCEN], Memr[y], Memb[junk], 1, n)

	call tbcptr (tp, cp[XPRED], Memr[x], 1, n)
	call tbcptr (tp, cp[YPRED], Memr[y], 1, n)
	call tbtflu (tp)

	call sfree (sp)
end
