procedure logfile (table, logfile)

string	table			{prompt="Output table name"}
string	logfile			{prompt="Logfile for abridged table listing"}

bool	append = yes		{prompt="Silently append to output?"}

begin
	string	sp_col = ""

	string	ltable, llogfile, tmp1, lrewrite

	cache ("tinfo")

	tmp1 = mktemp ("tmp$tmp")

	ltable = table
	llogfile = logfile

	if (! append) {
	    if (access (llogfile)) {
		printf ("Log file %s exists", llogfile)
		lrewrite = _qpars.rewrite
		if (lrewrite == "replace") {
		    delete (llogfile, ver-, >& "dev$null")
		} else if (lrewrite != "append") {
		    printf ("\nChoose another filename and try again.\n")
		    return
		}
	    }
	}

	if (! access (llogfile))
	    printf ("", > llogfile)

	# PROGRAM OBJECTS
	tselect (ltable, tmp1, "OBJ_FLAG == 1")
	tinfo (tmp1, ttout-)

	if (tinfo.nrows <= 0) {
	    print ("No program objects.", >> llogfile)

	} else {
	    print ("List of program objects:\n", >> llogfile)
	    tprint (tmp1, prparam=no, prdata=yes,
		pwidth=80, plength=0, showrow=yes, showhdr=yes, lgroup=0,
		columns="GSC_ID,X_CENTER,Y_CENTER,CEN_FLAG,SUB_FLAG",
		rows="-", option="plain", align=yes, sp_col=sp_col,
		>> llogfile)
	}


	# CENTERED CATALOG SOURCES
	tdelete (tmp1, ver-, >& "dev$null")

	tselect (ltable,tmp1, "CEN_FLAG == 1 && SUB_FLAG == 1 && OBJ_FLAG != 1")
	tinfo (tmp1, ttout-)

	if (tinfo.nrows <= 0) {
	    print ("\n\nNo sources were centered.", >> llogfile)

	} else {
	    print ("\n\nList of successfully centered sources:\n", >> llogfile)
	    tprint (tmp1, prparam=no, prdata=yes,
		pwidth=80, plength=0, showrow=yes, showhdr=yes, lgroup=0,
		columns="REGION,GSC_ID,X_CENTER,Y_CENTER,MAG_BAND,MAG,CLASS,PLATE_ID",
		rows="-", option="plain", align=yes, sp_col=sp_col,
		>> llogfile)
	}


	# UNCENTERED CATALOG SOURCES
	tdelete (tmp1, ver-, >& "dev$null")

	tselect (ltable,tmp1, "CEN_FLAG != 1 && SUB_FLAG == 1 && OBJ_FLAG != 1")
	tinfo (tmp1, ttout-)

	if (tinfo.nrows <= 0) {
	    print ("\n\nAll sources were centered.", >> llogfile)

	} else {
	    print ("\n\nList of UNcentered sources:\n", >> llogfile)
	    tprint (tmp1, prparam=no, prdata=yes,
		pwidth=80, plength=0, showrow=yes, showhdr=yes, lgroup=0,
		columns="REGION,GSC_ID,X_PRED,Y_PRED,MAG_BAND,MAG,CLASS,PLATE_ID",
		rows="-", option="plain", align=yes, sp_col=sp_col, >> llogfile)
	}


	# SOURCES OMITTED BY SELECTPARS
	tdelete (tmp1, ver-, >& "dev$null")

	tselect (ltable, tmp1, "SUB_FLAG != 1 && OBJ_FLAG != 1")
	tinfo (tmp1, ttout-)

	if (tinfo.nrows <= 0) {
	    print ("\n\nNo sources omitted by selectpars.", >> llogfile)

	} else {
	    print ("\n\nList of sources omitted by selectpars:\n", >> llogfile)
	    tprint (tmp1, prparam=no, prdata=yes,
		pwidth=80, plength=0, showrow=yes, showhdr=yes, lgroup=0,
		columns="REGION,GSC_ID,X_PRED,Y_PRED,MAG_BAND,MAG,CLASS,PLATE_ID,CEN_FLAG",
		rows="-", option="plain", align=yes, sp_col=sp_col, >> llogfile)
	}

	tdelete (tmp1, ver-, >& "dev$null")
end
