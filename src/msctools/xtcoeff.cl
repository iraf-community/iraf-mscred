#From rhoads@noao.edu Mon Aug 30 11:58:43 1999
#Date: Mon, 30 Aug 99 11:58:40 MST
#From: rhoads@noao.edu (James Rhoads)
#To: jannuzi@noao.edu
#Subject: Instructions for crosstalk script
#Cc: dmac@as.arizona.edu, fvaldes@noao.edu, gjacoby@noao.edu,
#        tarmandroff@noao.edu
#
#Hi Buell, Dan, George, Taft, Frank:
#
#Most of you have expressed some interest in my crosstalk measurement
#script at some time.  The script will follow under separate cover.
#To use it, save it as "xtcoeff.cl" and get Iraf to recognize it using
# task xtcoeff = "xtcoeff.cl" .
#Remember as always that this was a quick hack; anybody who wants to
#improve on it is more than welcome.
#
#Here is the parameter list for the script, with some values for a run on
#Mosaic South data:
#
#cl> lpar xtcoeff
#        objin = "obj039.fits"   Input Mosaic image name
#         thug = 7               Chip causing ghosts
#       victim = 8               Chip where ghosts appear
#      (satmin = 20000)          Saturation minimum level in thug chip
#      (satmax = INDEF)          Saturation maximum level in thug chip
#     (mingood = -100)           Minimum good data value in victim chip
#       (xbox1 = 8)              initial filter and blkavg X size
#       (ybox1 = 8)              initial filter and blkavg Y size
#       (xbox2 = 25)             second filter X size
#       (ybox2 = 25)             second filter Y size
#     (verbose = no)             verbose output?
#        (coef = 1.3082177912399E-5) Returned crosstalk coefficient
#       (dcoef = 1.1942475451301E-6) Returned coefficient error estimate
#
#You will probably want to set mingood=-100 or so (any value that is a
#few sigma below the mode in all regions of your input image is fine).
#Leave [xy]box[12] alone; they're tuned for reasonably fast operation and
#have no other important effect.  "coef" and "dcoef" are output
#parameters; there's no need to fiddle with them on input.
#
#satmin and satmax determine the range of pixel values in the "thug" chip
#that get studied.  For the saturated pixels, take satmin to be perhaps a
#hundred or a thousand ADU below the saturation level, and satmax=INDEF.
#If you want to check that the crosstalk is really linear, try some other
#values of satmin and satmax (e.g. satmin=7000 satmax=9000) and see if
#the coefficients remain consistent within their quoted error bars.
#
#To characterize the crosstalk completely you need to run the script 56
#times, for all possible combinations of "thug" and "victim".  Really,
#eight times is enough if you believe that the effect is in the arcons
#and can only go between "paired" chips.  What I've done lately is to do
#56 runs and use the chip 1 vs. chips 3-8 coefficients and similar ones
#that ought to be zero to estimate the systematic errors in the
#coefficients.
#
#(I just write a dumb batch file called [e.g.] batch_xt to do them all,
#and [starting at a unix prompt] do  "script xtlog ; cl ; task xtcoeff =
#"xtcoeff.cl" ; cl < batch_xt ; logout ; exit" after which I examine the
#log in "xtlog".  Running all 56 possible chip pairs takes several hours.)
#
#Let me know how it goes, any of you that try it.
#							James.
#
#>From rhoads@noao.edu Mon Aug 30 12:02:07 1999
#Date: Mon, 30 Aug 99 12:02:04 MST
#From: rhoads@noao.edu (James Rhoads)
#To: bjannuzi@noao.edu, dmac@as.arizona.edu, fvaldes@noao.edu, gjacoby@noao.edu,
#        tarmandroff@noao.edu
#Subject: Here's the script

procedure xtcoeff( objin, thug, victim )
# purpose of this procedure is to characterize level of electronics
# ghosts in Mosaic images.  These ghosts are produced by some sort
# of crosstalk in ARCON readout electronics shared by pairs of chips.

string objin {prompt="Input Mosaic image name"}
int thug   {4,prompt="Chip causing ghosts"}
int victim {3,prompt="Chip where ghosts appear"}
real satmin {prompt="Saturation minimum level in thug chip"}
real satmax {INDEF,prompt="Saturation maximum level in thug chip"}
real mingood {prompt="Minimum good data value in victim chip"}
int xbox1=8  {prompt = "initial filter and blkavg X size"}
int ybox1=8  {prompt = "initial filter and blkavg Y size"}
int xbox2=25  {prompt = "second filter X size"}
int ybox2=25  {prompt = "second filter Y size"}
bool verbose  {no, prompt="verbose output?"}
real coef {prompt = "Returned crosstalk coefficient"}
real dcoef {prompt = "Returned coefficient error estimate"}

# Procedure:  First make an copy of "victim" with all saturated
#  pixels clobbered.
# Median smooth this image (excluding clobbered pixels) to make a sky frame.
# Subtract this from the victim image.
# Now clobber all pixels in the sky-subtracted victim image whose
#  corresponding pixels in the thug image have intensities outside
#  the interval (satmin, satmax).
# Generate image statistics of the unflagged pixels.
# Then get image statistics of the same set of pixels (more or less) in the
# thug image, and statistics of sky in the thug image.
# Finally, compute the crosstalk coefficient and the dominant term
# in its error estimate.

begin
  int lt, lv, nxpix, nypix
  real lsmin, lsmax, lmgood, flag, x1, x2, y1, y2, maxsky, maxthug
  real t_slev, t_glev, v_glev, v_grms, t_grms, t_srms
  real v_dglev, t_dglev, t_dslev
  int v_gnpix, t_gnpix, t_snpix
  string l_in, l_out, tmpsky, tmpskysmall

  l_in = objin
  lt = thug
  lv = victim
  lsmin = satmin
  lsmax = satmax
  lmgood = mingood
  flag = lmgood - abs(lmgood) - 100.
  l_out = mktemp( "xtcoef_im_" )

  tmpsky = mktemp("xtcoef_s1_")
  tmpskysmall = mktemp("xtcoef_s2_")

  if (lsmax == INDEF) {
     imstat( l_in//"["//str(lt)//"]", fields="max", lower=INDEF,
        upper=INDEF, format=no) | scan(maxthug)
     lsmax = 2.*abs(maxthug)
  }

  imexpr( "b>c && b<d ? e : a" , tmpsky,
     l_in//"["//str(lv)//"]",l_in//"["//str(lt)//"]",
     str(lsmin),str(lsmax),str(flag),
     dims="auto",intype="auto",outtype="auto",
     refim="auto",bwidth=0,btype="nearest",bpixval=flag,
     rangecheck=no,verbose=verbose,exprdb="none" )
  # use the "nested median" approach from "msflsmooth" to get the sky
  # level estimate in finite time. (Otherwise would need abt 3 hours cpu
  #  to smooth with a 99x99 box median filter on chomper...)
  median( tmpsky, tmpsky, xbox1, ybox1, zloreject=lmgood,
    zhireject=INDEF, boundary="constant", constant=flag, verbose=verbose )
  blkavg( tmpsky, tmpskysmall, xbox1, ybox1, option="average")
  median( tmpskysmall, tmpskysmall, xbox2, ybox2,  zloreject=lmgood,
    zhireject=INDEF, boundary="constant", constant=flag, verbose=verbose )
  imdelete( tmpsky, verify- )

   # Figure out number of pixels in input
   imgets(  l_in//"["//str(lv)//"]", "i_naxis1" )
   nxpix = int(imgets.value)
   imgets(  l_in//"["//str(lv)//"]", "i_naxis2" )
   nypix = int(imgets.value)
   x1 = 0.5 + 1./real(xbox1)
   y1 = 0.5 + 1./real(ybox1)
   x2 = (real(nxpix-1)/real(xbox1)) + x1
   y2 = (real(nypix-1)/real(ybox1)) + y1
   if (verbose) {
     magnify(tmpskysmall, tmpsky, real(xbox1),real(ybox1),x1=x1,x2=x2,
       y1=y1,y2=y2, interpolatio="linear", boundary="nearest",
       fluxconserve=no )
   } else {
     magnify(tmpskysmall, tmpsky, real(xbox1),real(ybox1),x1=x1,x2=x2,
       y1=y1,y2=y2, interpolatio="linear", boundary="nearest",
       fluxconserve=no, >& "dev$null" )
   }

  imstat( tmpskysmall, fields="max", lower=INDEF, upper=INDEF, 
    format=no) | scan(maxsky)
  lmgood = lmgood - maxsky
  flag = flag - maxsky - abs(lmgood)

  imexpr( "b>d && b<e ? a-c : f" , l_out,
     l_in//"["//str(lv)//"]",l_in//"["//str(lt)//"]",tmpsky,
     str(lsmin),str(lsmax),str(flag),
     dims="auto",intype="auto",outtype="auto",
     refim="auto",bwidth=0,btype="nearest",bpixval=flag,
     rangecheck=no,verbose=verbose,exprdb="none" )

  # clean up
  imdel( tmpsky, verify- )
  imdel( tmpskysmall, verify- )

  # Get statistics of ghosts on sky-subtracted "victim" chip
  if (verbose)
	print( "Statistics of electronics ghosts on chip "//str(lv) )
  iterstat( l_out, nsigrej=3., maxiter=15, print=verbose, verbose=no,
     lower=lmgood, upper=INDEF )
  v_glev = iterstat.mean
  v_grms = iterstat.sigma
  v_gnpix = iterstat.npix
  v_dglev = v_grms / sqrt(real(v_gnpix-1))
  if (verbose)
	print( "Ghost level is ",str(v_glev)//" +- "//str(v_dglev)//
	" ADU on chip "//str(lv) )

  # Get statistics of sky on "thug" chip with lower=upper=INDEF.
  iterstat( l_in//"["//str(lt)//"]", nsigrej=3., maxiter=15, print=verbose,
	verbose=no, lower=INDEF, upper=INDEF )
  t_slev = iterstat.mean
  t_srms = iterstat.sigma
  t_snpix = iterstat.npix
  t_dslev = t_srms / sqrt(real(t_snpix-1))
  if (verbose)
	 print( "Sky level is ",str(t_slev)//" +- "//str(t_dslev)//
	 " ADU on chip "//str(lt) )

  # # # get statistics of "thug" chip with lower=satmin, upper=satmax
  imstat(  l_in//"["//str(lt)//"]", fields="npix,mean,stddev",
	lower=satmin, upper=satmax, 
	format=no ) | scan(t_gnpix,t_glev,t_grms)
  # Note, for ghost pixels on "thug" chip, we can't really get
  # a sensible error estimate without understanding a lot about repeatibility
  # of saturation levels... let it drop.

  # Compute the desired coefficient as  (ghost_level) / (sky_sat - sky_all)
  #  and put it in an output parameter
  coef = v_glev / ( t_glev - t_slev )
  # error estimate:  ASSUME this is dominated by error in ghost level on
  # victim chip.  We could do better here... but it probably does not matter.
  dcoef = v_dglev / ( t_glev - t_slev )

  print( "Crosstalk Coefficient is "//str(coef)//" +- "//str(dcoef)//
	" from chip "//str(lt)//" to chip "//str(lv) )
  # Finally, clean up a little.
  imdelete( l_out, verify=no )
end

#############################
#You'll also need this modified version of "iterstat.cl", which has an
#output "npix" parameter.
#############################
#procedure iterstat(image)
#
## Script to find image statistics excluding deviant pixels
## 4 August 1992 by John Ward
## Minor modifications 4 August 1992 MD
## Various subsequent variations.
## Latest revision:  18 Aug 1993 MD
#
#string	image	{prompt="Input image(s)"}
#real	nsigrej	{5.,min=0.,prompt="Number of sigmas for limits"}
#int	maxiter	{10,min=1,prompt="Maximum number of iterations"}
#bool	print	{yes,prompt="Print final results?"}
#bool	verbose	{yes,prompt="Show results of iterations?"}
#real	lower	{INDEF,prompt="Initial lower limit for data range"}
#real	upper	{INDEF,prompt="Initial upper limit for data range"}
#real	mean	{prompt="Returned value of mean"}
#real	sigma	{prompt="Returned value of sigma"}
#real	median	{prompt="Returned value of sigma"}
#real	valmode	{prompt="Returned value of mode"}
#int	npix	{prompt="Returned number of retained pixels"}
##          	Must be "valmode" to avoid conflict w/ omnipresent task 
##		parameter "mode" 
#struct	*inimglist
#
#begin
#
#	string	imglist		# equals image
#	string	infile		# temporary list for files
#	string  img		# image name from fscan
#	real	mn		# mean from imstat
#	real	sig		# stddev from imstat
#	real	med		# midpt from imstat
#	real	mod		# mode from imstat
#	real	ll		# lower limit for imstat
#	real	ul		# upper limit for imstat
#	int	nx, npx		# number of pixels used
#	int	m		# dummy for countdown
#
## Get query parameter
#	imglist = image
#
## Expand file lists into temporary files.
#	infile =  mktemp("tmp$iterstat")
#	sections (imglist,option="fullname",>infile)
#	inimglist = infile
#
## Loop through images
#	while (fscan(inimglist,img) != EOF) {
#
#	   imstat(img,fields="mean,stddev,npix,midpt,mode",
#		lower=lower,upper=upper,for-) | scan(mn,sig,npx,med,mod)
#
#	   m = 1
##	   if (verbose) print(img//" :")
#	   while (m <= maxiter)  {
#	   	if (verbose)
#	   	   print(img,": mean=",mn," rms=",sig," npix=",npx," median=",med,
#	   	      " mode=",mod)
#	   	ll = mn - (nsigrej*sig)
#	   	ul = mn + (nsigrej*sig)
#		if (lower != INDEF && ll < lower) ll = lower
#		if (upper != INDEF && ul > upper) ul = upper
#	   	imstat(img,fields="mean,stddev,npix,midpt,mode",
#	   	       lower=ll,upper=ul,for-) | scan(mn,sig,nx,med,mod)
#	   	if (nx == npx)
#	   		break
#	   	npx = nx
#	   	m = m + 1
#	   }
#
##	   if (print && !verbose) print (img//" :")
#	   if (print && !verbose) 
#	      print(img,": mean=",mn," rms=",sig," npix=",npx," median=",med,
#	   	      " mode=",mod)
#	   mean = mn
#	   sigma = sig
#	   median = med
#	   valmode = mod
#	   npix = npx
#
#	}
#
#	delete (infile,ver-)
#	inimglist = ""
#
#end
