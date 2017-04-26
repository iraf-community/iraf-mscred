include	<imset.h>
include	<pmset.h>


# XT_PMUNMAP -- Unmap a mask image.
# Note that the imio pointer may be purely an internal pointer opened
# with im_pmmapo so we need to free the pl pointer explicitly.

procedure yt_pmunmap (im)

pointer	im			#I IMIO pointer for mask

pointer	pm
int	imstati()

begin
	pm = imstati (im, IM_PMDES)
	call pm_close (pm)
	call imseti (im, IM_PMDES, NULL)
	call imunmap (im)
end
