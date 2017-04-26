# AMPINFO.com -- Amplifier information common

int	namps		# Number of amplifiers.
pointer	amplist		# LIST of amplifier names.
pointer	offset_key	# Name of zero offset header keyword
pointer	gain_key	# Name of gain        header keyword
pointer	dark_key	# Name of dark rate   header keyword
pointer	offset		# Zero offset for each amp.
pointer	gain		# Gain        for each amp.
pointer	dark		# Dark rate   for each amp.

common	/ampinfo/ namps, amplist, offset_key, gain_key, dark_key, 
	offset, gain, dark
