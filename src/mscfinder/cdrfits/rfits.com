
# FITS reader common

int	len_record		# Length of FITS records in bytes
int	data_type		# Output data type
real	blank			# Blank value

# Option flags
int	long_header		# Print a long header (FITS header cards)
int	short_header		# Print a short header (Title and size)
int	tape			# is input a mag tape?
int	scale			# Scale the data
int	old_name		# Use old IRAF name?
int	ieee			# Fits data in IEEE floating point standard?
int	gkey			# Image type to be created

common	/rfitscom/ len_record, data_type, blank, long_header,
	short_header, tape, scale, old_name, ieee, gkey
