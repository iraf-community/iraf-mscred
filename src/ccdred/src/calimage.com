# Common for calibration images.

pointer	cal		# Pointer calibration data
int	ncal		# Number of images
common	/calib/ cal, ncal
