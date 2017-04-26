if (fmedian)
    fmedian (input, output, xwindow, ywindow, zloreject=zloreject,
	zhireject=zhireject, boundary=boundary, constant=constant,
	verbose=verbose, hmin=hmin, hmax=hmax, zmin=zmin, zmax=zmax, unmap=yes)
else
    median (input, output, xwindow, ywindow, zloreject=zloreject,
	zhireject=zhireject, boundary=boundary, constant=constant,
	verbose=verbose)
