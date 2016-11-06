### Fix GPX file from Flightlogger.app 0.7.5
# Usage:
#  fixLogGPX.py file.gpx
# 
# Output:
#  file.gpx in place with <extension> tag replaced with <extensions>

import fileinput, sys

fname = sys.argv[1]

for line in fileinput.FileInput(fname,inplace=1):
	line = line.replace("extension>","extensions>")
	print line,