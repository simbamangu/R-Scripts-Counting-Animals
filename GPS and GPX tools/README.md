## GPS and GPX tools

### CreateRoute.R

Experimental for creating a 'snaking' GPS route.

### fixLogGPX.py

Fix the incorrectly-written GPX file produced by the Vulcan datalogger. Better to use fixLog.sh which uses `sed` and also fixes the OGR:AGL tag.

### fixLog.sh

Fix the incorrectly-written GPX file produced by the Vulcan datalogger <= 0.7.5

### gpx2csv_proj.R

Read waypoints from a GPX file and convert it to a CSV, projecting if asked. 

Requires GPSBabel.

### readGPXw.R

Read GPX file, filtering out saved tracks (retaining "Active" only) and output a table with time in UTC.

This doesn't work with GPX files that have no names for the tracks - i.e. 'cleaned'

Requires GPSBabel.

## Dependencies

### GPSBabel

Binary installers available at www.gpsbabel.org

Debian - `sudo apt-get install gpsbabel`

Mac - use homebrew `brew install gpsbabel`