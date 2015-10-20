#############################################
## Read elevation data from Google's elevation
## maps API. Function requires lat/lon values
## as vectors of numbers.
#############################################
#x <- seq(-5, -1, 0.5)
#y <- seq(33, 37, 0.5)
# 2015-07-14 Not working? googleElevation(36.5, -2.3) gives elev -537?
require(XML)

googleElevation <- function (x, y) {
  coords <- paste(x, y, sep = ",", collapse = "|")
  prefix <- "http://maps.googleapis.com/maps/api/elevation/xml?locations="
  suffix <- "&sensor=false"
  # create the URL
  u <- paste(prefix, coords, suffix, sep="")
  
  # Get the data
  dat = xmlRoot(xmlTreeParse(u))
  closeAllConnections()
  latitude <- as.numeric(lapply(getNodeSet(dat,"//location/lat"), function(x) xmlSApply(x, xmlValue)))
  longitude <- as.numeric(lapply(getNodeSet(dat,"//location/lng"), function(x) xmlSApply(x, xmlValue)))
  elevation <- as.numeric(lapply(getNodeSet(dat,"//elevation"), function(x) xmlSApply(x, xmlValue)))
  data.frame(latitude, longitude, elevation)
}