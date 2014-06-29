## raster2points
## Function for converting a raster to a SpatialPointsDataFrame
## GPL license
## See www.underdiverwaterman.com for more tools
#
# Usage:
# To create a new object from a raster:
# rpoints <- raster2points("~/workspace/TEMP/rast_3.asc", write=F)
# 
# To create a shapefile in the same directory (rast_3.shp):
# rpoints <- raster2points("~/workspace/TEMP/rast_3.asc", write=T)

library(maptools)
library(raster)

raster2points <- function(rast, write = FALSE) {
  r <- raster(rast)
  p <- as(r, "SpatialPointsDataFrame")
  if (write == TRUE) { writeSpatialShape(p, fn=substr(rast, start = 1, stop = nchar(rast) -3)) }
  else { return(p) }
}
