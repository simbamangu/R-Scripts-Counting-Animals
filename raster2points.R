library(maptools)
library(raster)

# Load the raster from a file
r <- raster("~/workspace/TEMP/rast_3.asc")

# Convert to spatial points
p <- as(r, "SpatialPointsDataFrame")

# Save as a shapefile
writeSpatialShape(p, "~/workspace/TEMP/rast_3_points")

raster2points <- function(rast, write = FALSE) {
  r <- raster(rast)
  p <- as(r, "SpatialPointsDataFrame")
  if (write == TRUE) { writeSpatialShape(p, fn=substr(rast, start = 1, stop = nchar(rast) -3)) }
  else { return(p) }
}
