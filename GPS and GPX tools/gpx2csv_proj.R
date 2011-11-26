# Read waypoints from a GPX file and convert it to a CSV, projecting if asked.
# Requires gpsbabel

gpx2csv <- function (f = "~/", pr = CRS("+init=epsg:32735")) { # UTM 35S
  # Read GPX file waypoints only.
  GB <- Sys.which("gpsbabel")
  f.original <- f # save the original filename for saving later
  f <- gsub("\ ", "\\\\\ ", f) # if file has spaces, escape them (OSX only?). 
    if (nchar(GB) == 0 || !file.exists(GB)) stop("gpsbabel not found")

  if (.Platform$OS.type == "windows") 
        gpsdata <- system(paste(GB, " -", "w", " -i ", "gpx", " -f ", f, " -o unicsv -F -", sep = ","), intern = TRUE, invisible = invisible)

  else gpsdata <- system(paste(GB, " -", "w", " -i ", "gpx", " -f ", f, " -o unicsv -F -", sep = ""), intern = TRUE) 
  gpsdf <- read.csv(con <- textConnection(gpsdata), fill = TRUE)
  close(con)
  coordinates(gpsdf) <- ~Longitude+Latitude
  proj4string(gpsdf) <- CRS("+proj=longlat +ellps=WGS84")
  gpsdf.proj <- spTransform(gpsdf, pr)
  write.csv(gpsdf.proj, gsub(pattern=".gpx", replacement=".csv", f.original))
}