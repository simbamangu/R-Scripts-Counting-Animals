readGPXw <- function (f = "~/", filter = 1) {
  # Read GPX file.
  # Filter out saved tracks ("Active" only) and output a table with time in UTC.
  # This doesn't work with GPX files that have no names for the tracks - i.e. 'cleaned'
  GB <- Sys.which("gpsbabel")
  f <- gsub("\ ", "\\\\\ ", f) # if file has spaces, escape them (OSX only?). 
  if (filter == 1) {filter <- " -x track,name='ACTIVE *',speed" } else { filter <- " -x track,speed"} # If filter=T then do only actives.
  if (nchar(GB) == 0 || !file.exists(GB)) stop("gpsbabel not found")

  if (.Platform$OS.type == "windows") 
        gpsdata <- system(paste(GB, " -", "w", " -i ", "gpx", " -f ", f, filter, " -o unicsv -F -", sep = ","), intern = TRUE, invisible = invisible)

  else gpsdata <- system(paste(GB, " -", "w", " -i ", "gpx", " -f ", f, filter, " -o unicsv -F -", sep = ""), intern = TRUE) 
  gpsdf <- read.csv(con <- textConnection(gpsdata), fill = TRUE)
  close(con)
  gpsdf$Time <- strptime(paste(gpsdf$Description), "%d-%b-%y %H:%M:%S", tz ="UTC")
  gpsdf
}
