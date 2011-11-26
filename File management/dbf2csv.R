require(foreign)
dbf2csv <- function(file) {
  table <- read.dbf(file)
  substr(file, start=nchar(file)-3, nchar(file)) <- ".csv"
  write.csv(table, file)
}