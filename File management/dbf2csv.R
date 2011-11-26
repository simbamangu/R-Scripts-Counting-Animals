# Turn a DBF into a CSV file (if Excel doesn't support reading DBF).
# Saves it back into the same directory with new file extension.
require(foreign)
dbf2csv <- function(file) {
  table <- read.dbf(file)
  substr(file, start=nchar(file)-3, nchar(file)) <- ".csv"
  write.csv(table, file)
}