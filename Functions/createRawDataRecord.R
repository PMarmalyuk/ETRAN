createRawDataRec <- function(filepath, readSettings)
{
  if (!file.exists(filepath))
  {
    stop("Datafile not found!")
  }
  else
  {
    settings <- readSettings@readSettings
    headerLines <- readLines(con = filepath, n = settings$skip, encoding = settings$encoding)
    asIsData <- read.csv(filepath, sep = settings$sep,   
                         skip = settings$skip, 
                         comment.char = settings$comment.char, 
                         header = settings$header,
                         blank.lines.skip = T, 
                         check.names = F,
                         stringsAsFactors = F)
  }
  rawDataRecord <- new(Class = "RawDataRecord",
                       filePath = filepath,
                       header = headerLines,
                       data = asIsData)
  rawDataRecord
}
rawSett <- new(Class = "ReadSettings")
rawDataRec <- createRawDataRec("F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI\\Drozdov Samples.txt", rawSett)

createRawDataRecords <- new(Class = "RawDataRecords")
