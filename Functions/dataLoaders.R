createRawDataRec <- function(filePath, settings)
{
  if (!file.exists(filePath))
  {
    stop("Datafile not found!")
  }
  else
  {
    settings <- settings$rawSettings@readSettings
    headerLines <- readLines(con = filePath, n = settings$skip, encoding = settings$encoding)
    asIsData <- read.csv(filePath, sep = settings$sep,   
                         skip = settings$skip, 
                         comment.char = settings$comment.char, 
                         header = settings$header,
                         blank.lines.skip = T, 
                         check.names = F,
                         stringsAsFactors = F)
    rawDataRecord <- new(Class = "RawDataRecord",
                         filePath = filePath,
                         headerLines = headerLines,
                         data = asIsData)
  }
  rawDataRecord
}



