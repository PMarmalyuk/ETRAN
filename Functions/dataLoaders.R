createRawDataRec <- function(filePath, readSettings, useExt, extFun, extSettings)
{
  if (!file.exists(filePath))
  {
    stop("Datafile not found!")
  }
  else
  {
    if (useExt)
    {
      # implement data loading using extFun
      extData <- extFun(filePath, readSettings, extSettings)
      headerLines <- extData[[1]]
      asIsData <- extData[[2]]
      rawDataRecord <- new(Class = "RawDataRecord",
                           filePath = filePath,
                           headerLines = headerLines,
                           data = asIsData)
    }
    else
    {
      settings <- readSettings@readSettings
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
  }
  rawDataRecord
}