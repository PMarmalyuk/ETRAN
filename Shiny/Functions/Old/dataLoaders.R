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
    asIsData <- read.csv(file = filePath, sep = settings$sep, dec = settings$dec,   
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

loadFactorsData <- function(file, header, sep, dec, encoding)
{
  factorsData <- read.csv(file = file, header = header, sep = sep, dec = dec, encoding = encoding)
  varNames <- c()
  varClasses <- c()
  varLevels <- list()
  
  for (i in 2:length(factorsData))
  {
    varNames <- c(varNames, colnames(factorsData)[i])
    varClasses <- c(varClasses, class(factorsData[,i]))
    if(class(factorsData[,i]) == "factor")
    {
      varLevels <- append(varLevels, list(levels(factorsData[,i])))
    }
    if(class(factorsData[,i]) == "numeric")
    {
      varLevels <- append(varLevels, NA)
    }
    if(class(factorsData[,i]) == "integer")
    {
      varLevels <- append(varLevels, NA)
    }
  }
  res <- list(data = factorsData, names = varNames, classes = varClasses, levels = varLevels)
  return(res)
}