readFiles <- function(folderPath)
{
  d <- dir(folderPath, pattern = "*.txt", full.names = T)
  return(d)
}

readFile <- function(path, settings)
{
  headerLines <- NA
  if (settings$skip != 0)
  {
    headerLines <- c()
    fileLines <- readLines(path, n = settings$skip)
    for (line in fileLines)
    {
      if (gregexpr(pattern = settings$comment.char, line)[[1]][1] == 1)
      {
        headerLines <- c(headerLines, line)
      }
    }
  }
  samples <- read.csv(file = path, 
           sep = settings$sep,   
           skip = settings$skip, 
           comment.char = settings$comment.char, 
           header = settings$header,
           blank.lines.skip = T, 
           check.names = F,
           stringsAsFactors = F)
  list(headerLines = headerLines, samples = samples)
}

createRawDataRecord <- function(rawDataRecord)
{
  new("RawDataRecord",
      filepath = rawDataRecord$filepath,
      headerLines = rawDataRecord$headerLines,
      samples = as.data.frame(rawDataRecord$samples),
      settings = rawDataRecord$settings
      )
}

loadDataFiles <- function(folderPath, fileReadSettings)
{
  filePaths <- readFiles(folderPath)
  filesCount <- length(filePaths)
  asIsDataFromFilesInFolder <- lapply(filePaths, FUN = readFile, settings = fileReadSettings)
  headersLines <- lapply(asIsDataFromFilesInFolder, FUN = function(x) {x[1]})
  samples <- lapply(asIsDataFromFilesInFolder, FUN = function(x) {x[2]})
  lst <- cbind(filepath = filePaths, headerLines = headersLines, samples = samples)
  rawDataRecordsList <- apply(lst, MARGIN = 1, FUN = createRawDataRecord)
  new("RawDataTable", records = rawDataRecordsList)
}

getwd()
obj <- serialize(data, NULL, TRUE)
obj2 <- unserialize(obj)
