# Reading raw CSV data: header lines and table body
readRawCSVData <- function(filePath, encoding, sep, dec,
                           infoHeaderLines, tableHeader, commentChar)
{
  if (!file.exists(filePath))
  {
    warning("Datafile not found!")
    return(NA)
  }
  else
  {
    headerLines <- readLines(con = filePath, 
                             ok = T, warn = F,
                             encoding,
                             n = infoHeaderLines)
    asIsData <- read.csv(file = filePath, 
                         encoding = encoding,
                         skip = infoHeaderLines, 
                         header = tableHeader,
                         sep = sep, 
                         dec = dec, 
                         comment.char = commentChar, 
                         blank.lines.skip = T, 
                         check.names = F,
                         stringsAsFactors = F)
    
  }
  rawData <- list(filePath = filePath, headerLines = headerLines, data = asIsData)
  return(rawData)
}

# Searching for a string with a key specified and extracting everything after a colon considering specified separator
findKeyValue <- function(key, sep, headerLines)
{
  keyValueType <- key$type
  key <- key$keyString
  ## Searching for a string with a UNIQUE key specified (may be problems with common keys' strings)
  if (any(grepl(pattern = key, x = headerLines, fixed = T)))
  {
    ## If there's any string with a key then we read it
    stringNum <- grep(pattern = key, x = headerLines, fixed = T)
    string <- headerLines[stringNum]
    ## Searching for a colon in a string with a key and a colon
    keyPos <- regexpr(pattern = paste(key, ":", sep = ""), string)
    keyStart <- keyPos[1]
    keyLen <- attr(keyPos, "match.length")
    ## Finding a colon
    colonPos <- regexpr(pattern = ":", string)[1]
    ## Reading everything after a colon
    keyRawVal <- substr(x = string, start = colonPos+1, stop = nchar(string))
    ## Deleting spaces
    keyRawVal <- gsub(" ", "", keyRawVal)
    ## Parsing key value(s)
    res <- regmatches(keyRawVal, gregexpr(sep, keyRawVal), invert = T)[[1]][-1]
    ## Converting type to specified type of a key value
    res <- new(Class = keyValueType, res)
  } else 
  {
    res <- NA
  }
  res
}

# Appying specified parser to headerLines to search values of specified keys
headerParser <- function(parser, headerLines, keys, sep)
{
  keyValues <- list()
  if (length(headerLines) != 0)
  {
    keyValues <- lapply(headerKeys, FUN = parser, headerLines = headerLines, sep = sep)
  }
  return(keyValues)
}

# Getting specified columns from given data frame
getDataColumns <- function(df, columns)
{
  if (all(is.na(columns)))
  {
    return(NA)
  }
  selectedData <- df[,unlist(columns[!is.na(columns)])]
  names(selectedData) <- names(columns[!is.na(columns)])
  return(selectedData)
}

# Getting specified event markers columns from given data frame
# For event markers columns one need to specify additional information
# such as eventsType and detector name (e.g. "ExternalSMIEventDetector")
getExternalEventsMarkers <- function(df, eventsDataInfo)
{
  eventData <- NA
  if (!is.na(eventsDataInfo))
  {
    eventData <- lapply(eventsDataInfo, 
                        FUN = function(x)
                        {
                          eventMarkers =  factor(df[,x$column], levels = x$eventsNames)
                          evmarks <- data.frame(firstEv = eventMarkers[-length(eventMarkers)], secondEv = eventMarkers[-1])
                          transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
                          eventGroups <- c(1,cumsum(transitions)+1)
                          eventDataInfo <- list(eventMarkers =  eventMarkers,
                                                eventGroups = eventGroups,
                                                eventsClass = x$type,
                                                detector = x$detector)
                          return(eventDataInfo)
                        }
    )
    names(eventData) <- names(eventsDataInfo)
  }
  return(eventData)
}

columnsReader <- function(rawDataColumns, columnsPositions)
{
  commonData <- getDataColumns(df = rawDataColumns, columns = columnsPositions$commonData)
  leftEyeData <- getDataColumns(df = rawDataColumns, columns = columnsPositions$leftEyeData)
  leftAddData <- getDataColumns(df = rawDataColumns, columns = columnsPositions$leftAddData)
  
  leftEventsData <- getExternalEventsMarkers(df = rawDataColumns, eventsDataInfo = columnsPositions$leftEventsData)
  
  rightEyeData <- getDataColumns(df = rawDataColumns, columns = columnsPositions$rightEyeData)
  rightAddData <- getDataColumns(df = rawDataColumns, columns = columnsPositions$rightAddData)
  
  rightEventsData <- getExternalEventsMarkers(df = rawDataColumns, eventsDataInfo = columnsPositions$rightEventsData)
  
  data <- list(commonData = commonData,
               leftEyeData = leftEyeData,
               leftAddData = leftAddData,
               leftEventsData = leftEventsData,
               rightEyeData = rightEyeData,
               rightAddData = rightAddData,
               rightEventsData = rightEventsData)
  return(data)
}

readCSVData <- function(filePath, encoding, infoHeaderLines, infoHeaderParser, headerKeys,
                        tableHeader, sep, dec, smpMarker, commentChar, columnsPositions)
{
  rawData <- readRawCSVData(filePath = filePath,
                            encoding = encoding,
                            sep = sep,
                            dec = dec,
                            infoHeaderLines = infoHeaderLines,
                            tableHeader = tableHeader,
                            commentChar = commentChar)
  
  if (all(is.na(rawData)))
  {
    warning("Cannot read data!")
    return(NA)
  }
  settings <- list(filename = basename(filePath))
  # Deleting all samples which are not marked by specified smpMarker (e.g. "SMP" in SMI datasets)
  # (in case data have smpFlag column)
  if (!is.na(columnsPositions$commonData$smpFlag))
  {
    smpFlag <- columnsPositions$commonData$smpFlag
    rawData$data <- rawData$data[rawData$data[,smpFlag] == smpMarker,]
  }

  # Reading values of specified keys from file's header lines
  keysValues <- headerParser(parser = infoHeaderParser, 
                             headerLines = rawData$headerLines, 
                             keys = headerKeys, sep = sep)
  settings <- append(settings, keysValues)

  # Evaluating recording mode
  leftEyeData <- F; rightEyeData <- F
  if (!all(is.na(columnsPositions$leftEyeData))) {leftEyeData <- T; mode <- "left"}
  if (!all(is.na(columnsPositions$rightEyeData))) {rightEyeData <- T; mode <- "right"}
  if (leftEyeData & rightEyeData) {mode <- "binocular"}
  
  # Evaluating pupil shape approximation type
  pupilShape <- NA
  if (mode == "left" | mode == "binocular")
  {
    if (!is.na(columnsPositions$leftEyeData$pupx) & !is.na(columnsPositions$leftEyeData$pupy))
    {
      pupilShape <- "ellipse"
    }
    if (!is.na(columnsPositions$leftEyeData$pupx) & is.na(columnsPositions$leftEyeData$pupy))
    {
      pupilShape <- "circle"
    }
  }
  if (mode == "right" | mode == "binocular")
  {
    if (!is.na(columnsPositions$rightEyeData$pupx) & !is.na(columnsPositions$rightEyeData$pupy))
    {
      pupilShape <- "ellipse"
    }
    if (!is.na(columnsPositions$rightEyeData$pupx) & is.na(columnsPositions$rightEyeData$pupy))
    {
      pupilShape <- "circle"
    }
  }
  settings <- append(settings, list(mode = mode))
  settings <- append(settings, list(pupilShape = pupilShape))
  # Reading data columns
  data <- columnsReader(rawDataColumns = rawData$data, columnsPositions = columnsPositions)
  # TO DO: ADD HEAD DATA BY COLUMNS READER
  # data <- append(data, list(headData = NA))
  data <- append(data, list(representations = NA))
  data <- append(data, list(descriptives = NA))
  eyeDataRecord <- append(list(settings = settings), data)
  return(eyeDataRecord)
}
