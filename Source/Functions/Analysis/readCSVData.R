readRawCSVData <- function(filePath, encoding, sep, dec,
                           infoLinesCnt, header, commentChar)
{
  if (!file.exists(filePath))
  {
    warning("Datafile not found!"); return(NA)
  }
  else
  {
    headerLines <- readLines(con = filePath, 
                             ok = T, warn = F,
                             encoding,
                             n = infoLinesCnt)
    asIsData <- read.csv(file = filePath, 
                         encoding = encoding,
                         skip = infoLinesCnt, 
                         header = header,
                         sep = sep, 
                         dec = dec, 
                         comment.char = commentChar, 
                         blank.lines.skip = T, 
                         check.names = F,
                         stringsAsFactors = F)
  }
  rawData <- list(filePath = filePath, 
                  headerLines = headerLines, 
                  data = asIsData)
  return(rawData)
}

# Searching for a string with a key specified and extracting
# everything after a colon considering specified separator
findKeyValue <- function(key, sep, headerLines)
{
  keyValueType <- key$type
  key <- key$keyString
  ## Searching for a string with a UNIQUE key specified (may
  # be problems with common keys' strings)
  if (any(grepl(pattern = key, x = headerLines, fixed = T)))
  {
    ## If there's any string with a key then we read it
    stringNum <- grep(pattern = key, x = headerLines, 
                      fixed = T)
    string <- headerLines[stringNum]
    ## Searching for a colon in a string with a key and a colon
    keyPos <- regexpr(pattern = paste(key, ":", sep = ""), 
                      string)
    keyStart <- keyPos[1]
    keyLen <- attr(keyPos, "match.length")
    ## Finding a colon
    colonPos <- regexpr(pattern = ":", string)[1]
    ## Reading everything after a colon
    keyRawVal <- substr(x = string, start = colonPos+1, 
                        stop = nchar(string))
    ## Deleting spaces
    keyRawVal <- gsub(" ", "", keyRawVal, fixed = T)
    ## Parsing key value(s)
    res <- regmatches(keyRawVal, gregexpr(sep, keyRawVal), 
                      invert = T)[[1]][-1]
    ## Converting type to specified type of a key value
    res <- new(Class = keyValueType, res)
  } else res <- NA
  res
}

# Applying specified parser to headerLines to search values 
# of specified keys
headerParser <- function(parser, headerLines, keys, sep)
{
  keyValues <- list()
  if (length(headerLines) != 0)
  {
    keyValues <- lapply(headerKeys, FUN = parser, 
                        headerLines = headerLines, 
                        sep = sep)
  }
  return(keyValues)
}

# Getting specified columns from given data frame
getDataColumns <- function(df, columns)
{
  if (all(is.na(columns)))
  {
    return(NULL)
  }
  selectedData <- data.frame(df[,unlist(columns[!is.na(columns)])])
  colnames(selectedData) <- names(columns[!is.na(columns)])
  return(selectedData)
}

# Getting specified event markers columns from given data frame
# For event markers columns one need to specify additional information
# such as eventsType and detector name (e.g. "ExternalSMIEventDetector")
getExternalEventsMarkers <- function(df, eventsInfo)
{
  eventData <- NULL
  if (!is.na(eventsInfo))
  {
    eventData <- lapply(eventsInfo,
                        FUN = function(x)
                        {
                          eventMarkers <- factor(df[,x$column], 
                                                 levels = ifelse(is.null(x$eventsNames), 
                                                                 unique(df[,x$column]), 
                                                                 x$eventsNames))
                          eventGroups <- markersGroups(eventMarkers)
                          
                          eventInfo <- list(eventMarkers =  eventMarkers,
                                            eventGroups = eventGroups,
                                            eventsClass = x$type,
                                            eye = NA)
                          
                          eventsPositions <- getEventsPositions(eventInfo)
                          
                          return(eventsPositions)
                        }
    )
    names(eventData) <- names(eventsInfo)
  }
  return(eventData)
}

columnsReader <- function(rawDataColumns, columnsPositions)
{
  data <- list()
  data$commonData <- getDataColumns(df = rawDataColumns, 
                                    columns = columnsPositions$commonData)
  data$leftEyeData <- getDataColumns(df = rawDataColumns, 
                                     columns = columnsPositions$leftEyeData)
  data$rightEyeData <- getDataColumns(df = rawDataColumns, 
                                      columns = columnsPositions$rightEyeData)
  data$commonEvents <- getExternalEventsMarkers(df = rawDataColumns, 
                                                eventsInfo = columnsPositions$commonEvents)
  data$leftEvents <- getExternalEventsMarkers(df = rawDataColumns, 
                                              eventsInfo = columnsPositions$leftEvents)
  data$rightEvents <- getExternalEventsMarkers(df = rawDataColumns, 
                                               eventsInfo = columnsPositions$rightEvents)
  return(data)
}

readCSVData <- function(filePath, encoding, columnsPositions,
                        header, sep, dec, msgMarker = NA, 
                        msgFlagColumn = NA, msgKey = NA,
                        infoLinesCnt = 0, infoParser = NA, 
                        headerKeys = NA, commentChar = "#")
{
  rawData <- readRawCSVData(filePath = filePath,
                            encoding = encoding,
                            sep = sep,
                            dec = dec,
                            infoLinesCnt = infoLinesCnt,
                            header = header,
                            commentChar = commentChar)
  if (all(is.na(rawData)))
  {
    stop("Cannot read data!")
    return(NA)
  }
  settings <- list(filename = basename(filePath))
  colNames <- colnames(rawData$data)
  settings <- append(settings, list(colNames = colNames))
  # Deleting all samples which are marked by specified 
  # syncMarker (e.g. "SMP" in SMI datasets)
  # Creating syncEventMarkers markers indicating locations 
  # of different syncronisation messages
  # (in case msgMarker, msgFlagColumn and msgKey columns are specified)
  if (!is.na(msgMarker) & !is.na(msgFlagColumn))
  {
    msgPositions <- which(rawData$data[, msgFlagColumn] == msgMarker)
    if (any(msgPositions))
    {
      if (!is.na(!is.na(msgKey)))
      {
        lines <- readLines(filePath)
        lnums <- grep(msgKey, lines, fixed = T)
        keyEnds <- regexpr(pattern = ":", lines[lnums], fixed = T)
        messages <- substr(x = lines[lnums], start = keyEnds+1, 
                           stop = nchar(lines[lnums]))
        msgPosAdd <- c(1, msgPositions, nrow(rawData$data)+1)
        l <- data.frame(len = diff(msgPosAdd), msg = c("", messages), 
                        grp = c(0,1:length(msgPositions)))
        l <- split(l, seq(nrow(l)))
        syncMarkers <- unlist(sapply(l, FUN = function(x) {rep(x$msg, x$len)}), 
                              recursive = F, use.names = F)[-msgPositions]
        syncGroups <- unlist(sapply(l, FUN = function(x) {rep(x$grp, x$len)}), 
                             recursive = F, use.names = F)[-msgPositions]
        rawData$data <- rawData$data[-msgPositions, ]
        syncMarkers <- list(eye = NA,
                            eventMarkers = syncMarkers,
                            eventGroups = syncGroups)
        syncEvents <- getEventsPositions(syncMarkers)
        if (syncEvents$event[1] == "")
        {
          syncEvents <- syncEvents[-1]
        }
      }
    }
    else syncEvents <- NULL
  } else syncEvents <- NULL
  # Reading values of specified keys from file's header lines
  options(warn=-1)
  if (!is.na(infoParser))
  {
    keysValues <- headerParser(parser = infoParser, 
                               headerLines = rawData$headerLines, 
                               keys = headerKeys, sep = sep)
    # SMI.msgKey
    settings <- append(settings, keysValues)
  }
  options(warn=0)
  # Evaluating recording mode
  leftEyeData <- F; rightEyeData <- F
  if (!all(is.na(columnsPositions$leftEyeData))) 
    {leftEyeData <- T; mode <- "left"}
  if (!all(is.na(columnsPositions$rightEyeData))) 
    {rightEyeData <- T; mode <- "right"}
  if (leftEyeData & rightEyeData) {mode <- "binocular"}
  # Evaluating pupil shape approximation type
  pupilShape <- NA
  if (mode == "left" | mode == "binocular")
  {
    if (!is.na(columnsPositions$leftEyeData$pupSizeX) &
        !is.na(columnsPositions$leftEyeData$pupSizeY))
    {
      pupilShape <- "ellipse"
    }
    if (!is.na(columnsPositions$leftEyeData$pupSizeX) &
        is.na(columnsPositions$leftEyeData$pupSizeY))
    {
      pupilShape <- "circle"
    }
  }
  if (mode == "right" | mode == "binocular")
  {
    if (!is.na(columnsPositions$rightEyeData$pupSizeX) &
        !is.na(columnsPositions$rightEyeData$pupSizeY))
    {
      pupilShape <- "ellipse"
    }
    if (!is.na(columnsPositions$rightEyeData$pupSizeX) &
        is.na(columnsPositions$rightEyeData$pupSizeY))
    {
      pupilShape <- "circle"
    }
  }
  settings <- append(settings, list(mode = mode))
  settings <- append(settings, list(pupilShape = pupilShape))
  # Reading data columns
  data <- columnsReader(rawDataColumns = rawData$data, 
                        columnsPositions = columnsPositions)
  eyeDataRecord <- append(list(settings = settings), data)
  eyeDataRecord$commonEvents$syncEvents <- syncEvents
  return(eyeDataRecord)
}
