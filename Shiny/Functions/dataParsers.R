readRawCSVData <- function(filePath, settings)
{
  if (!file.exists(filePath))
  {
    warning("Datafile not found!")
    return(NULL)
  }
  else
  {
    headerLines <- readLines(con = filePath, 
                             n = settings$skip, 
                             encoding = settings$encoding)
    asIsData <- read.csv(file = filePath, 
                         sep = settings$sep, 
                         dec = settings$dec,   
                         skip = settings$skip, 
                         comment.char = settings$comment.char, 
                         header = settings$header,
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
  ## Searching for a string with a UNIQUE key specified (may be problems with common keys' strings)
  if (any(grepl(pattern = key, x = headerLines)))
  {
    ## If there's any string with a key then we read it
    stringNum <- grep(pattern = key, x = headerLines)
    string <- headerLines[stringNum]
    ## Searching for a colon in a string with a key and a colon
    keyPos <- regexpr(pattern = paste(key, ":", sep = ""), string)
    keyStart <- keyPos[1]
    keyLen <- attr(keyPos, "match.length")
    ## Finding a colon
    colonPos <- regexpr(pattern = ":", string)[1]
    ## Reading everythin after a colon
    keyRawVal <- substr(x = string, start = colonPos+1, stop = nchar(string))
    ## Deleting spaces
    keyRawVal <- gsub(" ", "", keyRawVal)
    ## Parsing key value(s)
    res <- regmatches(keyRawVal, gregexpr(sep, keyRawVal), invert = T)[[1]][-1]
  } else 
  {
    res <- NA
  }
  res
}

createEyesDataObject <- function(data, dataFields, fieldNames, conditions)
{
  eyeDataObject <- new(Class = "EyesData", fieldNames = fieldNames, conditions = conditions)
  if (!is.na(dataFields@availableFields$frame))
  {
    frames <- new(Class = "FrameSamples", frame = as.numeric(data[,dataFields@availableFields$frame]))
    eyeDataObject@frame <- frames
  }
  if (conditions@conditions$eye == "left" | conditions@conditions$eye == "both")
  {
    porx <- data[,dataFields@availableFields$lporx]
    pory <- data[,dataFields@availableFields$lpory]
    smpCnt <- length(porx)
    eyeSamples <- new(Class = "TrajectorySamples", eyeData = data.frame(porx, pory))
    eyeDataObject@leftEyeSamples <- eyeSamples
    if (conditions@conditions$pupilShape == "circle")
    {
      pupxsize <- data[,dataFields@availableFields$lpupxsize]
      pupSamples <- new(Class = "PupilSamples", pupilData = data.frame(pupxsize))
      eyeDataObject@leftPupilSamples <- pupSamples
    }
    if (conditions@conditions$pupilShape == "ellipse")
    {
      pupxsize <- data[,dataFields@availableFields$lpupxsize]
      pupysize <- data[,dataFields@availableFields$lpupysize]
      pupSamples <- new(Class = "PupilSamples", pupilData = data.frame(pupxsize, pupysize))
      eyeDataObject@leftPupilSamples <- pupSamples
    }
  }
  if (conditions@conditions$eye == "right" | conditions@conditions$eye == "both")
  {
    porx <- data[,dataFields@availableFields$rporx]
    pory <- data[,dataFields@availableFields$rpory]
    smpCnt <- length(porx)
    eyeSamples <- new(Class = "TrajectorySamples", eyeData = data.frame(porx, pory))
    eyeDataObject@rightEyeSamples <- eyeSamples
    if (conditions@conditions$pupilShape == "circle")
    {
      pupxsize <- data[,dataFields@availableFields$rpupxsize]
      pupSamples <- new(Class = "PupilSamples", pupilData = data.frame(pupxsize))
      eyeDataObject@rightPupilSamples <- pupSamples
    }
    if (conditions@conditions$pupilShape == "ellipse")
    {
      pupxsize <- data[,dataFields@availableFields$rpupxsize]
      pupysize <- data[,dataFields@availableFields$rpupysize]
      pupSamples <- new(Class = "PupilSamples", pupilData = data.frame(pupxsize, pupysize))
      eyeDataObject@rightPupilSamples <- pupSamples
    }
  }
  if (!all(is.na(dataFields@availableFields$leftAdditionalFields)) & 
      conditions@conditions$eye == "left" | conditions@conditions$eye == "both")
  {
    leftAddSmp <- lapply(dataFields@availableFields$leftAdditionalFields, FUN = function(x) {data[,x[[1]]]})
    leftAddSmp <- new(Class = "OtherSamples", otherData = as.data.frame(leftAddSmp))
    eyeDataObject@leftAdditionalSamples <- leftAddSmp
  }
  if (!all(is.na(dataFields@availableFields$rightAdditionalFields)) & 
      conditions@conditions$eye == "right" | conditions@conditions$eye == "both")
  {
    rightAddSmp <- lapply(dataFields@availableFields$rightAdditionalFields, FUN = function(x) {data[,x[[1]]]})
    rightAddSmp <- new(Class = "OtherSamples", otherData = as.data.frame(rightAddSmp))
    eyeDataObject@rightAdditionalSamples <- rightAddSmp
  }
  if (!is.na(dataFields@availableFields$time))
  {
    if (!is.na(conditions@conditions$timeUnits))
    {
      t <- data[,dataFields@availableFields$time]*conditions@conditions$timeUnits
    }
    else
    {
      warning("Time unit is not specified! It is important to take it into account for correct velocity estimation!")
      t <- data[,dataFields@availableFields$time]
    }
    t <- t-min(t[t>=0])
    times <- new(Class = "TimeSamples", time = t)
    eyeDataObject@time <- times
  }
  else
  {
    warning("Time samples are not present! Using sample rate to generate time sequence")
    if (!is.na(conditions@conditions$sampleRate))
    {
      dt <- 1/conditions@conditions$sampleRate
      t <- seq(from = 0, to = dt*(smpCnt-1), by = dt)
      times <- new(Class = "TimeSamples", time = t)
      eyeDataObject@conditions@conditions$timeUnits <- 1
      eyeDataObject@time <- times
    }
    else
    {
      warning("Sample rate is not specified! Using sample numbers instead!")
      eyeDataObject@time <- 0:(smpCnt-1)
    }
  }
  return(eyeDataObject)
}

## CORE PARSER ##
# raw data record parsing
parseRawCSVData <- function(filePath, settings)
{
  conditions <- settings$conditions
  dataFields <- settings$dataFields
  sampleKey <- settings$sampleKey
  headerKeys <- settings$headerKeys
  sep <- settings$sep
  rawData <- readRawCSVData(filePath = filePath, settings = settings)
  if (is.null(rawData)) {return(NULL)}
  
  ## Deleting all samples with sample type other than specified sampleKey
  if (!is.na(dataFields@availableFields$smptype))
  {
    rawData$data <- rawData$data[rawData$data[, dataFields@availableFields$smptype] == sampleKey,]
  }
  
  ## Reading trial number samples
  trials <- NA
  if (!is.na(dataFields@availableFields$trial))
  {
    trialColumn <- rawData$data[,dataFields@availableFields$trial]
    trials <- unique(trialColumn)
  }
  ## Reading stimuli name samples as trials indicators
  if (is.na(dataFields@availableFields$trial) & !is.na(dataFields@availableFields$stimname))
  {
    trialColumn <- rawData$data[,dataFields@availableFields$stimname]
    trials <- unique(trialColumn)
  }
  ## Splitting data by trials
  if (!is.na(trials[1]))
  {
    trialsData <- split(rawData$data, f = trials)
  }
  else
  {
    trialsData <- list(rawData$data)
  }
  
  ## Getting field names using rawData$data column names
  fNames <- lapply(head(dataFields@availableFields, length(dataFields@availableFields)-2), 
                   FUN = function(x) {if (!is.na(x)) {colnames(rawData$data)[x]} else {"NA"} })
  leftAddFNames <- NA
  rightAddFNames <- NA
  leftAddFields <- tail(dataFields@availableFields, 2)[[1]]
  if (!all(is.na(leftAddFields)))
  {
    leftAddFNames <- lapply(leftAddFields, 
                            FUN = function(x) 
                            {
                              if (!is.na(x[[1]])) {colnames(rawData$data)[x[[1]]]} else {"NA"} 
                            })  
  }
  rightAddFields <- tail(dataFields@availableFields, 1)
  if (!all(is.na(rightAddFields)))
  {
    rightAddFNames <- lapply(rightAddFields, 
                             FUN = function(x) 
                             {
                               if (!is.na(x[[1]])) {colnames(rawData$data)[x[[1]]]} else {"NA"} 
                             })  
  }
  
  fieldNames <- new(Class = "DataFieldNames", 
                    fieldNames = list(fNames, 
                                      leftAdditionalFields = leftAddFNames, 
                                      rightAdditionalFields = rightAddFNames))
  
  ## Figuring out conditions
  ### Recording mode (monocular: left, right or both - binocular)
  if (is.na(conditions@conditions$eye))
  {
    eyeLeft <- F
    eyeRight <- F
    if (!is.na(dataFields@availableFields$lporx) & !is.na(dataFields@availableFields$lpory))
    {
      eyeLeft <- T
    }
    if (!is.na(dataFields@availableFields$rporx) & !is.na(dataFields@availableFields$rpory))
    {
      eyeRight <- T
    }
    # TO DO: RETURN SOMETHING IF NO GAZE DATA IS PRESENT
    if (!eyeLeft & !eyeRight) {warning("You have not specified gaze samples columns!")}
    if (eyeLeft & eyeRight) {eye = "both"}
    if (eyeLeft & !eyeRight) {eye = "left"}
    if (!eyeLeft & eyeRight) {eye = "right"}
    conditions@conditions$eye <- eye
  }
  
  ## Determining pupil shape
  if (is.na(conditions@conditions$pupilShape))
  {
    if (is.na(dataFields@availableFields$lpupxsize) & is.na(dataFields@availableFields$lpupysize) |
        is.na(dataFields@availableFields$rpupxsize) & is.na(dataFields@availableFields$rpupysize)
    )
    {
      pupilShape <- NA
    }
    if (!is.na(dataFields@availableFields$lpupxsize) & is.na(dataFields@availableFields$lpupysize) |
        !is.na(dataFields@availableFields$rpupxsize) & is.na(dataFields@availableFields$rpupysize)
    )
    {
      pupilShape <- "circle"
    }            
    if (!is.na(dataFields@availableFields$lpupxsize) & !is.na(dataFields@availableFields$lpupysize) |
        !is.na(dataFields@availableFields$rpupxsize) & !is.na(dataFields@availableFields$rpupysize)
    )
    {
      pupilShape <- "ellipse"
    }
    conditions@conditions$pupilShape <- pupilShape
  }
  
  subjectCode <- NA  
  ## Reading keys from header lines
  if (length(rawData$headerLines) != 0)
  {
    keyValues <- lapply(headerKeys@keys, FUN = findKeyValue, headerLines = rawData$headerLines, sep = sep)
    subjectCode <- keyValues$subjectCode
    if (is.na(conditions@conditions$sampleRate))
    {
      conditions@conditions$sampleRate <- as.numeric(keyValues$sampleRate)
    }
    if (is.na(conditions@conditions$screenDistance))
    {
      conditions@conditions$screenDistance <- as.numeric(keyValues$screenDistance)
    }
  }
  filePath <- rep(filePath, length(trials))
  subjectCode <- rep(subjectCode, length(trials))
  eyesDataObjects <- lapply(trialsData, 
                            FUN = createEyesDataObject, 
                            dataFields = dataFields,
                            fieldNames = fieldNames,
                            conditions = conditions)
  res <- list(filePath = filePath, 
              subjectCode = subjectCode, 
              trials = trials, 
              eyesDataObjects = eyesDataObjects)
  return(res)
}


