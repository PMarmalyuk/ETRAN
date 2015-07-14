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
  if (conditions@conditions$eye == "left")
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
  if (conditions@conditions$eye == "right")
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
  if (conditions@conditions$eye == "both")
  {
    lporx <- data[,dataFields@availableFields$lporx]
    lpory <- data[,dataFields@availableFields$lpory]
    smpCnt <- length(lporx)
    rporx <- data[,dataFields@availableFields$rporx]
    rpory <- data[,dataFields@availableFields$rpory]
    leftEyeSamples <- new(Class = "TrajectorySamples", eyeData = data.frame(lporx, lpory))
    rightEyeSamples <- new(Class = "TrajectorySamples", eyeData = data.frame(rporx, rpory))
    eyeDataObject@leftEyeSamples <- leftEyeSamples
    eyeDataObject@rightEyeSamples <- rightEyeSamples
    if (conditions@conditions$pupilShape == "circle")
    {
      lpupxsize <- data[,dataFields@availableFields$lpupxsize]
      rpupxsize <- data[,dataFields@availableFields$rpupxsize]
      leftPupSamples <- new(Class = "PupilSamples", pupilData = data.frame(lpupxsize))
      rightPupSamples <- new(Class = "PupilSamples", pupilData = data.frame(rpupxsize))
      eyeDataObject@leftPupilSamples <- leftPupSamples
      eyeDataObject@rightPupilSamples <- rightPupSamples
    }
    if (conditions@conditions$pupilShape == "ellipse")
    {
      lpupxsize <- data[,dataFields@availableFields$lpupxsize]
      rpupxsize <- data[,dataFields@availableFields$rpupxsize]
      lpupysize <- data[,dataFields@availableFields$lpupysize]
      rpupysize <- data[,dataFields@availableFields$rpupysize]
      leftPupSamples <- new(Class = "PupilSamples", pupilData = data.frame(lpupxsize, lpupysize))
      rightPupSamples <- new(Class = "PupilSamples", pupilData = data.frame(rpupxsize, rpupysize))
      eyeDataObject@leftPupilSamples <- leftPupSamples
      eyeDataObject@rightPupilSamples <- rightPupSamples
    }
  }
  if (!is.na(dataFields@availableFields$leftAdditionalFields[1]) & conditions@conditions$eye == "left")
  {
    leftAddSmp <- lapply(dataFields@availableFields$leftAdditionalFields, FUN = function(x) {data[,x]})
    leftAddSmp <- new(Class = "OtherSamples", otherData = as.data.frame(leftAddSmp))
    eyeDataObject@leftAdditionalSamples <- leftAddSmp
  }
  if (!is.na(dataFields@availableFields$rightAdditionalFields[1]) & conditions@conditions$eye == "right")
  {
    rightAddSmp <- lapply(dataFields@availableFields$rightAdditionalFields, FUN = function(x) {data[,x]})
    rightAddSmp <- new(Class = "OtherSamples", otherData = as.data.frame(rightAddSmp))
    eyeDataObject@rightAdditionalSamples <- rightAddSmp
  }
  if (!is.na(dataFields@availableFields$leftAdditionalFields[1]) &
      !is.na(dataFields@availableFields$rightAdditionalFields[1]) & conditions@conditions$eye == "both")
  {
    leftAddSmp <- lapply(dataFields@availableFields$leftAdditionalFields, FUN = function(x) {data[,x]})
    leftAddSmp <- new(Class = "OtherSamples", otherData = as.data.frame(leftAddSmp))
    rightAddSmp <- lapply(dataFields@availableFields$rightAdditionalFields, FUN = function(x) {data[,x]})
    rightAddSmp <- new(Class = "OtherSamples", otherData = as.data.frame(rightAddSmp))
    eyeDataObject@leftAdditionalSamples = leftAddSmp
    eyeDataObject@rightAdditionalSamples = rightAddSmp
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
    times <- new(Class = "TimeSamples", time = t)
    eyeDataObject@time <- times
  }
  else
  {
    warning("Time samples are not present! Using sample rate to generate time sequence")
    if (!is.na(conditions@conditions$sampleRate))
    {
      dt <- 1/conditions@conditions$sampleRate
      t <- seq(from = dt, to = dt*smpCnt, by = dt)
      times <- new(Class = "TimeSamples", time = t)
      eyeDataObject@conditions@conditions$timeUnits <- 1
      eyeDataObject@time <- times
    }
    else
    {
      warning("Sample rate is not specified! Many functions will not work!")
      return(NULL)
    }
  }
  return(eyeDataObject)
}

## CORE PARSER ##
# raw data record parsing
coreParser <- function(RawDataRecord, settings)
{
  self <- RawDataRecord
  conditions <- settings$conditions
  filePath <- self@filePath
  dataFields <- settings$dataFields
  sampleKey <- settings$sampleKey
  
  # NOT USED FOR NOW:
  headerKeys <- settings$headerKeys
  sep <- settings$sep
  # 
  
  ## Deleting all samples with sample type other than sampleKey
  if (!is.na(dataFields@availableFields$smptype))
  {
    self@data <- self@data[self@data[, dataFields@availableFields$smptype] == sampleKey,]
  }
  
  ## Reading trial number samples
  trials <- NA
  if (!is.na(dataFields@availableFields$trial))
  {
    trialColumn <- self@data[,dataFields@availableFields$trial]
    trials <- unique(trialColumn)
  }
  ## Reading stimuli name samples as trials indicators
  if (is.na(dataFields@availableFields$trial) & !is.na(dataFields@availableFields$stimname))
  {
    trialColumn <- self@data[,dataFields@availableFields$stimname]
    trials <- unique(trialColumn)
  }
  ## Splitting data by trials
  if (!is.na(trials[1]))
  {
    trialsData <- split(self@data, f = trials)
  }
  else
  {
    trialsData <- list(self@data)
  }
  
  ## Getting field names using self@data column names
  fNames <- lapply(dataFields@availableFields[-c(14,15)], FUN = function(x) {if (!is.na(x)) {colnames(self@data)[x]} else {"NA"} })
  leftAddFNames <- NA
  rightAddFNames <- NA
  if (!is.na(dataFields@availableFields[[14]][1]))
  {
    leftAddFNames <- lapply(dataFields@availableFields[[14]], FUN = function(x) {if (!is.na(x)) {colnames(self@data)[x]} else {"NA"} })  
  }
  if (!is.na(dataFields@availableFields[[15]][1]))
  {
    rightAddFNames <- lapply(dataFields@availableFields[[15]], FUN = function(x) {if (!is.na(x)) {colnames(self@data)[x]} else {"NA"} })
  }
  fieldNames <- new(Class = "DataFieldNames", 
                    fieldNames = list(fNames, 
                                      leftAdditionalFields = leftAddFNames, 
                                      rightAdditionalFields = rightAddFNames))
  
  ## Figuring out conditions
  ### Recording mode (monocular: left or right,- or binocular)
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
    if (!eyeLeft & !eyeRight) {warning("You must specify eye samples disposition in your dataset!"); return(NULL)}
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
  if (length(self@headerLines) != 0)
  {
    keyValues <- lapply(headerKeys@keys, FUN = findKeyValue, headerLines = self@headerLines, sep = sep)
    subjectCode <- keyValues$subjectCode
#     stimDim <- as.numeric(keyValues$stimDim)
#     conditions@conditions$sampleRate <- as.numeric(keyValues$sampleRate)
#     conditions@conditions$screenDistance <- as.numeric(keyValues$headDist)
  }

  ## Deterimining frames count
#   framesCnt <- NA
#   if (!is.na(dataFields@availableFields$frame))
#   {
#     frameColumn <- self@data[,dataFields@availableFields$frame]
#     samplesNum <- length(frameColumn)
#     framesCnt <- frameColumn[samplesNum] 
#   }

  ## Creating list of data to be disassembled in application layer
  eyesDataObjects <- lapply(trialsData, FUN = createEyesDataObject, dataFields = dataFields, fieldNames = fieldNames, conditions = conditions)
  filePath <- rep(filePath, length(trials))
  subjectCode <- rep(subjectCode, length(trials))
  res <- list(filePath = filePath, subjectCode = subjectCode, trials = trials, eyesDataObjects = eyesDataObjects)
  return(res)
}