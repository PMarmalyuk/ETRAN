coreEstimator <- function(DataRecord, settings)
{
  subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@operation == "Record Analysis"})]
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  res <- lapply(subFunctions, FUN = function(x) 
  {
    fun <- x@fun
    settings <- append(x@settings, list(conditions = conditions))
    applyTo <- x@applyTo
    if (applyTo == "EyesData")
    {
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
      {
        eyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
        res <- fun(data = eyeData, settings = settings)
        if (!is.null(res))
        {
          resLeft <- res
        }
        else resLeft <- list(NULL)
        resRight <- list(NULL)
      }
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
      {
        eyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
        res <- fun(data = eyeData, settings = settings)
        if (!is.null(res))
        {
          resRight <- res
        }
        else resRight <- list(NULL)
        resLeft <- list(NULL)
      }
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
      {
        leftEyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
        rightEyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
        resL <- fun(data = leftEyeData, settings = settings)
        if (!is.null(resL))
        {
          resLeft <- resL
        }
        else resLeft <- list(NULL)
        resR <- fun(data = rightEyeData, settings = settings)
        if (!is.null(resR))
        {
          resRight <- resR
        }
        else resRight <- list(NULL)
      }
    }
    if (applyTo == "EventData")
    {
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
      {
      }
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
      {
      }
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
      {
      }
    }
    return(list(resLeft = resLeft, resRight = resRight))
  })
  res2left <- unlist(lapply(res, FUN = function(x) {return(x$resLeft)}), recursive = F)
  res2right <- unlist(lapply(res, FUN = function(x) {return(x$resRight)}), recursive = F)
  DataRecord@statistics$left <- modifyList(x = DataRecord@statistics$left, val = res2left, keep.null = F)
  DataRecord@statistics$right <- modifyList(x = DataRecord@statistics$right, val = res2right, keep.null = F)
  return(DataRecord)
}

generalParamEstimator <- function(data, operation, settings)
{
  
}

paramsDataAsFactorsData <- function(estimatorsResults, operation, mainFactorsDef, settings)
{
  
}

coreDataRecordParamEstimator <- function(DataRecord, settings)
{
  eventFactorsDef <- settings$eventFactorsDef
  eventFactorsData <- DataRecord@analysisResults$eventFactorsData
  
  mainFactorsDef <- settings$mainFactorsDef
  mainFactorsData <- settings$mainFactorsData
  
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  operation <- settings$operation
  eye <- conditions$eye
  
  if (operation == "Eyes Data")
  {
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "Eyes Data"})]
    if (eye == "left")
    {
      data <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
      eventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
      filterMarkerNames <- DataRecord@eyesDataObject@leftFilterMarkers@markerNames
      settings <- append(settings, list(fmn = filterMarkerNames, evmn = eventMarkerNames, conditions = conditions))
      estimatorsResults <- generalParamEstimator(data, operation, settings)
      factorsData <- paramsDataAsFactorsData(estimatorsResults, operation, mainFactorsDef, settings)
      factorsDef <- factorsData$mainFactorsDef
      factorsData <- factorsData$mainFactorsData
      mainFactorsData <- new(Class = "FactorsData", factorsDataList = as.data.frame(eventFactorsData))
      # Here we should add new main factors definitions and their values to 
    }
    if (eye = "right")
    {
      
    }
    if (eye = "both")
    {
      
    }
  }
  if (operation == "Event Data")
  {
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "Event Data"})]
  }
  if (operation == "AOI Data")
  {
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "AOI Data"})]
  }
  if (operation == "AOI Sequence")
  {
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "AOI Sequence"})]
  }
  if (operation == "AOI Stats Vector")
  {
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "AOI Stats Vector"})]
  }
  if (operation == "AOI Transition Matrix")
  {
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "AOI Transition Matrix"})]
  }
  return(list(dataRec = DataRecord, mainFactorsData = factorsData, mainFactorsDef = factorsDef))
}
