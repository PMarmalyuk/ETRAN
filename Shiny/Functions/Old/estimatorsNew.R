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
        eyeData <- getEyeDataFrame(DataRecord, eye = "left")
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
        eyeData <- getEyeDataFrame(DataRecord, eye = "right")
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
        leftEyeData <- getEyeDataFrame(DataRecord, eye = "left")
        rightEyeData <- getEyeDataFrame(DataRecord, eye = "right")
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

generalParamEstimator <- function(data, settings)
{
  subFuns <- settings$subFunctions
  detectorID <- settings$detectorID
  eye <- settings$conditions$eye
  res <- list()
  for (i in 1:length(subFuns))
  {
    for (j in 1:length(subFuns[[i]]@classes))
    {
      if (subFuns[[i]]@classes[[j]]$mainClass == "EyesData")
      {
        # FUN = calculateSubFunResultsForEyesData
        res <- append(res, lapply(data$data, FUN = calculateSubFunResultForEyesData, subFun = subFuns[[i]]))
      }
      if (subFuns[[i]]@classes[[j]]$mainClass == "FactorsData")
      {
        factorClass <- subFuns[[i]]@classes[[j]]$subClass
        
        
        res <- append(res, lapply(events@events, FUN = calculateSubFunResultsForEvent, subFun = subFuns[[i]]))
      }
    }
    
  }
  res <- as.data.frame(do.call("rbind", res))
  res <- res[,-c(which(names(res) %in% c("description", "name", "owners")))]
  return(res)
}

### TO DO: append analysisResults with replacement, not whole replace!!!
coreParamEstimator <- function(DataRecord, settings)
{
  factorsDef <- settings$factorsDef
  factorsData <- DataRecord@analysisResults$eventFactorsData
  mainFactorsDef <- settings$mainFactorsDef
  mainFactorsData <- settings$mainFactorsData
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  eye <- conditions$eye
  dataRecIdentifier <- list(expID = DataRecord@expID, subjectID = DataRecord@subjectID, trialID = DataRecord@trialID)
  if (eye == "left")
  {
    # getting data record's data lying inside of data record
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "left", getFactorsData = T)
    
    # TO DO: getting data record's data lying inside of mainFactorsData (e.g. representations of EyesData object)
    # selectedMainFactorsData <- getFactorsDataByOwner(mainFactorsData, owner = list(mainClass = "EyesData"))
    # selectedMainFactorsData <- getFactorsDataByOwnerID(selectedMainFactorsData, ownerID = dataRecIdentifier)
    
    eventMarkersAndData <- append(eventMarkersAndData, list(mainFactorsData = selectedMainFactorsData))
    settings <- append(settings, list(conditions = conditions, dataRecIdentifier = dataRecIdentifier))
    estimatorResults <- generalParamEstimator(eventMarkersAndData, settings)
    factorsData <- createFactorsDataList(estimatorResults, mainFactorsDef)
    mainFactorsDef <- factorsData$mainFactorsDef
    mainFactorsData <- new(Class = "FactorsData", factorsDataList = as.data.frame(factorsData$mainFactorsData))
  }
  if (eye == "right")
  {
    
  }
  if (eye == "both")
  {
    
  }
  return(list(dataRec = DataRecord, mainFactorsData = mainFactorsData, mainFactorsDef = mainFactorsDef))
}
