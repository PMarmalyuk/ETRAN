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
  # res3left <- res2left[sapply(res2left, FUN = function(x) {ifelse(is.null(x[[1]]),F,T)})]
  # res3right <- res2right[sapply(res2right, FUN = function(x) {ifelse(is.null(x[[1]]),F,T)})]
  # res4left <- unlist(res3left, recursive = F)
  # res4right <- unlist(res3right, recursive = F)
  DataRecord@statistics$left <- modifyList(x = DataRecord@statistics$left, val = res2left, keep.null = F)
  DataRecord@statistics$right <- modifyList(x = DataRecord@statistics$right, val = res2right, keep.null = F)
  return(DataRecord)
}