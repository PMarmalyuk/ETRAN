trajDurationEstimator <- function(data, settings)
{
  t <- data$time
  return(list(duration = tail(t, 1) - t[1]))
}
trajLengthEstimator <- function(data, settings)
{
  x <- data$porx
  y <- data$pory
  angular <- settings$angular
  if (angular)
  {
    screenDist <- settings$screenDist
    screenDim <- settings$screenDim
    screenSize <- settings$screenSize
    pos <- calcAngPos(x = x, y = y, screenDist = screenDist, screenDim = screenDim, screenSize = screenSize, refPoint = c(screenDim[1]/2, screenDim[2]/2))
    xAng <- pos$xAng
    yAng <- pos$yAng
    dxs <- xAng[-1] - xAng[-length(xAng)]
    dys <- yAng[-1] - yAng[-length(yAng)]
  } else
  {
    dxs <- x[-1] - x[-length(x)]
    dys <- y[-1] - y[-length(y)]
  }
  return(list(length = sum(sqrt(dxs^2 + dys^2))))
}

coreEstimator <- function(DataRecord, settings)
{
  subFunctions <- settings$subFunctions
  res <- lapply(subFunctions, FUN = function(x) 
  {
    fun <- x@fun
    settings <- x@settings
    applyTo <- x@applyTo
    if (applyTo == "EyesData")
    {
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
      {
        eyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
        resLeft <- fun(data = eyeData, settings = settings)
        resRight <- list(par = NULL)
      }
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
      {
        eyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
        resLeft <- list(par = NULL)
        resRight <- fun(data = eyeData, settings = settings)
      }
      if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
      {
        leftEyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
        rightEyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
        resLeft <- fun(data = leftEyeData, settings = settings)
        resRight <- fun(data = rightEyeData, settings = settings)
      }
    }
    return(list(resLeft = resLeft, resRight = resRight))
  })
  res2left <- lapply(res, FUN = function(x) {return(x$resLeft)})
  res2right <- lapply(res, FUN = function(x) {return(x$resRight)})
  res3left <- res2left[sapply(res2left, FUN = function(x) {ifelse(is.null(x[[1]]),F,T)})]
  res3right <- res2right[sapply(res2right, FUN = function(x) {ifelse(is.null(x[[1]]),F,T)})]
  DataRecord@statistics$left <- unlist(res3left, recursive = F)
  DataRecord@statistics$right <- unlist(res3right, recursive = F)
  return(DataRecord)
}