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
  estimator <- settings$subfun
  applyTo <- settings$applyTo
  if (applyTo == "EyesData")
  {
    if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
    {
      eyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
      DataRecord@statistics$left <- append(DataRecord@statistics$left, estimator(eyeData, settings))
    }
    if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
    {
      eyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
      res <- estimator(eyeData, settings)
    }
    if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
    {
      leftEyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
      rightEyeData <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
      resLeft <- estimator(leftEyeData, settings)
      resRight <- estimator(rightEyeData, settings)
      res <- resLeft
    }
    return(DataRecord)
  }

  if (applyTo == "EventData")
  {
    
  }
}