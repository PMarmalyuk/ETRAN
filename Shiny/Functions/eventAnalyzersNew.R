## ALL EVENTS PARAMS
getValCode <- function(data, settings)
{
  if (any(data$eventMarkers == "Gap")) 
  {
    valCode <- 0
  }
  else
  {
    valCode <- 1
  }
  return(list(valCode = valCode))
}

## ALL EVENTS PARAMS EXCEPT GAPS AND ARTIFACTS
getOnOffSetDuration <- function(data, settings)
{
  onset = data$time[1]
  offset = tail(data$time, 1)
  duration = offset - onset
  return(list(onset = onset, offset = offset, duration = duration))
}

getStartEndPositionsXY <- function(data, settings)
{
  angular <- settings$angular
  
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$conditions$screenDistance, 
                           screenDim = settings$conditions$screenDim, 
                           screenSize = settings$conditions$screenSize, 
                           refPoint = c(settings$conditions$screenDim[1]/2, settings$conditions$screenDim[2]/2))
    startPositionX <- position$xAng[1]
    endPositionX <- position$xAng[length(position$xAng)]
    startPositionY <- position$yAng[1]
    endPositionY <- position$yAng[length(position$yAng)]
  }
  else 
  {
    startPositionX <- data$porx[1]
    startPositionY <- data$pory[1]
    endPositionX <- tail(data$porx, 1)
    endPositionY <- tail(data$pory, 1)
  }
  return(list(startPositionX = startPositionX, startPositionY = startPositionY, 
              endPositionX = endPositionX, endPositionY = endPositionY))
}

getCenterOfMassXY <- function(data, settings)
{
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$conditions$screenDistance, 
                           screenDim = settings$conditions$screenDim, 
                           screenSize = settings$conditions$screenSize, 
                           refPoint = c(settings$conditions$screenDim[1]/2, settings$conditions$screenDim[2]/2))
    positionX <- mean(position$xAng)
    positionY <- mean(position$yAng)
  }
  else 
  {
    positionX <- mean(data$porx)
    positionY <- mean(data$pory)
  }
  return(list(positionX = positionX, positionY = positionY))
}

getDispersionXYAndRadius <- function(data, settings)
{
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$conditions$screenDistance, 
                           screenDim = settings$conditions$screenDim, 
                           screenSize = settings$conditions$screenSize, 
                           refPoint = c(settings$conditions$screenDim[1]/2, settings$conditions$screenDim[2]/2))
    positionX <- mean(position$xAng)
    positionY <- mean(position$yAng)
    dispersionX <- sd(position$xAng)
    dispersionY <- sd(position$yAng)
    radius <- mean(sqrt((positionX - position$xAng)^2 + (positionY- position$yAng)^2))
  }
  else
  {
    positionX <- mean(data$porx)
    positionY <- mean(data$pory)
    dispersionX <- sd(data$porx)
    dispersionY <- sd(data$pory)
    radius <- mean(sqrt((positionX - data$porx)^2 + (positionY - data$pory)^2))
  }
  return(list(dispersionX = dispersionX, dispersionY = dispersionY, radius = radius))
}

getPupilMeanAndSD <- function(data, settings)
{
  if ("pupxsize" %in% colnames(data))
  {
    if ("pupysize" %in% colnames(data))
    {
      meanPupilSize <- (mean(data$pupxsize, na.rm = T) + mean(data$pupysize, na.rm = T))/2
      sdPupilSize <- (sd(data$pupxsize, na.rm = T) + sd(data$pupysize, na.rm = T))/2
    }
    else
    {
      meanPupilSize <- mean(data$pupxsize, na.rm = T)
      sdPupilSize <- sd(data$pupxsize, na.rm = T)
    }
  }
  else
  {
    meanPupilSize <- NA
    sdPupilSize <- NA
  }
  return(list(meanPupilSize = meanPupilSize, sdPupilSize = sdPupilSize))
}

analyzeSaccade <- function(group, data, settings)
{
  if (!settings$angular)
  {
    startPositionX <- data$porx[1]
    startPositionY <- data$pory[1]
    endPositionX <- tail(data$porx, 1)
    endPositionY <- tail(data$pory, 1)
    amplitudeX <- abs(endPositionX - startPositionX)
    amplitudeY <- abs(endPositionY - startPositionY)
    amplitude <- sqrt(amplitudeX^2 + amplitudeY^2)
    length <- sum(sqrt((data$porx[-1]-data$porx[-length(data$porx)])^2 + (data$pory[-1]-data$pory[-length(data$pory)])^2))
    curvature <- length/amplitude
    vels <- calcPxVel(t = data$time, x = data$porx, y = data$pory)$vels
  }
  else
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$screenDist, 
                           screenDim = settings$screenDim, 
                           screenSize = settings$screenSize, 
                           refPoint = c(settings$screenDim[1]/2, settings$screenDim[2]/2))
    startPositionX <- position$xAng[1]
    startPositionY <- position$yAng[1]
    endPositionX <- position$xAng[length(position$xAng)]
    endPositionY <- position$yAng[length(position$yAng)]
    amplitudeX <- abs(endPositionX - startPositionX)
    amplitudeY <- abs(endPositionY - startPositionY)
    amplitude <- sqrt(amplitudeX^2 + amplitudeY^2)
    length <- sum(sqrt((position$xAng[-1]-position$xAng[-length(position$xAng)])^2 + (position$yAng[-1]-position$xAng[-length(position$yAng)])^2))
    curvature <- length/amplitude
    vels <- calcAngVel(t = data$time, x = data$porx, y = data$pory, 
                       screenDist = settings$screenDist, 
                       screenDim = settings$screenDim, 
                       screenSize = settings$screenSize)$vels
  }
  onset <- data$time[1]
  offset <- tail(data$time, 1)
  duration <- offset - onset
  peakVelocity <- max(vels)
  dts <- data$time[-length(data$time)]-data$time[-1]
  dts2 <- dts[-1] + dts[-length(dts)]
  dvs <- vels[-1] - vels[-length(vels)]
  accels <- dvs/dts2
  peakAcceleration <- max(abs(accels))
  asymmetry <- sum(dts[which(accels > 0)])/sum(dts[which(accels < 0)])
  dx <- tail(data$porx, 1) - data$porx[1]
  dy <- data$pory[1] - tail(data$pory, 1)
  orientXAxis <- atan2(y = dy, x = dx) * (180/pi)
  sacParams <- data.frame(eventGroup = group, valCode = valCode, smpCnt = smpCnt, prevEventGroup = prevEventGroup,
                          startPositionX = startPositionX, startPositionY = startPositionY, endPositionX = endPositionX, endPositionY = endPositionY,
                          onset = onset, offset = offset, duration = duration, 
                          amplitudeX = amplitudeX, amplitudeY = amplitudeY, amplitude = amplitude,
                          peakVelocity = peakVelocity, peakAcceleration = peakAcceleration, asymmetry = asymmetry, length = length,
                          curvature = curvature, orientXAxis = orientXAxis)
}


standardAnalyzer <- function(data, eventMarkerNames, settings, conditions)
{
  data <- data[-which(data$eventGroups == 0),]
  sampleGroups <- split(data, data$eventGroups)
  fixationsParams <- list()
  saccadesParams <- list()
  glissadesParams <- list()
  smoothPursuitsParams <- list()
  gapsParams <- list()
  artifactsParams <- list()
  subFunctions <- settings$subFunctions
  evmn <- eventMarkerNames
  for (gr in 1:length(sampleGroups))
  {
    group <- as.numeric(names(sampleGroups[gr])[1])
    if (group == 1)
    {
      prevEventGroup <- NA
    } else prevEventGroup <- group - 1
    ev <- sampleGroups[[gr]]$eventMarkers[1]
    res <- lapply(subFunctions, FUN = function(x) 
    {
      fun <- x@fun
      settings <- append(x@settings, conditions)
      event <- x@event
      if (! (ev %in% event)) {return(list(par = NULL))}
      else {return(fun(data = sampleGroups[[gr]], settings = settings))}
    })
    res2 <- unlist(res, recursive = F)
    res2 <- append(list(group = group, prevEventGroup = prevEventGroup), res2)
    res2 <- data.frame(res2[sapply(res2, FUN = function(x) {ifelse(is.null(x),F,T)})])
    if (ev == evmn$fixation)
    {
      if (length(res2) != 0) fixationsParams <- rbind(fixationsParams, res2)
    }
    if (ev == evmn$saccade)
    {
      if (length(res2) != 0) saccadesParams <- rbind(saccadesParams, res2)
    }
    if (ev == evmn$glissade)
    {
      if (length(res2) != 0) glissadesParams <- rbind(glissadesParams, res2)
    }
    if (ev == evmn$smoothPursuit)
    {
      if (length(res2) != 0) smoothPursuitsParams <- rbind(smoothPursuitsParams, res2)
    }
    if (ev == evmn$gap)
    {
      if (length(res2) != 0) gapsParams <- rbind(gapsParams, res2)
    }
    if (ev == evmn$artifact)
    {
      if (length(res2) != 0) artifactsParams <- rbind(artifactsParams, res2)
    }
  }
  fx <- new(Class = "FixationsData", fixations = as.data.frame(fixationsParams, stringsAsFactors = F))
  sc <- new(Class = "SaccadesData", saccades = as.data.frame(saccadesParams, stringsAsFactors = F))
  gl <- new(Class = "GlissadesData", glissades = as.data.frame(glissadesParams, stringsAsFactors = F))
  sp <- new(Class = "SmoothPursuitsData", smoothPursuits = as.data.frame(smoothPursuitsParams, stringsAsFactors = F))
  gp <- new(Class = "GapsData", gaps = as.data.frame(gapsParams, stringsAsFactors = F))
  ar <- new(Class = "ArtifcatsData", artifacts = as.data.frame(artifactsParams, stringsAsFactors = F))
  events <- new(Class = "EventData", 
                fixations = fx, saccades = sc, 
                glissades = gl, smoothPursuits = sp, 
                gaps = gp, artifacts = ar, additionalEvents = list())
  return(events)
}

## CORE ANALYZER ##
coreAnalyzer <- function(DataRecord, settings)
{
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  if (conditions$eye == "left")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    eventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    DataRecord@analysisResults$leftEventData <- standardAnalyzer(data, eventMarkerNames, settings, conditions)
  }
  if (conditions$eye == "right")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    eventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    DataRecord@analysisResults$rightEventData <- standardAnalyzer(data, eventMarkerNames, settings, conditions)
  }
  if (conditions$eye == "both")
  {
    dataLeft <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    dataRight <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    leftEventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    rightEventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    DataRecord@analysisResults$leftEventData <- standardAnalyzer(dataLeft, leftEventMarkerNames, settings, conditions)
    DataRecord@analysisResults$rightEventData <- standardAnalyzer(dataRight, rightEventMarkerNames, settings, conditions)
  }
  return(DataRecord)
}
