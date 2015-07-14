## FIXATION SAMPLES PARAMS
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

getOnOffSetDuration <- function(data, settings)
{
  onset = data$time[1]
  offset = tail(data$time, 1)
  duration = offset - onset
  return(list(onset = onset, offset = offset, duration = duration))
}

getPositionsXY <- function(data, settings)
{
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$screenDist, 
                           screenDim = settings$screenDim, 
                           screenSize = settings$screenSize, 
                           refPoint = c(settings$screenDim[1]/2, settings$screenDim[2]/2))
    startPositionX <- position$xAng[1]
    endPositionX <- position$xAng[length(position$xAng)]
    startPositionY <- position$yAng[1]
    endPositionY <- position$yAng[length(position$yAng)]
    positionX <- mean(position$xAng)
    positionY <- mean(position$yAng)
  }
  else 
  {
    startPositionX <- data$porx[1]
    startPositionY <- data$pory[1]
    endPositionX <- tail(data$porx, 1)
    endPositionY <- tail(data$pory, 1)
    positionX <- mean(data$porx)
    positionY <- mean(data$pory)
  }
  return(list(startPositionX = startPositionX, startPositionY = startPositionY, 
              endPositionX = endPositionX, endPositionY = endPositionY,
              positionX = positionX, positionY = positionY))
}

getDispersionXYAndRadius <- function(data, settings)
{
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$screenDist, 
                           screenDim = settings$screenDim, 
                           screenSize = settings$screenSize, 
                           refPoint = c(settings$screenDim[1]/2, settings$screenDim[2]/2))
    dispersionX <- sd(position$xAng)
    dispersionY <- sd(position$yAng)
    radius <- mean(sqrt((positionX - position$xAng)^2 + (positionY- position$yAng)^2))
  }
  else
  {
    dispersionX <- sd(data$porx)
    dispersionY <- sd(data$pory)
    radius <- mean(sqrt((positionX - data$porx)^2 + (positionY - data$pory)^2))
  }
  return(dispersionX = dispersionX, dispersionY = dispersionY, radius = radius)
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



analyzeFixation <- function(group, data, settings)
{
  smpCnt <- nrow(data)
  if (group == 1)
  {
    prevEventGroup <- NA
  } else prevEventGroup <- group - 1
  
#   if (any(data$eventMarkers == "Gap")) 
#   {
#     valCode <- 0
#   }
#   else
#   {
#     valCode <- 1
#   }
#   onset = data$time[1]
#   offset <- tail(data$time, 1)
#   duration <- offset - onset
#   if (!settings$angular)
#   {
    # startPositionX <- data$porx[1]
    # startPositionY <- data$pory[1]
#     endPositionX <- tail(data$porx, 1)
#     endPositionY <- tail(data$pory, 1)
#     positionX <- mean(data$porx)
#     positionY <- mean(data$pory)
#     dispersionX <- sd(data$porx)
#     dispersionY <- sd(data$pory)
    # radius <- mean(sqrt((positionX - data$porx)^2 + (positionY - data$pory)^2))
#   }
#   else
#   {
#     position <- calcAngPos(data$porx, data$pory, 
#                            screenDist = settings$screenDist, 
#                            screenDim = settings$screenDim, 
#                            screenSize = settings$screenSize, 
#                            refPoint = c(settings$screenDim[1]/2, settings$screenDim[2]/2))
#     startPositionX <- position$xAng[1]
#     startPositionY <- position$yAng[1]
#     endPositionX <- position$xAng[length(position$xAng)]
#     endPositionY <- position$yAng[length(position$yAng)]
#     positionX <- mean(position$xAng)
#     positionY <- mean(position$yAng)
#     dispersionX <- sd(position$xAng)
#     dispersionY <- sd(position$yAng)
#     radius <- mean(sqrt((positionX - position$xAng)^2 + (positionY- position$yAng)^2))
  # }
#   if ("pupxsize" %in% colnames(data))
#   {
#     if ("pupysize" %in% colnames(data))
#     {
#       meanPupilSize <- (mean(data$pupxsize, na.rm = T) + mean(data$pupysize, na.rm = T))/2
#       sdPupilSize <- (sd(data$pupxsize, na.rm = T) + sd(data$pupysize, na.rm = T))/2
#     }
#     else
#     {
#       meanPupilSize <- mean(data$pupxsize, na.rm = T)
#       sdPupilSize <- sd(data$pupxsize, na.rm = T)
#     }
#   }
#   else
#   {
#     meanPupilSize <- NA
#     sdPupilSize <- NA
#   }
#   fixParams <- data.frame(eventGroup = group, smpCnt = smpCnt, valCode = valCode, prevEventGroup = prevEventGroup, 
#                     startPositionX = startPositionX, startPositionY = startPositionY,
#                     endPositionX = endPositionX, endPositionY = endPositionY,
#                     positionX = positionX, positionY = positionY,
#                     dispersionX = dispersionX, dispersionY = dispersionY, radius = radius, 
#                     onset = onset, offset = offset, duration = duration,
#                     meanPupilSize = meanPupilSize, sdPupilSize = sdPupilSize)
  return(fixParams)
}

analyzeSaccade <- function(group, data, settings)
{
  smpCnt <- nrow(data)
  if (group == 1)
  {
    prevEventGroup <- NA
  } else prevEventGroup <- group - 1
  if (any(data$eventMarkers == "Gap"))
  {
    valCode <- 0
  }
  else
  {
    valCode <- 1
  }
  
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
#   onset <- data$time[1]
#   offset <- tail(data$time, 1)
#   duration <- offset - onset
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

analyzeGlissade <- function(group, data, settings)
{
  smpCnt <- nrow(data)
  if (group == 1)
  {
    prevEventGroup <- NA
  } else prevEventGroup <- group - 1
  
  if (!settings$angular)
  {
    
  }
  else
  {
    
  }
  
  glisParams <- data.frame(eventGroup = group, smpCnt = smpCnt, prevEventGroup = prevEventGroup)
  return(glisParams)
}

analyzeSmoothPursuit <- function(group, data, settings)
{
  smpCnt <- nrow(data)
  if (group == 1)
  {
    prevEventGroup <- NA
  } else prevEventGroup <- group - 1
  
  if (!settings$angular)
  {
    
  }
  else
  {
    
  }
  
  smpurParams <- data.frame(eventGroup = group, smpCnt = smpCnt, prevEventGroup = prevEventGroup)
  return(smpurParams)
}

analyzeArtifact <- function(group, data, settings)
{
  smpCnt <- nrow(data)
  if (group == 1)
  {
    prevEventGroup <- NA
  } else prevEventGroup <- group - 1

  if (!settings$angular)
  {
    
  }
  else
  {
    
  }
 
  artParams <- data.frame(eventGroup = group, smpCnt = smpCnt, prevEventGroup = prevEventGroup)
  return(artParams)
}

analyzeGap <- function(group, data, settings)
{
  # fields: startPosition, endPosition, onset, offset, duration, pupSizeStart, pupSizeEnd
  smpCnt <- nrow(data)
  if (group == 1)
  {
    prevEventGroup <- NA
  } else prevEventGroup <- group - 1
  
  if (!settings$angular)
  {
    
  }
  else
  {
    
  }
  
  gapParams <- data.frame(eventGroup = group, smpCnt = smpCnt, prevEventGroup = prevEventGroup)
  return(gapParams)
}

standardAnalyzer <- function(data, eventMarkerNames, settings)
{
  sampleGroups <- split(data, data$eventGroups)
  fixationsParams <- list()
  saccadesParams <- list()
  glissadesParams <- list()
  smoothPursuitsParams <- list()
  gapsParams <- list()
  artifactsParams <- list()
  evmn <- eventMarkerNames
  for (gr in 1:length(sampleGroups))
  {
    if (sampleGroups[[gr]]$eventMarkers[1] == evmn$fixation)
    {
      params <- analyzeFixation(group = as.numeric(names(sampleGroups[gr])[1]), data = sampleGroups[[gr]], settings = settings)
      fixationsParams <- rbind(fixationsParams, params)
    }
    if (sampleGroups[[gr]]$eventMarkers[1] == evmn$saccade)
    {
      params <- analyzeSaccade(group = as.numeric(names(sampleGroups[gr])[1]), data = sampleGroups[[gr]], settings = settings)
      saccadesParams <- rbind(saccadesParams, params)
    }
    if (sampleGroups[[gr]]$eventMarkers[1] == evmn$glissade)
    {
      params <- analyzeGlissade(group = as.numeric(names(sampleGroups[gr])[1]), data = sampleGroups[[gr]], settings = settings)
      glissadesParams <- rbind(glissadesParams, params)
    }
    if (sampleGroups[[gr]]$eventMarkers[1] == evmn$smoothPursuit)
    {
      params <- analyzeSmoothPursuit(group = as.numeric(names(sampleGroups[gr])[1]), data = sampleGroups[[gr]], settings = settings)
      smoothPursuitsParams <- rbind(smoothPursuitsParams, params)
    }
    if (sampleGroups[[gr]]$eventMarkers[1] == evmn$gap)
    {
      params <- analyzeGap(group = as.numeric(names(sampleGroups[gr])[1]), data = sampleGroups[[gr]], settings = settings)
      gapsParams <- rbind(gapsParams, params)
    }
    if (sampleGroups[[gr]]$eventMarkers[1] == evmn$artifact)
    {
      params <- analyzeArtifact(group = as.numeric(names(sampleGroups[gr])[1]), data = sampleGroups[[gr]], settings = settings)
      artifactsParams <- rbind(artifactsParams, params)
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
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    eventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    DataRecord@analysisResults$leftEventData <- standardAnalyzer(data, eventMarkerNames, settings)
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    eventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    DataRecord@analysisResults$rightEventData <- standardAnalyzer(data, eventMarkerNames, settings)
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    dataLeft <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    dataRight <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    leftEventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    rightEventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    DataRecord@analysisResults$leftEventData <- analyzer(dataLeft, leftEventMarkerNames, settings)
    DataRecord@analysisResults$rightEventData <- analyzer(dataRight, rightEventMarkerNames, settings)
  }
  return(DataRecord)
}
