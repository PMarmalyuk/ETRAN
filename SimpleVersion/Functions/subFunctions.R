# EVENTS DATA SUB FUNCTIONS

# OCULOMOTOR EVENTS' ANALYSIS FUNCTIONS DEFINITIONS SECTION

### getRepres - TEST FUNCTION ###
getRepres <- function(data, settings)
{
  repres <- list(AOISeq = c("AOI1", "AOI2", "AOI3"),
                 times = c(100, 200, 150))
  return(list(vals = list(repres1 = repres),
              src = list(eventClass = settings$eventClass,
                         detector = settings$eventClass,
                         eventType = settings$eventType)))
}
### END TEST FUNCTION ###

getStartIdx <- function(data, settings)
{
  startIdx <- data$start
  return(list(vals = list(startIdx = as.numeric(startIdx))))
}
getEndIdx <- function(data, settings)
{
  endIdx <- data$end
  return(list(vals = list(endIdx = as.numeric(endIdx))))
}
getSmpCnt <- function(data, settings)
{
  startIdx <- getStartIdx(data, settings)$vals$startIdx
  endIdx <- getEndIdx(data, settings)$vals$endIdx
  smpCnt <- endIdx - startIdx + 1
  return(list(vals = list(smpCnt = as.numeric(smpCnt))))
}

getOnset <- function(data, settings)
{
  data <- data$channelsData
  onset <- head(data$time, 1)
  return(list(vals = list(onset = as.numeric(onset))))
}
getOffset <- function(data, settings)
{
  data <- data$channelsData
  offset <- tail(data$time, 1)
  return(list(vals = list(offset = as.numeric(offset))))
}
getDuration <- function(data, settings)
{
  onset <- getOnset(data, settings)$vals$onset
  offset <- getOffset(data, settings)$vals$offset
  duration <- offset - onset
  return(list(vals = list(duration = as.numeric(duration))))
}

## Start/End positions
## settings: angular
getStartEndPositionsXY <- function(data, settings)
{
  data <- data$channelsData
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, settings)
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
  return(list(vals = list(startPositionX = as.numeric(startPositionX), 
                          startPositionY = as.numeric(startPositionY), 
                          endPositionX = as.numeric(endPositionX), 
                          endPositionY = as.numeric(endPositionY))))
}

## Fixation center of mass
## settings: angular
getCenterOfMassXY <- function(data, settings)
{
  data <- data$channelsData
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, settings)
    centerX <- mean(position$xAng)
    centerY <- mean(position$yAng)
  }
  else 
  {
    centerX <- mean(data$porx)
    centerY <- mean(data$pory)
  }
  return(list(vals = list(centerX = as.numeric(centerX), 
                          centerY = as.numeric(centerY))))
}

## Fixation dispersions and radius
## settings: angular
getDispersionXYAndRadius <- function(data, settings)
{
  data <- data$channelsData
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, settings)
    positionX <- mean(position$xAng)
    positionY <- mean(position$yAng)
    dispersionX <- var(position$xAng)
    dispersionY <- var(position$yAng)
    radius <- mean(sqrt((positionX - position$xAng)^2 + (positionY- position$yAng)^2))
  }
  else
  {
    positionX <- mean(data$porx)
    positionY <- mean(data$pory)
    dispersionX <- var(data$porx)
    dispersionY <- var(data$pory)
    radius <- mean(sqrt((positionX - data$porx)^2 + (positionY - data$pory)^2))
  }
  return(list(vals = list(dispersionX = as.numeric(dispersionX), 
                          dispersionY = as.numeric(dispersionY), 
                          radius = as.numeric(radius))))
}

## Mean and sd of pupil size (all events)
## no settings
getPupilMeanAndSD <- function(data, settings)
{
  data <- data$channelsData
  if ("pupx" %in% colnames(data))
  {
    if ("pupy" %in% colnames(data))
    {
      meanPupilSizeX <- mean(data$pupx, na.rm = T)
      meanPupilSizeY <- mean(data$pupy, na.rm = T)
      sdPupilSizeX <- sd(data$pupx, na.rm = T)
      sdPupilSizeY <- sd(data$pupy, na.rm = T)
      return(list(vals = list(meanPupilSizeX = as.numeric(meanPupilSizeX), 
                              sdPupilSizeX = as.numeric(sdPupilSizeX),
                              meanPupilSizeY = as.numeric(meanPupilSizeY), 
                              sdPupilSizeY = as.numeric(sdPupilSizeY))))
    }
    else
    {
      meanPupilSizeX <- mean(data$pupx, na.rm = T)
      sdPupilSizeX <- sd(data$pupx, na.rm = T)
      return(list(vals = list(meanPupilSizeX = as.numeric(meanPupilSizeX), 
                              sdPupilSizeX = as.numeric(sdPupilSizeX))))
    }
  }
  else
  {
    meanPupilSizeX <- NA
    sdPupilSizeX <- NA
    return(list(vals = list(meanPupilSizeX = as.numeric(meanPupilSizeX), 
                            sdPupilSizeX = as.numeric(sdPupilSizeX))))
  }
}

## Saccades & Glissades amplitude
## settings: angular
getAmplitude <- function(data, settings)
{
  positions <- getStartEndPositionsXY(data, settings)$vals
  posX0 <- positions$startPositionX
  posX1 <- positions$endPositionX
  posY0 <- positions$startPositionY
  posY1 <- positions$endPositionY
  amplitudeX <- abs(posX1-posX0)
  amplitudeY <- abs(posY1-posY0)
  amplitude <- sqrt(amplitudeX^2 + amplitudeY^2)
  return(list(vals = list(amplitudeX = as.numeric(amplitudeX), 
                          amplitudeY = as.numeric(amplitudeY), 
                          amplitude = as.numeric(amplitude))))
}

## Length and curvature of a path of a part of a trajectory related to a processed event (all events except gaps)
## settings: angular
getPathLengthAndCurvature <- function(data, settings)
{
  amplitude <- getAmplitude(data, settings)$vals$amplitude
  data <- data$channelsData
  length <- sum(sqrt((data$porx[-1]-data$porx[-length(data$porx)])^2 + (data$pory[-1]-data$pory[-length(data$pory)])^2))
  curvature <- length/amplitude
  return(list(vals = list(length = as.numeric(length), 
                          curvature = as.numeric(curvature))))
}

## Mean velocity, acceleration and deceleration during event
## settings: angular, velType ("finDiff or "analytical"), fl (length of a Savitsky-Golay filter expressed by samples count)
getMeanVelAcDecel <- function(data, settings)
{
  data <- data$channelsData
  meanVelocity <- as.numeric(NA)
  meanAcceleration <- as.numeric(NA)
  meanDeceleration <- as.numeric(NA)
  smpCnt <- nrow(data)
  if (smpCnt > 1)
  {
    vel <- calcVel(t = data$time, x = data$porx, y = data$pory, settings)
    vels <- vel$vels
    if (any(!is.na(vels)))
    {
      meanVelocity <- mean(vels, na.rm = T)
    }
    if (smpCnt > 2)
    {
      accels <- vel$accels[vel$accels >= 0 & !is.na(vel$accels)]
      accel_dts <- vel$dts[vel$accels >= 0 & !is.na(vel$accels)]
      decels <- vel$accels[vel$accels < 0 & !is.na(vel$accels)]
      decel_dts <- vel$dts[vel$accels < 0 & !is.na(vel$accels)]
      if (length(accels) > 0)
      {
        meanAcceleration <- mean(accels)
      }
      if (length(decels) > 0)
      {
        meanDeceleration <- mean(decels)
      }
    }
  }
  return(list(vals = list(meanVelocity = as.numeric(meanVelocity), 
                          meanAcceleration = as.numeric(meanAcceleration), 
                          meanDeceleration = as.numeric(meanDeceleration))))
}

## Peak velocity, acceleration, deceleration during event and asymmetry of accel & decel stages
## settings: angular, velType ("finDiff or "analytical"), fl (length of a Savitsky-Golay filter expressed by samples count)
getPeakVelAcDecelAndAsymmetry <- function(data, settings)
{
  data <- data$channelsData
  peakVelocity <- as.numeric(NA)
  peakAcceleration <- as.numeric(NA)
  peakDeceleration <- as.numeric(NA)
  asymmetry <- as.numeric(NA)
  smpCnt <- nrow(data)
  if (smpCnt > 1)
  {
    vel <- calcVel(t = data$time, x = data$porx, y = data$pory, settings)
    vels <- vel$vels
    if (any(!is.na(vels)))
    {
      # if (eventGroup == 3) {print(vels)}
      peakVelocity <- max(vels, na.rm = T)
    }
    if (smpCnt > 2)
    {
      accels <- vel$accels[vel$accels >= 0 & !is.na(vel$accels)]
      accel_dts <- vel$dts[vel$accels >= 0 & !is.na(vel$accels)]
      decels <- vel$accels[vel$accels < 0 & !is.na(vel$accels)]
      decel_dts <- vel$dts[vel$accels < 0 & !is.na(vel$accels)]
      if (length(accels) > 0)
      {
        peakAcceleration <- max(accels)
      }
      if (length(decels) > 0)
      {
        peakDeceleration <- min(decels)
      }
      if (length(accels) > 0 & length(decels) > 0)
      {
        asymmetry <- sum(accel_dts)/sum(decel_dts)
      }
    }
  }
  return(list(vals = list(peakVelocity = as.numeric(peakVelocity), 
                          peakAcceleration = as.numeric(peakAcceleration), 
                          peakDeceleration = as.numeric(peakDeceleration),
                          asymmetry = as.numeric(asymmetry))))
}

## Orientation of a saccade/glissade/smooth pursuit
## no settings
getXAxisOrientation <- function(data, settings)
{
  data <- data$channelsData
  samplesCnt <- nrow(data)
  if (samplesCnt < 2)
  {
    orientXAxis <- NA
  }
  else
  {
    dx <- tail(data$porx, 1) - data$porx[1]
    dy <- data$pory[1] - tail(data$pory, 1)
    orientXAxis <- atan2(y = dy, x = dx) * (180/pi)
    if (orientXAxis < 0) {orientXAxis <- 360 + orientXAxis}
  }
  return(list(vals = list(orientXAxis = as.numeric(orientXAxis))))
}

## Area covered by gaze points inside event
## settings: angular

getGazePointsArea <- function(data, settings) {
  data <- data$channelsData
  x <- data$porx; y <- data$pory
  angular <- settings$angular
  if (angular)
  {
    angPos <- calcAngPos(x = x, y = y, settings = setings)
    x <- angPos$xAng
    y <- angPos$yAng
  }
  area <- pointsArea(x = x, y = y)
  return(list(vals = list(gazePointsArea = as.numeric(area))))
}



## Area covered by fixation points inside event
## settings: angular
getFixationPointsArea <- function(data, settings)
{
#   
#   repres <- data$representations
#   positionsX <- selectRepresentations(repres, varNames = c("posX"), query = createQuery(eye = "left",
#                                                                                   eventsClass = "OculomotorEvents",
#                                                                                   detector = "IDT.Detection2",
#                                                                                   eventType = "Fixation"))
#   positionsY <- selectRepresentations(repres, varNames = c("posX"), query = createQuery(eye = "left",
#                                                                                         eventsClass = "OculomotorEvents",
#                                                                                         detector = "IDT.Detection2",
#                                                                                         eventType = "Fixation"))
#   
#   pointsArea(x = positionsX, y = )
  
  data <- data$channelsData
  angular <- settings$angular
  fixationsPointsArea <- NA
  return(list(vals = list(fixationsPointsArea = as.numeric(fixationsPointsArea))))
}

# AOI EVENTS' ANALYSIS FUNCTIONS DEFINITIONS SECTION


# REPRESENTATIONS DATA SUB FUNCTIONS