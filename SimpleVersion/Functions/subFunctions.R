# EVENTS DATA SUB FUNCTIONS

# OCULOMOTOR EVENTS' ANALYSIS FUNCTIONS DEFINITIONS SECTION
## On/Offset and duration of any event

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
  channels <- data$channelsData
  startIdx <- head(channels$rowNum, 1)
  return(list(vals = list(startIdx = as.numeric(startIdx))))
}

getEndIdx <- function(data, settings)
{
  channels <- data$channelsData
  endIdx <- tail(channels$rowNum, 1)
  return(list(vals = list(endIdx = as.numeric(endIdx))))
}

getOnset <- function(data, settings)
{
  channels <- data$channelsData
  onset <- head(channels$time, 1)
  return(list(vals = list(onset = as.numeric(onset))))
}

getOffset <- function(data, settings)
{
  channels <- data$channelsData
  offset <- tail(channels$time, 1)
  return(list(vals = list(offset = as.numeric(offset))))
}

getDuration <- function(data, settings)
{
  channels <- data$channelsData
  onset <- getOnset(data, settings)$vals$onset
  offset <- getOffset(data, settings)$vals$offset
  duration <- offset - onset
  return(list(vals = list(offset2 = offset,
                          onset2 = onset,
                          duration = as.numeric(duration))))
}

## Start/End positions of all events except gap
## settings: angular
getStartEndPositionsXY <- function(data, settings)
{
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$conditions$screenDistance, 
                           screenResolution = settings$conditions$screenResolution, 
                           screenSize = settings$conditions$screenSize)
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
                          endPositionY = as.numeric(endPositionY)),
              info = list(startPositionX = "Horisontal position of the first sample of an event", 
                          startPositionY = "Vertical position of the first sample of an event", 
                          endPositionX = "Horisontal position of the last sample of an event", 
                          endPositionY = "Vertical position of the last sample of an event")))
}

## Fixation center of mass
## settings: angular
getCenterOfMassXY <- function(data, settings)
{
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$conditions$screenDistance, 
                           screenResolution = settings$conditions$screenResolution, 
                           screenSize = settings$conditions$screenSize)
    centerX <- mean(position$xAng)
    centerY <- mean(position$yAng)
  }
  else 
  {
    centerX <- mean(data$porx)
    centerY <- mean(data$pory)
  }
  return(list(vals = list(centerX = as.numeric(centerX), 
                          centerY = as.numeric(centerY)),
              info = list(centerX = "Center of mass of event X coordinates",
                          centerY = "Center of mass of event Y coordinates")))
}

## Fixation dispersions and radius
## settings: angular
getDispersionXYAndRadius <- function(data, settings)
{
  angular <- settings$angular
  if (angular) 
  {
    position <- calcAngPos(data$porx, data$pory, 
                           screenDist = settings$conditions$screenDistance, 
                           screenResolution = settings$conditions$screenResolution, 
                           screenSize = settings$conditions$screenSize)
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
  return(list(vals = list(dispersionX = as.numeric(dispersionX), 
                          dispersionY = as.numeric(dispersionY), 
                          radius = as.numeric(radius)),
              info = list(dispersionX = "Dispersion of event X coordinates",
                          dispersionY = "Dispersion of event Y coordinates",
                          radius = "Radius of event coordinates (mean distance from event's center of mass)")))
}

## Mean and sd of pupil size (all events)
## no settings
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
  return(list(vals = list(meanPupilSize = as.numeric(meanPupilSize), 
                          sdPupilSize = as.numeric(sdPupilSize)),
              info = list(meanPupilSize = "Mean size of a pupil",
                          sdPupilSize = "Standard deviation of a size of a pupil")))
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
                          amplitude = as.numeric(amplitude)),
              info = list(amplitudeX = "Horisontal amplitude of a saccade/glissade",
                          amplitudeY = "Vertical amplitude of a saccade/glissade",
                          amplitude = "Amplitude of a saccade/glissade")))
}

## Length and curvature of a path of a part of a trajectory related to a processed event (all events except gaps)
## settings: angular
getPathLengthAndCurvature <- function(data, settings)
{
  amplitude <- getAmplitude(data, settings)$vals$amplitude
  length <- sum(sqrt((data$porx[-1]-data$porx[-length(data$porx)])^2 + (data$pory[-1]-data$pory[-length(data$pory)])^2))
  curvature <- length/amplitude
  return(list(vals = list(length = as.numeric(length), 
                          curvature = as.numeric(curvature)),
              info = list(length = "Length of a gaze path during an event",
                          curvature = "Curvature of a gaze path during an event")))
}

## Peak velocity and acceleration during event
## settings: angular, velType ("finDiff or "analytical"), screenDist, screenResolution, screenSize
## fs (sampling rate), fl (length of a Savitsky-Golay filter)
getPeakVelAcDecelAndAsymmetry <- function(data, settings)
{
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
                          asymmetry = as.numeric(asymmetry)),
              info = list(peakVelocity = "Peak velocity of gaze movement during an event",
                          peakAcceleration = "Peak acceleration of gaze movement during an event",
                          peakDeceleration = "Peak deceleration of gaze movement during an event",
                          asymmetry = "Asymmetry of gaze movement during an event (acceleration and deceleration times ratio)")))
}

## Orientation of a saccade/glissade/smooth pursuit
## no settings
getXAxisOrientation <- function(data, settings)
{
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
  return(list(vals = list(orientXAxis = as.numeric(orientXAxis)),
              info = list(orientXAxis = "Orientation of an event (saccade, glissade or smooth pursuit) with respect to X axis")))
}

# AOI EVENTS' ANALYSIS FUNCTIONS DEFINITIONS SECTION

# FRAME EVENTS' ANALYSIS FUNCTIONS DEFINITIONS SECTION

# EYES DATA SUB FUNCTIONS
trajDurationEstimator <- function(data, settings)
{
  t <- data$time
  return(list(vals = list(trajDur = as.numeric(tail(t, 1) - t[1])),
              info = list(trajDur = "Trajectory duration")))
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
    pos <- calcAngPos(x = x, y = y, settings)
    xAng <- pos$xAng
    yAng <- pos$yAng
    dxs <- xAng[-1] - xAng[-length(xAng)]
    dys <- yAng[-1] - yAng[-length(yAng)]
  } else
  {
    dxs <- x[-1] - x[-length(x)]
    dys <- y[-1] - y[-length(y)]
  }
  return(list(vals = list(trajLen = as.numeric(sum(sqrt(dxs^2 + dys^2)))),
              info = list(trajLen = "Trajectory length")))
}

# REPRESENTATIONS DATA SUB FUNCTIONS