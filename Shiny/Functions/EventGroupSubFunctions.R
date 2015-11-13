## Validity code of all events except gap
## no settings

## CHANGE LOGICAL EXPRESSION TO THAT IS USING FILTER MARKERS
getValCode <- function(data, settings)
{
  evmn <- settings$evmn
  if (any(data$filterMarkers == evmn$gap)) 
  {
    valCode <- "Invalid"
  }
  else
  {
    valCode <- "Valid"
  }
  return(list(valCode = factor(valCode, levels = c("Invalid", "Valid"))))
}

## On/Offset and duration of any event
## no settings
getOnOffSetDuration <- function(data, settings)
{
  onset = data$time[1]
  offset = tail(data$time, 1)
  duration = offset - onset
  return(list(onset = as.numeric(onset), offset = as.numeric(offset), duration = as.numeric(duration)))
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
  return(list(startPositionX = as.numeric(startPositionX), startPositionY = as.numeric(startPositionY), 
              endPositionX = as.numeric(endPositionX), endPositionY = as.numeric(endPositionY)))
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
  return(list(centerX = as.numeric(centerX), centerY = as.numeric(centerY)))
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
  return(list(dispersionX = as.numeric(dispersionX), dispersionY = as.numeric(dispersionY), radius = as.numeric(radius)))
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
  return(list(meanPupilSize = as.numeric(meanPupilSize), sdPupilSize = as.numeric(sdPupilSize)))
}

## Saccades & Glissades amplitude
## settings: angular
getAmplitude <- function(data, settings)
{
  positions <- getStartEndPositionsXY(data, settings)
  posX0 <- positions$startPositionX
  posX1 <- positions$endPositionX
  posY0 <- positions$startPositionY
  posY1 <- positions$endPositionY
  amplitudeX <- abs(posX1-posX0)
  amplitudeY <- abs(posY1-posY0)
  amplitude <- sqrt(amplitudeX^2 + amplitudeY^2)
  return(list(amplitudeX = as.numeric(amplitudeX), amplitudeY = as.numeric(amplitudeY), amplitude = as.numeric(amplitude)))
}

## Length and curvature of a path of a part of a trajectory related to a processed event (all events except gaps)
## settings: angular
getPathLengthAndCurvature <- function(data, settings)
{
  amplitude <- getAmplitude(data, settings)$amplitude
  length <- sum(sqrt((data$porx[-1]-data$porx[-length(data$porx)])^2 + (data$pory[-1]-data$pory[-length(data$pory)])^2))
  curvature <- length/amplitude
  return(list(length = as.numeric(length), curvature = as.numeric(curvature)))
}

## Peak velocity and acceleration during event
## settings: angular, velType ("finDiff or "analytical"), screenDist, screenResolution, screenSize
## fs (sampling rate), fl (lenght of a Savitsky-Golay filter)
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
  return(list(peakVelocity = as.numeric(peakVelocity), 
              peakAcceleration = as.numeric(peakAcceleration), 
              peakDeceleration = as.numeric(peakDeceleration),
              asymmetry = as.numeric(asymmetry)))
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
  return(list(orientXAxis = as.numeric(orientXAxis)))
}
