# EVENTS DATA SUB FUNCTIONS
getSmpCnt <- function(data, settings)
{
  smpCnt <- nrow(data)
  return(list(smpCnt = smpCnt))
}
getOnset <- function(data, settings)
{
  if (nrow(data) > 0) onset <- head(data$time, 1) 
  else onset <- NA
  return(list(onset = onset))
}
getOffset <- function(data, settings)
{
  if (nrow(data) > 0) offset <- tail(data$time, 1) 
  else offset <- NA
  return(list(offset = offset))
}
getDuration <- function(data, settings)
{
  onset <- getOnset(data, settings)$onset
  offset <- getOffset(data, settings)$offset
  duration <- offset - onset
  return(list(duration = duration))
}

## Start/End positions
## settings: angular
getStartEndPositionsXY <- function(data, settings)
{
  angular <- settings$angular
  if (nrow(data) > 0)
  {
    if (angular) 
    {
      xPositions <- data$xAng
      yPositions <- data$yAng
    }
    else 
    {
      xPositions <- data$porx
      yPositions <- data$pory
    }
    startPositionX <- head(xPositions, 1)
    startPositionY <- head(yPositions, 1)
    endPositionX <- tail(xPositions, 1)
    endPositionY <- tail(yPositions, 1)
  } 
  else 
  {
    startPositionX <- NA
    startPositionY <- NA
    endPositionX <- NA
    endPositionY <- NA
  }
  return(list(startPositionX = startPositionX, 
              startPositionY = startPositionY, 
              endPositionX = endPositionX, 
              endPositionY = endPositionY))
}

## Fixation center of mass
## settings: angular
getCenterOfMassXY <- function(data, settings)
{
  angular <- settings$angular
  if (nrow(data) > 0)
  {
    if (angular) 
    {
      xPositions <- data$xAng
      yPositions <- data$yAng
    }
    else 
    {
      xPositions <- data$porx
      yPositions <- data$pory
    }
    centerX <- mean(xPositions, na.rm = T)
    centerY <- mean(yPositions, na.rm = T)
  } 
  else 
  {
    centerX <- NA
    centerY <- NA
  }
  return(list(centerX = centerX, 
              centerY = centerY))
}

## Fixation dispersions and radius
## settings: angular
getDispersionXYAndRadius <- function(data, settings)
{
  angular <- settings$angular
  if (nrow(data) > 0)
  {
    if (angular) 
    {
      xPositions <- data$xAng
      yPositions <- data$yAng
    }
    else
    {
      xPositions <- data$porx
      yPositions <- data$pory
    }
    centerX <- mean(xPositions, na.rm = T)
    centerY <- mean(yPositions, na.rm = T)
    dispersionX <- var(xPositions, na.rm = T)
    dispersionY <- var(yPositions, na.rm = T)
    radius <- mean(sqrt((centerX - xPositions)^2 + 
                          (centerY- yPositions)^2))
  } 
  else 
  {
    dispersionX <- NA
    dispersionY <- NA
    radius <- NA
  }
  
  return(list(dispersionX = dispersionX, 
              dispersionY = dispersionY, 
              radius = radius))
}

## Mean and sd of pupil size (all events)
getPupilMeanAndSD <- function(data, settings)
{
  if (settings$pupilShape == "circle")
  {
    if (nrow(data) > 0)
    {
      meanPupilSizeX <- mean(data$pupSizeX, na.rm = T)
      sdPupilSizeX <- sd(data$pupSizeX, na.rm = T)
    }
    else
    {
      meanPupilSizeX <- NA
      sdPupilSizeX <- NA
    }
    return(list(meanPupilSizeX = meanPupilSizeX, 
                sdPupilSizeX = sdPupilSizeX))
  }
  else if (settings$pupilShape == "ellipse")
  {
    if (nrow(data) != 0)
    {
      meanPupilSizeX <- mean(data$pupSizeX, na.rm = T)
      meanPupilSizeY <- mean(data$pupSizeY, na.rm = T)
      sdPupilSizeX <- sd(data$pupSizeX, na.rm = T)
      sdPupilSizeY <- sd(data$pupSizeY, na.rm = T)
    }
    else
    {
      meanPupilSizeX <- NA
      meanPupilSizeY <- NA
      sdPupilSizeX <- NA
      sdPupilSizeY <- NA
    }
    return(list(meanPupilSizeX = meanPupilSizeX, 
                sdPupilSizeX = sdPupilSizeX,
                meanPupilSizeY = meanPupilSizeY, 
                sdPupilSizeY = sdPupilSizeY))
  }
}

## Saccades & Glissades X,Y and spatial amplitudes
## settings: angular
getAmplitudes <- function(data, settings)
{
  positions <- getStartEndPositionsXY(data, settings)
  x0 <- positions$startPositionX
  x1 <- positions$endPositionX
  y0 <- positions$startPositionY
  y1 <- positions$endPositionY
  amplitudes <- amplitude(x0, y0, x1, y1)
  amplitudeX <- amplitudes$amplitudeX
  amplitudeY <- amplitudes$amplitudeY
  amplitude <- amplitudes$amplitude
  return(list(amplitudeX = amplitudeX, 
              amplitudeY = amplitudeY, 
              amplitude = amplitude))
}

## Piecewise length of a path of a part of a trajectory 
## settings: commonSettings, angular
getPathLength <- function(data, settings)
{
  if (nrow(data) > 1)
  {
    if (settings$angular) 
    {
      xPositions <- data$xAng
      yPositions <- data$yAng
    }
    else
    {
      xPositions <- data$porx
      yPositions <- data$pory  
    }
    pathLength <- pathLength(x = xPositions, y = yPositions)
  } 
  else pathLength <- NA
  return(list(pathLength = pathLength))
}

## Length and curvature of a path of a part of a trajectory 
## related to a processed event (all events except gaps)
## settings: angular
getCurvature <- function(data, settings)
{
  if (nrow(data) > 1)
  {
    amplitude <- getAmplitudes(data, settings)$amplitude
    pathLength <- getPathLength(data, settings)$pathLength
    if (amplitude == 0 | pathLength == 0)
    {
      curvature <- NA
    } else curvature <- pathLength/amplitude
  } else curvature <- NA
  return(list(curvature = curvature))
}

## Mean velocity during event
## settings: angular
getMeanVelocity <- function(data, settings)
{
  if (nrow(data) > 0 )
  {
    if (settings$angular)
    {
      vel <- data$velAng
    } else vel <- data$vel
    if (any(!is.na(vel)))
    {
      meanVelocity <- mean(vel, na.rm = T)
    } else meanVelocity <- NA
  } else meanVelocity <- NA
  return(list(meanVelocity = meanVelocity))
}

## Mean acceleration during event
## settings: angular
getMeanAcceleration <- function(data, settings)
{
  if (nrow(data) > 0 )
  {
    if (settings$angular)
    {
      accel <- data$accelAng
    } else accel <- data$accel
    accelerations <- accel[accel >= 0]
    if (any(!is.na(accelerations)))
    {
      meanAcceleration <- mean(accelerations, na.rm = T)
    } else meanAcceleration <- NA
  } else meanAcceleration <- NA
  
  return(list(meanAcceleration = meanAcceleration))
}


## Mean deceleration during event
## settings: angular
getMeanDeceleration <- function(data, settings)
{
  if (nrow(data) > 0 )
  {
    if (settings$angular) accel <- data$accelAng else 
      accel <- data$accel
    
    decelerations <- accel[accel < 0]
    
    if (any(!is.na(decelerations)))
    {
      meanDeceleration <- mean(decelerations, na.rm = T)
    } else meanDeceleration <- NA
  } else meanDeceleration <- NA
  
  return(list(meanDeceleration = meanDeceleration))
}

## Peak velocity, acceleration, deceleration during event 
## and asymmetry of accel & decel stages
## settings: angular
getPeakVelocity <- function(data, settings)
{
  if (nrow(data) > 0)
  {
    if (settings$angular)
    {
      vel <- data$velAng
    }
    else
    {
      vel <- data$vel
    }
    if (any(!is.na(vel)))
    {
      peakVelocity <- max(vel, na.rm = T)
    } else peakVelocity <- NA
  } else peakVelocity <- NA
  return(list(peakVelocity = peakVelocity))
}

getPeakAcceleration <- function(data, settings)
{
  if (nrow(data) > 0)
  {
    if (settings$angular)
    {
      accel <- data$accelAng
    }
    else
    {
      accel <- data$accel
    }
    if (any(!is.na(accel) & accel > 0))
    {
      peakAcceleration <- max(accel[accel > 0], na.rm = T)
    } else peakAcceleration <- NA
  } else peakAcceleration <- NA
  return(list(peakAcceleration = peakAcceleration))
}

getPeakDeceleration <- function(data, settings)
{
  if (nrow(data) > 0)
  {
    if (settings$angular)
    {
      accel <- data$accelAng
    }
    else
    {
      accel <- data$accel
    }
    if (any(!is.na(accel) & accel < 0))
    {
      peakDeceleration <- min(accel[accel < 0], na.rm = T)
    } else peakDeceleration <- NA
  } else peakDeceleration <- NA
  return(list(peakDeceleration = peakDeceleration))
}

getAsymmetry <- function(data, settings)
{
  if (nrow(data) > 0)
  {
    if (settings$angular)
    {
      accel <- data$accelAng
    }
    else
    {
      accel <- data$accel
    }
    accel <- accel[!is.na(accel)]
    time <- data$time[!is.na(accel)]
    flags <- ifelse(accel > 0, 1, ifelse(accel == 0, 0, -1))
    df <- data.frame(accel = accel, time = time, flags = flags, 
                     group = markersGroups(flags))
    df_accels <- df[df$flags == 1,]
    
    if (nrow(df_accels) > 1)
    {
      splitted_df <- split(df_accels, f = df_accels$group)
      accel_total_dur <- sum(sapply(splitted_df, FUN = function(x)
      {
        dur <- tail(x$time,1)-head(x$time,1)
        return(dur)
      }))
    } else accel_total_dur <- NA
    
    df_decels <- df[df$flags == -1,]
    if (nrow(df_decels) > 1)
    {
      splitted_df <- split(df_decels, f = df_decels$group)
      decel_total_dur <- sum(sapply(splitted_df, FUN = function(x)
      {
        dur <- tail(x$time,1)-head(x$time,1)
        return(dur)
      }))
    } else decel_total_dur <- NA
    
    if (!is.na(accel_total_dur) & !is.na(decel_total_dur))
    {
      asymmetry <- accel_total_dur/decel_total_dur
    } else asymmetry <- NA
  }
  else asymmetry <- NA
  return(list(asymmetry = asymmetry))
}

## Orientation of a saccade/glissade/smooth pursuit
## no settings
getXAxisOrientation <- function(data, settings)
{
  if (nrow(data) < 2)
  {
    orientXAxis <- NA
  }
  else
  {
    x0 <- head(data$porx, 1)
    x1 <- tail(data$porx, 1)
    y0 <- head(data$pory, 1)
    y1 <- tail(data$pory, 1)
    orientXAxis <- orientationAngleXAxis(x0 = x0, y0 = y0, 
                                         x1 = x1, y1 = y1)
  }
  return(list(orientXAxis = orientXAxis))
}

## Area covered by gaze points inside event
## settings: angular
getGazePointsArea <- function(data, settings) 
{
  # data <- data$channelsData
  if (nrow(data) > 0)
  {
    angular <- settings$angular
    if (angular)
    {
      xPositions <- data$xAng
      yPositions <- data$yAng
    }
    else
    {
      xPositions <- data$porx
      yPositions <- data$pory
    }
    pointsArea <- pointsArea(x = xPositions, y = yPositions)
  } else pointsArea <- NA
  return(list(gazePointsArea = pointsArea))
}