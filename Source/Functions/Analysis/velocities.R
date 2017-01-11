getVelAccelFinDiff <- function(t, x, y)
{
  samplesCnt <- length(t)
  if (samplesCnt > 1)
  {
    dt <- abs(diff(t, 1))
    x1 <- x[-samplesCnt]; x2 <- x[-1]
    y1 <- y[-samplesCnt]; y2 <- y[-1]
    dl <- sqrt((x2-x1)^2 + (y2-y1)^2)
    vel <- dl/dt
    vel <- c(vel, tail(vel,1))
  }
  else 
  {
    dl <- NA
    dt <- NA
    vel <- NA
    accel <- NA
  }
  if (samplesCnt > 2)
  {
    dv <- diff(vel, 1)
    accel <- dv/dt
    accel <- c(accel, tail(accel,1))
  }
  else accel <- NA
  return(list(dists = dl, dts = dt, 
              vels = vel, 
              accels = accel))
}

getVelAccelSavGol <- function(t, x, y, fl)
{
  samplesCnt <- length(t)
  if (fl < 2)
  {
    dl <- NA
    dt <- NA
    vel <- NA
    accel <- NA
    warning("Filter length should be greater than 2! 
            Returning NA for velocities and accelerations")
  }
  if (samplesCnt >= fl)
  {
    if (fl %% 2 == 0) fl <- fl + 1 # turn even number to odd
    xdash <- sgolayfilt(x = x, p = 2, n = fl, m = 1, ts = 1)
    # x2dash <- sgolayfilt(x = x, p = 2, n = fl, m = 2, ts = 1)
    ydash <- sgolayfilt(x = y, p = 2, n = fl, m = 1, ts = 1)
    # y2dash <- sgolayfilt(x = y, p = 2, n = fl, m = 2, ts = 1)
    dt <- abs(t[-1] - t[-samplesCnt])
    dl <- sqrt(xdash^2+ydash^2);
    vel <- dl/c(dt, tail(dt,1))
    
    dv <- vel[-1]-vel[-length(vel)]
    accel <- dv/dt
    accel <- c(accel, tail(accel,1))
    # accel <- (vel[-1]-vel[-length(vel)])/c(dt,tail(dt,1))
  }
  else
  {
    dl <- NA
    dt <- NA
    vel <- NA
    accel <- NA
    warning("Samples number is less than filter length! 
            Returning NA for velocities and accelerations")
  }
  return(return(list(dists = dl, dts = dt, 
                     vels = vel, accels = accel)))
}

calcVel <- function(t, x, y, settings, angular)
{
  velType <- settings$velType
  fl <- settings$fl
  
  if (velType == "finDiff")
  {
    pxVelAccel <- getVelAccelFinDiff(t, x, y) 
  }
  if (velType == "analytical")
  {
    pxVelAccel <- getVelAccelSavGol(t, x, y, fl) 
  }
  
  if (angular) {
    angPositions <- calcAngPos(x = x, y = y, settings)
    if (velType == "finDiff")
    {
      angVelAccel <- getVelAccelFinDiff(t, x = angPositions$xAng, 
                                        y = angPositions$yAng) 
    }
    if (velType == "analytical")
    {
      angVelAccel <- getVelAccelSavGol(t, x = angPositions$xAng, 
                                       y = angPositions$yAng, fl) 
    }
    return(list(pxVelAccel = pxVelAccel, angVelAccel = angVelAccel))
  }
  else {
    return(list(pxVelAccel = pxVelAccel))
  }
}

calculateVelAcc <- function(ETD, velocitySettings, angular = TRUE)
{
  
  mode <- ETD$settings$mode
  velocitySettings <- append(velocitySettings, 
                             list(headDistance = ETD$settings$headDistance,
                                  screenResolution = ETD$settings$screenResolution,
                                  screenSize = ETD$settings$screenSize))
  if (mode == "left" | mode == "binocular")
  {
    velLeft <- calcVel(t = ETD$commonData$time, 
                       x = ETD$leftEyeData$porx, 
                       y = ETD$leftEyeData$pory,
                       angular = angular,
                       settings = velocitySettings) 
    ETD$leftEyeData <- modifyList(x = ETD$leftEyeData, 
                                  val = list(vel = velLeft$pxVelAccel$vels, 
                                             accel =  velLeft$pxVelAccel$accels,
                                             velAng = velLeft$angVelAccel$vels, 
                                             accelAng =  velLeft$angVelAccel$accels))
  }
  
  if (mode == "right" | mode == "binocular")
  {
    velRight <- calcVel(t = ETD$commonData$time, 
                        x = ETD$rightEyeData$porx, 
                        y = ETD$rightEyeData$pory,
                        settings = velocitySettings) 
    ETD$rightEyeData <- modifyList(x = ETD$rightEyeData, 
                                   val = list(vel = velRight$pxVelAccel$vels, 
                                              accel =  velRight$pxVelAccel$accels,
                                              velAng = velRight$angVelAccel$vels, 
                                              accelAng =  velRight$angVelAccel$accels))
  }
  return(ETD)
}
