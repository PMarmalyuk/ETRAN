createLoader <- function(name, fun, settings)
{
  loader <- new(Class = "Loader", name = name, fun = fun, settings = settings)
  return(loader)
}

createParser <- function(name, fun, settings)
{
  parser <- new(Class = "Parser", name = name, fun = fun, settings = settings)
  return(parser)
}

createSmoother <- function(name, fun, settings)
{
  smoother <- new(Class = "Smoother", name = name, fun = fun, settings = settings)
  return(smoother)
}

createDetector <- function(name, fun, settings)
{
  detector <- new(Class = "EventDetector", name = name, fun = fun, settings = settings)
  return(detector)
}

createAnalyzer <- function(name, fun, settings)
{
  analyzer <- new(Class = "EventAnalyzer", name = name, fun = fun, settings = settings)
  return(analyzer)
}

createEstimator <- function(name, fun, applyTo, settings)
{
  estimator <- new(Class = "ParamEstimator", name = name, fun = fun, settings = settings)
  return(estimator)
}

# Calculates angular height and width in degrees of the eye position 
# relative to the screen left corner (default) or the given reference point
calcAngPos <- function(x, y, screenDist, screenResolution, screenSize, refPoint = c(0,0))
{
  d <- screenDist
  w <- screenSize[1]; h <- screenSize[2]
  wPx <- screenResolution[1]; hPx <- screenResolution[2]
  xAng <- (180/pi)*atan(((x-refPoint[1])/(d*wPx/w)))
  yAng <- (180/pi)*atan(((y-refPoint[2])/(d*hPx/h)))
  return(list(xAng = xAng, yAng = yAng))
}

# TO DO: 
# Calculates horiz. and vert. momentum velocities (px or degrees per timeUnit) of the eye movements given by vectors <t, x, y>
# calcXYShiftsVel <- function(t, x, y)
# {
#   samplesCnt <- length(t)
#   dts <- t[-1] - t[-samplesCnt]
#   dxs <- x[-1] - x[-samplesCnt]
#   dys <- y[-1] - y[-samplesCnt]
#   xVels <- dxs/dts
#   yVels <- dys/dts
#   return(list(xVels = xVels, yVels = yVels))
# }

calcVel <- function(t, x, y, settings)
{
  velType <- settings$velType
  angular <- settings$angular
  screenDist <- settings$screenDist
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  samplesCnt <- length(t)
  dl = NA
  dt = NA
  vel = NA
  accel = NA
  if (samplesCnt >= 2)
  {
    dt <- abs(t[-1] - t[-samplesCnt])
    if (velType == "finDiff")
    {
      if (angular)
      {
        angPositions <- calcAngPos(x = x, y = y, screenDist, screenResolution, screenSize)
        x <- angPositions$xAng
        y <- angPositions$yAng
      }
      x1 <- x[-samplesCnt]; x2 <- x[-1]
      y1 <- y[-samplesCnt]; y2 <- y[-1]
      dl <- sqrt((x2-x1)^2 + (y2-y1)^2)
      vel <- dl/dt
      if (samplesCnt >= 3)
      {
        accel <- (vel[-1]-vel[-length(vel)])/dt[-length(dt)]
      }
    }
    if (velType == "analytical")
    {
      fs <- settings$sampleRate # sampling frequency in Hz
      flt <- settings$fl # filter length in msec
      if (samplesCnt >= flt)
      {
        if (angular)
        {
          angPositions <- calcAngPos(x = x, y = y, screenDist, screenResolution, screenSize)
          x <- angPositions$xAng
          y <- angPositions$yAng
        }
        flt <- flt/1000 # change units of filter length to seconds
        fl <- ceiling(flt*fs) # expressing filter length in samples number
        if (fl %% 2 == 0) fl <- fl + 1 # turn even number to odd number by incrementing it by 1
        xdash <- sgolayfilt(x = x, p = 2, n = fl, m = 1, ts = 1)
        x2dash <- sgolayfilt(x = x, p = 2, n = fl, m = 2, ts = 1)
        ydash <- sgolayfilt(x = y, p = 2, n = fl, m = 1, ts = 1)
        y2dash <- sgolayfilt(x = y, p = 2, n = fl, m = 2, ts = 1)
        dt <- abs(t[-1] - t[-samplesCnt])
        dldash <- sqrt(xdash^2+ydash^2); dl <- dldash
        dl2dash <- sqrt(x2dash^2+y2dash^2)
        vel <- dldash*fs
        accel <- dl2dash*fs
      }
      else
      {
        warning("Samples number is greater than filter length! Returning NA for velocities and accelerations")
      }
    }
  }
  return(list(dists = dl, dts = dt, vels = vel, accels = accel))
}

createFactorFromReturnedValue <- function(x)
{
  factor_new <- new(Class = "Factor", varName = as.character(names(x)))
  x <- unlist(x)
  cls <- class(x)[1]
  if (cls == "integer")
  {
    factor_new@type <- "integer"
    factor_new@levels <- as.character(NA)
  }
  if (cls == "numeric")
  {
    factor_new@type <- "numeric"
    factor_new@levels <- as.character(NA)
  }
  if (cls == "factor")
  {
    factor_new@type <- "factor"
    factor_new@levels <- levels(x)
  }
  if (cls == "ordered")
  {
    factor_new@type <- "ordFactor"
    factor_new@levels <- levels(x)
  }
  return(factor_new)
}


