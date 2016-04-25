kurtosis <- function (x, na.rm = FALSE) 
{
  if (any(ina <- is.na(x))) {
    if (na.rm) 
      x <- x[!ina]
    else return(NA)
  }

  n <- length(x)
  x <- x - mean(x)
  r <- n * sum(x^4)/(sum(x^2)^2)
  y <- r * (1 - 1/n)^2 - 3
  y
}

skewness <- function (x, na.rm = FALSE) 
{
  if (any(ina <- is.na(x))) {
    if (na.rm) 
      x <- x[!ina]
    else return(NA)
  }
  n <- length(x)
  x <- x - mean(x)
  y <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
  y <- y * ((1 - 1/n))^(3/2)
  y
}

pointsArea <- function(x, y) {
  geron <- function(x,y) {
    if ((length(x)!=length(y))|(length(x)!=3))
      stop("Wrong number of vertexes.")
    a <- sqrt((x[2]-x[1])^2+(y[2]-y[1])^2)
    b <- sqrt((x[3]-x[2])^2+(y[3]-y[2])^2)
    c <- sqrt((x[1]-x[3])^2+(y[1]-y[3])^2)
    p <- (a+b+c)/2
    return(sqrt(p*(p-a)*(p-b)*(p-c)))
  }
  area <- 0
  shape <- chull(x,y)
  x <- x[shape]; y <- y[shape]  
  len <- length(x)
  if (len<3) area <- NA 
  else 
  {
    for (i in 2:(len-1)) area <- area + geron(x[c(1,i,i+1)],y[c(1,i,i+1)])
  }
  return(area)
}

shannon.entropy <- function(p)
{
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <- p[p>0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}

# Calculates angular height and width in degrees of the eye position 
# relative to the screen left corner (default) or the given reference point
calcAngPos <- function(x, y, settings, refPoint = c(0,0))
{
  d <- settings$headDistance
  w <- settings$screenSize[1]; h <- settings$screenSize[2]
  wPx <- settings$screenResolution[1]; hPx <- settings$screenResolution[2]
  xAng <- (180/pi)*atan(((x-refPoint[1])/(d*wPx/w)))
  yAng <- (180/pi)*atan(((y-refPoint[2])/(d*hPx/h)))
  return(list(xAng = xAng, yAng = yAng))
}

calcVel <- function(t, x, y, settings)
{
  angular <- settings$angular
  velType <- settings$velType
  fs <- settings$fs 
  fl <- settings$fl
  screenDist <- settings$headDistance
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
        angPositions <- calcAngPos(x = x, y = y, settings)
        x <- angPositions$xAng
        y <- angPositions$yAng
      }
      x1 <- x[-samplesCnt]; x2 <- x[-1]
      y1 <- y[-samplesCnt]; y2 <- y[-1]
      dl <- sqrt((x2-x1)^2 + (y2-y1)^2)
      vel <- dl/dt
      vel <- c(vel, tail(vel,1))
    }
    if (velType == "analytical")
    {
      if (samplesCnt >= fl)
      {
        if (angular)
        {
          angPositions <- calcAngPos(x = x, y = y, settings)
          x <- angPositions$xAng
          y <- angPositions$yAng
        }
        if (fl %% 2 == 0) fl <- fl + 1 # turn even number to odd number by incrementing it by 1
        xdash <- sgolayfilt(x = x, p = 2, n = fl, m = 1, ts = 1)
        # x2dash <- sgolayfilt(x = x, p = 2, n = fl, m = 2, ts = 1)
        ydash <- sgolayfilt(x = y, p = 2, n = fl, m = 1, ts = 1)
        # y2dash <- sgolayfilt(x = y, p = 2, n = fl, m = 2, ts = 1)
        dt <- abs(t[-1] - t[-samplesCnt])
        dl <- sqrt(xdash^2+ydash^2);
        vel <- dl/c(dt, tail(dt,1))
      }
      else
      {
        warning("Samples number is less than filter length! Returning NA for velocities and accelerations")
      }
    }
  }
  if (samplesCnt >= 3)
  {
    accel <- (vel[-1]-vel[-length(vel)])/c(dt[-length(dt)],tail(dt[-length(dt)],1))
  }
  return(list(dists = dl, dts = dt, vels = vel, accels = accel))
}
# 
# v1 <- calcVel(t = etd$commonData$time, x = etd$leftEyeData$porx, y = etd$leftEyeData$pory,
#         settings = list(angular = T, velType = "finDiff",
#                         headDistance = etd$settings$headDistance,
#                         screenResolution = etd$settings$screenResolution,
#                         screenSize = etd$settings$screenSize))
# 
# v2 <- calcVel(t = etd$commonData$time, x = etd$leftEyeData$porx, y = etd$leftEyeData$pory,
#               settings = list(angular = T, velType = "analytical", fl = 5, fs = 500,
#                               headDistance = etd$settings$headDistance,
#                               screenResolution = etd$settings$screenResolution,
#                               screenSize = etd$settings$screenSize))

