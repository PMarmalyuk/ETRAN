
calcPxVel <- function(t, x, y)
{
  samplesCnt <- length(t)
  dts <- abs(t[-1] - t[-samplesCnt])
  dls <- sqrt((x[-1] - x[-samplesCnt])^2 + (y[-1] - y[-samplesCnt])^2)
  vels <- 
    res <- list(dists = dls, dts = dts, vels = dls/dts)
  res
}

calcAngVel <- function(t, x, y, screenDist, screenDim, screenSize)
{
  samplesCnt <- length(t)
  dts <- abs(t[-1] - t[-samplesCnt])
  d <- screenDist
  w <- screenSize[1]; h <- screenSize[2]
  wPx <- screenDim[1]; hPx <- screenDim[2]
  x1s <- x[-samplesCnt]; x2s <- x[-1]
  y1s <- y[-samplesCnt]; y2s <- y[-1]
  dP0P1 <- sqrt((((x1s-wPx/2)/wPx)*w)^2 + (((y1s-hPx/2)/hPx)*h)^2)
  dP0P2 <- sqrt((((x2s-wPx/2)/wPx)*w)^2 + (((y2s-hPx/2)/hPx)*h)^2)
  alphaW <- atan(dP0P1/d)
  alphaH <- atan(dP0P2/d)
  dEyeP1 <- dP0P1/sin(alphaW)
  dEyeP2 <- dP0P2/sin(alphaH)
  dP1P2 <- sqrt((((x2s-x1s)/wPx)*w)^2 + (((y2s-y1s)/hPx)*h)^2)
  angles <- acos((dEyeP1^2 + dEyeP2^2 - dP1P2^2)/(2*dEyeP1*dEyeP2))
  res <- list(dists = angles*(180/pi), dts = dts, vels = angles*(180/pi)/dts)
  res
}

IVT <- function(t, x, y, VT, angular = F, screenDist, screenDim, screenSize, MaxTBetFix, MaxDistBetFix)
{
x <- smooth(x, kind = "3")
y <- smooth(y, kind = "3")

# 3. Velocities estimation stage
if (!angular)
{
  vel <- calcPxVel(t, x, y)
} 
else
{
  vel <- calcAngVel(t, x, y, screenDist, screenDim, screenSize)
}

events <- data.frame(t = t[-length(t)], x = x[-length(t)], y = y[-length(t)], vel = vel$vels)

fixationGroup <- list()
fixationGroups <- list()
saccadeGroup <- list()
saccadeGroups <- list()
lastFixPos <- c()
lastFixTime <- c()
e <- 2
while(e <= nrow(events))
{
  previousEvent <- ifelse(events$vel[e-1] <= VT, "Fixation", "Saccade")
  currentEvent <- ifelse(events$vel[e] <= VT, "Fixation", "Saccade")
  if (previousEvent == "Fixation" && currentEvent == "Fixation") 
  {
    fixationGroup <- rbind(fixationGroup, events[e,])
    e <- e + 1
  }
  if (previousEvent == "Saccade" && currentEvent == "Saccade")
  {
    saccadeGroup <- rbind(saccadeGroup, events[e,])
    e <- e + 1
  }
  if (previousEvent == "Fixation" && currentEvent == "Saccade")
  {
    lastFixPos <- c(events$x[e-1], events$y[e-1])
    lastFixTime <- events$t[e-1]
    saccadeGroup <- events[e,]
    e <- e + 1
  }
  if (previousEvent == "Saccade" && currentEvent == "Fixation")
  {
    if (length(lastFixTime) != 0)
    {
      fixPos <- c(events$x[e], events$y[e])
      fixTime <- events$t[e]
      if (fixTime - lastFixTime <= MaxTBetFix)
      {
        if (!angular)
        {
          pxVel <- calcPxVel(t = c(lastFixTime, fixTime), 
                             x = c(lastFixPos[1], fixPos[1]), 
                             y = c(lastFixPos[2], fixPos[2]))
          dist <- pxVel$dist
        }
        else
        {
          angVel <- calcAngVel(t = c(lastFixTime, fixTime), 
                               x = c(lastFixPos[1], fixPos[1]), 
                               y = c(lastFixPos[2], fixPos[2]),
                               screenDist,
                               screenDim,
                               screenSize)
          dist <- angVel$dist
        }
        if (dist <= MaxDistBetFix)
        {
          saccadeGroup <- list()
          fixationGroup <- rbind(fixationGroup, events[e,])
          e <- e + 1
        }
        else
        {
          fixationGroups <- append(fixationGroups, list(fixationGroup))
          saccadeGroups <- append(saccadeGroups, list(saccadeGroup))
          fixationGroup <- events[e,]
          e <- e + 1
        }
      }
      else
      {
        fixationGroups <- append(fixationGroups, list(fixationGroup))
        saccadeGroups <- append(saccadeGroups, list(saccadeGroup))
        fixationGroup <- events[e,]
        e <- e + 1
      }
    }
    else
    {
      saccadeGroups <- append(saccadeGroups, list(saccadeGroup))
      fixationGroup <- events[e,]
      e <- e + 1
    }
  }
}
res <- list(fixations = fixationGroups, saccades = saccadeGroups)
return(res)
}

load(file = "data")
numberOfSamples <- 20000
t <- data$t[1:numberOfSamples]
x <- data$x[1:numberOfSamples]
y <- data$y[1:numberOfSamples]
t1 <- Sys.time()
events <- IVT(t, x, y, VT = 30, angular = T, 
              screenDist = 100, screenDim = c(1280, 1024), screenSize = c(33.7, 27),
              MaxTBetFix = 0.075, MaxDistBetFix = 0.6)
t2 <- Sys.time()
t1 <- unclass(t1)
t2 <- unclass(t2)
t2-t1

length(events$fixations)
for (i in 1:length(events$fixations))
{
  if (i == 1) {plot(events$fixations[[i]]$x, events$fixations[[i]]$y, xlim = c(0, 1280), ylim = c(0, 1024), col = i)}
  if (i != 1) {points(events$fixations[[i]]$x, events$fixations[[i]]$y, xlim = c(0, 1280), ylim = c(0, 1024), col = i)}
}

for (i in 1:length(events$saccades))
{
  points(events$saccades[[i]]$x, events$saccades[[i]]$y, xlim = c(0, 1280), ylim = c(0, 1024), col = i, type = "l")
}
