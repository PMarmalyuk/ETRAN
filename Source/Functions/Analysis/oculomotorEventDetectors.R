getEventsTable <- function(t, event, group) 
{
  if (length(t) == length(event) & 
      length(event) == length(group) & 
      !is.null(t) & !is.null(event) & !is.null(group)) {
    df <- data.frame(t = t, event = event, group = group)
    dfs <- split(df, df$group)
    locations <- lapply(dfs, FUN = function(x) {
      list(start = head(x$t, 1),
           end   = tail(x$t, 1),
           event = unique(x$event)[1],
           group = unique(x$group)[1])
    })
    locations <- rbindlist(locations)
  }
  else locations <- NULL
  return(locations)
}

getEventsPositions <- function(eventMarkers)
{
  if (all(!is.null(eventMarkers)))
  {
    markers <- eventMarkers$eventMarkers
    groups <- eventMarkers$eventGroups
    eye <- eventMarkers$eye
    df <- data.frame(position = 1:length(markers), 
                     event = markers, 
                     group = groups)
    
    dfs <- split(df, df$group)
    locations <- lapply(dfs, FUN = function(x) 
    {
      list(eye = eye,
           start = head(x$position, 1),
           end = tail(x$position, 1),
           event = unique(x$event)[1],
           group = unique(x$group)[1])
    })
    locations <- rbindlist(locations)
  } else locations <- NULL
  return(locations)
}

IVT <- function(t, vel, VT) 
{
  fixMarker <- "Fixation"; sacMarker <- "Saccade"
  event <- ifelse(vel <= VT, fixMarker, sacMarker)
  group <- markersGroups(event)
  location <- getEventsTable(t, event, group)
  return(location)
}

ANH <- function(t, vel, minSac = .01, minFix = .04) 
{
  fixMarker <- "Fixation"
  sacMarker <- "Saccade"
  gliMarker <- "Glissade"
  unpMarker <- "Unclassified"
  #Preprocess
  size <- length(t); dur <- diff(t); dur <- c(dur, 0)
  windowSize <- round(minFix/mean(dur), 0)
  event <- rep(unpMarker, size)
  peakVel.0 <- max(vel); accur <- 1e-5
  #Peak velocity threshold estimation
  getThreshold <- function(Vel, sigmas = 6, PT = peakVel.0, PT.cur = .9 * PT) {
    while (abs(PT - PT.cur) > accur) {
      velos <- Vel[Vel < PT.cur]
      m <- mean(velos); s <- sd(velos)
      PT.cur <- PT; PT <- m + sigmas * s
    }
    return(PT)
  }
  peakVel <- getThreshold(vel)
  #Saccade detection
  ##Peaks detection
  above.Threshold <- (vel > peakVel)
  peaks <- which(above.Threshold[-1] != above.Threshold[-length(above.Threshold)])
  if (peaks[1] <= windowSize) peaks <- peaks[-c(1:2)]
  peaks.size <- length(peaks)
  if (tail(peaks, 1) >= size - windowSize) peaks <- peaks[-c(peaks.size - 1, peaks.size)]
  right.peaks <- seq(2, length(peaks), 2)
  peaks[right.peaks] <- peaks[right.peaks] + 1
  ##Peaks separation
  onsets <- c(); offsets <- c()
  left.peaks <- peaks[seq(1, length(peaks), 2)]
  right.peaks <- peaks[seq(2, length(peaks), 2)]
  ##Saccades onsets
  onset.Threshold <- getThreshold(vel, 3)
  for (i in left.peaks) {
    while((vel[i] >= onset.Threshold) & (vel[i] >= vel[i - 1]) & (i > 1)) 
      i <- i - 1
    onsets <- c(onsets, i)
  }
  ##Saccades offsets
  offset.Thresholds <- c()
  for (j in 1:length(right.peaks)) {
    i <- onsets[j]
    first <- ifelse(i - windowSize > 0, i - windowSize, 1)
    last <- i
    local.Threshold <- getThreshold(vel[first:last], 3)
    offset.Threshold <- .7 * onset.Threshold + .3 * local.Threshold
    offset.Thresholds <- c(offset.Thresholds, offset.Threshold)
    i <- right.peaks[j]
    while(((vel[i] >= offset.Threshold) & (vel[i] >= vel[i + 1])) & (i < size))
      i <- i + 1
    offsets <- c(offsets, i)
  }
  ##Glissade onsets
  gli.onsets <- offsets
  ##Glissade offsets
  gli.offsets <- c()
  for (i in gli.onsets) {
    while((vel[i] >= vel[i + 1]) & (i < size))
      i <- i + 1
    while((vel[i] <= vel[i + 1]) & (i < size))
      i <- i + 1 
    gli.offsets <- c(gli.offsets, i)
  }
  #Event detection
  ##Saccades
  for (i in 1:length(onsets)) {
    indexes <- onsets[i]:offsets[i]
    if (sum(dur[indexes]) >= minSac)
      event[indexes] <- sacMarker
  }
  ##Glissades
  for (i in 1:length(gli.onsets)) {
    l <- gli.onsets[i]; r <- gli.offsets[i]
    #Glissade peak must be greater than previous saccade offset threshold
    if (!is.na(table(vel[l:r] >= offset.Thresholds[i])["TRUE"]))
      event[l:r] <- gliMarker
  }
  ##Fixations
  event[event == unpMarker] <- fixMarker
  #Result
  group <- markersGroups(event)
  location <- getEventsTable(t, event, group)
  return(location)
}

IDT <- function(t, x, y, disp.Threshold, dur.Threshold) 
{
  fixMarker <- "Fixation"
  sacMarker <- "Saccade"
  dur <- c(diff(t), 0)
  size <- length(t)
  event <- rep(sacMarker, size)
  disp.FUN <- function(x, y) {
    return((max(x) - min(x)) + (max(y) - max(y)))
  }
  left <- 1; right <- 2
  while (right < size) {
    while (sum(dur[left:right]) < dur.Threshold & right < size) 
      right <- right + 1
    d <- disp.FUN(x[left:right], y[left:right])
    if (d <= disp.Threshold) {
      while (d <= disp.Threshold & right < size) {
        right <- right + 1
        d <- disp.FUN(x[left:right], y[left:right])
      }
      right <- right - 1
      event[left:right] <- fixMarker
    }
    left <- right + 1
    right <- left + 1
  }
  group <- markersGroups(event)
  location <- getEventsTable(t, event, group)
  return(location)
}
 
closeInSpaceFixations <- function(df, locations, dispersion.Measure) {
  
}

shortEvents <- function(locations, minDuration, marker, artMarker = "Artefact") {
  levs <- levels(locations$event)
  levels(locations$event) <- c(levs, artMarker)
  #input check
  if (length(minDuration) != length(marker))
    stop("minDuration and marker must be the same length.")
  for (i in 1:length(marker)) {
    for (j in which(locations$event == marker[i])) {
      dur <- diff(as.numeric(locations[j,][c("start", "end")]))
      if (dur < minDuration[i])
        locations$event[j] <- artMarker
    }
  }
  return(locations)
}

highVelocity <- function(locations, t, vel, maxVel, artMarker = "Artefact") {
  levs <- levels(locations$event)
  levels(locations$event) <- c(levs, artMarker)
  for (i in 1:nrow(locations)) {
    t0 <- locations$start[i]
    t1 <- locations$end[i]
    if (is.element(T, vel[t >= t0 & t <= t1] > maxVel))
      locations$event[i] <- artMarker
  }
  return(locations)
}

closeInSpaceFixations <- function(locations, t, x, y, maxDist, artMarker = "Artefact") {
  
}

postProcessor <- function(df, events) {
  #Input check
  
  #Data preparation
  
  #Post processing
  ##Fixations
  ###Close in space and time
  ###Short fixations
  ##Saccades
  ###Blink inside saccade
  ###Extreme velocity
  ###Short saccade
  ##Glissades
  ###Blink inside glissade
  ###Amplitude greater than previous saccade has
  ###Glissade without previous saccade
  
  
  return(locations)
}

## CORE DETECTOR ##
# This detector uses specified function (IVT, IDT, Ada-NH, ...) to detect oculomotor events
oculomotorEventDetector <- function(ETD, detector, filterMarkerNames, detectorMarkerNames, detectionSettings)
{
  getFilterMarkers <- function(filterLocations)
  {
    fl <- filterLocations
    fm <- lapply(1:nrow(fl), FUN = function(x) 
    {
      list(marker = rep(fl$event[x], fl$end[x]-fl$start[x] + 1))
    })
    return(rbindlist(fm))
  }
  
  mode <- etd$settings$mode
  if (mode == "left" | mode == "binocular")
  {
    if (detectionSettings$angular)
    {
      if (is.null(ETD$leftEyeData$xAng) | is.null(ETD$leftEyeData$yAng))
      {
        stop("You should calculate angular gaze positions!")
      }
      else
      {
        x <- ETD$leftEyeData$xAng
        y <- ETD$leftEyeData$yAng
      }
      if (is.null(ETD$leftEyeData$velAng) | is.null(ETD$leftEyeData$accelAng))
      {
        stop("You should calculate angular velocities and accelerations!")
      }
      else
      {
        vel <- ETD$leftEyeData$velAng
        accel <- ETD$leftEyeData$accelAng
      }
    }
    else
    {
      x <- ETD$leftEyeData$porx
      y <- ETD$leftEyeData$pory
      if (is.null(ETD$leftEyeData$vel) | is.null(ETD$leftEyeData$accel))
      {
        stop("You should calculate velocities and accelerations!")
      }
      else
      {
        vel <- ETD$leftEyeData$vel
        accel <- ETD$leftEyeData$accel
      }
    }
    
    filterMarkers <- getFilterMarkers(ETD$leftEvents$filter)
    data <- data.frame(t = ETD$commonData$time, x = x, y = y, vel = vel, accel = accel)
    detectorResLeft <- detector(data,
                                settings = detectionSettings, 
                                filterMarkers = filterMarkers, 
                                filterMarkerNames = filterMarkerNames,
                                detectorMarkerNames = detectorMarkerNames)
    detectorName <- detectorResLeft$detector
    detectorResLeft$detector <- NULL
    detectorResLeft <- getEventsPositions(append(detectorResLeft, list(eye = "left")))
    eventMarkers <- list(detectorResLeft)
    names(eventMarkers) <- detectorName
    ETD$leftEvents <- modifyList(x = ETD$leftEvents, val = eventMarkers)
  }
  if (mode == "right" | mode == "binocular")
  {
    if (detectionSettings$angular)
    {
      if (is.null(ETD$rightEyeData$xAng) | is.null(ETD$rightEyeData$yAng))
      {
        stop("You should calculate angular gaze positions!")
      }
      else
      {
        x <- ETD$rightEyeData$xAng
        y <- ETD$rightEyeData$yAng
      }
      if (is.null(ETD$rightEyeData$velAng) | is.null(ETD$rightEyeData$accelAng))
      {
        stop("You should calculate angular velocities and accelerations!")
      }
      else
      {
        vel <- ETD$rightEyeData$velAng
        accel <- ETD$rightEyeData$accelAng
      }
    }
    else
    {
      x <- ETD$rightEyeData$porx
      y <- ETD$rightEyeData$pory
      if (is.null(ETD$rightEyeData$vel) | is.null(ETD$rightEyeData$accel))
      {
        stop("You should calculate velocities and accelerations!")
      }
      else
      {
        vel <- ETD$rightEyeData$vel
        accel <- ETD$rightEyeData$accel
      }
    }
    data <- data.frame(t = ETD$commonData$time, x = x, y = y, vel = vel, accel = accel)
    filterMarkers <- getFilterMarkers(ETD$rightEvents$filter)
    detectorResRight <- detector(data,
                                 settings = detectionSettings, 
                                 filterMarkers = filterMarkers,
                                 filterMarkerNames = filterMarkerNames,
                                 detectorMarkerNames = detectorMarkerNames) 
    detectorName <- detectorResLeft$detector
    detectorResLeft$detector <- NULL
    detectorResRight <- getEventsPositions(append(detectorResRight, list(eye = "right")))
    eventMarkers <- list(detectorResRight)
    names(eventMarkers) <- detectorName
    ETD$rightEvents <- modifyList(ETD$rightEvents, val = eventMarkers)
  }
  return(ETD)
}