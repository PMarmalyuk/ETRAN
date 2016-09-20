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


IVT <- function(data, settings, filterMarkers, 
                filterMarkerNames, detectorMarkerNames)
{
  t <- data$t
  x <- data$x
  y <- data$y
  vel <- data$vel
  accel <- data$accel

  #Detection settings
  VT <- settings$VT
  postProcess <- settings$postProcess
  classifyGaps <- settings$classifyGaps
  MaxTBetFix <- settings$MaxTBetFix
  MaxDistBetFix <- settings$MaxDistBetFix
  minFixLen <- settings$minFixLen
  maxGapLen <- settings$maxGapLen 
  maxVel <- settings$maxVel
  maxAccel <- settings$maxAccel
  #Filter markers and marker names
  filterOkMarker <- filterMarkerNames$okMarker
  #Detector markers  
  fixMarker <- detectorMarkerNames$fixMarker
  sacMarker <- detectorMarkerNames$sacMarker
  gapMarker <- detectorMarkerNames$gapMarker
  
  # 2. Classification stage: getting raw event markers
  rawEventMarkers <- ifelse(vel <= VT, fixMarker, sacMarker)
  rawEventMarkers[filterMarkers != filterOkMarker] <- gapMarker
  eventMarkers <- rawEventMarkers
  group <- markersGroups(eventMarkers)

  markers <- factor(eventMarkers, 
                    levels = c(detectorMarkerNames$fixMarker,
                               detectorMarkerNames$sacMarker,
                               detectorMarkerNames$gapMarker))
  group <- markersGroups(markers)
  return(list(eventMarkers = markers, 
              eventGroups = group,
              eventsClass = "OculomotorEvents",
              detector = "IVT"))
}

ANH <- function(data, settings, filterMarkers, 
                filterMarkerNames, detectorMarkerNames) 
{
  t <- data$t
  x <- data$x
  y <- data$y
  vel <- data$vel
  accel <- data$accel

  # Detection settings reading
  maxSaccadeVel  <- settings$maxSaccadeVel
  maxSaccadeAcc  <- settings$maxSaccadeAcc
  minSaccadeDur  <- settings$minSaccadeDur
  minFixationDur <- settings$minFixationDur
  # Filter markers names
  filterOkMarker <- filterMarkerNames$okMarker
  #Detector markers names
  fixMarker <- detectorMarkerNames$fixMarker
  sacMarker <- detectorMarkerNames$sacMarker
  gliMarker <- detectorMarkerNames$gliMarker
  gapMarker <- detectorMarkerNames$gapMarker
  
  size <- length(t)
  dt <- diff(t)
  
  windowSize <- as.integer(minFixationDur/mean(dt))
  rawEvM <- ifelse(filterMarkers != filterOkMarker, 
                   gapMarker, filterOkMarker) 
  #Peak velocity base
  PT0 <- max(vel)
  tolerance <- .000001
  #Peak velocity Threshold. Data-driven algorithm
  getThreshold <- function(Vel, sigmas) {
    PT <- PT0
    repeat {
      PTcur <- PT; velos <- Vel[which(Vel<PTcur)]
      if (length(velos) > windowSize) {
        m <- mean(velos); s <- sd(velos)
      }
      else {
        m <- mean(Vel); s <- sd(Vel)
      }
      PT <- m + sigmas*s
      if (abs(PT - PTcur) < tolerance) break
    }
    return(PT)
  }
  
  PT <- getThreshold(vel[which(rawEvM==filterOkMarker)],6); 
  
  #Saccade detection
  ##Peaks detection
  above_Threshold <- (vel > PT)
  peaks <- which(above_Threshold[-1]!=
                   above_Threshold[-length(above_Threshold)])
  if (peaks[1]<=windowSize) peaks <- peaks[-c(1:2)]
  ##Onsets
  STon <- getThreshold(vel[which(rawEvM==filterOkMarker)],3)
  leftmost_peaks <- peaks[seq(1,length(peaks),2)]
  onsets <- c()
  for (i in leftmost_peaks){
    nsample <- i
    if (nsample <= windowSize) break
    repeat {
      if (vel[nsample] < STon) 
        if ((vel[nsample]-vel[nsample-1]) <= 0) {
          onsets <- c(onsets, nsample)
          break 
        }
      else if((nsample-1)==0) break
      else nsample <- nsample - 1
      else nsample <- nsample - 1
    }
  }
  
  ##Offsets
  rightmost_peaks <- peaks[seq(2,length(peaks),2)]
  offsets <- c()
  offset_Thresholds <- c()
  alpha <- 0.7; beta <- 0.3
  for (i in 1:length(rightmost_peaks)) {
    nsample <- rightmost_peaks[i]
    from <- (onsets[i]-windowSize)
    to <- onsets[i]
    LNT <- getThreshold(vel[from:to],3)
    
    SToff <- alpha*STon + beta*LNT
    
    offset_Thresholds <- c(offset_Thresholds, SToff)
    
    repeat {
      if (vel[nsample] < SToff) {
        if ((vel[nsample] - vel[nsample+1]) <=0) {
          offsets <- c(offsets, nsample)
          break
        }
        else {
          nsample <- nsample + 1
        }
      }
      else {
        nsample <- nsample + 1
      }
      if (nsample >= size) {
        break
      }
    }
  }
  ## Saccades
  for (i in 1:length(offsets))
  {
    indexes <- onsets[i]:offsets[i]
    correctDuration <- (sum(dt[indexes])>=minSaccadeDur)
    correctHighestVel <- (max(vel[indexes])<=maxSaccadeVel)
    correctHighestAcc <- (max(accel[indexes])<=maxSaccadeAcc)
    if (correctHighestVel & correctHighestAcc)
      if (correctDuration){
        rawEvM[indexes] <- sacMarker
      }
    else {}
    else rawEvM[indexes] <- gapMarker
  }
  
  #Glissade detection
  for (i in 1:length(offsets)) {
    n <- ifelse((offsets[i]+windowSize)>=size,size-1,
                offsets[i]+windowSize)
    for (j in offsets[i]:n) {
      if (rawEvM[j]==filterOkMarker) 
        rawEvM[j] <- ifelse((vel[j] > PT), 
                            gliMarker, 
                            ifelse(vel[j] > offset_Thresholds[i], 
                                   gliMarker, 
                                   gapMarker))
    }
  }
  #Fixation detection
  rawEvM <- ifelse(rawEvM == filterOkMarker, fixMarker, rawEvM)
  #Results
  group <- markersGroups(rawEvM)
  markers <- factor(rawEvM, 
                    levels = c(detectorMarkerNames$fixMarker,
                               detectorMarkerNames$sacMarker,
                               detectorMarkerNames$gliMarker,
                               detectorMarkerNames$gapMarker))
  return(list(eventMarkers = markers, 
              eventGroups = group,
              eventsClass = "OculomotorEvents",
              detector = "ANH"))
}

IDT <- function(data, settings, filterMarkers, 
                filterMarkerNames, detectorMarkerNames) 
{
  t <- data$t
  x <- data$x
  y <- data$y

  #Detection settings reading
  dispersionThreshold <- settings$DT # in px or degrees
  durationThreshold <- settings$durT # in seconds
  #Filter markers names
  filterOkMarker <- filterMarkerNames$okMarker
  #Detector markers names
  fixMarker <- detectorMarkerNames$fixMarker
  sacMarker <- detectorMarkerNames$sacMarker
  gapMarker <- detectorMarkerNames$gapMarker

  size <- length(t)
  rawEvM <- c()
  rawEvM[1:(size-1)] <- sacMarker
  coords <- data.frame(x=x[-size], y=y[-size], 
                       dur= t[-1]-t[-size])
  left <- 1; right <- 2
  while (right < size) {
    if (sum(coords$dur[left:right]) < durationThreshold) 
      right <- right + 1
    else 
    {
      d <- (max(coords$x[left:right])-min(coords$x[left:right]))
      +(max(coords$y[left:right])-min(coords$y[left:right]))
      if (d > dispersionThreshold) {
        left <- right + 1
        right <- left + 1
      }
      else {
        while ((d <= dispersionThreshold) & (right<size)) {
          right <- right + 1
          d <- (max(coords$x[left:right])-min(coords$x[left:right]))
          +(max(coords$y[left:right])-min(coords$y[left:right]))
        }
        right <- right - 1
        rawEvM[left:right] <- fixMarker
        left <- right + 1
        right <- left + 1
      }
    }
  }
  rawEvM[which(filterMarkers != filterOkMarker)[-size]] <- gapMarker
  rawEvM <- append(rawEvM, gapMarker)
  group <- markersGroups(rawEvM)
  markers <- factor(rawEvM, levels = c(detectorMarkerNames$fixMarker,
                                       detectorMarkerNames$sacMarker,
                                       detectorMarkerNames$gapMarker))
  return(list(eventMarkers = markers, 
              eventGroups = group,
              eventsClass = "OculomotorEvents",
              detector = "IDT"))
}

## CORE DETECTOR ##
# This detector uses specified function (IVT, IDT, Ada-NH, ...) 
# to detect oculomotor events
oculomotorEventDetector <- function(ETD, detector, 
                                    filterMarkerNames, 
                                    detectorMarkerNames, 
                                    detectionSettings)
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
      if (is.null(ETD$leftEyeData$xAng) | 
          is.null(ETD$leftEyeData$yAng))
      {
        stop("You should calculate angular gaze positions!")
      }
      else
      {
        x <- ETD$leftEyeData$xAng
        y <- ETD$leftEyeData$yAng
      }
      if (is.null(ETD$leftEyeData$velAng) | 
          is.null(ETD$leftEyeData$accelAng))
      {
        stop("You should calculate angular velocities 
             and accelerations!")
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
      if (is.null(ETD$leftEyeData$vel) | 
          is.null(ETD$leftEyeData$accel))
      {
        stop("You should calculate velocities 
             and accelerations!")
      }
      else
      {
        vel <- ETD$leftEyeData$vel
        accel <- ETD$leftEyeData$accel
      }
    }
    
    filterMarkers <- getFilterMarkers(ETD$leftEvents$filter)
    data <- data.frame(t = ETD$commonData$time, x = x, y = y, 
                       vel = vel, accel = accel)
    detectorResLeft <- detector(data,
                                settings = detectionSettings, 
                                filterMarkers = filterMarkers, 
                                filterMarkerNames = filterMarkerNames,
                                detectorMarkerNames = detectorMarkerNames)
    detectorName <- detectorResLeft$detector
    detectorResLeft$detector <- NULL
    detectorResLeft <- getEventsPositions(append(detectorResLeft,
                                                 list(eye = "left")))
    eventMarkers <- list(detectorResLeft)
    names(eventMarkers) <- detectorName
    ETD$leftEvents <- modifyList(x = ETD$leftEvents, val = eventMarkers)
  }
  if (mode == "right" | mode == "binocular")
  {
    if (detectionSettings$angular)
    {
      if (is.null(ETD$rightEyeData$xAng) | 
          is.null(ETD$rightEyeData$yAng))
      {
        stop("You should calculate angular gaze positions!")
      }
      else
      {
        x <- ETD$rightEyeData$xAng
        y <- ETD$rightEyeData$yAng
      }
      if (is.null(ETD$rightEyeData$velAng) | 
          is.null(ETD$rightEyeData$accelAng))
      {
        stop("You should calculate angular velocities 
             and accelerations!")
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
      if (is.null(ETD$rightEyeData$vel) | 
          is.null(ETD$rightEyeData$accel))
      {
        stop("You should calculate velocities 
             and accelerations!")
      }
      else
      {
        vel <- ETD$rightEyeData$vel
        accel <- ETD$rightEyeData$accel
      }
    }
    data <- data.frame(t = ETD$commonData$time, x = x, y = y, 
                       vel = vel, accel = accel)
    filterMarkers <- getFilterMarkers(ETD$rightEvents$filter)
    detectorResRight <- detector(data,
                                 settings = detectionSettings, 
                                 filterMarkers = filterMarkers,
                                 filterMarkerNames = filterMarkerNames,
                                 detectorMarkerNames = detectorMarkerNames) 
    detectorName <- detectorResLeft$detector
    detectorResLeft$detector <- NULL
    detectorResRight <- getEventsPositions(append(detectorResRight, 
                                                  list(eye = "right")))
    eventMarkers <- list(detectorResRight)
    names(eventMarkers) <- detectorName
    ETD$rightEvents <- modifyList(ETD$rightEvents, val = eventMarkers)
  }
  return(ETD)
}