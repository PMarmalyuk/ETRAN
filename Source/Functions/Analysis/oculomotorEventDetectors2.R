# 
# getMarkersGroups <- function(markers)
# {
#   adjacentMarkers <- data.frame(firstEv = markers[-length(markers)], secondEv = markers[-1])
#   transitions <- apply(adjacentMarkers, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
#   groups <- c(1,cumsum(transitions)+1)
#   return(groups)
# }

IVT <- function(data, settings, filterMarkers, filterMarkerNames, detectorMarkerNames)
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
  gapMarkers <- ifelse(filterMarkers != filterOkMarker, gapMarker, filterOkMarker)
  rawEventMarkers <- ifelse(gapMarkers[1:length(accel)] != okMarker, gapMarker)
  
                            ifelse(vel[1:length(accel)] <= VT, fixMarker, sacMarker))
  eventMarkers <- rawEventMarkers
  group <- markersGroups(eventMarkers)
  
  if (postProcess)
  {
    samplesCnt <- length(t)
    dt <- t[-1]-t[-samplesCnt]
    dl <- sqrt((x[-1]-x[-samplesCnt])^2 + (y[-1]-y[-samplesCnt])^2)
    
    events <- data.frame(t = t[1:length(accel)], 
                         x = x[1:length(accel)], 
                         y = y[1:length(accel)], 
                         dls = dl[1:length(accel)], 
                         dts = dt[1:length(accel)], 
                         vel = vel[1:length(accel)], 
                         accel = accel, 
                         evm = rawEventMarkers, 
                         gr = group)
    eventGroups <- split(events, group)
    fixationGroups <- list()
    saccadeGroups <- list()
    gapGroups <- list()
    artifactGroups <- list()
    eventMarkersGroups <- list()
    group <- 0
    lastGroup = NA
    for (gr in 1:length(eventGroups))
    {
      currentGroup <- eventGroups[[gr]]$evm[1]
      # Если текущая группа сэмплов - фиксация
      if (currentGroup == fixMarker)
      {
        # то вычисляем её длительность
        fixLen <- (eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1])
        # если фиксация короткая, то рассматриваем её как артефакт
        if (fixLen < minFixLen)
        {
          artifactGroups <- append(artifactGroups, eventGroups[gr])
          group <- group + 1
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(gapMarker, nrow(eventGroups[[gr]]))))
        }
        # если фиксация не короткая
        if (fixLen >= minFixLen)
        {
          anyGroupBefore <- !is.na(lastGroup)
          prevGroupIsSaccade <- F
          anyFixBefore <- F
          fixCloseInTime <- F
          fixCloseInSpace <- F
          
          if (anyGroupBefore) {prevGroupIsSaccade <- (lastGroup == sacMarker)}
          if (prevGroupIsSaccade) {anyFixBefore <- (length(fixationGroups) != 0)}
          if (anyFixBefore) 
          {
            currentFixOnset <- eventGroups[[gr]]$t[1]
            lastFixation <- tail(fixationGroups, n = 1)[[1]]
            lastFixOffset <- tail(lastFixation$t, 1)
            fixCloseInTime <- (lastFixOffset - currentFixOnset) <= MaxTBetFix
          }
          if (fixCloseInTime) 
          {
            currentFixPos <- c(eventGroups[[gr]]$x[1], eventGroups[[gr]]$y[1])
            lastFixPos <- c(tail(lastFixation$x, 1), tail(lastFixation$y, 1))
            dist <- sqrt((lastFixPos[1]-currentFixPos[1])^2 + (lastFixPos[2]-currentFixPos[2])^2)
            fixCloseInSpace <- (dist <= MaxDistBetFix)
          }
          if (fixCloseInSpace)
          {
            # то предыдущую саккаду рассматриваем как артефакт записи
            # newEvents[tail(newEvents, nrow(saccadeGroups[[length(saccadeGroups)]]))] <- rep(markerNames$artifact, nrow(saccadeGroups[[length(saccadeGroups)]]))
            artifactGroups <- append(artifactGroups, saccadeGroups[length(saccadeGroups)])
            eventMarkersGroups[length(eventMarkersGroups)] <- list(rep(gapMarker, length(eventMarkersGroups[[length(eventMarkersGroups)]])))
            saccadeGroups <- saccadeGroups[-length(saccadeGroups)]
            
            # а текущую фиксацию рассматриваем как продолжение предыдущей
            lastFixation <- list(rbind(lastFixation, eventGroups[[gr]]))
            fixationGroups[length(fixationGroups)] <- lastFixation
            lastGroup <- fixMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(fixMarker, nrow(eventGroups[[gr]]))))
          }
          
          if (!anyGroupBefore | !prevGroupIsSaccade | !anyFixBefore | !fixCloseInTime | !fixCloseInSpace)
          {
            # то дополняем список фиксаций текущей фиксацией
            fixationGroups <- append(fixationGroups, eventGroups[gr])
            lastGroup <- fixMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(fixMarker, nrow(eventGroups[[gr]]))))
          }
        }
      }
      # Если текущая группа сэмплов - саккада
      if (currentGroup == sacMarker)
      {
        # то вычисляем параметры maxVel и maxAccel
        maxSaccadeVel <- max(eventGroups[[gr]]$vel, na.rm = T)
        maxSaccadeAccel <- max(eventGroups[[gr]]$accel, na.rm = T)
        
        # и если саккада аномальна (включает сэмплы с аномальными значениями скорости или ускорения), 
        # то дополняем список артефактов этой саккадой
        if (maxSaccadeVel > maxVel | maxSaccadeAccel > maxAccel)
        {
          artifactGroups <- append(artifactGroups, eventGroups[gr])
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(gapMarker, nrow(eventGroups[[gr]]))))
        }
        #	если саккада не аномальна
        else
        {
          # то если предыдущая группа - саккада, то дополняем последнюю саккаду сэмплами текущей саккады
          if (!is.na(lastGroup) & lastGroup == sacMarker)
          {
            lastSaccade <- list(rbind(saccadeGroups[[length(saccadeGroups)]], eventGroups[[gr]]))
            saccadeGroups[length(saccadeGroups)] <- lastSaccade
            lastGroup <- sacMarker
            lastMarkers <- list(c(eventMarkersGroups[[length(eventMarkersGroups)]], rep(sacMarker, nrow(eventGroups[[gr]]))))
            eventMarkersGroups[length(eventMarkersGroups)] <- lastMarkers
          }
          else
            # иначе дополняем список саккад текущей саккадой
          {
            saccadeGroups <- append(saccadeGroups, eventGroups[gr])
            lastGroup <- sacMarker
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(sacMarker, nrow(eventGroups[[gr]]))))
          }
        }    
      }
    }
    eventMarkers <- unlist(eventMarkersGroups)
    eventMarkers <- c(eventMarkers, gapMarker)
  }
  
  markers <- factor(eventMarkers, levels = c(detectorMarkerNames$fixMarker,
                                             detectorMarkerNames$sacMarker,
                                             detectorMarkerNames$gapMarker))
  group <- markersGroups(markers)
  return(list(eventMarkers = markers, 
              eventGroups = group,
              eventsClass = "OculomotorEvents",
              detector = "IVT"))
}

ANH <- function(data, settings, filterMarkers, filterMarkerNames, detectorMarkerNames) 
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
  rawEvM <- ifelse(filterMarkers != filterOkMarker, gapMarker, filterOkMarker) 
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
  peaks <- which(above_Threshold[-1]!=above_Threshold[-length(above_Threshold)])
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
    else
      rawEvM[indexes] <- gapMarker
  }
  
  #Glissade detection
  for (i in 1:length(offsets)) {
    n <- ifelse((offsets[i]+windowSize)>=size,size-1,offsets[i]+windowSize)
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
  markers <- factor(rawEvM, levels = c(detectorMarkerNames$fixMarker,
                                       detectorMarkerNames$sacMarker,
                                       detectorMarkerNames$gliMarker,
                                       detectorMarkerNames$gapMarker))
  return(list(eventMarkers = markers, 
              eventGroups = group,
              eventsClass = "OculomotorEvents",
              detector = "ANH"))
}

IDT <- function(data, settings, filterMarkers, filterMarkerNames, detectorMarkerNames) 
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
  coords <- data.frame(x=x[-size], y=y[-size], dur= t[-1]-t[-size])
  left <- 1; right <- 2
  while (right < size) {
    if (sum(coords$dur[left:right]) < durationThreshold) right <- right + 1
    else {
      d <- (max(coords$x[left:right])-min(coords$x[left:right]))+(max(coords$y[left:right])-min(coords$y[left:right]))
      if (d > dispersionThreshold) {
        left <- right + 1
        right <- left + 1
      }
      else {
        while ((d <= dispersionThreshold) & (right<size)) {
          right <- right + 1
          d <- (max(coords$x[left:right])-min(coords$x[left:right]))+(max(coords$y[left:right])-min(coords$y[left:right]))
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
# This detector uses specified function (IVT, IDT, Ada-NH, ...) to detect oculomotor events
oculomotorEventDetector <- function(ETD, detector, filterMarkerNames, detectorMarkerNames, detectionSettings)
{
  mode <- etd$settings$mode
  
  if (mode == "left" | mode == "binocular")
  {
    if (detectionSettings$angular)
    {
      x <- ETD$leftEyeData$xAng
      y <- ETD$leftEyeData$yAng
      vel <- ETD$leftEyeData$velAng
      accel <- ETD$leftEyeData$accelAng
    }
    else
    {
      x <- ETD$leftEyeData$porx
      y <- ETD$leftEyeData$pory
      vel <- ETD$leftEyeData$vel
      accel <- ETD$leftEyeData$accel
    }
    data <- data.frame(t = ETD$commonData$time, x = x, y = y, vel = vel, accel = accel)
    detectorResLeft <- detector(data,
                                settings = detectionSettings, 
                                filterMarkers = ETD$leftEventsData$filterMarkers$eventMarkers, 
                                filterMarkerNames = filterMarkerNames,
                                detectorMarkerNames = detectorMarkerNames)
    detectorName <- detectorResLeft$detector
    detectorResLeft$detector <- NULL
    detectorResLeft <- append(detectorResLeft, list(eye = "left"))
    eventMarkers <- list(detectorResLeft)
    names(eventMarkers) <- detectorName
    ETD$leftEvents <- modifyList(x = ETD$leftEvents, val = eventMarkers)
  }
  if (mode == "right" | mode == "binocular")
  {
    if (detectionSettings$angular)
    {
      x <- ETD$rightEyeData$xAng
      y <- ETD$rightEyeData$yAng
      vel <- ETD$rightEyeData$velAng
      accel <- ETD$rightEyeData$accelAng
    }
    else
    {
      x <- ETD$rightEyeData$porx
      y <- ETD$rightEyeData$pory
      vel <- ETD$rightEyeData$vel
      accel <- ETD$rightEyeData$accel
    }
    data <- data.frame(t = ETD$commonData$time, x = x, y = y, vel = vel, accel = accel)
    detectorResRight <- detector(data,
                                 settings = detectionSettings, 
                                 filterMarkers = ETD$rightEventsData$filterMarkers$eventMarkers,
                                 filterMarkerNames = filterMarkerNames,
                                 detectorMarkerNames = detectorMarkerNames) 
    detectorName <- detectorResLeft$detector
    detectorResLeft$detector <- NULL
    detectorResRight <- append(detectorResRight, list(eye = "right"))
    eventMarkers <- list(detectorResRight)
    names(eventMarkers) <- detectorName
    ETD$rightEvents <- modifyList(ETD$rightEvents, val = eventMarkers)
  }
  return(ETD)
}