IVT <- function(t, x, y, settings, filterMarkers, filterMarkerNames, detectorMarkerNames, detectionSettings)
{
  
  screenDist <- settings$headDistance
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  fs <- settings$fs
  
  VT <- detectionSettings$VT
  angular <- detectionSettings$angular
  velType <- detectionSettings$velType
  fl <- detectionSettings$fl
  postProcess <- detectionSettings$postProcess
  MaxTBetFix <- detectionSettings$MaxTBetFix
  MaxDistBetFix <- detectionSettings$MaxDistBetFix
  minFixLen <- detectionSettings$minFixLen
  maxGapLen <- detectionSettings$maxGapLen
  maxVel <- detectionSettings$maxVel
  maxAccel <- detectionSettings$maxAccel
  classifyGaps <- detectionSettings$classifyGaps

  filterOkMarker <- filterMarkerNames$okMarker
  filterGapMarker <- filterMarkerNames$gapMarker
  filterArtMarker <- filterMarkerNames$artMarker
  filterBliMarker <- filterMarkerNames$bliMarker
  
  fixMarker <- detectorMarkerNames$fixMarker
  sacMarker <- detectorMarkerNames$sacMarker
  gapMarker <- detectorMarkerNames$gapMarker
  artMarker <- detectorMarkerNames$artMarker
  
  # 1. Velocities and accelerations estimation
  vel <- calcVel(t, x, y, settings, angular, velType, fs, fl)
  accels <- vel$accels
  
  # 2. Classification stage: getting raw event markers
  gapMarkers <- ifelse(filterMarkers != filterOkMarker, filterGapMarker, filterOkMarker)
  rawEventMarkers <- ifelse(gapMarkers[1:length(accels)] == filterGapMarker, gapMarker, ifelse(vel$vels[1:length(accels)] <= VT, fixMarker, sacMarker))
  evmarks <- data.frame(firstEv = rawEventMarkers[-length(rawEventMarkers)], secondEv = rawEventMarkers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  if (postProcess)
  {
    if (angular)
    {
      angPos <- calcAngPos(x, y, settings)
      x <- angPos$xAng
      y <- angPos$yAng
    }
    events <- data.frame(t = t[1:length(accels)], x = x[1:length(accels)], y = y[1:length(accels)], 
                         dls = vel$dists[1:length(accels)], dts = vel$dts[1:length(accels)], vel = vel$vels[1:length(accels)], accel = accels, 
                         evm = rawEventMarkers, gr = group)
    eventGroups <- split(events, group)
    fixationGroups <- list()
    saccadeGroups <- list()
    gapGroups <- list()
    artifactGroups <- list()
    eventMarkersGroups <- list()
    group <- 0
    newGroups <- c()
    newEvents <- c()
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
          newGroups <- c(newGroups, rep(group, nrow(eventGroups[[gr]])))
          newEvents <- c(newEvents, rep(artMarker, nrow(eventGroups[[gr]])))
          #eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$artifact, nrow(eventGroups[[gr]])))
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
            newEvents[tail(newEvents, nrow(saccadeGroups[[length(saccadeGroups)]]))] <- rep(artMarker, nrow(saccadeGroups[[length(saccadeGroups)]]))
            artifactGroups <- append(artifactGroups, saccadeGroups[length(saccadeGroups)])
            eventMarkersGroups[length(eventMarkersGroups)] <- list(rep(artMarker, length(eventMarkersGroups[[length(eventMarkersGroups)]])))
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
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(artMarker, nrow(eventGroups[[gr]]))))
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
      # Если текущая группа сэмплов - пропуск
      if (currentGroup == gapMarker)
      {
        if (classifyGaps)
        {
          # то проверяем по параметру maxGapLen, длинный ли пропуск или не длинный
          gapLen <- eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1]
          # если длинный, или lastGroup = NA, или группа является последней
          # то классифицируем его как пропуск
          if (gapLen > maxGapLen | is.na(lastGroup) | gr == length(eventGroups))
          {
            gapClass <- gapMarker
          }
          # если не длинный, и группа не является последней, и группа не является первой
          # то классифицируем пропуск по близости сэмплов, смежных с пропуском 
          if (gapLen <= maxGapLen & gr != length(eventGroups) & !is.na(lastGroup))
          {
            
            if (lastGroup != gapMarker & eventGroups[[gr+1]]$evm[1] != gapMarker)
            {
              lastSmpBeforeGap <- eventGroups[[gr-1]][nrow(eventGroups[[gr-1]]),]
              firstSmpAfterGap <- eventGroups[[gr+1]][1,]
              t1 <- lastSmpBeforeGap$t
              t2 <- firstSmpAfterGap$t
              pos1 <- c(lastSmpBeforeGap$x, lastSmpBeforeGap$y)
              pos2 <- c(firstSmpAfterGap$x, firstSmpAfterGap$y)
              if (t2-t1 <= MaxTBetFix)
              {
                dist <- sqrt((pos1[1]-pos2[1])^2 + (pos1[2]-pos2[2])^2)
                # если смежные с пропуском сэмплы близки во времени и пространстве, 
                # то он классифицируется как фиксация
                if (dist <= MaxDistBetFix)
                {
                  gapClass <- fixMarker
                }
                else
                  # если не близки в пространстве, то пропуск классифицируется как саккада
                {
                  gapClass <- sacMarker
                }
              }
              else
                # если не близки во времени, то пропуск классифицируется как саккада
              {
                gapClass <- sacMarker
              }
            }
            if (lastGroup == gapMarker | eventGroups[[gr+1]]$evm[1] == gapMarker)
            {
              gapClass <- gapMarker
            }
          }
        }
        else
        {
          gapClass <- gapMarker
        }
        
        # результат классификации пропуска позволяет отнести его сэмплы к тому или иному списку событий
        # если пропуск - длинный пропуск, то пополняем список пропусков
        if (gapClass == gapMarker)
        {
          gapGroups <- append(gapGroups, eventGroups[gr])
          lastGroup <- gapMarker
          eventMarkersGroups <- append(eventMarkersGroups, rep(gapMarker, nrow(eventGroups[[gr]])))
        }
        # если пропуск - фиксация
        if (gapClass == fixMarker)
        {
          # то если последняя группа - фиксация, то добавляем сэмплы пропуска в эту группу
          if (lastGroup == fixMarker)
          {
            lastFixation <- rbind(eventGroups[[gr-1]], eventGroups[[gr]])
            fixationGroups[length(fixationGroups)] <- list(lastFixation)
            lastGroup <- fixMarker
            eventMarkersGroups <- append(eventMarkersGroups, rep(fixMarker, nrow(eventGroups[[gr]])))
          }
          # иначе добавляем новую группу в список фиксаций
          else
          {
            fixationGroups <- append(fixationGroups, eventGroups[gr])
            lastGroup <- fixMarker
            eventMarkersGroups <- append(eventMarkersGroups, rep(fixMarker, nrow(eventGroups[[gr]])))
          }
        }
        # если пропуск - саккада
        if (gapClass == sacMarker)
        {
          # то если последняя группа - саккада, то добавляем сэмплы пропуска в эту группу
          if (lastGroup == sacMarker)
          {
            lastSaccade <- rbind(eventGroups[[gr-1]], eventGroups[[gr]])
            saccadeGroups[length(saccadeGroups)] <- list(lastSaccade)
            lastGroup <- sacMarker
            eventMarkersGroups <- append(eventMarkersGroups, rep(sacMarker, nrow(eventGroups[[gr]])))
          }
          # иначе добавляем новую группу в список саккад
          if (lastGroup == fixMarker | lastGroup == gapMarker)
          {
            saccadeGroups <- append(saccadeGroups, eventGroups[gr])
            lastGroup <- sacMarker
            eventMarkersGroups <- append(eventMarkersGroups, rep(sacMarker, nrow(eventGroups[[gr]])))
          }
        }
      }
    }
    eventMarkers <- eventMarkersGroups
  }
  else
  {
    eventMarkersGroups <- rawEventMarkers
  }
  # Is group filled correctly?
  ## group is a vector with event ordinal numbers (including the case of post processing results)
  markers <- factor(eventMarkersGroups, levels = c(detectorMarkerNames$fixMarker,
                                                   detectorMarkerNames$sacMarker,
                                                   detectorMarkerNames$gapMarker,
                                                   detectorMarkerNames$artMarker))
  evmarks <- data.frame(firstEv = markers[-length(markers)], secondEv = markers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  
  res <- list(eventMarkers = markers, 
              eventGroups = group,
              eventsClass = "OculomotorEvents", 
              detector = paste0("IDT.",detectionSettings$resultIdentifier))
  return(res)
}

ANH <- function(t, x, y, settings, filterMarkers, filterMarkerNames, detectorMarkerNames, detectionSettings) 
{
  # ANH internal functions definitions
  getThreshold <- function(Vel,PT0,tolerance,sigmaCoef) {
    PT <- PT0
    repeat {
      PTcur <- PT
      m <- mean(Vel[which(Vel<PTcur)], na.rm = T)
      s <- sd(Vel[which(Vel<PTcur)], na.rm = T)
      PT <- m + sigmaCoef*s
      if (abs(PT - PTcur) < tolerance) {break}
    }
    PT
  }
  getThreshold3 <- function(Vel,PT0,tolerance) {
    PT <- PT0
    repeat {
      PTcur <- PT
      m <- mean(Vel)
      s <- sd(Vel)
      PT <- m + 3*s
      if (abs(PT - PTcur) < tolerance) {break}
    }
    PT
  }

  screenDist <- settings$headDistance
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  fs <- settings$fs

  angular <- detectionSettings$angular
  velType <- detectionSettings$velType
  fl <- detectionSettings$fl
  maxSaccadeVel <- detectionSettings$maxSaccadeVel
  maxSaccadeAcc <- detectionSettings$maxSaccadeAcc
  minSaccadeDur <- detectionSettings$minSaccadeDur
  minFixationDur <- detectionSettings$minFixationDur
  
  filterOkMarker <- filterMarkerNames$okMarker
  filterGapMarker <- filterMarkerNames$gapMarker
  filterArtMarker <- filterMarkerNames$artMarker
  filterBliMarker <- filterMarkerNames$bliMarker
  
  fixMarker <- detectorMarkerNames$fixMarker
  sacMarker <- detectorMarkerNames$sacMarker
  gliMarker <- detectorMarkerNames$gliMarker
  gapMarker <- detectorMarkerNames$gapMarker
  artMarker <- detectorMarkerNames$artMarker
  
  size <- length(t)
  if (is.na(fs))
  {
    meandt <- mean(t[-1] - t[-size], na.rm = T)
    fs <- 1/meandt
  }

  # Using Savitsky-Golay filter to get velocities and accelerations using derivatves of approximated x and y signals
  vel <- calcVel(t, x, y, settings, angular, velType, fs, fl)
  accels <- vel$accels
  rawEvM <- ifelse(filterMarkers != filterOkMarker, filterGapMarker, filterOkMarker)[-size]
  windowSize <- floor(minFixationDur/mean(vel$dts, na.rm = T))
  
  ### Peak velocity Threshold calculation
  PT <- getThreshold(vel$vels[which(rawEvM == filterOkMarker)], PT0 = 250, tolerance = 0.1, sigmaCoef = 6)
  ### Saccades detection
  ### Velocity peaks, saccades onsets and offsets search
  
  #Находим номера сэмплов начала пика и конца пика
  above_Threshold <- (vel$vels > PT)
  peaks <- which(above_Threshold[-1]!=above_Threshold[-length(above_Threshold)])
  if (peaks[1]<=windowSize) peaks <- peaks[-c(1:2)]
  #Находим onset-ы
  STon <- getThreshold(vel$vels[which(rawEvM == filterOkMarker)], PT0 = 250, tolerance = 0.1, sigmaCoef = 3)
  leftmost_peaks <- peaks[seq(1,length(peaks),2)]
  onsets <- c()
  for (i in leftmost_peaks){
    nsample <- i
    if (nsample <= windowSize) break
    repeat {
      if (vel$vels[nsample] < STon) 
        if ((vel$vels[nsample]-vel$vels[nsample-1]) <= 0) {
          onsets <- c(onsets, nsample)
          break 
        }
      else if((nsample-1)==0) break
      else nsample <- nsample - 1
      else nsample <- nsample - 1
    }
  }
  #Находим offset-ы
  rightmost_peaks <- peaks[seq(2,length(peaks),2)]
  offsets <- c()
  offset_Thresholds <- c()
  alpha <- 0.7; beta <- 0.3
  for (i in 1:length(rightmost_peaks)) {
    nsample <- rightmost_peaks[i]
    from <- (onsets[i]-windowSize)
    to <- onsets[i]
    LNT <- getThreshold3(vel$vels[from:to], PT0 = 250, tolerance = 0.1)
    SToff <- alpha*STon + beta*LNT
    offset_Thresholds <- c(offset_Thresholds, SToff)
    repeat {
      if (vel$vels[nsample] < SToff)
        if ((vel$vels[nsample] - vel$vels[nsample+1]) <= 0) {
          offsets <- c(offsets, nsample)
          break
        }
      else nsample <- nsample + 1
      else nsample <- nsample + 1
      if((nsample)>=size) break
    }
  }
  ### Saccade detection
  for (i in 1:length(offsets))
    if (sum(vel$dts[onsets[i]:offsets[i]])>minSaccadeDur)
      rawEvM[onsets[i]:offsets[i]] <- ifelse(rawEvM[onsets[i]:offsets[i]]==filterGapMarker, gapMarker, sacMarker)
  
  ### Glissade detection
  for (i in 1:length(offsets)) {
    n <- ifelse((offsets[i]+windowSize)>=size,size-1,offsets[i]+windowSize)
    for (j in offsets[i]:n) {
      if (rawEvM[j]==filterOkMarker) 
        rawEvM[j] <- ifelse((vel$vels[j] > PT), 
                            gliMarker, #HighVelGlissade
                            ifelse(vel$vels[j] > offset_Thresholds[i], 
                                   gliMarker, #LowVelGlissade
                                   gapMarker))
    }
  }

  ############################
  ### Fixation detection ###
  ############################
  rawEvM <- ifelse(rawEvM == filterOkMarker, fixMarker, rawEvM)
  evmarks <- data.frame(firstEv = rawEvM[-length(rawEvM)], secondEv = rawEvM[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  
  markers <- factor(rawEvM, levels = c(detectorMarkerNames$fixMarker,
                                       detectorMarkerNames$sacMarker,
                                       detectorMarkerNames$gliMarker,
                                       detectorMarkerNames$gapMarker,
                                       detectorMarkerNames$artMarker))
  res <- list(eventMarkers = markers, 
              eventGroups = group,
              eventsClass = "OculomotorEvents", 
              detector = paste0("IDT.",detectionSettings$resultIdentifier))
  return(res)
}

IDT <- function(t, x, y, settings, filterMarkers, filterMarkerNames, detectorMarkerNames, detectionSettings)
{
  screenDist <- settings$headDistance
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  
  angular <- detectionSettings$angular
  DT <- detectionSettings$DT
  durT <- detectionSettings$durT

  filterOkMarker <- filterMarkerNames$okMarker
  filterGapMarker <- filterMarkerNames$gapMarker
  filterArtMarker <- filterMarkerNames$artMarker
  filterBliMarker <- filterMarkerNames$bliMarker
  
  fixMarker <- detectorMarkerNames$fixMarker
  sacMarker <- detectorMarkerNames$sacMarker
  gapMarker <- detectorMarkerNames$gapMarker
  
  if (angular)
  {
    angPositions <- calcAngPos(x = x, y = y, settings)
    x <- angPositions$xAng
    y <- angPositions$yAng
  }
  
  size <- length(t)
  rawEvM <- c()
  rawEvM[1:(size-1)] <- detectorMarkerNames$sacMarker
  coords <- data.frame(x=x[-size], y=y[-size], dur= t[-1]-t[-size])
  left <- 1; right <- 2
  while (right < size) {
    if (sum(coords$dur[left:right]) < durT) right <- right + 1
    else {
      d <- (max(coords$x[left:right])-min(coords$x[left:right]))+(max(coords$y[left:right])-min(coords$y[left:right]))
      if (d > DT) {
        left <- right + 1
        # left <- left + 1
        right <- left + 1
      }
      else {
        while ((d <= DT) & (right<size)) {
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


  markers <- factor(c(rawEvM, tail(rawEvM,1)), levels = c(detectorMarkerNames$fixMarker,
                                                          detectorMarkerNames$sacMarker,
                                                          detectorMarkerNames$gapMarker))
  evmarks <- data.frame(firstEv = markers[-length(rawEvM)], secondEv = markers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  
  res <- list(eventMarkers = markers, 
              eventGroups = group,
              eventsClass = "OculomotorEvents", 
              detector = paste0("IDT.",detectionSettings$resultIdentifier))
  return(res)
}

## CORE DETECTOR ##
# This detector uses specified function (IVT, IDT, Ada-NH, ...) to detect oculomotor events
oculomotorEventDetector <- function(ETD, detector, filterMarkerNames, detectorMarkerNames, detectionSettings)
{
  mode <- etd$settings$mode
  if (mode == "left" | mode == "binocular")
  {
    detectorResLeft <- detector(t = ETD$commonData$time, 
                                x = ETD$leftEyeData$porx, 
                                y = ETD$leftEyeData$pory,
                                settings = ETD$settings, 
                                filterMarkers = ETD$leftEventsData$filterEventMarkers$eventMarkers, 
                                filterMarkerNames = filterMarkerNames,
                                detectorMarkerNames = detectorMarkerNames,
                                detectionSettings = detectionSettings) 
    eventMarkersToAppend <- list(detectorResLeft)
    names(eventMarkersToAppend) <- detectorResLeft$detector
    ETD$leftEventsData <- modifyList(ETD$leftEventsData, val = eventMarkersToAppend)
  }
  if (mode == "right" | mode == "binocular")
  {
    detectorResRight <-  detector(t = ETD$commonData$time, 
                                  x = ETD$rightEyeData$porx, 
                                  y = ETD$rightEyeData$pory,
                                  settings = ETD$settings, 
                                  filterMarkers = ETD$rightEventsData$filterEventMarkers$eventMarkers,
                                  filterMarkerNames = filterMarkerNames,
                                  detectorMarkerNames = detectorMarkerNames,
                                  detectionSettings = detectionSettings) 
    eventMarkersToAppend <- list(detectorResRight)
    names(eventMarkersToAppend) <- detectorResLeft$detector
    ETD$rightEventsData <- modifyList(ETD$rightEventsData, val = eventMarkersToAppend)
  }
  return(ETD)
}