install.packages("signal")
install.packages("gridExtra")
install.packages("ggplot2")
library(signal)
library(gridExtra)
library(ggplot2)

## Classes definition
setClass("EventMarkers",
         representation(eventMarkers = "character",
                        eventGroups = "numeric",
                        markerNames = "list"
         ),
         prototype(markerNames = list(fixation = "Fixation", 
                                      saccade = "Saccade",
                                      glissade = "Glissade",
                                      smoothPursuit = "Smooth pursuit",
                                      gap = "Gap",
                                      artifact = "Artifact")
         )
)

setClass("FilterMarkers",
         representation(filterMarkers = "character",
                        markerNames = "list"
         ),
         prototype(markerNames = list(ok = "Ok",
                                      zeroes = "0", 
                                      outOfBounds = "Out of bounds")
         )
)

## Functions definition
calcAngPos <- function(x, y, screenDist, screenResolution, screenSize, refPoint = c(0,0)) {
  d <- screenDist
  w <- screenSize[1]; h <- screenSize[2]
  wPx <- screenResolution[1]; hPx <- screenResolution[2]
  xAng <- (180/pi)*atan(((x-refPoint[1])/(d*wPx/w)))
  yAng <- (180/pi)*atan(((y-refPoint[2])/(d*hPx/h)))
  return(list(xAng = xAng, yAng = yAng))
}
calcVel <- function(t, x, y, settings) {
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
IVT <- function(t, x, y, filterMarkers, settings) {
  VT <- settings$VT
  angular <- settings$angular
  screenDist <- settings$screenDistance
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  postProcess <- settings$postProcess
  # 1. Velocities and accelerations estimation
  vel <- calcVel(t, x, y, settings)
  accels <- vel$accels
  
  # 2. Classification stage: getting raw event markers
  evm <- new(Class = "EventMarkers")
  gapMarkers <- ifelse(filterMarkers@filterMarkers != filterMarkers@markerNames$ok, "Gap", "Not gap")
  rawEventMarkers <- ifelse(gapMarkers[1:length(accels)] == "Gap", evm@markerNames$gap, ifelse(vel$vels[1:length(accels)] <= VT, evm@markerNames$fixation, evm@markerNames$saccade))
  evm@eventMarkers <- rawEventMarkers
  evmarks <- data.frame(firstEv = rawEventMarkers[-length(rawEventMarkers)], secondEv = rawEventMarkers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  if (!postProcess)
  {
    evm@eventGroups <- group
  }
  else
  {
    classifyGaps <- settings$classifyGaps
    MaxTBetFix <- settings$MaxTBetFix
    MaxDistBetFix <- settings$MaxDistBetFix
    minFixLen <- settings$minFixLen
    maxGapLen <- settings$maxGapLen 
    maxVel <- settings$maxVel
    maxAccel <- settings$maxAccel
    if (angular)
    {
      angPos <- calcAngPos(x, y, screenDist, screenResolution, screenSize)
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
      if (currentGroup == evm@markerNames$fixation)
      {
        # то вычисляем её длительность
        fixLen <- (eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1])
        # если фиксация короткая, то рассматриваем её как артефакт
        if (fixLen < minFixLen)
        {
          artifactGroups <- append(artifactGroups, eventGroups[gr])
          group <- group + 1
          newGroups <- c(newGroups, rep(group, nrow(eventGroups[[gr]])))
          newEvents <- c(newEvents, rep(evm@markerNames$artifact, nrow(eventGroups[[gr]])))
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
          
          if (anyGroupBefore) {prevGroupIsSaccade <- (lastGroup == evm@markerNames$saccade)}
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
            newEvents[tail(newEvents, nrow(saccadeGroups[[length(saccadeGroups)]]))] <- rep(evm@markerNames$artifact, nrow(saccadeGroups[[length(saccadeGroups)]]))
            artifactGroups <- append(artifactGroups, saccadeGroups[length(saccadeGroups)])
            eventMarkersGroups[length(eventMarkersGroups)] <- list(rep(evm@markerNames$artifact, length(eventMarkersGroups[[length(eventMarkersGroups)]])))
            saccadeGroups <- saccadeGroups[-length(saccadeGroups)]
            
            # а текущую фиксацию рассматриваем как продолжение предыдущей
            lastFixation <- list(rbind(lastFixation, eventGroups[[gr]]))
            fixationGroups[length(fixationGroups)] <- lastFixation
            lastGroup <- evm@markerNames$fixation
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(evm@markerNames$fixation, nrow(eventGroups[[gr]]))))
          }
          
          if (!anyGroupBefore | !prevGroupIsSaccade | !anyFixBefore | !fixCloseInTime | !fixCloseInSpace)
          {
            # то дополняем список фиксаций текущей фиксацией
            fixationGroups <- append(fixationGroups, eventGroups[gr])
            lastGroup <- evm@markerNames$fixation
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(evm@markerNames$fixation, nrow(eventGroups[[gr]]))))
          }
        }
      }
      # Если текущая группа сэмплов - саккада
      if (currentGroup == evm@markerNames$saccade)
      {
        # то вычисляем параметры maxVel и maxAccel
        maxSaccadeVel <- max(eventGroups[[gr]]$vel, na.rm = T)
        maxSaccadeAccel <- max(eventGroups[[gr]]$accel, na.rm = T)
        
        # и если саккада аномальна (включает сэмплы с аномальными значениями скорости или ускорения), 
        # то дополняем список артефактов этой саккадой
        if (maxSaccadeVel > maxVel | maxSaccadeAccel > maxAccel)
        {
          artifactGroups <- append(artifactGroups, eventGroups[gr])
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(evm@markerNames$artifact, nrow(eventGroups[[gr]]))))
        }
        #	если саккада не аномальна
        else
        {
          # то если предыдущая группа - саккада, то дополняем последнюю саккаду сэмплами текущей саккады
          if (!is.na(lastGroup) & lastGroup == evm@markerNames$saccade)
          {
            lastSaccade <- list(rbind(saccadeGroups[[length(saccadeGroups)]], eventGroups[[gr]]))
            saccadeGroups[length(saccadeGroups)] <- lastSaccade
            lastGroup <- evm@markerNames$saccade
            lastMarkers <- list(c(eventMarkersGroups[[length(eventMarkersGroups)]], rep(evm@markerNames$saccade, nrow(eventGroups[[gr]]))))
            eventMarkersGroups[length(eventMarkersGroups)] <- lastMarkers
          }
          else
            # иначе дополняем список саккад текущей саккадой
          {
            saccadeGroups <- append(saccadeGroups, eventGroups[gr])
            lastGroup <- evm@markerNames$saccade
            eventMarkersGroups <- append(eventMarkersGroups, list(rep(evm@markerNames$saccade, nrow(eventGroups[[gr]]))))
          }
        }    
      }
      # Если текущая группа сэмплов - пропуск
      if (currentGroup == evm@markerNames$gap)
      {
        if (classifyGaps)
        {
          # то проверяем по параметру maxGapLen, длинный ли пропуск или не длинный
          gapLen <- eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1]
          # если длинный, или lastGroup = NA, или группа является последней
          # то классифицируем его как пропуск
          if (gapLen > maxGapLen | is.na(lastGroup) | gr == length(eventGroups))
          {
            gapClass <- evm@markerNames$gap
          }
          # если не длинный, и группа не является последней, и группа не является первой
          # то классифицируем пропуск по близости сэмплов, смежных с пропуском 
          if (gapLen <= maxGapLen & gr != length(eventGroups) & !is.na(lastGroup))
          {
            
            if (lastGroup != evm@markerNames$gap & eventGroups[[gr+1]]$evm[1] != evm@markerNames$gap)
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
                  gapClass <- evm@markerNames$fixation
                }
                else
                  # если не близки в пространстве, то пропуск классифицируется как саккада
                {
                  gapClass <- evm@markerNames$saccade
                }
              }
              else
                # если не близки во времени, то пропуск классифицируется как саккада
              {
                gapClass <- evm@markerNames$saccade
              }
            }
            if (lastGroup == evm@markerNames$gap | eventGroups[[gr+1]]$evm[1] == evm@markerNames$gap)
            {
              gapClass <- evm@markerNames$gap
            }
          }
        }
        else
        {
          gapClass <- evm@markerNames$gap
        }
        
        # результат классификации пропуска позволяет отнести его сэмплы к тому или иному списку событий
        # если пропуск - длинный пропуск, то пополняем список пропусков
        if (gapClass == evm@markerNames$gap)
        {
          gapGroups <- append(gapGroups, eventGroups[gr])
          lastGroup <- evm@markerNames$gap
          eventMarkersGroups <- append(eventMarkersGroups, rep(evm@markerNames$gap, nrow(eventGroups[[gr]])))
        }
        # если пропуск - фиксация
        if (gapClass == evm@markerNames$fixation)
        {
          # то если последняя группа - фиксация, то добавляем сэмплы пропуска в эту группу
          if (lastGroup == evm@markerNames$fixation)
          {
            lastFixation <- rbind(eventGroups[[gr-1]], eventGroups[[gr]])
            fixationGroups[length(fixationGroups)] <- list(lastFixation)
            lastGroup <- evm@markerNames$fixation
            eventMarkersGroups <- append(eventMarkersGroups, rep(evm@markerNames$fixation, nrow(eventGroups[[gr]])))
          }
          # иначе добавляем новую группу в список фиксаций
          else
          {
            fixationGroups <- append(fixationGroups, eventGroups[gr])
            lastGroup <- evm@markerNames$fixation
            eventMarkersGroups <- append(eventMarkersGroups, rep(evm@markerNames$fixation, nrow(eventGroups[[gr]])))
          }
        }
        # если пропуск - саккада
        if (gapClass == evm@markerNames$saccade)
        {
          # то если последняя группа - саккада, то добавляем сэмплы пропуска в эту группу
          if (lastGroup == evm@markerNames$saccade)
          {
            lastSaccade <- rbind(eventGroups[[gr-1]], eventGroups[[gr]])
            saccadeGroups[length(saccadeGroups)] <- list(lastSaccade)
            lastGroup <- evm@markerNames$saccade
            eventMarkersGroups <- append(eventMarkersGroups, rep(evm@markerNames$saccade, nrow(eventGroups[[gr]])))
          }
          # иначе добавляем новую группу в список саккад
          if (lastGroup == evm@markerNames$fixation | lastGroup == evm@markerNames$gap)
          {
            saccadeGroups <- append(saccadeGroups, eventGroups[gr])
            lastGroup <- evm@markerNames$saccade
            eventMarkersGroups <- append(eventMarkersGroups, rep(evm@markerNames$saccade, nrow(eventGroups[[gr]])))
          }
        }
      }
    }
    evm@eventMarkers <- unlist(eventMarkersGroups)
    evm@eventGroups <- group
  }
  return(evm)
}
ANH <- function(t, x, y, filterMarkers, settings) {
  angular <- settings$angular
  screenDist <- settings$screenDistance
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  postProcess <- settings$postProcess
  velType <- settings$velType
  fl <- settings$fl
  fs <- settings$sampleRate
  if (is.na(fs))
  {
    meandt <- mean(t[-1] - t[-length(t)], na.rm = T)
    fs <- 1/meandt
  }
  screenDist <- settings$screenDistance
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  angular <- settings$angular
  velType <- settings$velType
  fl <- settings$fl
  fs <- settings$sampleRate
  size <- length(t)
  if (is.na(fs))
  {
    meandt <- mean(t[-1] - t[-size], na.rm = T)
    fs <- 1/meandt
  }
  maxSaccadeVel  <- settings$maxSaccadeVel
  maxSaccadeAcc  <- settings$maxSaccadeAcc
  minSaccadeDur  <- settings$minSaccadeDur
  minFixationDur <- settings$minFixationDur
  # Using Savitsky-Golay filter to get velocities and accelerations using derivatves of approximated x and y signals
  vel <- calcVel(t, x, y, settings)
  accels <- vel$accels
  
  evm <- new(Class = "EventMarkers")
  rawEvM <- ifelse(filterMarkers@filterMarkers != filterMarkers@markerNames$ok, "Gap", "Not Gap")[-size]
  windowSize <- floor(minFixationDur/mean(vel$dts, na.rm = T))
  ### Peak velocity Threshold calculation
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
  PT <- getThreshold(vel$vels[which(rawEvM == "Not Gap")], PT0 = 250, tolerance = 0.1, sigmaCoef = 6)
  ### Saccades detection
  ### Velocity peaks, saccades onsets and offsets search
  
  #Находим номера сэмплов начала пика и конца пика
  above_Threshold <- (vel$vels > PT)
  peaks <- which(above_Threshold[-1]!=above_Threshold[-length(above_Threshold)])
  if (peaks[1]<=windowSize) peaks <- peaks[-c(1:2)]
  #Находим onset-ы
  STon <- getThreshold(vel$vels[which(rawEvM == "Not Gap")], PT0 = 250, tolerance = 0.1, sigmaCoef = 3)
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
      rawEvM[onsets[i]:offsets[i]] <- ifelse(rawEvM[onsets[i]:offsets[i]]=="Gap",evm@markerNames$gap, evm@markerNames$saccade)
  
  ### Glissade detection
  for (i in 1:length(offsets)) {
    n <- ifelse((offsets[i]+windowSize)>=size,size-1,offsets[i]+windowSize)
    for (j in offsets[i]:n) {
      if (rawEvM[j]=="Not Gap") 
        rawEvM[j] <- ifelse((vel$vels[j] > PT), 
                            evm@markerNames$glissade, #HighVelGlissade
                            ifelse(vel$vels[j] > offset_Thresholds[i], 
                                   evm@markerNames$glissade, #LowVelGlissade
                                   evm@markerNames$gap))
    }
  }
  
  ############################
  ### Fixation detection ###
  ############################
  rawEvM <- ifelse(rawEvM == "Not Gap", evm@markerNames$fixation, rawEvM)
  evmarks <- data.frame(firstEv = rawEvM[-length(rawEvM)], secondEv = rawEvM[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  evm@eventMarkers <- rawEvM
  evm@eventGroups <- group 
  return(evm)
}
IDT <- function(t, x, y, filterMarkers, settings) {
  
  dispersionThreshold <- settings$dispersionThreshold # in px or degrees
  durationThreshold <- settings$durationThreshold # in milliseconds
  durationThreshold <- durationThreshold/1000 # now in seconds
  
  angular <- settings$angular
  screenDist <- settings$screenDist
  screenResolution <- settings$screenResolution
  screenSize <- settings$screenSize
  
  if (angular)
  {
    angPositions <- calcAngPos(x = x, y = y, screenDist, screenResolution, screenSize)
    x <- angPositions$xAng
    y <- angPositions$yAng
  }
  
  evm <- new(Class = "EventMarkers")
  
  size <- length(t)
  rawEvM <- c()
  rawEvM[1:(size-1)] <- evm@markerNames$saccade
  coords <- data.frame(x=x[-size], y=y[-size], dur= t[-1]-t[-size])
  left <- 1; right <- 2
  while (right < size) {
    if (sum(coords$dur[left:right]) < durationThreshold) right <- right + 1
    else {
      d <- (max(coords$x[left:right])-min(coords$x[left:right]))+(max(coords$y[left:right])-min(coords$y[left:right]))
      if (d > dispersionThreshold) {
        left <- right + 1
        # left <- left + 1
        right <- left + 1
      }
      else {
        while ((d <= dispersionThreshold) & (right<size)) {
          right <- right + 1
          d <- (max(coords$x[left:right])-min(coords$x[left:right]))+(max(coords$y[left:right])-min(coords$y[left:right]))
        }
        right <- right - 1
        rawEvM[left:right] <- evm@markerNames$fixation
        left <- right + 1
        right <- left + 1
      }
    }
  }
  rawEvM[which(filterMarkers@filterMarkers != filterMarkers@markerNames$ok)[-size]] <- "Gap"
  evmarks <- data.frame(firstEv = rawEvM[-length(rawEvM)], secondEv = rawEvM[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  evm@eventMarkers <- rawEvM
  evm@eventGroups <- group 
  return(evm)
}

## Settings definition
settingsVels <- list(#the next args are used to calculate angular positions and velocity
                      angular          = T,
                      screenDist       = 50, # cm
                      screenSize       = c(30, 20), # cm
                      screenResolution = c(1280,1024), # px
                      velType          = "analytical", #finDiff
                      sampleRate       = 500, # Hz   
                      fl               = 20 # msec
                    )
settingsIVT <- list(VT               = 30, # in deg/second if angular is TRUE (30 is a standard value, rule of thumb)
                    postProcess      = F # do not touch
)

settingsIDT <- list(dispersionThreshold = 1/2, # minimum DT in IDT algorithm! (see Identifying Fixations and Saccades in Eye-Tracking Protocols)
                    durationThreshold   = 100 # minimum DurationT in IDT algorithm (-||-)
)

settingsANH <- list(maxSaccadeVel    = 1000, # deg/s (all args for ANH are set to recommended values)
                    maxSaccadeAcc    = 100000, # deg/s^2
                    minSaccadeDur    = 0.01, # seconds
                    minFixationDur   = 0.04, # seconds
                    PT0              = 0.250, # seconds
                    tolerance        = 0.00001 # seconds                 
)

#-----------------------------------Визуализация----------------------------
mainpath <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI\\"
filepath <- paste(mainpath,"Marmalyuk_Yuriev_problem_solving_Lugancov_1232_Trial001 Samples.txt", sep="")
test_data = read.csv(file = filepath, header = T, dec = ".", comment.char = "#", sep = "\t")[-1,]
t <- test_data$Time/1000000 # !!! Need to convert time to seconds !!!
x <- test_data$L.POR.X..px. # !!! NOT RAW X !!! 
y <- test_data$L.POR.Y..px. # !!! NOT RAW Y !!! 

### Checking velocity plot
velos <- calcVel(t,x,y,settings)
plot(velos$vels)

### Detecting events
fm <- new(Class = "FilterMarkers", filterMarkers = rep("Ok", length(t)))
ANHdata <- ANH(t,x,y,filterMarkers = fm,append(settingsANH, settingsVels))
IDTdata <- IDT(t,x,y,filterMarkers = fm, append(settingsIDT, settingsVels))
IVTData <- IVT(t, x, y, filterMarkers = fm, settingsIVT)

indexes <- 80:180
leg_posANH <- c(.82,.84)
leg_posIDT <- c(.82,.91)


IVT_coords_plot <- qplot(x[indexes],y[indexes],
                         colour = IVTData@eventMarkers[indexes], 
                         size = I(3),
                         main = "IVT, координаты",
                         xlab = "x координата",
                         ylab = "y координата") +
  scale_colour_manual(labels = c("Фиксация","Саккада"), 
                      values = c("black", "red")) +
  theme(legend.position=leg_posANH,
        legend.title=element_blank()) 

IVTplot <- qplot(indexes,velos$vels[indexes], 
                 colour = IVTData@eventMarkers[indexes], 
                 size = I(4),
                 main = "IVT, скорости",
                 xlab = "Время",
                 ylab = "Скорость") +
  scale_colour_manual(labels = c("Фиксация","Саккада"), 
                      values = c("black", "red")) +
  theme(legend.position=leg_posANH,
        legend.title=element_blank()) 

ANH_coords_plot <- qplot(x[indexes],y[indexes],
                         colour = ANHdata@eventMarkers[indexes], 
                         size = I(3),
                         main = "AdaptiveNH, координаты",
                         xlab = "x координата",
                         ylab = "y координата") +
  scale_colour_manual(labels = c("Фиксация","Артефакт","Глиссада","Саккада"), 
                      values = c("black",   "green",  "blue",    "red")) +
  theme(legend.position=leg_posANH,
        legend.title=element_blank()) 

ANHplot <- qplot(indexes,velos$vels[indexes], 
      colour = ANHdata@eventMarkers[indexes], 
      size = I(4),
      main = "AdaptiveNH, скорости",
      xlab = "Время",
      ylab = "Скорость") +
scale_colour_manual(labels = c("Фиксация","Артефакт","Глиссада","Саккада"), 
                    values = c("black",   "green",  "blue",    "red")) +
theme(legend.position=leg_posANH,
      legend.title=element_blank()) 

IDT_coords_plot <- qplot(x[indexes],y[indexes],
                         colour = IDTdata@eventMarkers[indexes], 
                         size = I(3),
                         main = "IDT, координаты",
                         xlab = "x координата",
                         ylab = "y координата") +
  scale_colour_manual(labels = c("Фиксация","Саккада"), 
                      values = c("black",   "red")) +
  theme(legend.position=leg_posIDT,
        legend.title=element_blank())

IDTplot <- qplot(indexes,velos$vels[indexes], 
      colour = IDTdata@eventMarkers[indexes], 
      size = I(4),
      main = "IDT, скорости",
      xlab = "Время",
      ylab = "Скорость") +
scale_colour_manual(labels = c("Фиксация","Саккада"), 
                    values = c("black",   "red")) +
theme(legend.position=leg_posIDT,
      legend.title=element_blank())

grid.arrange(IVT_coords_plot,IVTplot,
             ANH_coords_plot,ANHplot,
             IDT_coords_plot,IDTplot)
