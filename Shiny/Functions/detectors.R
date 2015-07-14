IVT <- function(t, x, y, filterMarkers, settings)
{
  VT <- settings$VT
  angular <- settings$angular
  screenDist <- settings$screenDist
  screenDim <- settings$screenDim
  screenSize <- settings$screenSize
  postProcess <- settings$postProcess
  if (postProcess)
  {
    classifyGaps <- settings$classifyGaps
    MaxTBetFix <- settings$MaxTBetFix
    MaxDistBetFix <- settings$MaxDistBetFix
    minFixLen <- settings$minFixLen
    maxGapLen <- settings$maxGapLen 
    maxVel <- settings$maxVel
    maxAccel <- settings$maxAccel
  }
  else
  {
    # 1. Velocities and accelerations estimation
    if (!angular)
    {
      vel <- calcPxVel(t, x, y)
      vel$vels <- as.numeric(smooth(vel$vels, kind = "3"))
    } 
    else
    {
      vel <- calcAngVel(t, x, y, screenDist, screenDim, screenSize)
      vel$vels <- as.numeric(smooth(vel$vels, kind = "3"))
    }
    accels <- c(((vel$vels[-1]-vel$vels[-length(vel$vels)])/vel$dts[-length(vel$dts)]), 0)
    # 2. Classification stage: getting raw event markers
    evm <- new(Class = "EventMarkers")
    gapMarkers <- ifelse(filterMarkers@filterMarkers != filterMarkers@markerNames$ok, "Gap", "Not gap")
    rawEventMarkers <- ifelse(gapMarkers[-length(gapMarkers)] == "Gap", "Gap", ifelse(vel$vels <= VT, evm@markerNames$fixation, evm@markerNames$saccade))
    evm@eventMarkers <- rawEventMarkers
    
    evmarks <- data.frame(firstEv = rawEventMarkers[-length(rawEventMarkers)], secondEv = rawEventMarkers[-1])
    transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
    group <- c(1,cumsum(transitions)+1)
    evm@eventGroups <- group
  }
  return(evm)
}

# TO DO: implement post processing
# TO DO: evaluate closeness of fixations in space by center of their mass
IVTWithPostProcess <- function(t, x, y, filterMarkers, settings)
{
  VT <- settings$VT
  angular <- settings$angular
  screenDist <- settings$screenDist
  screenDim <- settings$screenDim
  screenSize <- settings$screenSize
  MaxTBetFix <- settings$MaxTBetFix
  MaxDistBetFix <- settings$MaxDistBetFix
  minFixLen <- settings$minFixLen
  maxGapLen <- settings$maxGapLen 
  maxVel <- settings$maxVel
  maxAccel <- settings$maxAccel
  classifyGaps <- settings$classifyGaps
  
  # 1. Velocities and accelerations estimation
  if (!angular)
  {
    vel <- calcPxVel(t, x, y)
    vel$vels <- as.numeric(smooth(vel$vels, kind = "3"))
  } 
  else
  {
    vel <- calcAngVel(t, x, y, screenDist, screenDim, screenSize)
    vel$vels <- as.numeric(smooth(vel$vels, kind = "3"))
  }
  accels <- c(((vel$vels[-1]-vel$vels[-length(vel$vels)])/vel$dts[-length(vel$dts)]), 0)
  # 2. Classification stage: getting raw event markers
  gapMarkers <- ifelse(filterMarkers@filterMarkersData != filterMarkers@markerNames$ok, "GAP", "NOT GAP")
  rawEventMarkers <- ifelse(gapMarkers[-length(gapMarkers)] == "GAP", "GAP", ifelse(vel$vels <= VT, "Fixation", "Saccade"))
  # 3. Post-processing stage
  
  
  evmarks <- data.frame(firstEv = rawEventMarkers[-length(rawEventMarkers)], secondEv = rawEventMarkers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  events <- data.frame(t = t[-length(t)], x = x[-length(t)], y = y[-length(t)], dls = vel$dists, dts = vel$dts, vel = vel$vels, accel = accels, evm = rawEventMarkers, gr = group)
  eventGroups <- split(events, group)
  fixationGroups <- list()
  saccadeGroups <- list()
  gapGroups <- list()
  artifactGroups <- list()
  eventMarkers <- new(Class = "EventMarkers")
  eventMarkersGroups <- list()
  group <- 0
  newGroups <- c()
  newEvents <- c()
  lastGroup = NA
  for (gr in 1:length(eventGroups))
  {
    currentGroup <- eventGroups[[gr]]$evm[1]
    # Если текущая группа сэмплов - фиксация
    if (currentGroup == "Fixation")
    {
      # то вычисляем её длительность
      fixLen <- eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1]
      # если фиксация короткая, то рассматриваем её как артефакт
      if (fixLen < minFixLen)
      {
        artifactGroups <- append(artifactGroups, eventGroups[gr])
        group <- group + 1
        newGroups <- c(newGroups, rep(group, nrow(eventGroups[[gr]])))
        newEvents <- c(newEvents, rep(eventMarkers@markerNames$artifact, nrow(eventGroups[[gr]])))
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
        
        if (anyGroupBefore) {prevGroupIsSaccade <- (lastGroup == "Saccade")}
        if (prevGroupIsSaccade) {anyFixBefore <- (length(fixationGroups) != 0)}
        if (anyFixBefore) 
        {
          fixTime <- eventGroups[[gr]]$t[1]
          lastFixation <- tail(fixationGroups, n = 1)[[1]]
          lastFixTime <- lastFixation$t[nrow(lastFixation)]
          fixCloseInTime <- (lastFixTime - fixTime <= MaxTBetFix)
        }
        if (fixCloseInTime) 
        {
          fixPos <- c(eventGroups[[gr]]$x[1], eventGroups[[gr]]$y[1])
          lastFixPos <- c(lastFixation$x[nrow(lastFixation)], lastFixation$y[nrow(lastFixation)])
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
          fixCloseInSpace <- (dist <= MaxDistBetFix)
        }
        if (fixCloseInSpace)
        {
          # то предыдущую саккаду рассматриваем как артефакт записи
          newEvents[tail(newEvents, nrow(saccadeGroups[[length(saccadeGroups)]]))] <- rep(nrow(eventMarkers@markerNames$artifact, saccadeGroups[[length(saccadeGroups)]]))
          artifactGroups <- append(artifactGroups, saccadeGroups[length(saccadeGroups)])
          eventMarkersGroups[length(eventMarkersGroups)] <- list(rep(eventMarkers@markerNames$artifact, length(eventMarkersGroups[[length(eventMarkersGroups)]])))
          saccadeGroups <- saccadeGroups[-length(saccadeGroups)]
          
          # а текущую фиксацию рассматриваем как продолжение предыдущей
          lastFixation <- list(rbind(lastFixation, eventGroups[[gr]]))
          fixationGroups[length(fixationGroups)] <- lastFixation
          lastGroup <- "Fixation"
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]]))))
        }
        
        if (!anyGroupBefore | !prevGroupIsSaccade | !anyFixBefore | !fixCloseInTime | !fixCloseInSpace)
        {
          # то дополняем список фиксаций текущей фиксацией
          fixationGroups <- append(fixationGroups, eventGroups[gr])
          lastGroup <- "Fixation"
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]]))))
        }
      }
    }
    # Если текущая группа сэмплов - саккада
    if (currentGroup == "Saccade")
    {
      # то вычисляем параметры maxVel и maxAccel
      maxSaccadeVel <- max(eventGroups[[gr]]$vel)
      maxSaccadeAccel <- max(abs(eventGroups[[gr]]$accel))
      
      # и если саккада аномальна (включает сэмплы с аномальными значениями скорости или ускорения), 
      # то дополняем список артефактов этой саккадой
      if (maxSaccadeVel > maxVel | maxSaccadeAccel > maxAccel)
      {
        artifactGroups <- append(artifactGroups, eventGroups[gr])
        eventMarkersGroups <- append(eventMarkersGroups, list(rep(eventMarkers@markerNames$artifact, nrow(eventGroups[[gr]]))))
      }
      #	если саккада не аномальна
      else
      {
        # то если предыдущая группа - саккада, то дополняем последнюю саккаду сэмплами текущей саккады
        if (!is.na(lastGroup) & lastGroup == "Saccade")
        {
          lastSaccade <- list(rbind(saccadeGroups[[length(saccadeGroups)]], eventGroups[[gr]]))
          saccadeGroups[length(saccadeGroups)] <- lastSaccade
          lastGroup <- "Saccade"
          lastMarkers <- list(c(eventMarkersGroups[[length(eventMarkersGroups)]], rep(eventMarkers@markerNames$saccade, nrow(eventGroups[[gr]]))))
          eventMarkersGroups[length(eventMarkersGroups)] <- lastMarkers
        }
        else
          # иначе дополняем список саккад текущей саккадой
        {
          saccadeGroups <- append(saccadeGroups, eventGroups[gr])
          lastGroup <- "Saccade"
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(eventMarkers@markerNames$saccade, nrow(eventGroups[[gr]]))))
        }
      }    
    }
    # Если текущая группа сэмплов - пропуск
    if (currentGroup == "GAP")
    {
      if (classifyGaps)
      {
        # то проверяем по параметру maxGapLen, длинный ли пропуск или не длинный
        gapLen <- eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1]
        # если длинный, или lastGroup = NA, или группа является последней
        # то классифицируем его как пропуск
        if (gapLen > maxGapLen | is.na(lastGroup) | gr == length(eventGroups))
        {
          gapClass <- "GAP"
        }
        # если не длинный, и группа не является последней, и группа не является первой
        # то классифицируем пропуск по близости сэмплов, смежных с пропуском 
        if (gapLen <= maxGapLen & gr != length(eventGroups) & !is.na(lastGroup))
        {
          
          if (lastGroup != "GAP" & eventGroups[[gr+1]]$evm[1] != "GAP")
          {
            lastSmpBeforeGap <- eventGroups[[gr-1]][nrow(eventGroups[[gr-1]]),]
            firstSmpAfterGap <- eventGroups[[gr+1]][1,]
            t1 <- lastSmpBeforeGap$t
            t2 <- firstSmpAfterGap$t
            pos1 <- c(lastSmpBeforeGap$x, lastSmpBeforeGap$y)
            pos2 <- c(firstSmpAfterGap$x, firstSmpAfterGap$y)
            if (t2-t1 <= MaxTBetFix)
            {
              if (!angular)
              {
                pxVel <- calcPxVel(t = c(t2, t1),
                                   x = c(pos2[1], pos1[1]),
                                   y = c(pos2[2], pos1[2]))
                dist <- pxVel$dist
              }
              else
              {
                angVel <- calcAngVel(t = c(t2, t1),
                                     x = c(pos2[1], pos1[1]),
                                     y = c(pos2[2], pos1[2]),
                                     screenDist,
                                     screenDim,
                                     screenSize)
                dist <- angVel$dist
              }
              # если смежные с пропуском сэмплы близки во времени и пространстве, 
              # то он классифицируется как фиксация
              if (dist <= MaxDistBetFix)
              {
                gapClass <- "Fixation"
              }
              else
                # если не близки в пространстве, то пропуск классифицируется как саккада
              {
                gapClass <- "Saccade"
              }
            }
            else
              # если не близки во времени, то пропуск классифицируется как саккада
            {
              gapClass <- "Saccade"
            }
          }
          if (lastGroup == "GAP" | eventGroups[[gr+1]]$evm[1] == "GAP")
          {
            gapClass <- "GAP"
          }
        }
      }
      else
      {
        gapClass <- "GAP"
      }
      
      # результат классификации пропуска позволяет отнести его сэмплы к тому или иному списку событий
      # если пропуск - длинный пропуск, то пополняем список пропусков
      if (gapClass == "GAP")
      {
        gapGroups <- append(gapGroups, eventGroups[gr])
        lastGroup <- "GAP"
        eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$gap, nrow(eventGroups[[gr]])))
      }
      # если пропуск - фиксация
      if (gapClass == "Fixation")
      {
        # то если последняя группа - фиксация, то добавляем сэмплы пропуска в эту группу
        if (lastGroup == "Fixation")
        {
          lastFixation <- rbind(eventGroups[[gr-1]], eventGroups[[gr]])
          fixationGroups[length(fixationGroups)] <- list(lastFixation)
          lastGroup <- "Fixation"
          eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]])))
        }
        # иначе добавляем новую группу в список фиксаций
        else
        {
          fixationGroups <- append(fixationGroups, eventGroups[gr])
          lastGroup <- "Fixation"
          eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]])))
        }
      }
      # если пропуск - саккада
      if (gapClass == "Saccade")
      {
        # то если последняя группа - саккада, то добавляем сэмплы пропуска в эту группу
        if (lastGroup == "Saccade")
        {
          lastSaccade <- rbind(eventGroups[[gr-1]], eventGroups[[gr]])
          saccadeGroups[length(saccadeGroups)] <- list(lastSaccade)
          lastGroup <- "Saccade"
          eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$saccade, nrow(eventGroups[[gr]])))
        }
        # иначе добавляем новую группу в список саккад
        if (lastGroup == "Fixation" | lastGroup == "GAP")
        {
          saccadeGroups <- append(saccadeGroups, eventGroups[gr])
          lastGroup <- "Saccade"
          eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$saccade, nrow(eventGroups[[gr]])))
        }
      }
    }
  }
  eventMarkers@eventMarkersData <- unlist(eventMarkersGroups)
  res <- list(eventMarkers = eventMarkers,
              eventGroups = 
                list(
                  fixationGroups = fixationGroups, 
                  saccadeGroups = saccadeGroups,
                  glissadeGroups = NA,
                  smoothPursuitGroups = NA,
                  gapGroups = gapGroups, 
                  artifactGroups = artifactGroups
                )
  )
  return(res)
}



## CORE DETECTOR ##
# This detector uses specified method (I-VT, I-DT, Ada-NH) to detect oculomotor events
# and also it uses eventGroupsAnalyser function to extract events' parameters
coreDetector <- function(DataRecord, settings)
{
  scrDim <- DataRecord@eyesDataObject@conditions@conditions$screenDim
  scrSize <- DataRecord@eyesDataObject@conditions@conditions$screenSize
  dist <- DataRecord@eyesDataObject@conditions@conditions$screenDist
  t <- DataRecord@eyesDataObject@time@time
  
  algorithm <- settings$subfun

  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    filterMarkers <- DataRecord@eyesDataObject@leftFilterMarkers
    res <- algorithm(t = t, x = leftX, y = leftY, filterMarkers, settings)
    DataRecord@eyesDataObject@leftEventMarkers <- res
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    filterMarkers <- DataRecord@eyesDataObject@rightFilterMarkers
    res <- algorithm(t = t, x = rightX, y = rightY, filterMarkers, settings)
    DataRecord@eyesDataObject@rightEventMarkers <- res
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    leftFilterMarkers <- DataRecord@eyesDataObject@leftFilterMarkers
    rightFilterMarkers <- DataRecord@eyesDataObject@rightFilterMarkers
    resLeft <- algorithm(t = t, x = leftX, y = leftY, leftFilterMarkers, settings)
    resRight <- algorithm(t = t, x = rightX, y = rightY, rightFilterMarkers, settings)
    DataRecord@eyesDataObject@leftEventMarkers <- resLeft
    DataRecord@eyesDataObject@rightEventMarkers <- resRight
  }
  return(DataRecord)
}