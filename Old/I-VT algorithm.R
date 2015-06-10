# TO DO: estimate max saccade acceleration
# TO DO: evaluate closeness of fixations in space by center of their mass
IVT <- function(t, x, y, filterMarkers, VT, angular = F, screenDist, screenDim, screenSize, 
                MaxTBetFix, MaxDistBetFix, minFixLen, maxGapLen, maxVel, maxAccel, classifyGaps)
{
  # 1. Velocities estimation
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

  # 2. Classification stage: getting raw event markers
  gapMarkers <- ifelse(filterMarkers@filterMarkersData != filterMarkers@markerNames$ok, "GAP", "NOT GAP")
#   print(markers1)
#   if (!is.na(screenDim)[1])
#   {
#     markers2 <- ifelse(x > screenDim[1] | y > screenDim[2], filterMarkers@markerNames$outOfBounds, filterMarkers@markerNames$ok)
#     markers1[which(markers1 == filterMarkers@markerNames$ok)] <- markers2[which(markers1 == filterMarkers@markerNames$ok)]
#   }
#   gapMarkers <- ifelse(markers1 != filterMarkers@markerNames$ok, "GAP", "NOT GAP")

  rawEventMarkers <- ifelse(gapMarkers[-length(gapMarkers)] == "GAP", "GAP", ifelse(vel$vels <= VT, "Fixation", "Saccade"))
  #print(rawEventMarkers)
  # 3. Post-processing stage
  evmarks <- data.frame(firstEv = rawEventMarkers[-length(rawEventMarkers)], secondEv = rawEventMarkers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  events <- data.frame(t = t[-length(t)], x = x[-length(t)], y = y[-length(t)], vel = vel$vels, evm = rawEventMarkers, gr = group)
  eventGroups <- split(events, group)
  fixationGroups <- list()
  saccadeGroups <- list()
  gapGroups <- list()
  artifactGroups <- list()
  eventMarkers <- new(Class = "EventMarkers")
  eventMarkersGroups <- list()
  lastGroup = NA
  for (gr in 1:length(eventGroups))
  {
    currentGroup <- eventGroups[[gr]]$evm[1]
    # Если текущая группа сэмплов - фиксация
    if (currentGroup == "Fixation")
    {
      # то вычисляем её длительность
      fixLen <- eventGroups[[gr]]$t[nrow(eventGroups[[gr]])]-eventGroups[[gr]]$t[1]
      # если фиксация короткая
      if (fixLen < minFixLen)
      {
          # то если ей предшествовала саккада, то рассматриваем её как часть прошлой саккады
          if (!is.na(lastGroup) & lastGroup == "Saccade")
          {
            lastSaccade <- list(rbind(saccadeGroups[[length(saccadeGroups)]], eventGroups[[gr]]))
            saccadeGroups[length(saccadeGroups)] <- lastSaccade
            lastGroup <- "Saccade"
            lastMarkers <- list(rep(eventMarkers@markerNames$saccade, nrow(saccadeGroups[[length(saccadeGroups)]])))
            eventMarkersGroups[length(eventMarkersGroups)] <- lastMarkers
          }
          else
          {
            artifactGroups <- append(artifactGroups, eventGroups[gr])
            eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$artifact, nrow(eventGroups[[gr]])))
          }
      }
      # если фиксация не короткая
      if (fixLen >= minFixLen)
      {
        # и до этого ещё не была выделена группа сэмплов
        if (is.na(lastGroup))
        {
          # то дополняем список фиксаций текущей фиксацией
          fixationGroups <- append(fixationGroups, eventGroups[gr])
          lastGroup <- "Fixation"
          eventMarkersGroups <- append(eventMarkersGroups, list(rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]]))))
        }
        # и предыдущая группа - саккада
        if (!is.na(lastGroup) & lastGroup == "Saccade")
        {
          # и уже были обнаружены фиксации до этого
          if (length(fixationGroups) != 0)
          {
            # то определяем время и позицию начала текущей фиксации
            fixTime <- eventGroups[[gr]]$t[1]
            fixPos <- c(eventGroups[[gr]]$x[1], eventGroups[[gr]]$y[1])
            # а также время и позицию конца последней обнаруженной фиксации
            lastFixation <- tail(fixationGroups, n = 1)[[1]]
            lastFixTime <- lastFixation$t[nrow(lastFixation)]
            lastFixPos <- c(lastFixation$x[nrow(lastFixation)], lastFixation$y[nrow(lastFixation)])
            # и оцениваем, близки ли эти две фиксации во времени и пространстве
            if (lastFixTime - fixTime <= MaxTBetFix)
            {
              # если они близки во времени, то оцениваем расстояние в пространстве
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
              # если они близки и в пространстве
              if (dist <= MaxDistBetFix)
              {
                # то предыдущую саккаду рассматриваем как артефакт записи
                artifactGroups <- append(artifactGroups, saccadeGroups[length(saccadeGroups)])
                eventMarkersGroups[length(eventMarkersGroups)] <- list(rep(eventMarkers@markerNames$artifact, length(eventMarkersGroups[[length(eventMarkersGroups)]])))
                saccadeGroups <- saccadeGroups[-length(saccadeGroups)]
                # а текущую фиксацию рассматриваем как продолжение предыдущей
                lastFixation <- list(rbind(lastFixation, eventGroups[[gr]]))
                fixationGroups[length(fixationGroups)] <- lastFixation
                lastGroup <- "Fixation"
                eventMarkersGroups <- append(eventMarkersGroups, list(rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]]))))
              }
              # если они не близки в пространстве, то дополняем список фиксаций текущей фиксацией
              else
              {
                fixationGroups <- append(fixationGroups, eventGroups[gr])
                lastGroup <- "Fixation"
                eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]])))
              }
            }
            else
            # если они не близки во времени, то дополняем список фиксаций текущей фиксацией
            {
              fixationGroups <- append(fixationGroups, eventGroups[gr])
              lastGroup <- "Fixation"
              eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]])))
            }
          }
          # если фиксаций ранее не было обнаружено, то дополняем список фиксаций текущей фиксацией
          else
          {
            fixationGroups <- append(fixationGroups, eventGroups[gr])
            lastGroup <- "Fixation"
            eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]])))
          }
        }
        # если предыдущее событие - пропуск, то дополняем список фиксаций текущей фиксацией
        if (lastGroup == "GAP")
        {
          fixationGroups <- append(fixationGroups, eventGroups[gr])
          lastGroup <- "Fixation"
          eventMarkersGroups <- append(eventMarkersGroups, rep(eventMarkers@markerNames$fixation, nrow(eventGroups[[gr]])))
        }
      }
    }
    # Если текущая группа сэмплов - саккада
    if (currentGroup == "Saccade")
    {
      # то вычисляем параметры maxVel и maxAccel
      maxSaccadeVel <- max(eventGroups[[gr]]$vel)
      maxSaccadeAccel <- 0
      
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
  res <- list(ev = rawEventMarkers, vel = vel$vels, evm = eventMarkers, fixations = fixationGroups, saccades = saccadeGroups, gaps = gapGroups, artifacts = artifactGroups)
  
  # 4. Events' parameters estimation stage

  # 5. Return eventMarkers and eventData objects
  return(res)
}