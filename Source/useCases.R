  currentEtd <- etdSmoothed
  oculomotorEvents <- currentEtd$leftEvents$IVT
  # Getting locations of different subsets of data
  ## Trials locations
  trials <- getEventsLocations(eventMarkers = currentEtd$commonEvents$trial)
  ## Blinks locations
  blinks <- getEventsLocations(eventMarkers = currentEtd$leftEvents$filterMarkers, eventType = "Blink")
  ## all events locations
  allEvents <- getEventsLocations(eventMarkers = oculomotorEvents)
  ## Fixations locations
  
  fixations <- getEventsLocations(eventMarkers = oculomotorEvents, eventType = detectorMarkerNames$fixMarker)
  ## Saccades locations
  saccades <- getEventsLocations(eventMarkers = oculomotorEvents, eventType = detectorMarkerNames$sacMarker)
  
  # Calculating trajectory parameters ---------------------------------------
  trajParams <- evaluateSubFunctions(ETD = etd,
                                     eye = "left",
                                     locations = NA,
                                     subFunctions = list(subFunctions$smpCnt,
                                                         subFunctions$duration, 
                                                         subFunctions$pupilMeanAndSD,
                                                         subFunctions$gazePointsArea,
                                                         subFunctions$dispersionXYAndRadius,
                                                         subFunctions$pathLength,
                                                         subFunctions$peakVelocity,
                                                         subFunctions$meanVelocity,
                                                         subFunctions$meanAcceleration,
                                                         subFunctions$meanDeceleration),
                                     excludeFiltered = T,
                                     okMarker = "Ok")
  # Calculating trials parameters -------------------------------------------
  trialParams <- evaluateSubFunctions(ETD = etd,
                                      eye = "left",
                                      locations = trials,
                                      subFunctions = list(subFunctions$duration, 
                                                          subFunctions$smpCnt,
                                                          subFunctions$pupilMeanAndSD,
                                                          subFunctions$gazePointsArea),
                                      excludeFiltered = T,
                                      okMarker = "Ok")

  ## trials proportion times 
  # trialParams$proportionDuration <- trialParams$duration/trajParams$duration
  
  # Calculating events parameters -------------------------------------------
  ## BLINK PARAMETERS
  bliParams <- evaluateSubFunctions(ETD = etd,
                                    eye = "left",
                                    locations = blinks,
                                    subFunctions = list(subFunctions$duration),
                                    excludeFiltered = F)
  ## FIXATION PARAMETERS
  fixParams <- evaluateSubFunctions(ETD = etd,
                                    eye = "left",
                                    locations = fixations,
                                    subFunctions = list(subFunctions$duration,
                                                        subFunctions$pupilMeanAndSD,
                                                        subFunctions$centerOfMassXY,
                                                        subFunctions$gazePointsArea,
                                                        subFunctions$dispersionXYAndRadius),
                                    excludeFiltered = T,
                                    okMarker = "Ok")
  ## SACCADE PARAMETERS
  getDataSubsets(ETD = etd, 
                 eye = "left", 
                 locations = saccades, 
                 excludeFiltered = T, 
                 okMarker = "Ok")
  sacParams <- evaluateSubFunctions(ETD = etd,
                                    eye = "left",
                                    locations = saccades,
                                    subFunctions = list(subFunctions$duration,
                                                        subFunctions$xAxisOrientation,
                                                        subFunctions$amplitude,
                                                        subFunctions$pathLength,
                                                        subFunctions$curvature,
                                                        subFunctions$peakVelocity,
                                                        subFunctions$meanVelocity,
                                                        subFunctions$meanAcceleration,
                                                        subFunctions$meanDeceleration),
                                    excludeFiltered = T,
                                    okMarker = "Ok")
  
  # Calculating derivative parameters -------------------------------------------
  ## TRAJECTORY PARAMETERS
  ## fixations points area for a whole trajectory
  ## in pixels
#   trajParams$fixPointsArea <- pointsArea(fixParams$centerX, fixParams$centerY)
#   
  ## in degrees
  angFixPositions <- calcAngPos(fixParams$centerX, fixParams$centerY, settings = etd$settings)
  trajParams$fixPointsArea <- pointsArea(angFixPositions$xAng, angFixPositions$yAng)
  
  ## TRIALS PARAMETERS
  ### Getting blinks parameters tables for each trial
  blinksInTrials <- getEventsHits(extEvLocations = trials,intEvLocations = bliParams, splitBy = "group")
  
  ### Getting fixation parameters tables for each trial
  fixationsInTrials <- getEventsHits(extEvLocations = trials, intEvLocations = fixParams, splitBy = "group")
  
  ### Getting saccade parameters tables for each trial
  saccadesInTrials <- getEventsHits(extEvLocations = trials,intEvLocations = sacParams, splitBy = "group")
  
  
  ### fixations count
  trialParams$fixCnt <- sapply(fixationsInTrials, nrow)
  
  ### total fixation duration
  trialParams$totalFixDur <- sapply(fixationsInTrials, FUN = function(x) {
    sum(x$duration, na.rm = T)
  })
  
  ### mean fixation radius
  trialParams$meanFixRadius <- sapply(fixationsInTrials, FUN = function(x) {
    mean(x$radius, na.rm = T)
  })
  
  ### mean of mean pup size x
  trialParams$meanPupSizeX <- sapply(fixationsInTrials, FUN = function(x) {
    mean(x$meanPupilSizeX, na.rm = T)
  })
  
  ### mean of mean pup size y
  trialParams$meanPupSizeY <- sapply(fixationsInTrials, FUN = function(x) {
    mean(x$meanPupilSizeY, na.rm = T)
  })
  
  ### mean gazeArea
  trialParams$meanGazeArea <- sapply(fixationsInTrials, FUN = function(x) {
    mean(x$gazePointsArea, na.rm = T)
  })
  
  ### mean fixation duration
  trialParams$meanFixDur <- trialParams$totalFixDur/trialParams$fixCnt

  ### fixation rate
  trialParams$fixRate <- trialParams$fixCnt/trialParams$duration

  ### saccades count
  trialParams$sacCnt <- sapply(saccadesInTrials, nrow)
  
  ### total saccade duration
  trialParams$totalSacDur <- sapply(saccadesInTrials, FUN = function(x) {
    sum(x$duration, na.rm = T)
  })
  
  ### mean saccade duration
  trialParams$meanSacDur <- trialParams$totalSacDur/trialParams$sacCnt
  
  ### saccade rate
  trialParams$sacRate <- trialParams$sacCnt/trialParams$duration
  
  ### blink count
  trialParams$blinkCnt <- sapply(blinksInTrials, nrow)
  
  ### total blinks duration
  trialParams$totalBlinkDur <- sapply(blinksInTrials, FUN = function(x)
  {
    sum(x$duration, na.rm = T)
  })
  
  ### mean blinks duration
  trialParams$meanSacDur <- trialParams$totalBlinkDur/trialParams$blinkCnt

  ### blink rate
  trialParams$blinkRate <- trialParams$blinkCnt/trialParams$duration
  
  ### Calculating saccades/fixations durations proportions
  trialParams$sacFixDurProp <- trialParams$totalSacDur/trialParams$totalFixDur
  
  ### Calculating fixations points area for each trial
  angFixPositions <- calcAngPos(fixParams$centerX, 
                                fixParams$centerY, 
                                settings = etd$settings)
  trialParams$fixAreaAng <- sapply(fixationsInTrials, FUN = function(x) 
  {
    angPos <- calcAngPos(x$centerX, 
                         x$centerY, 
                         settings = etd$settings)
    pointsArea(angPos$xAng, angPos$yAng)
  })


## Window locations
windows <- getEventsLocations(eventMarkers = currentEtd$commonEventsData$window)

# ### All events in trials
# evInTrials <- getEventsHits(extEvLocations = trials, intEvLocations = allEvents)
# 
# ### Fixations in trials
# fixInTrials <- getEventsHits(extEvLocations = trials, intEvLocations = fixations)
# 
# ### Saccades in trials
# sacInTrials <- getEventsHits(extEvLocations = trials, intEvLocations = saccades)

# Calculating windows parameters -------------------------------------------
# windowParams <- evaluateSubFunctions(ETD = etd2,
#                                      eye = "left",
#                                      locations = windows,
#                                      subFunctions = list(subFunctions$duration, 
#                                                          subFunctions$smpCnt,
#                                                          subFunctions$pupilMeanAndSD,
#                                                          subFunctions$gazePointsArea),
#                                      excludeFiltered = T,
#                                      okMarker = "Ok")



# Windows parameters ----------------------------------------------
# ### Getting fixation groups for each window
# fixInWind <- getEventsHits(extEvLocations = windows, intEvLocations = fixParams, splitBy = "group")
# 
# ### Calculating mean fixations durations for each sliding window
# mfdwind <- sapply(fixInWind, FUN = function(x) {
#   mean(x$duration, na.rm = T)
# })
# plot(smooth(mfdwind), type = "b")

# # ScanPath -----------------------------------------------------
# 
# 
# ### Calculating scanPath length for each trial
# angular <- T
# trialParams$scanPathLength <- sapply(trialParams$scanPath, FUN = function(scanPath){
#   if (angular)
#   {
#     angFixPositions <- calcAngPos(scanPath$x, 
#                                   scanPath$y, 
#                                   settings = etd$settings)
#     x <- angFixPositions$xAng; y <- angFixPositions$yAng
#   }
#   else
#   {
#     x <- scanPath$x; y <- scanPath$y
#   }
#   pathLength(x, y)
# })
# subFunctions$pathLength
# 
# ### Calculating scanPath orientation (from first to last fixation center) for each trial
# trialParams$scanPathOrientation <- sapply(trialParams$scanPath, FUN = function(scanPath) {
#   x0 <- scanPath$x[1]; y0 <- scanPath$y[1]
#   x1 <- tail(scanPath$x, 1); y1 <- tail(scanPath$y, 1)
#   orientationAngleXAxis(x0, y0, x1, y1)
# })
# 
# ### Calculating scanPath mean orientaion for each trial
# trialParams$scanPathMeanOrientation <- sapply(trialParams$scanPath, FUN = function(scanPath) {
#   meanAngle(orientationsPointsXY(x = scanPath$x,
#                                  y = scanPath$y))
# })
# 
# ### Calculating scanPath amplitude for each trial
# angular <- F
# trialParams$scanPathAmplitude <- sapply(trialParams$scanPath, FUN = function(scanPath) {
#   if (angular)
#   {
#     angFixPositions <- calcAngPos(scanPath$x, 
#                                   scanPath$y, 
#                                   settings = etd$settings)
#     x0 <- angFixPositions$xAng[1]; y0 <- angFixPositions$yAng[1]
#     x1 <- tail(angFixPositions$xAng, 1); y1 <- tail(angFixPositions$yAng, 1)
#   }
#   else
#   {
#     x0 <- scanPath$x[1]; y0 <- scanPath$y[1]
#     x1 <- tail(scanPath$x, 1); y1 <- tail(scanPath$y, 1)
#   }
#   amplitude(x0, y0, x1, y1)$amplitude
# })
# 
# ### Calculating scanPath curvature for each trial
# trialParams$scanPathLength/trialParams$scanPathAmplitude
