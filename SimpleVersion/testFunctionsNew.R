setwd("F:/Институт/Проекты/EyeTrackingPackage/Git/EyeTrackingProject/SimpleVersion/")
source("initialisation.R", local = T)

################## DATA READING SETTINGS ################## 
headerKeys <- list(subjectCode = list(type = "character", keyString = "Subject"),
                   fs = list(type = "numeric", keyString = "Sample Rate"),
                   headDistance = list(type = "numeric", keyString = "Head Distance [mm]"),
                   stimulusSize = list(type = "numeric", keyString = "Stimulus Dimension [mm]"))
columnsPositions <- list(commonData = list(time = 1, 
                                           trial = 3, 
                                           frame = NA, 
                                           stimName = NA, 
                                           smpFlag = 2),
                         leftEyeData = list(porx = 10,
                                            pory = 11,
                                            porz = NA,
                                            pupx = 6,
                                            pupy = 7),
                         leftAddData = list(rawx = 4,
                                            rawy = 5,
                                            crx = 8,
                                            cry = 9),
                         leftEventsData = NA,
#                          leftEventsData = list(ExtEvent1 = list(column = 12,
#                                                type = "MyArtificialEvents",
#                                                eventsNames = c("Event1", 
#                                                                "Event2", 
#                                                                "Event3",
#                                                                "LastEvent"),
#                                                detector = "ByHandDetector"),
                         rightEyeData = NA,
                         rightAddData = NA,
                         rightEventsData = NA,
                         headData = NA)

################## DATA LOADING ################## 
filePath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Git\\EyeTrackingProject\\TestData\\dataWithExternalEventsMarkers.txt"
# A LONG FILE:
# filePath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\TestDataLong\\Ephimov.txt"
etd <- readCSVData(filePath = filePath, 
                   encoding = "UTF-8",
                   infoHeaderLines = 46,
                   infoHeaderParser = findKeyValue,
                   headerKeys = headerKeys,
                   tableHeader = T,
                   sep = "\t", dec = ".", commentChar = "#", smpMarker = "SMP",
                   columnsPositions = columnsPositions)
# plot(etd$commonData$time, etd$leftEyeData$porx)
# plot(etd$leftEyeData$porx, etd$leftEyeData$pory, type = "l")

etd$settings

################## DATA PREPARATION ################## 
etd$commonData$time <- etd$commonData$time/1000000
etd$commonData$time <- etd$commonData$time - min(etd$commonData$time)
etd$settings$screenResolution <- c(1280, 1024)
etd$settings$screenSize <- c(33.7, 27)
etd$settings$headDistance <- etd$settings$headDistance*0.1

# TO DO:
# setRecordStartTimeToZero(ETD)
# scaleTime(ETD, coefficient = 0.000001)

################## DATA FILTERING ################## 
filterMarkerNames <- list(okMarker = "Ok", 
                          gapMarker = "Gap",
                          bliMarker = "Blink")

# SOMETHING WRONG WITH INTERPOLATION:
# APP IS EATING TOO MUCH MEMORY!
# RESOLVE THIS ISSUE!
etd <- dataFilter(ETD = etd, interpolate = F, filterMarkerNames = filterMarkerNames)
# etd$leftEventsData$filterEventMarkers

################## DATA SMOOTHING ################## 

smoothingSettings <- list(fl = 13, forder = 2)
etdSmoothed <- dataSmoother(ETD = etd, smoother = savGolSmoother, smoothingSettings = smoothingSettings)
plot(etdSmoothed$commonData$time, etd$leftEyeData$porx)

################## OCULOMOTOR EVENTS DETECTION ################## 
detectorMarkerNames <- list(fixMarker = "Fixation", 
                            sacMarker = "Saccade",
                            gliMarker = "Glissade",
                            smpMarker = "Smooth Pursuit",
                            gapMarker = "Gap")
etd$leftEventsData$IDT.Detection2

detectionSettings <- list(resultIdentifier = "Detection1",
                          angular = T,
                          VT = 15,
                          DT = 0.5,
                          durT = 0.03,
                          velType = "analytical",
                          fl = 33,
                          postProcess = F,
                          MaxTBetFix = 0.075,
                          MaxDistBetFix = 0.5,
                          minFixLen = 0.05,
                          maxGapLen = 0.07,
                          maxVel = 1000,
                          maxAccel = 1000000,
                          classifyGaps = F)

etd <- oculomotorEventDetector(ETD = etd, detector = IDT, 
                               filterMarkerNames = filterMarkerNames, 
                               detectorMarkerNames = detectorMarkerNames,
                               detectionSettings = detectionSettings)


plot(etd$commonData$time, etd$leftEyeData$porx, col = as.numeric(etd$leftEventsData$IDT.Detection2$eventMarkers))
plot(etd$leftEyeData$porx, etd$leftEyeData$pory, col = as.numeric(etd$leftEventsData$IDT.Detection2$eventMarkers))
fixFlags <- etd$leftEventsData$IDT.Detection2$eventMarkers == detectorMarkerNames$fixMarker
plot(etd$leftEyeData$porx[fixFlags], etd$leftEyeData$pory[fixFlags])

################## EVENTS ANALYSIS ################## 
 

################## VISUALISATIONS ################## 

### EXAMPLE PLOT OF PORX(t)

settingsInfo <- paste(apply(cbind(names(etd$settings), etd$settings), 
                            MARGIN = 1, FUN = function(x) {paste0(x[1], ": ", x[2])}),
                      collapse = "\n" )

plot(etd$commonData$time, etd$leftEyeData$porx, 
     type = "b", xlab = "time", ylab = "rawx",
     col = as.numeric(etd$leftEventsData$MyDetectionResult$eventMarkers), 
     main = paste0("Detection by: ", etd$leftEventsData$MyDetectionResult$detector,"\n", settingsInfo),
     cex.main = 0.7, cex = 0.5)
legend("bottomleft", 
       legend = levels(etd$leftEventsData$MyDetectionResult$eventMarkers),
       pch = 1,
       col = labels(levels(etd$leftEventsData$MyDetectionResult$eventMarkers)))
