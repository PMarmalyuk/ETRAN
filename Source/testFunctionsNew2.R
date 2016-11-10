setwd("F:/Институт/Проекты/EyeTrackingPackage/Git/EyeTrackingProject/Source")
setwd("~/ETRAN-master/Source")
source("initialisation.R", local = T)

################## DATA LOADING ################## 
# filePath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI\\Ephimov.txt"
filePath = "Ephimov-eye_data.txt"
filePath = "1.txt"
etd <- readCSVData(filePath = filePath, 
                   encoding = "UTF-8",
                   columnsPositions = columnsPositions,
                   header = T,
                   sep = "\t", 
                   dec = ".", 
                   infoLinesCnt = 46,
                   infoParser = findKeyValue,
                   headerKeys = headerKeys,
                   commentChar = "#",
                   msgMarker = "MSG",
                   msgFlagColumn = 2, 
                   msgKey = "Message")
etd$settings$colNames
################## DATA PREPARATION ################## 
{
etd$commonData$time <- etd$commonData$time/1000000
etd$commonData$time <- etd$commonData$time - min(etd$commonData$time)
etd$settings$headDistance <- etd$settings$headDistance * .1
etd$settings$screenResolution <- c(1280, 1024)
etd$settings$screenSize <- c(33.7, 27)
}
# POSSIBLE PACKAGE STRUCTURE:
# loadData ($data)
# parseHeaderLines ($settings)
# loadEvents ($events)
# (column with trial or stimulus name/inline messages with 
# clicks, trial or stimulus change)
# dataSelector (dataTable, eventsPosTable, select, exclude)
# data filter (t, x, y, interpShort, minDur, detectBlinks)
# data smoother (x, y, type, fl, forder)
# angular x, y calculator (x, y, headDist, scrRes, scrSize)
# vel/accel calculator (t, x, y, type, fl)
# IVT(t, vel, VT)
# IDT(t, x, y, DT, minFixDur)
# ANH(t, vel)
# eventsPostProcess(blinks, ocEvents, )
# AOI Detector (t, x, y)
# functions for events parameters calculation
# params calculator (dataList, functions)
# functions for visualisations


# DATA FILTERING ----------------------------------------------------------
etdFiltered <- dataFilter(ETD = etd)
sf <- standardFilter(t = etd$commonData$time, x = etd$leftEyeData$porx, y = etd$leftEyeData$pory)
str(sf)
sf$eye <- "left"
getEventsPositions(sf)
# DATA SMOOTHING ----------------------------------------------------------
etdSmoothed <- dataSmoother(ETD = etdFiltered, 
                            smoother = smoothingFunctions$medianSmoother, 
                            smoothingSettings = list(fl = 15, 
                                                     forder = 2))

# VELOCITIES AND ACCELERATION CALCULATION ---------------------------------
etdSmoothed <- calculateAngPos(ETD = etdSmoothed)
etdSmoothed <- calculateVelAcc(ETD = etdSmoothed, 
                               velocitySettings = 
                                 list(velType = "analytical", fl = 15))

# OCULOMOTOR EVENTS DETECTION ---------------------------------------------
etdSmoothed <- oculomotorEventDetector(ETD = etdSmoothed,
                                       detector = IVT,
                                       filterOkMarker = "Ok",
                                       VT = 30)

etdSmoothed <- oculomotorEventDetector(ETD = etdSmoothed,
                                       detector = ANH,
                                       filterOkMarker = "Ok")

str(etdSmoothed$leftEvents)
# EVENTS ANALYSIS ----------------------------------------------------------
fixParams <- evaluateSubFunctions(ETD = etdSmoothed,
                                  eye = "left",
                                  locations = etdSmoothed$leftEvents$IVT,
                                  events = "Fixation",
                                  subFunctions = list(subFunctions$duration,
                                                      subFunctions$pupilMeanAndSD,
                                                      subFunctions$centerOfMassXY,
                                                      subFunctions$gazePointsArea,
                                                      subFunctions$dispersionXYAndRadius),
                                  excludeFiltered = T)
fixParams
# VISUALISATIONS EXAMPLES --------------------------------------------------
###For channels
#Time
{
t <- etdSmoothed$commonData$time

x.name <- "X" 
x <- etdSmoothed$leftEyeData$porx

y.name <- "Y"
y <- etdSmoothed$leftEyeData$pory

pupil.name <- "Раскрытие зрачка (пикс.)"
pupil <- etdSmoothed$leftEyeData$pupSizeX

angVel.name <- "Скорость (град./сек.)"
angVel <- etdSmoothed$leftEyeData$velAng

angAccel.name <- "Ускорение (град./сек.^2)"
angAccel <- etdSmoothed$leftEyeData$accelAng
}

{
filterEvents <- etdSmoothed$leftEvents$filter
filterEvents$start <- t[filterEvents$start]
filterEvents$end <- t[filterEvents$end]
levels(filterEvents$event)
levels(filterEvents$event) <- c("Моргание", "Корректные \nданные")
}
plotChannel(t = t, value = as.numeric(y), value.name = "Y", events = filterEvents, xlim = c(121,123),
            title = NULL, xlab = "Время (сек)", ylab = "Y-координата взора (пикс.)",
            events.title = "События", 
            channels.title = "Каналы \nзаписи")
{
IVTEvents <- etdSmoothed$leftEvents$IVT
IVTEvents$start <- t[IVTEvents$start]
IVTEvents$end <- t[IVTEvents$end]
levels(IVTEvents$event)
levels(IVTEvents$event) <- c("Фиксация", "Саккада")
}
{
ANHEvents <- etdSmoothed$leftEvents$ANH
ANHEvents$start <- t[ANHEvents$start]
ANHEvents$end <- t[ANHEvents$end]
levels(ANHEvents$event)
levels(ANHEvents$event) <- c("Фиксация", "Глиссада", "Саккада")
}
plotChannel(t = t, value = angVel, value.name = angVel.name, events = IVTEvents, 
            xlim = c(121, 122), ylim = c(0, 500),
            title = NULL, xlab = "Время (сек)", 
            ylab = "Моментальная угловая скорость (град./сек.)",
            events.title = "События", 
            channels.title = NULL)

plotChannel(t = t, value = angVel, value.name = angVel.name, events = ANHEvents, 
            xlim = c(121, 122), ylim = c(0, 500),
            title = NULL, xlab = "Время (сек)", 
            ylab = "Моментальная угловая скорость (град./сек.)",
            events.title = "События", 
            channels.title = NULL)

plotChannel(t = t, value = angAccel, value.name = angAccel.name, events = IVTEvents, xlim = c(121, 122), ylim = c(-200, 200),
            title = NULL, xlab = "Время (сек)", 
            ylab = "Моментальное угловое ускорение (град./сек.)",
            events.title = "События", 
            channels.title = NULL)

plotChannel(t = t, value = list(x, y), value.name = c(x.name, y.name), events = IVTEvents, xlim = c(121, 122), ylim = c(0, 1000),
            title = NULL, xlab = "Время (сек)", 
            ylab = "Координата (пикс.)",
            events.title = "События", 
            channels.title = "Каналы \nзаписи")

## XY-plot
stim <- readJPEG("3.jpg")
IVTE <- etdSmoothed$leftEvents$IVT
ev <- unlist(apply(IVTE, 1, function(x) rep(x[4], as.numeric(x[3]) - as.numeric(x[2]))))

plotStimulus(stimulus = stim, x = x, y = y)
plotStimulus(stimulus = stim, x = x, y = y, add.background.line = F)
plotStimulus(stimulus = stim, x = x, y = y, add.background.line = F, xlim = c(100, 1500), ylim = c(100, 200))
plotStimulus(stimulus = stim, x = x, y = y, add.background.line = F, xlim = c(100, 1500), ylim = c(100, 200), fixed.aspect.ratio = F)
plotStimulus(stimulus = stim, 
             title = NULL,
             xlab = "X координата (пикс.)",
             ylab = "Y координата (пикс.)",
             legend.title = "События", 
             x = x[3000:10000], 
             y = y[3000:10000], 
             events = ev[3000:10000], 
             xlim = c(0, 1280),
             ylim = c(50, 400),
             add.background.line = T,
             point.size = 2,
             point.alpha = 0.5)

## Heatmap
fixParams2 <- fixParams[c("centerX", "centerY")]
fixParams2 <- fixParams2[complete.cases(fixParams2),]

plotHeatmap(stimulus = stim, 
            title = NULL,
            x = fixParams2$centerX, 
            y = fixParams2$centerY, 
            xlab = NULL, ylab = NULL, 
            xlim = c(0, 1280),
            ylim = c(0, 1024),
            legend.title = F,
            gridsize = c(40, 30))

## Scanpath
fixParams2 <- fixParams[c("centerX", "centerY", "duration")]
fixParams2 <- fixParams2[complete.cases(fixParams2),]

plotScanpath(x.center = fixParams2$centerX, 
             y.center = fixParams2$centerY, 
             duration = fixParams2$duration, 
             xlab = "X, пикс.", ylab = "Y, пикс.",
             title = "'Путь сканирования' на участке стимула", legend.title = "Длительность (сек.)", 
             plotting.indexes = 70:100, 
             legend.levels = 10, 
             arrow.color = "green", 
             arrow.head = 20, 
             arrow.alpha = .4, 
             xlim = c(400, 1280), 
             ylim = c(200, 400))

plotScanpath(x.center = fixParams2$centerX, 
             y.center = fixParams2$centerY, 
             duration = fixParams2$duration, 
             xlab = "X, пикс.", ylab = "Y, пикс.",
             title = "'Путь сканирования' на участке стимула", legend.title = "Длительность (сек.)", 
             plotting.indexes = 70:100, 
             legend.levels = 10, 
             arrow.color = "green", 
             arrow.head = 20, 
             arrow.alpha = .4, 
             xlim = c(400, 1280), 
             ylim = c(200, 400), fixed.aspect.ratio = F)

# SAMPLE ANALYSIS ----------------------------------------------------------
filesFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI\\"
files <- paste0(filesFolder, list.files(path = filesFolder))

allRecordsTrialsParams <- data.frame()
sizesAndTimes <- data.frame()

for (i in 1:length(files))
{
  print(paste("Current trajectory = ", i))
  etd <- readCSVData(filePath = files[i], 
                     encoding = "UTF-8",
                     columnsPositions = columnsPositions,
                     header = T,
                     sep = "\t", 
                     dec = ".", 
                     infoLinesCnt = 46,
                     infoParser = findKeyValue,
                     headerKeys = headerKeys,
                     commentChar = "#",
                     msgMarker = "MSG",
                     msgFlagColumn = 2, 
                     msgKey = "Message")
  print("Reading ok")
  t0 <- unclass(Sys.time())
  file <- etd$settings$filename
  size <- as.numeric(object.size(etd))
  ################## DATA PREPARATION ################## 
  etd$commonData$time <- etd$commonData$time/1000000
  etd$commonData$time <- etd$commonData$time - min(etd$commonData$time)
  etd$settings$headDistance <- etd$settings$headDistance * .1
  etd$settings$screenResolution <- c(1280, 1024)
  etd$settings$screenSize <- c(33.7, 27)
  # Creating sliding window
  etd <- addWindow(ETD = etd, width = 1000, overlap = 600)
  
  # DATA FILTERING ----------------------------------------------------------
  print("Start filtering")
  
  etd <- dataFilter(ETD = etd, 
                    filterMarkerNames = filterMarkerNames, 
                    filterSettings = list(interpolateShort = T,
                                          blinkDetection = T,
                                          minGapDuration = 0.02,
                                          smoothLen = 3))
  print("Filter ok")
  # DATA SMOOTHING ----------------------------------------------------------
  etd <- dataSmoother(ETD = etd, 
                      smoother = smoothingFunctions$medianSmoother, 
                      smoothingSettings = list(fl = 33, 
                                               forder = 2))
  print("Smoothing ok")
  # VELOCITIES AND ACCELERATION CALCULATION ---------------------------------
  etd <- calculateAngPos(ETD = etd)
  etd <- calculateVelAcc(ETD = etd, 
                         velocitySettings = list(velType = "analytical",
                                                 fl = 13))
  print("Vel ok")
  # OCULOMOTOR EVENTS DETECTION ---------------------------------------------
  etd <- oculomotorEventDetector(ETD = etd, detector = IVT, 
                                 filterMarkerNames = filterMarkerNames, 
                                 detectorMarkerNames = detectorMarkerNames,
                                 detectionSettings = settingsList$IVTSettings)
  print("Detection ok")
  ################## EVENTS ANALYSIS ################## 
  trials <- etd$commonEvents$trial
  windows <- etd$commonEvents$window
  eventsFilter <- etd$leftEvents$filter
  eventsIVT <- etd$leftEvents$IVT
  blinks <- eventsFilter[eventsFilter$event == filterMarkerNames$bliMarker]
  fixations <- eventsIVT[eventsIVT$event == detectorMarkerNames$fixMarker]
  saccades <- eventsIVT[eventsIVT$event == detectorMarkerNames$sacMarker]
  
  
  # Calculating trajectory parameters ---------------------------------------
  trajParams <- evaluateSubFunctions(ETD = etd,
                                     eye = "left",
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
  print("trajectory params ok")
  # Calculating trials parameters -------------------------------------------
  trialParams <- evaluateSubFunctions(ETD = etd,
                                      eye = "left",
                                      locations = etd$commonEvents$trial,
                                      subFunctions = list(subFunctions$duration, 
                                                          subFunctions$smpCnt,
                                                          subFunctions$pupilMeanAndSD,
                                                          subFunctions$gazePointsArea),
                                      excludeFiltered = T,
                                      okMarker = "Ok")
  print("trial params ok")
  trialParams$file <- file
  
  ## trials proportion times 
  # trialParams$proportionDuration <- trialParams$duration/trajParams$duration
  
  
  
  # Calculating events parameters -------------------------------------------
  ## BLINK PARAMETERS
  bliParams <- evaluateSubFunctions(ETD = etd,
                                    eye = "left",
                                    locations = blinks,
                                    subFunctions = list(subFunctions$duration))
  print("blink params ok")
  ## FIXATION PARAMETERS
  fixParams <- evaluateSubFunctions(ETD = etd,
                                    eye = "left",
                                    locations = fixations,
                                    subFunctions = list(subFunctions$duration,
                                                        subFunctions$pupilMeanAndSD,
                                                        subFunctions$centerOfMassXY,
                                                        subFunctions$gazePointsArea,
                                                        subFunctions$dispersionXYAndRadius))
  ## SACCADE PARAMETERS
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
                                                        subFunctions$meanDeceleration))
  print("saccade params ok")
  # Calculating derivative parameters -------------------------------------------
  ## TRIALS PARAMETERS
  ### Getting blinks parameters tables for each trial
  blinksInTrials <- getEventsHits(extEvLocations = trials,intEvLocations = bliParams, splitBy = "group")
  print("blinksInTrials")
  ### Getting fixation parameters tables for each trial
  fixationsInTrials <- getEventsHits(extEvLocations = trials, intEvLocations = fixParams, splitBy = "group")
  print("fixationsInTrials")
  ### Getting saccade parameters tables for each trial
  saccadesInTrials <- getEventsHits(extEvLocations = trials,intEvLocations = sacParams, splitBy = "group")
  print("saccadesInTrials")
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
  print("finished")
  allRecordsTrialsParams <- rbind(allRecordsTrialsParams, trialParams)
  t1 <- unclass(Sys.time())
  sizesAndTimes <- rbind(sizesAndTimes, data.frame(size = size, dur = t1-t0))
}
sizesAndTimes$size <- sizesAndTimes$size/(1024^2)
summary(lm(dur ~ size, sizesAndTimes))

