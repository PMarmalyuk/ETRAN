setwd("~/ETRAN-final/Source")
source("initialisation.R", local = T)

## Подготовка вектора путей к файлам
Raven_raw_paths <- list.files("../Data/Gaze Data/CSV/SMI RED - Raven's test/", 
                              recursive = T, full.names = T)

################## DATA READING SETTINGS ################## 
headerKeys <- list(subjectCode = list(type = "character", 
                                      keyString = "Subject"),
                   taskCode = list(type = "character",
                                   keyString = "Stimulus"),
                   fs = list(type = "numeric", 
                             keyString = "Sample Rate"),
                   headDistance = list(type = "numeric", 
                                       keyString = "Head Distance [mm]"),
                   stimulusSize = list(type = "numeric", 
                                       keyString = "Stimulus Dimension [mm]"),
                   screenResolution = list(type = "numeric",
                                           keyString = "Calibration Area"))
columnsPositions <- list(commonData = list(time = 1),
                         leftEyeData = list(porx = 22,
                                            pory = 23,
                                            pupSizeX = 8,
                                            pupSizeY = 9,
                                            rawx = 4,
                                            rawy = 5,
                                            crx = 14,
                                            cry = 15),
                         rightEyeData = list(porx = 24,
                                             pory = 25,
                                             pupSizeX = 10,
                                             pupSizeY = 11,
                                             rawx = 6,
                                             rawy = 7,
                                             crx = 18,
                                             cry = 19),
                         commonEvents = list(trial = list(column = 3,
                                                          type = "TrialEvents")),
                         leftEvents = NA,
                         rightEvents = NA)

## Считывание данных из файлов
Raven_etd_list <- lapply(Raven_raw_paths, FUN = function(x) {
  readCSVData(filePath = x, 
                     encoding = "UTF-8",
                     columnsPositions = columnsPositions,
                     header = T,
                     sep = "\t", 
                     dec = ".", 
                     infoLinesCnt = 43,
                     infoParser = findKeyValue,
                     headerKeys = headerKeys,
                     commentChar = "#",
                     msgMarker = "MSG",
                     msgFlagColumn = 2, 
                     msgKey = "Message")
})

## Подготовка к дальнейшей обработке
Raven_etd_prepared_list <- lapply(Raven_etd_list, FUN = function(x) {
  x$commonData$time <- x$commonData$time/1000000
  x$commonData$time <- x$commonData$time - min(x$commonData$time)
  x$settings$headDistance <- x$settings$headDistance * .1
  x$settings$screenSize <- c(33.7, 27)
  x$settings$subjectCode <- as.numeric(sub(pattern = "S", replacement = "", 
                                           x = x$settings$subjectCode))
  x$settings$taskCode <- as.numeric(sub(pattern = ".jpg", replacement = "", 
                                        x = x$settings$taskCode))
  x
})

## Фильтрация
Raven_etd_filtered_list <- lapply(Raven_etd_prepared_list, FUN = function(x) {
  dataFilter(ETD = x)
})

## Сглаживание
Raven_etd_smoothed_list <- lapply(Raven_etd_filtered_list, FUN = function(x) {
  dataSmoother(ETD = x, 
               smoother = smoothingFunctions$medianSmoother, 
               smoothingSettings = list(fl = 15, 
                                        forder = 2))
})

## Пересчёт координат в угловые градусы
Raven_etd_angpos_list <- lapply(Raven_etd_smoothed_list, FUN = function(x) {
  calculateAngPos(ETD = x)
})

## Расчёт скоростей в угловых градусах
Raven_etd_angvelacc_list <- lapply(Raven_etd_angpos_list, FUN = function(x) {
  calculateVelAcc(ETD = x, 
                  velocitySettings = 
                    list(velType = "finDiff", fl = 15))  
})

## Определение окуломоторных событий
Raven_etd_detected_list <- lapply(Raven_etd_angvelacc_list, FUN = function(x) {
  oculomotorEventDetector(ETD = x,
                          detector = IVT,
                          filterOkMarker = "Ok",
                          VT = 30)
})

## График угловой скорости и событий
test_etd <- Raven_etd_detected_list[[30]]
t <- test_etd$commonData$time
test_events <- test_etd$leftEvents$IVT %>%
  mutate(start = c(1, start[-1] - 1),
         start = t[start],
         end = t[end])
plotChannel(t = t, value = test_etd$leftEyeData$velAng,
            events = test_events, xlim = c(30, 32))
