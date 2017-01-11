setwd("~/ETRAN-final/Source")
source("initialisation.R", local = T)

## Подготовка вектора путей к файлам
Reading_raw_paths <- list.files("../Data/Gaze Data/CSV/Remote Interactive Minds - Text reading/", 
                              recursive = T, full.names = T)

################## DATA READING SETTINGS ################## 
headerKeys <- NULL
columnsPositions <- list(commonData = list(time = 6),
                         leftEyeData = list(porx = 3,
                                            pory = 4,
                                            pupSizeX = NA,
                                            pupSizeY = NA,
                                            rawx = NA,
                                            rawy = NA,
                                            crx = NA,
                                            cry = NA),
                         rightEyeData = NA,
                         commonEvents = list(trial = list(column = 1,
                                                          type = "TrialEvents")),
                         leftEvents = NA,
                         rightEvents = NA)

## Считывание данных из файлов
Reading_etd_list <- lapply(Reading_raw_paths, FUN = function(x) {
  readCSVData(filePath = x, 
              encoding = "CP1251",
              columnsPositions = columnsPositions,
              header = T,
              sep = "\t", 
              dec = ",")
})

## Подготовка к дальнейшей обработке
Reading_etd_prepared_list <- lapply(Reading_etd_list, FUN = function(x) {
  subcode <- x$settings$filename
  subcode <- sub(pattern = "s", replacement = "", x = subcode)
  subcode <- sub(pattern = "t5.txt", replacement = "", x = subcode)
  subcode <- as.numeric(subcode)
  x$settings$subjectCode <- subcode
  x
})

## Фильтрация
Reading_etd_filtered_list <- lapply(Reading_etd_prepared_list, FUN = function(x) {
  dataFilter(ETD = x)
})

## Сглаживание
Reading_etd_smoothed_list <- lapply(Reading_etd_filtered_list, FUN = function(x) {
  dataSmoother(ETD = x, 
               smoother = smoothingFunctions$medianSmoother, 
               smoothingSettings = list(fl = 15, 
                                        forder = 2))
})

## Расчёт скоростей в угловых градусах
Reading_etd_angvelacc_list <- lapply(Reading_etd_angpos_list, FUN = function(x) {
  calculateVelAcc(ETD = x, 
                  velocitySettings = 
                    list(velType = "finDiff", fl = 15, angular = F))  
})

## Определение окуломоторных событий
Reading_etd_detected_list <- lapply(Reading_etd_angvelacc_list, FUN = function(x) {
  oculomotorEventDetector(ETD = x,
                          detector = IVT,
                          filterOkMarker = "Ok",
                          VT = 30)
})

## График угловой скорости и событий
test_etd <- Reading_etd_detected_list[[30]]
t <- test_etd$commonData$time
test_events <- test_etd$leftEvents$IVT %>%
  mutate(start = c(1, start[-1] - 1),
         start = t[start],
         end = t[end])
plotChannel(t = t, value = test_etd$leftEyeData$velAng,
            events = test_events, xlim = c(30, 32))