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
                  # angular = T,
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

## Вычисление матриц представления преемника и выделение
## паттернов методом главных компонент
all_SRs <- matrix(ncol = 100)
colnames(all_SRs) <- paste0("SR", 1:100)

for (i in 1:length(Raven_etd_detected_list)) {
  test_etd <- Raven_etd_detected_list[[i]]
  t <- test_etd$commonData$time
  x <- test_etd$leftEyeData$porx
  y <- test_etd$leftEyeData$pory
  aoi_vector <- dAOI(t, x, y, aoi$data) %>% 
    filter(event != "_milk") %>% 
    dplyr::select(event) %>% 
    unlist()
  aoi_vector <- as.numeric(levels(aoi_vector))[aoi_vector]
  SR <- getSR(size = 10, classified = aoi_vector)
  names(SR) <- paste0("SR", 1:100)
  all_SRs <- rbind(all_SRs, SR)
}

all_SRs <- all_SRs[-1,]

loads <- princomp(all_SRs)

loading <- 1
m = matrix(data = loads$loadings[, loading], ncol = 10, byrow=T)
palette <- colorRampPalette(colors = c("#0000FF","#FFFFFF","#FF0000"))
levelplot(m, col.regions = palette(100), ylim=c(10.5,0.5), 
          main = paste(loading," component loadings",sep=""), 
          xlab = "", ylab = "", useRaster = T)
