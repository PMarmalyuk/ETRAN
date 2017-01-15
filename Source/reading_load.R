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
  x$settings$headDistance <- 70
  x$settings$screenSize <- c(33.7, 27)
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
                                        forder = 3))
})

## Расчёт скоростей в угловых градусах
Reading_etd_velacc_list <- lapply(Reading_etd_smoothed_list, FUN = function(x) {
  calculateVelAcc(ETD = x, angular = F,
                  velocitySettings = 
                    list(velType = "analytical", fl = 15))  
})

## Определение окуломоторных событий
Reading_etd_detected_list <- lapply(Reading_etd_velacc_list, FUN = function(x) {
  oculomotorEventDetector(ETD = x,
                          detector = IVT,
                          filterOkMarker = "Ok",
                          VT = 30,
                          angular = F)
})

## График угловой скорости и событий
test_etd <- Reading_etd_detected_list[[60]]
t <- test_etd$commonData$time
test_events <- test_etd$leftEvents$IVT %>%
  mutate(start = c(1, start[-1] - 1),
         start = t[start],
         end = t[end])
levels(test_events$event) <- c("Фиксация", "Саккада")
val <- test_etd$leftEyeData$vel
plotChannel(t = t, value = val, events = test_events,
            xlim = c(1, 2.5), ylim = c(0, 150),
            value.name = "град/сек", line.size = 1.2, 
            xlab = "Время, сек", ylab = "Угловая скорость",
            title = "Зависимость угловой скорости от времени", 
            events.title = "Событие", channels.title = "Канал",
            events.color.palette = adjustcolor(c("green", "red")))

# График координат и событий
plotChannel(t = t, value = list(
  "Х координата" = test_etd$leftEyeData$porx,
  "Y координата" = test_etd$leftEyeData$pory
), events = test_events,
            xlim = c(8, 12.5), line.size = 1.2, 
            xlab = "Время, сек", ylab = "Координата, пикс",
            title = "Зависимость координат от времени", 
            events.title = "Событие", channels.title = "Канал",
            events.color.palette = adjustcolor(c("green", "red")))

# Подготовка данных для графиков на стимуле
test_etd <- Reading_etd_detected_list[[60]]
events <- test_etd$leftEvents$IVT
levels(events$event) <- c("Фиксация", "Саккада")
events_vector <- c()
for (i in 1:nrow(events)) {
  events_vector[events$start[i]:events$end[i]] <- as.character(events$event[i])
}
plotting_indexes <- 1000:25000
x <- test_etd$leftEyeData$porx
y <- test_etd$leftEyeData$pory
events_vector <- events_vector
stimulus <- readJPEG("../Data/Stimuli/Text reading/1424.jpg")
# График точек взора на стимуле
plotStimulus(stimulus = stimulus, x = x[plotting_indexes], y = y[plotting_indexes],
             add.background.line = F, events = events_vector[plotting_indexes],
             xlab = NULL, ylab = NULL, title = "Точки взора на стимуле",
             xlim = c(320, 960), ylim = c(256, 768), grid.alpha = .15,
             point.alpha = .3, point.size = 4, fixed.aspect.ratio = T,
             legend.title = "Событие")

# Параметры фиксаций
fixParams <- evaluateSubFunctions(ETD = Reading_etd_detected_list[[60]],
                                  eye = "left",
                                  locations = Reading_etd_detected_list[[60]]$leftEvents$IVT,
                                  events = "Fixation",
                                  subFunctions = list(subFunctions$duration,
                                                      subFunctions$centerOfMassXY),
                                  excludeFiltered = T)

# Тепловая карта
plotHeatmap(stimulus = stimulus, 
            x = fixParams$centerX, y = fixParams$centerY, w = fixParams$duration,
            xlab = NULL, ylab = NULL, title = "Тепловая карта",
            xlim = c(320, 960), ylim = c(256, 768),
            percent.to.hide = .35, gridsize = c(512, 512), 
            H = matrix(c(500, 0, 0, 500), nrow = 2),
            legend.title = "Плотность")

# График "Путь сканирования"
plotScanpath(x.center = fixParams$centerX, y.center = fixParams$centerY,
             duration = fixParams$duration, stimulus = stimulus,
             xlab = NULL, ylab = NULL, title = "Путь сканирования",
             xlim = c(320, 960), ylim = c(256, 768),
             plotting.indexes = seq(5, 850, 10),
             legend.title = "Продолжи-\nтельность\nфиксации", legend.levels = 7,
             fix.max.size = 15, draw.label = F, draw.arrow = T, arrow.head = 15,
             arrow.angle = 15, arrow.color = "green")
