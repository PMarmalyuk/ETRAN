getEventsTable <- function(event, group) 
{
  if (length(event) == length(group) & 
      !is.null(event) & !is.null(group)) {
    df <- data.frame(n = 1:length(event), event = event, group = group)
    dfs <- split(df, df$group)
    locations <- lapply(dfs, FUN = function(x) {
      list(start = head(x$n, 1),
           end   = tail(x$n, 1),
           event = unique(x$event)[1],
           group = unique(x$group)[1])
    })
    locations <- rbindlist(locations)
  }
  else locations <- NULL
  return(locations)
}

getEventsPositions <- function(eventMarkers)
{
  if (all(!is.null(eventMarkers)))
  {
    markers <- eventMarkers$eventMarkers
    groups <- eventMarkers$eventGroups
    eye <- unique(eventMarkers$eye)
    df <- data.frame(position = 1:length(markers), 
                     event = markers, 
                     group = groups)
    
    dfs <- split(df, df$group)
    locations <- lapply(dfs, FUN = function(x) 
    {
      list(eye = eye,
           start = head(x$position, 1),
           end = tail(x$position, 1),
           event = unique(x$event)[1],
           group = unique(x$group)[1])
    })
    locations <- rbindlist(locations)
  } else locations <- NULL
  return(locations)
}

IVT <- function(data, VT, detectorName = "IVT") 
{
  #Считывание нужных столбцов фрейма данных
  vel <- data$vel
  #Значения маркеров событий (фиксация и саккада)
  fixMarker <- "Fixation"; sacMarker <- "Saccade"
  #Векторизованная операция проверки на превышение порога скорости
  event <- ifelse(vel <= VT, fixMarker, sacMarker)
  #Вычисление номеров групп событий
  group <- markersGroups(event)
  #Создание таблицы событий
  location <- list(getEventsTable(event, group))
  names(location) <- detectorName
  return(location)
}

ANH <- function(data, minSac = .01, minFix = .04, detectorName = "ANH") 
{
  #Считывание нужных столбцов фрейма данных
  t <- data$t
  vel <- data$vel
  #Значения маркеров событий
  fixMarker <- "Fixation"
  sacMarker <- "Saccade"
  gliMarker <- "Glissade"
  #Вспомогательный маркер
  unpMarker <- "Unclassified"
  ##Подготовка к работе
  size <- length(t); dur <- diff(t); dur <- c(dur, 0)
  #Ширина окна определяется, как минимальная продолжительность фиксации, 
  #делённая на среднюю продолжительность сэмпла
  windowSize <- round(minFix/mean(dur), 0)
  event <- rep(unpMarker, size)
  peakVel.0 <- max(vel); accur <- 1e-5
  #Адаптивный поиск порога скорости саккад
  getThreshold <- function(Vel, sigmas = 6, PT = peakVel.0, PT.cur = .9 * PT) {
    while (abs(PT - PT.cur) > accur) {
      velos <- Vel[Vel < PT.cur]
      m <- mean(velos); s <- sd(velos)
      PT.cur <- PT; PT <- m + sigmas * s
    }
    return(PT)
  }
  peakVel <- getThreshold(vel)
  #Определение саккад
  ##Поиск пиков скорости, превышающей порог
  above.Threshold <- (vel > peakVel)
  peaks <- which(above.Threshold[-1] != above.Threshold[-length(above.Threshold)])
  if (peaks[1] <= windowSize) peaks <- peaks[-c(1:2)]
  peaks.size <- length(peaks)
  if (tail(peaks, 1) >= size - windowSize) peaks <- peaks[-c(peaks.size - 1, peaks.size)]
  right.peaks <- seq(2, length(peaks), 2)
  peaks[right.peaks] <- peaks[right.peaks] + 1
  ##Разделение пиков на два вектора - левые и правые границы
  onsets <- c(); offsets <- c()
  left.peaks <- peaks[seq(1, length(peaks), 2)]
  right.peaks <- peaks[seq(2, length(peaks), 2)]
  ##Онсеты саккад
  #Порог скорости онсетов саккад
  onset.Threshold <- getThreshold(vel, 3)
  for (i in left.peaks) {
    #Номер сэмпла уменьшается до тех пор, пока не нарушится условие
    while((vel[i] >= onset.Threshold) & (vel[i] >= vel[i - 1]) & (i > 1)) 
      i <- i - 1
    #Полученный номер дополняет вектор онсетов
    onsets <- c(onsets, i)
  }
  ##Оффсеты саккад
  #Для каждого оффсета вычисляется свой порог скорости
  offset.Thresholds <- c()
  for (j in 1:length(right.peaks)) {
    i <- onsets[j]
    first <- ifelse(i - windowSize > 0, i - windowSize, 1)
    last <- i
    local.Threshold <- getThreshold(vel[first:last], 3)
    offset.Threshold <- .7 * onset.Threshold + .3 * local.Threshold
    offset.Thresholds <- c(offset.Thresholds, offset.Threshold)
    i <- right.peaks[j]
    while(((vel[i] >= offset.Threshold) & (vel[i] >= vel[i + 1])) & (i < size))
      i <- i + 1
    offsets <- c(offsets, i)
  }
  ##Онсеты глиссад
  gli.onsets <- offsets
  ##Оффсеты глиссад
  gli.offsets <- c()
  for (i in gli.onsets) {
    ##Спуск до локального минимума, подъём до локального максимума
    ##и снова спуск, для определения полной глиссады
    while((vel[i] >= vel[i + 1]) & (i < size))
      i <- i + 1
    while((vel[i] <= vel[i + 1]) & (i < size))
      i <- i + 1 
    gli.offsets <- c(gli.offsets, i)
  }
  #Определение событий
  ##Саккады
  for (i in 1:length(onsets)) {
    indexes <- onsets[i]:offsets[i]
    #Продолжительность саккады должна превышать заданный порог
    if (sum(dur[indexes]) >= minSac)
      event[indexes] <- sacMarker
  }
  ##Глиссады
  for (i in 1:length(gli.onsets)) {
    l <- gli.onsets[i]; r <- gli.offsets[i]
    #Пиковое значение скорости глиссады должно превосходить
    #порог скорости оффсета предыдущей саккады
    if (!is.na(table(vel[l:r] >= offset.Thresholds[i])["TRUE"]))
      event[l:r] <- gliMarker
  }
  ##Фиксации - всё, что не является саккадами и глиссадами
  event[event == unpMarker] <- fixMarker
  #Вычисление номеров групп событий
  group <- markersGroups(event)
  #Создание таблицы событий
  location <- list(getEventsTable(event, group))
  names(location) <- detectorName
  return(location)
}

IDT <- function(data, disp.Threshold, dur.Threshold, detectorName = "IDT") 
{
  #Считывание нужных столбцов фрейма данных
  t <- data$t
  x <- data$x
  y <- data$y
  #Значения маркеров событий (фиксация и саккада)
  fixMarker <- "Fixation"; sacMarker <- "Saccade"
  #Вспомогательные переменные
  dur <- c(diff(t), 0)
  size <- length(t)
  event <- rep(sacMarker, size)
  #Функция, вычисляющая разброс
  disp.FUN <- function(x, y) {
    return((max(x) - min(x)) + (max(y) - max(y)))
  }
  #Инициализация
  left <- 1; right <- 2
  #Алгоритм IDT
  while (right < size) {
    while (sum(dur[left:right]) < dur.Threshold & right < size) 
      right <- right + 1
    d <- disp.FUN(x[left:right], y[left:right])
    if (d <= disp.Threshold) {
      while (d <= disp.Threshold & right < size) {
        right <- right + 1
        d <- disp.FUN(x[left:right], y[left:right])
      }
      right <- right - 1
      event[left:right] <- fixMarker
    }
    left <- right + 1
    right <- left + 1
  }
  #Вычисление номеров групп событий
  group <- markersGroups(event)
  #Создание таблицы событий
  location <- list(getEventsTable(event, group))
  names(location) <- detectorName
  return(location)
}

## CORE DETECTOR ##
# This detector uses specified function (IVT, IDT, Ada-NH, ...) to detect oculomotor events
oculomotorEventDetector <- function(ETD, detector, angular = T, filterOkMarker, ...)
{
  getFilterMarkers <- function(filterLocations)
  {
    fl <- filterLocations
    fm <- lapply(1:nrow(fl), FUN = function(x) 
    {
      list(marker = rep(fl$event[x], fl$end[x]-fl$start[x] + 1))
    })
    return(rbindlist(fm))
  }
  
  mode <- etd$settings$mode
  if (mode == "left" | mode == "binocular")
  {
    if (angular)
    {
      if (is.null(ETD$leftEyeData$xAng) | is.null(ETD$leftEyeData$yAng))
      {
        stop("You should calculate angular gaze positions!")
      }
      else
      {
        x <- ETD$leftEyeData$xAng
        y <- ETD$leftEyeData$yAng
      }
      if (is.null(ETD$leftEyeData$velAng) | is.null(ETD$leftEyeData$accelAng))
      {
        stop("You should calculate angular velocities and accelerations!")
      }
      else
      {
        vel <- ETD$leftEyeData$velAng
        accel <- ETD$leftEyeData$accelAng
      }
    }
    else
    {
      x <- ETD$leftEyeData$porx
      y <- ETD$leftEyeData$pory
      if (is.null(ETD$leftEyeData$vel) | is.null(ETD$leftEyeData$accel))
      {
        stop("You should calculate velocities and accelerations!")
      }
      else
      {
        vel <- ETD$leftEyeData$vel
        accel <- ETD$leftEyeData$accel
      }
    }
    #Используем данные фильтра для удаления из фрейма данных
    #сэмплов, не соответствующих метке "ОК" фильтра
    f <- ETD$leftEvents$filter
    f <- f[f$event == filterOkMarker, c("start", "end")]
    filterOkIndexes <- unlist(apply(f, 1, function(x) seq(x[1], x[2])))
    data <- data.frame(t = ETD$commonData$time, x = x, y = y, vel = vel, accel = accel)
    data <- data[filterOkIndexes,]
    #Определение событий ГДА с помощью выбранного детектора
    detectorResLeft <- detector(data, ...)
    detectorResLeft[[1]] <- data.frame(eye = "left", detectorResLeft[[1]])
    ETD$leftEvents <- modifyList(x = ETD$leftEvents, val = detectorResLeft)
  }
  if (mode == "right" | mode == "binocular")
  {
    if (angular)
    {
      if (is.null(ETD$rightEyeData$xAng) | is.null(ETD$rightEyeData$yAng))
      {
        stop("You should calculate angular gaze positions!")
      }
      else
      {
        x <- ETD$rightEyeData$xAng
        y <- ETD$rightEyeData$yAng
      }
      if (is.null(ETD$rightEyeData$velAng) | is.null(ETD$rightEyeData$accelAng))
      {
        stop("You should calculate angular velocities and accelerations!")
      }
      else
      {
        vel <- ETD$rightEyeData$velAng
        accel <- ETD$rightEyeData$accelAng
      }
    }
    else
    {
      x <- ETD$rightEyeData$porx
      y <- ETD$rightEyeData$pory
      if (is.null(ETD$rightEyeData$vel) | is.null(ETD$rightEyeData$accel))
      {
        stop("You should calculate velocities and accelerations!")
      }
      else
      {
        vel <- ETD$rightEyeData$vel
        accel <- ETD$rightEyeData$accel
      }
    }
    #Используем данные фильтра для удаления из фрейма данных
    #сэмплов, не соответствующих метке "ОК" фильтра
    f <- ETD$rightEvents$filter
    f <- f[f$event == filterOkMarker, c("start", "end")]
    filterOkIndexes <- unlist(apply(f, 1, function(x) seq(x[1], x[2])))
    data <- data.frame(t = ETD$commonData$time, x = x, y = y, vel = vel, accel = accel)
    data <- data[filterOkIndexes,]
    #Определение событий ГДА с помощью выбранного детектора
    detectorResRight <- detector(data, ...)
    detectorResRight[[1]] <- data.frame(eye = "right", detectorResRight[[1]])
    
    ETD$rightEvents <- modifyList(ETD$rightEvents, val = detectorResRight)
  }
  return(ETD)
}