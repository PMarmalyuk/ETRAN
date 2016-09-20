createWindow <- function(smpCnt, width, overlap)
{
  notOverlap <- (width-overlap)
  numOfWindows <- (smpCnt - width) %/% notOverlap + 1
  startPositions <- (0:(numOfWindows-1))*notOverlap + 1
  endPositions <- startPositions + width
  events <- data.frame(eye = NA, 
                       start = startPositions, 
                       end = endPositions, 
                       event = rep("Window", numOfWindows), 
                       group = 1:numOfWindows,
                       stringsAsFactors = F)
  return(events)
}

# etd$leftEventsData$WindowEventsGenerator <- createWindow(width = 100, overlap = 10, name = "MyWindow")
setWindow <- function(ETD, width, overlap)
{
  smpCnt <- length(ETD$commonData$time)
  window <- createWindow(smpCnt = smpCnt, width = width, overlap = overlap)
  ETD$commonEvents$window <- window
  return(ETD)
}
