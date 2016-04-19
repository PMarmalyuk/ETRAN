createWindow <- function(name, width, overlap)
{
  return(list(eventsClass = "WindowEvents", 
              window = list(width = width, overlap = overlap),
              detector = name))
}
# etd$leftEventsData$WindowEventsGenerator <- createWindow(width = 100, overlap = 10, name = "MyWindow")

