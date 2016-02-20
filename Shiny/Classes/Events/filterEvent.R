setClass("FilterEventDetector", 
         contains = "EventDetector")
setClass("FilterEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass(Class = "FilterEvent", 
         contains = "Event", 
         representation = representation(eventID = "numeric",
                                         group = "numeric"))