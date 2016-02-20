setClass("AOIEventDetector", 
         contains = "EventDetector")
setClass("AOIEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass(Class = "AOIEvent", 
         contains = "Event", 
         representation = representation(eventID = "numeric",
                                         group = "numeric"))
