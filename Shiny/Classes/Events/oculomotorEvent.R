setClass("OculomotorEventDetector", 
         contains = "EventDetector")
setClass("OculomotorEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric", groups = "numeric", detectorID = "numeric"))
setClass(Class = "OculomotorEvent", 
         contains = "Event", 
         representation = representation(eye = "character",
                                         eventID = "numeric",
                                         detectorID = "numeric",
                                         group = "numeric"))