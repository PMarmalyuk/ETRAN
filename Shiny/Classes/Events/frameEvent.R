setClass("FrameEventDetector", 
         contains = "EventDetector")
setClass("FrameEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass(Class = "FrameEvent", 
         contains = "Event", 
         representation = representation(eventID = "numeric",
                                         group = "numeric"))
