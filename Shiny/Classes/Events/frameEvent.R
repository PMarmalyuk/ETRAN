setClass("FrameEventDetector", 
         contains = "EventDetector",
         representation(markersDefinition = "EventMarkersDefinition") 
         )
setClass("FrameEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass(Class = "FrameEvent", 
         contains = "Event", 
         representation = representation(frameID = "numeric",
                                         group = "numeric"))
