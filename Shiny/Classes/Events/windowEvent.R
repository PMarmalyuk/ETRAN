setClass("WindowEventDetector",
         contains = "EventDetector",
         representation(markersDefinition = "EventMarkersDefinition")
         )
## 
setClass("WindowEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass("SlidingWindow", 
         representation(startIdx = "numeric", 
                        width = "numeric"))
setClass(Class = "WindowEvent", 
         contains = "Event", 
         representation = representation(window = "SlidingWindow"))
