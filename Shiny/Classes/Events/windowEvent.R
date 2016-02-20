setClass("WindowEventDetector",
         contains = "EventDetector")
setClass("WindowEventMarkers", 
         contains = "EventMarkers",
         representation(windows = "list")) # list of SlidingWindow objects

setClass("SlidingWindow", 
         representation(startIdx = "numeric", 
                        width = "numeric"))

setClass(Class = "WindowEvent", 
         contains = "Event", 
         representation = representation(window = "SlidingWindow",
                                         group = "numeric"))
