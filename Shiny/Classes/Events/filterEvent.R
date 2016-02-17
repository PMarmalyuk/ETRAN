setClass("FilterEventDetector", 
         contains = "EventDetector",
         representation(markersDefinition = "EventMarkersDefinition"))
setClass("FilterEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass(Class = "FilterEvent", 
         contains = "Event", 
         representation = representation(filterEventID = "numeric",
                                         group = "numeric"))