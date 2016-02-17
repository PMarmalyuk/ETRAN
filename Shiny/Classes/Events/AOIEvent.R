setClass("AOIEventDetector", 
         contains = "EventDetector",
         representation(markersDefinition = "EventMarkersDefinition"))
setClass("AOIEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass(Class = "AOIEvent", 
         contains = "Event", 
         representation = representation(AOIEventID = "numeric",
                                         group = "numeric"))
