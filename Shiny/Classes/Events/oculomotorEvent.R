setClass("OculomotorEventDetector", 
         contains = "EventDetector",
         representation(markersDefinition = "EventMarkersDefinition"))
setClass("OculomotorEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass(Class = "OculomotorEvent", 
         contains = "Event", 
         representation = representation(oculomotorEventID = "numeric",
                                         group = "numeric"))