setClass("SyncEventDetector",
         contains = "EventDetector",
         representation(markersDefinition = "EventMarkersDefinition")
         )
setClass("SyncEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric"))
setClass(Class = "SyncEvent", 
         contains = "Event", 
         representation = representation(syncEventID = "numeric",
                                         group = "numeric"))