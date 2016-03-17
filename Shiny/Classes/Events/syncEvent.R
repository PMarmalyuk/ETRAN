setClass("SyncEventDetector",
         contains = "EventDetector")
setClass("SyncEventMarkers", 
         contains = "EventMarkers",
         representation(markers = "numeric",
                        groups = "numeric"))
setClass(Class = "SyncEvent", 
         contains = "Event", 
         representation = representation(eventID = "numeric",
                                         group = "numeric"))