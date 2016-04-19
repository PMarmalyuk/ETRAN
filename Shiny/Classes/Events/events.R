setClass("EventMarkersDefinitions", 
         representation(definitions = "list"))

setClass("EventMarkersDefinition",
         representation(eventClass = "character", # FilterEvent, OculomotorEvent, etc.
                        eventTypesIDs = "numeric", # a vector of IDs of event types
                        typesMarkers = "character" # a vector of markers for corresponding event type IDs
         )
)

setClass(Class = "Event", 
         representation = representation(data = "data.frame"))

setClass(Class = "EventData", 
         representation(events = "list",
                        eventClass = "character",
                        detectorID = "numeric"))