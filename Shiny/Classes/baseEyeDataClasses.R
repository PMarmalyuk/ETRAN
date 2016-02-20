setClass("TimeSamples",
         representation(time = "numeric" # field: "time"
         )
)

setClass("FrameSamples",
         representation(frame = "numeric" # field: "frame"
         )
)

setClass("TrajectorySamples",
         representation(eyeData = "data.frame" # fields: "porx", "pory"
         )
)

setClass("PupilSamples",
         representation(pupilData = "data.frame" # fields: "pupx", ["pupy"]
         )
)

setClass("OtherSamples",
         representation(otherData = "data.frame" # any additional fields
         )
)

setClass("EventMarkers", representation(eventClass = "character"))

setClass(Class = "Event", 
         representation = representation(data = "data.frame"))

setClass(Class = "EventData", representation(events = "list",
                                             eventClass = "character",
                                             detectorID = "numeric"))

setClass("EyesData",
         representation(fieldNames = "DataFieldNames",
                        conditions = "Conditions",
                        time = "TimeSamples",
                        frame = "FrameSamples", # should move frame markers to left/rightEventsMarkers
                        leftEyeSamples = "TrajectorySamples",
                        rightEyeSamples = "TrajectorySamples",
                        leftPupilSamples = "PupilSamples",
                        rightPupilSamples = "PupilSamples",
                        leftAdditionalSamples = "OtherSamples",
                        rightAdditionalSamples = "OtherSamples",
                        leftEventsMarkers = "list", # list of EventMarkers objects
                        rightEventsMarkers = "list" # list of EventMarkers objects
         )
)
