setClass("TimeSamples",
         representation(time = "numeric" # field: "time"
         ),
         prototype(
         )
)

setClass("FrameSamples",
         representation(frame = "numeric" # field: "frame"
         ),
         prototype(
         )
)

setClass("TrajectorySamples",
         representation(eyeData = "data.frame" # fields: "PORX", "PORY"
         ),
         prototype(
         )
)

setClass("PupilSamples",
         representation(pupilData = "data.frame" # fields: "PUPX", ["PUPY"]
         ),
         prototype(
         )
)

setClass("OtherSamples",
         representation(otherData = "data.frame" # any additional fields
         ),
         prototype(
         )
)

setClass("FilterMarkers",
         representation(filterMarkersData = "character",
                        markerNames = "list"
         ),
         prototype(markerNames = list(zeroes = "0", 
                                 outOfBounds = "Out of bounds",
                                 abnormalSpeed = "Abnormal speed",
                                 abnormalAcceleration = "Abnormal acceleration")
         )
)

setClass("EventMarkers",
         representation(eventMarkersData = "character",
                        markerNames = "list"
         ),
         prototype(markerNames = list(fixation = "Fixation", 
                                      saccade = "Saccade",
                                      glissade = "Glissade",
                                      smoothPursuit = "Smooth pursuit",
                                      blink = "Blink",
                                      noise = "Noise")
         )
)

setClass("FixationsData",
         representation(fixations = "data.frame" # fields: position, dispersion, onset, offset, duration, etc.
         ),
         prototype(
         )
)

setClass("SaccadesData",
         representation(saccades = "data.frame" # fields: startPosition, endPosition, amplitude, onset, offset, duration, peakVelocity, peakAcceleration, asymmetry, curvature, orientation
         ),
         prototype(
         )
)

setClass("GlissadesData",
         representation(glissades = "data.frame" # fields: the same as for saccades + antecedentSaccadeNumber
         ),
         prototype(
         )
)

setClass("SmoothPursuitsData",
         representation(smoothPursuits = "data.frame" # fields: ???
         ),
         prototype(
         )
)

setClass("BlinksData",
         representation(blinks = "data.frame" # fields: onset, offset, duration, eyePosStart, eyePosEnd, pupSizeStart, pupSizeEnd
         ),
         prototype(
         )
)

setClass("AdditionalEventData",
         representation(eventName = "character", 
                        additionalEventsData = "data.frame"
         ),
         prototype(
         )
)

setClass("EventData",
         representation(fixations = "FixationsData",
                        saccades = "SaccadesData",
                        glissades = "GlissadesData",
                        smoothPursuits = "SmoothPursuitsData",
                        blinks = "BlinksData", 
                        additionalEvents = "list" # list of AdditionalEventData objects
         ),
         prototype(
         )
)

setClass("EyesData",
         representation(fieldNames = "DataFieldNames",
                        conditions = "Conditions",
                        time = "TimeSamples",
                        frame = "FrameSamples",
                        leftEyeSamples = "TrajectorySamples",
                        rightEyeSamples = "TrajectorySamples",
                        leftPupilSamples = "PupilSamples",
                        rightPupilSamples = "PupilSamples",
                        leftAdditionalSamples = "OtherSamples",
                        rightAdditionalSamples = "OtherSamples",
                        leftFilterMarkers = "FilterMarkers",
                        rightFilterMarkers = "FilterMarkers",
                        leftEventMarkers = "EventMarkers",
                        rightEventMarkers = "EventMarkers"
         ),
         prototype(
         )
)

setClass("AOISequence",
         representation(dynamic = "logical",
                        sequence = "data.frame",
                        AOISetID = "numeric"),
         prototype()
)

setClass("AOITransMatrix",
         representation(type = "character", #freqs, probs, SR, SRnorm
                        dynamic = "logical",
                        matrices = "list", # list of matrices
                        AOISetID = "numeric"),
         prototype()
         )

setClass("AOIStatsVector",
         representation(type = "character", #freqs, probs, times
                        dynamic = "logical",
                        vectors = "list", # list of vectors
                        AOISetID = "numeric"),
         prototype()
)