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
         representation(eyeData = "data.frame" # fields: "porx", "pory"
         ),
         prototype(
         )
)

setClass("PupilSamples",
         representation(pupilData = "data.frame" # fields: "pupx", ["pupy"]
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
         representation(filterMarkers = "character",
                        markerNames = "list"
         ),
         prototype(markerNames = list(ok = "Ok",
                                      zeroes = "0", 
                                      outOfBounds = "Out of bounds")
         )
)

setClass("EventMarkers",
         representation(eventMarkers = "character",
                        eventGroups = "numeric",
                        markerNames = "list"
         ),
         prototype(markerNames = list(fixation = "Fixation", 
                                      saccade = "Saccade",
                                      glissade = "Glissade",
                                      smoothPursuit = "Smooth pursuit",
                                      gap = "Gap",
                                      artifact = "Artifact")
         )
)

# setClass("FixationsData",
#          representation(fixations = "data.frame" # fields: position, dispersion, onset, offset, duration, etc.
#          ),
#          prototype(
#          )
# )
# 
# setClass("SaccadesData",
#          representation(saccades = "data.frame" # fields: startPosition, endPosition, amplitude, onset, offset, duration, peakVelocity, peakAcceleration, asymmetry, curvature, orientation
#          ),
#          prototype(
#          )
# )
# 
# setClass("GlissadesData",
#          representation(glissades = "data.frame" # fields: the same as for saccades + antecedentSaccadeNumber
#          ),
#          prototype(
#          )
# )
# 
# setClass("SmoothPursuitsData",
#          representation(smoothPursuits = "data.frame" # fields: ???
#          ),
#          prototype(
#          )
# )
# 
# setClass("GapsData",
#          representation(gaps = "data.frame" # fields: startPosition, endPosition, onset, offset, duration, pupSizeStart, pupSizeEnd
#          ),
#          prototype(
#          )
# )
# 
# setClass("ArtifcatsData",
#          representation(artifacts = "data.frame" # fields: startPosition, endPosition, onset, offset, duration, peakVelocity, peakAcceleration 
#          ),
#          prototype(
#          )
# )
# 
# 
# 
# setClass("AdditionalEventData",
#          representation(eventName = "character", 
#                         additionalEventsData = "data.frame"
#          ),
#          prototype(
#          )
# )
# 
# setClass("EventData",
#          representation(fixations = "FixationsData",
#                         saccades = "SaccadesData",
#                         glissades = "GlissadesData",
#                         smoothPursuits = "SmoothPursuitsData",
#                         gaps = "GapsData",
#                         artifacts = "ArtifcatsData",
#                         additionalEvents = "list" # list of AdditionalEventData objects
#          ),
#          prototype(
#          )
# )

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
#                         ,
#                         leftAOIMarkers = "AOIHitsMarkers",
#                         rightAOIMarkers = "AOIHitsMarkers",
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