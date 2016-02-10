# Core Sub Functions Initialization
# factors <- new(Class = "AvailableFactors")
subFunctions <- new(Class = "SubFunctions")

# TO DO: add default settings for each subFunction

## Creating core subfunctions
## EVENT STATISTICS
### Validity code of an event
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getValCode, 
                                                               name = "ValidityCode", 
                                                               description = "Validity code of an event",
                                                               applyTo = "Oculomotor Events", 
                                                               applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit"),
                                                               operation = "Event Analysis"))
### Onset, offset and duration of an event
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                                         fun = getOnOffSetDuration, 
                                                                         name = "OnOffsetDuration", 
                                                                         description = "Onset, offset and duration of an event",
                                                                         applyTo = "Oculomotor Events", 
                                                                         applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Gap", "Artifact"),
                                                                         operation = "Event Analysis"))
### Start and end positions of an event
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getStartEndPositionsXY, 
                                                               name = "StartEndPositionsXY", 
                                                               description = "Start and end positions of an event (along X and Y axis)",
                                                               applyTo = "Oculomotor Events",
                                                               applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Artifact"),
                                                               settings = list(angular = F),
                                                               operation = "Event Analysis"))
### Coordinates of center of mass of event samples 
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getCenterOfMassXY, 
                                                               name = "CenterOfMass", 
                                                               description = "Coordinates of center of mass of an event (along X and Y axis)",
                                                               applyTo = "Oculomotor Events", 
                                                               applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Artifact"),
                                                               settings = list(angular = F), 
                                                               operation = "Event Analysis"))

### Dispersion and radius of an event
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getDispersionXYAndRadius, 
                                                               name = "DispersionXYAndRadius", 
                                                               description = "Dispersion (along X and Y axis) and radius of an event",
                                                               applyTo = "Oculomotor Events", 
                                                               applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Artifact"),
                                                               settings = list(angular = F), 
                                                               operation = "Event Analysis"))
### Mean and sd of pupil size during an event
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getPupilMeanAndSD, 
                                                               name = "MeanAndSDOfPupilSize", 
                                                               description = "Mean and sd of pupil size during an event",
                                                               applyTo = "Oculomotor Events", 
                                                               applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Artifact"),
                                                               operation = "Event Analysis"))

### Amplitudes of an event
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getAmplitude, 
                                                               name = "Amplitudes", 
                                                               description = "1D Amplitudes along X and Y axis and 2D amplitude",
                                                               applyTo = "Oculomotor Events", 
                                                               applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Artifact"),
                                                               settings = list(angular = F), 
                                                               operation = "Event Analysis"))

### Length and curvature of a path of a part of a trajectory related to processed event
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getPathLengthAndCurvature, 
                                                               name = "PathLengthAndCurvature", 
                                                               description = "Length and curvature of a path of a part of a trajectory related to a processed event",
                                                               applyTo = "Oculomotor Events", 
                                                               applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Artifact"),
                                                               settings = list(angular = F), 
                                                               operation = "Event Analysis"))
### Peak velocity, ac/deceleration during event and accel/decel asymmetry
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getPeakVelAcDecelAndAsymmetry, 
                                                               name = "PeakVelAcDecelAndAsymmetry", 
                                                               description = "Peak velocity, ac/deceleration during event and accel/decel asymmetry",
                                                               applyTo = "Oculomotor Events", 
                                                               applyWhen = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Artifact"),
                                                               settings = list(angular = T, 
                                                                               velType = "finDiff",
                                                                               fl = 40),
                                                               operation = "Event Analysis"))
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = getXAxisOrientation, 
                                                               name = "Orientation", 
                                                               description = "Get orientation of event samples in degrees relative to x-axis",
                                                               applyTo = "Oculomotor Events",
                                                               applyWhen = c("Saccade", "Glissade", "Smooth Pursuit"),
                                                               operation = "Event Analysis"))


## TRAJECTORY STATISTICS
### Total duration (sec)
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = trajDurationEstimator, 
                                                               name = "Duration of a record", 
                                                               description = "Get duration of a record",
                                                               applyTo = "Eyes Data", 
                                                               operation = "Record Analysis"))

### Length (px/angles)
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = trajLengthEstimator, 
                                                               name = "Length of a record", 
                                                               description = "Get length of a record",
                                                               applyTo = "Eyes Data", 
                                                               settings = list(angular = F), 
                                                               operation = "Record Analysis"
))
### Event Counter
subFunctions <- addSubFunction(subFunctions, subFunction = new(Class = "SubFunction", 
                                                               fun = eventCounter, 
                                                               name = "Event Counts", 
                                                               description = "Get counts of events",
                                                               applyTo = "Eyes Data",
                                                               operation = "Record Analysis"))