# Classes v 1.4
#########################################################################################
                      #class_declaration_section_start
#########################################################################################

## Objects of class "Parser" are intended for specifying settings for initial raw data reading from text files
## it can be our "Own" default data parser or a function specified by a user (through 'funname' and 'funpath' params)
## It is supposed that data parsing method realization is specified for "rawDataTable" class
setClass("Parser",
         representation(name = "character",
                        funname = "function",
                        funpath = "character",
                        funSettings = "list"
                        ),
         prototype(name = "Own",
                   funname = parseDataFiles,
                   funSettings = list(sep = "\t",
                                   skip = 20,
                                   comment.char = "#",
                                   header = T,
                                   splitBy = "Trial"
                                   # if character: split by column with such name (header must exist), 
                                   # if numeric: split by column number
                                   # if NA: don't split
                                   )
                   )
         )

setClass("DataSettings",
         representation(availableDataFields = "list",
                        eyeTracker = "character",
                        eyeTrackerType = "numeric", #0 - tower, 1 - remote, 2 - head-mounted
                        dataFormat = "character", #"SMI" or "tobii" or "iView" or "theNewSuperFormat", etc.
                        eye = "numeric", # 0 - left, 1 - right, 2 - both
                        sampleRate = "numeric", # frequency in Hz
                        timeUnits = "numeric", # e.g. 1E-3 for milliseconds
                        eyeToScreenDistances = "list",
                        eyeToCameraDistance = "numeric",
                        chinRestSettings = "list",
                        distancesUnits = "numeric", # e.g. 1E-2 for millimeters
                        pupilShape = "character", #"circle" or "ellipse"
                        pupilDataType = "character", #"radius" or "diameter"
                        pupilDataUnits = "character" #"px", "mm", cm", etc 
                        ),
         prototype(availableDataFields = list(time = NA, trial = NA, frame = NA, stimname = NA, smptype = NA,
                                              lporx = NA, lpory = NA, rporx = NA, rpory = NA,
                                              lpupxsize = NA, lpupysize = NA, rpupxsize = NA, rpupysize = NA,
                                              additionalFields = NA),
                                              # e.g. additionalFields = list(lrawx = "L Raw X [px]", lrawy = "L Raw Y [px]", etc.)
                   eyeToScreenDistances = list(horizontalDistance = NA, eyeBottom = NA, eyeTop = NA)
                   )
         )

# Using headerLines and settings@dataFormat it is sometimes possible to figure out some additional settings
# We need a method which scans headerLines and returns useful settings if possible
setClass("RawDataRecord",
         representation(fileNumber = "numeric",
                        filepath = "character",
                        headerLines = "vector",
                        samples = "data.frame",
                        settings = "DataSettings",
                        subjectCode = "character",
                        expID = "numeric"
         ),
         prototype(
         )
)

## There (in the package distribution) should be a function which creates the empty RawDataTable
## Data parsing method should be implemented for this class which scans text files and returns RawDataRecords
## and then fill the RawDataTable object
setClass("RawDataTable",
         representation(records = "list"
         ),
         prototype(
         )
)

setClass("EventDetector",
         representation(algorithm = "character",
                        settings = "list"
                        ),
         prototype(
         )
         )

setClass("Filter",
         representation(name = "character",
                        settings = "list"
         ),
         prototype(
         )
         )

setClass("FilterRoutine",
         representation(filterRoutineName = "character",
                        filterList = "list" #ordered list of "Filter" class objects
         ),
         prototype(
         )
         )

setClass("Experiments",
         representation(experimentsData = "list" # list of "ExperimentData" objects
                        ),
         prototype(
         )
         )

setClass("MetaExperiment",
         representation(name = "character",
                        description = "character",
                        author = "character",
                        commonSettings = "DataSettings",
                        commonFilterRoutine = "FilterRoutine",
                        commonEventDetector = "EventDetector",
                        stimuli = "list",
                        AOIL = "list",
                        TAS = "data.frame",
                        trialFactors = "data.frame",
                        stimuliFactors = "data.frame",
                        subjectFactors = "data.frame"
                        ),
         prototype(name = "Not defined",
                   description = "Not defined",
                   author = "Not defined"
                   )
         )

setClass("ExperimentData",
         representation(expID = "numeric",
                        metaData = "MetaExperiment",
                        expData = "list" #list of expData
                        ),
         prototype(
         )
         )

setClass("TrajectoryData",
         representation(eyeData = "data.frame" # fields: "PORX", "PORY"
                        ),
         prototype(
         )
         )

setClass("PupilData",
         representation(pupilData = "data.frame" # fields: "PUPX", ["PUPY"]
                        ),
         prototype(
         )
         )

setClass("EventData",
         representation(fixations = "data.frame", # fields: position, dispersion, onset, offset, duration in ms
                        saccades = "data.frame", # fields: startPosition, endPosition, amplitude, onset, offset, duration, peakVelocity, peakAcceleration, asymmetry, curvature, orientation
                        glissades = "data.frame", # fields: the same as for saccades + antecedentSaccade
                        smoothPursuits = "data.frame",
                        blinks = "data.frame", # fields: onset, offset, duration, eyePosStart, eyePosEnd, pupSizeStart, pupSizeEnd
                        noise = "data.frame"
                        ),
         prototype(
         )
         )

setClass("OtherData",
         representation(otherData = "data.frame"
                        ),
         prototype(
         )
         )

setClass("FilterMarkers",
         representation(filterMarkersData = "list",
                        markersDescription = "list"
                        ),
         prototype(markersDescription = list(zeroes = "0", 
                                             outOfBounds = "Out of bounds",
                                             abnormalSpeed = "Abnormal speed",
                                             abnormalAcceleration = "Abnormal acceleration")
                   )
         )

setClass("FilterMarkersList",
         representation(filtersResults = "list"
                        ),
         prototype(
         )
         )

setClass("EventMarkers",
         representation(eventMarkersData = "list",
                        markersDescription = "list"
                        ),
         prototype(markersDescription = list(fixation = "Fixation", 
                                             saccade = "Saccade",
                                             glissade = "Glissade",
                                             blink = "Blink",
                                             smoothPursuit = "Smooth pursuit",
                                             noise = "Noise")
                   )
         )

setClass("EyesData",
         representation(time = "list",
                        frame = "list",
                        leftEyeSamples = "TrajectoryData",
                        rightEyeSamples = "TrajectoryData",
                        leftPupilSamples = "PupilData",
                        rightPupilSamples = "PupilData",
                        leftOtherSamples = "OtherData",
                        rightOtherSamples = "OtherData",
                        leftFilterResults = "FilterMarkersList",
                        rightFilterResults = "FilterMarkersList",
                        leftEventDetectionResults = "EventMarkers",
                        rightEventDetectionResults = "EventMarkers",
                        leftEyeEvents = "EventData",
                        rightEyeEvents = "EventData"
                        ),
         prototype(
         )
         )

setClass("ExpData",
         representation(subjectCode = "character",
                        trialID = "numeric",
                        eyeData = "EyesData",
                        specificDataSettings = "DataSettings",
                        specificEventSettings = "EventDetector",
                        specificFilterRoutineSettings = "FilterRoutine"
                        ),
         prototype(
         )
         )

setClass("AOI",
         representation(AOIID = "numeric",
                        name = "character",
                        type = "factor",
                        shape = "factor",
                        characteristics = "list",
                        description = "character"
                        ),
         prototype(name = "Not defined",
                   description = "Not defined"
                   )
         )



setClass("AOISet",
         representation(AOISet = "list"
                        ),
         prototype(
         )
         )


setClass("AOIList",
         representation(ListOfAOIS = "list"
                        ),
         prototype(
         )
         )


setClass("Stimulus",
         representation(stimulusID = "numeric",
                        name = "character",
                        description = "character",
                        path = "character",
                        dimensions = "list"
                        ),
         prototype(stimulusID = -1,
                   name = "Not defined",
                   description = "Not defined",
                   path = "Not defined"
                   )
         )

# !!! Must create events settings object also !!!

####################################################################################
                        #class_declaration_section_end#
####################################################################################