# Classes v 1.4
#########################################################################################
                      #class_declaration_section_start
#########################################################################################

## Objects of class "Parser" are intended for specifying settings for initial raw data reading from text files
## it can be our "Own" default data parser or a function specified by a user (through 'funname' and 'funpath' params)
## It is supposed that data parsing method realization is specified for "rawDataTable" class

#################
### OPERATORS ###
#################
setClass("DataLoader",
         representation(name = "character",
                        fun = "function",
                        settings = "list"
                        ),
         prototype(name = "Own",
                   fun = loadDataFile,
                   settings = list(sep = "\t",
                                   skip = 20,
                                   comment.char = "#",
                                   header = T,
                                   splitBy = "Trial"
                                   )
                   )
         )

setClass("DataParser",
         representation(name = "character",
                        fun = "function",
                        settings = "list"
         ),
         prototype(name = "Own",
                   fun = parseRawDataRecord,
                   settings = list(
                   )
         )
)

setClass("EventDetector",
         representation(name = "character",
                        fun = "function",
                        settings = "list"
         ),
         prototype(name = "IVT",
                   fun = IVT
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


###############
### OPTIONS ###
###############
setClass("RawDataSettings",
         representation(availableDataFields = "list",
                        eyeTracker = "character",
                        eyeTrackerType = "numeric", #0 - tower, 1 - remote, 2 - head-mounted
                        dataFormat = "character", #"SMI" or "tobii" or "iView" or "theNewSuperFormat", etc.
                        ),
         prototype(availableDataFields = list(time = NA, trial = NA, frame = NA, stimname = NA, smptype = NA,
                                              lporx = NA, lpory = NA, rporx = NA, rpory = NA,
                                              lpupxsize = NA, lpupysize = NA, rpupxsize = NA, rpupysize = NA,
                                              additionalFields = NA)
                                              # e.g. additionalFields = list(lrawx = "L Raw X [px]", lrawy = "L Raw Y [px]", etc.)
                   )
         )
setClass("EyeDataSettings",
         representation(eye = "character", # "left", "right" or "both"
                        sampleRate = "numeric", # frequency in Hz
                        timeUnits = "numeric", # e.g. 1E-3 for milliseconds
                        eyeToScreenDistances = "list",
                        eyeToCameraDistance = "numeric",
                        chinRestSettings = "list",
                        distancesUnits = "numeric", # e.g. 1E-2 for millimeters
                        pupilShape = "character", #"circle" or "ellipse"
                        pupilDataType = "character", #"radius" or "diameter"
                        pupilDataUnits = "character", #"px", "mm", cm", etc 
         ),
         prototype(
         )
)


####################
### BASE CLASSES ###
####################

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
                        leftAdditionalSamples = "AdditionalEyeData",
                        rightAdditionalSamples = "AdditionalEyeData",
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
                        eyesData = "EyesData"
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

###############
### RECORDS ###
###############
# Using headerLines and settings@dataFormat it is sometimes possible to figure out some additional settings
# We need a method which scans headerLines and returns useful settings if possible
setClass("RawDataRecord",
         representation(filepath = "character",
                        headerLines = "vector",
                        samples = "data.frame",
                        settings = "RawDataSettings"
         ),
         prototype(
         )
)

setClass("EyeDataRecord",
         representation(filepath = "character",
                        subjectCode = "character",
                        expID = "numeric",
                        trialID = "numeric",
                        #eyeData = "",
                        otherData = "data.frame",
                        settings = "DataSettings",
         ),
         prototype(
         )
)

setClass("EventDataRecord",
         representation(
         ),
         prototype(
         )
)

setClass("FeatureDataRecord",
         representation(
         ),
         prototype(
         )
)

#####################
### OBJECT FRAMES ###
#####################
## There (in the package distribution) should be a function which creates the empty RawDataTable
## Data parsing method should be implemented for this class which scans text files and returns RawDataRecords
## and then fill the RawDataTable object
setClass("RawDataTable",
         representation(records = "list"
         ),
         prototype(
         )
)

setClass("EyeDataTable",
         representation(records = "list"
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

# !!! Must create events settings object also !!!

####################################################################################
                        #class_declaration_section_end#
####################################################################################