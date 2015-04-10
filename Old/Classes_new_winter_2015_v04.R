# Classes v 1.1
# Last modifications:
# "Trail" renamed to "Trial"
#################################################################################
                  #generic_methods_declaration_section_start
#################################################################################

#begin_AOI_generic_methods_declaration_block
setGeneric("mPxArea", function(characteristics){standardGeneric("mPxArea")})


#declared only once in Stimulus_declaration_blok, have two realizations in AOI_realization_block and Stimulus_realization_blok
#setGeneric("mEvalAngularSize", function(characteristics, expCond){standardGeneric("mEvalAngularSize")})



#end_AOI_generic_methods_declaration_block



#begin_AOISet_generic_methods_declaration_block
setGeneric("mAddAOI", function(self, area){standardGeneric("mAddAOI")})
setGeneric("mRemoveAOI", function(self, name){standardGeneric("mRemoveAOI")})
setGeneric("mUpdateAOI", function(self, area, name, id){standardGeneric("mUpdateAOI")})
setGeneric("mGetNames", function(self){standardGeneric("mGetNames")})
#end_AOISet_generic_methods_declaration_block


#begin_AOIList_generic_methods_declaration_block
setGeneric("mAddSet", function(self, AOISet){standardGeneric("mAddSet")})
setGeneric("mRemoveSet", function(self, AOISetId){standardGeneric("mRemoveSet")})
setGeneric("mUpdateSet", function(self, AOISetId, AOISet){standardGeneric("mUpdateSet")})
setGeneric("mCreateGrid", function(self, w, h, dimX, dimY){standardGeneric("mCreateGrid")})
#end_AOIList_generic_methods_declaration_block




#begin_Stimulus_generic_methods_declaration_block
setGeneric("mEvalDimensions", function(path){standardGeneric("mEvalDimensions")})
setGeneric("mEvalAngularSize", function(characteristics, expCond){standardGeneric("mEvalAngularSize")})
setGeneric("mDrawAOISet", function(AOISet){standardGeneric("mDrawAOISet")})
setGeneric("mDrawHeatMap", function(type, points, fixations, AOISet){standardGeneric("mDrawHeatMap")})
#end_Stimulus_generic_methods_declaration_block



#begin_MetaExperiment_generic_methods_declaration_block
setGeneric("mLoadStimuli", function(self, path){standardGeneric("mLoadStimuli")})
setGeneric("mLoadAOIL", function(self, AOI){standardGeneric("mLoadAOIL")})
setGeneric("mSetTAS", function(self, TAS){standardGeneric("mSetTAS")})
setGeneric("mUpdateExpInf", function(self, name, description, author){standardGeneric("mUpdateExpInf")})
setGeneric("mSetExpCond", function(self, conditions){standardGeneric("mSetExpCond")})
#end_MetaExperiment_generic_methods_declaration_block



#begin_ExperimentData_generic_methods_declaration_block
setGeneric("mLoadData", function(self, folder, dataSource){standardGeneric("mLoadData")})
setGeneric("mAddSubject", function(self, name){standardGeneric("mAddSubject")})
setGeneric("mRemoveSubject", function(self, name){standardGeneric("mRemoveSubject")})
setGeneric("mSelectSubjectsByFactors", function(self, logicalExpression){standardGeneric("mSelectSubjectsByFactors")})
setGeneric("mEvalStatistics", function(self){standardGeneric("mEvalStatistics")})
setGeneric("mExportStatistics", function(self, fileName, format){standardGeneric("mExportStatistics")})
setGeneric("mEvalDistanceMatrix", function(self){standardGeneric("mEvalDistanceMatrix")})
#end_ExperimentData_generic_methods_declaration_block




#begin_Subject_generic_methods_declaration_block
setGeneric("mAddTrajData", function(trajData){standardGeneric("mAddTrajData")}) #We should use ad hoc polymorphic method
                                                                               #like "override by param" for monocular/binocular cases
setGeneric("mRemoveTrajData", function(id){standardGeneric("mRemoveTrajData")})
setGeneric("mSelectDataByTrials", function(TrialId){standardGeneric("mSelectDataByTrials")})
#end_Subject_generic_methods_declaration_block




#begin_EventList_generic_methods_declaration_block
setGeneric("mEventOverallStatistic", function(trajectory, event, statistic){standardGeneric("mEventOverallStatistic")})
setGeneric("mEventAOIStatistic", function(trajectory, event, statistic, AOISetId){standardGeneric("mEventAOIStatistic")})
#end_EventList_generic_methods_declaration_block



#begin_Trajectory_generic_methods_declaration_block
setGeneric("mFindArtefats", function(points, situationParam, markPoints){standardGeneric("mFindArtefats")})
setGeneric("mSummary", function(points){standardGeneric("mSummary")})#!NB not wounderfull method name.......
setGeneric("mAOISummary", function(AOISetId){standardGeneric("mAOISummary")})
setGeneric("mSmooth", function(points, method){standardGeneric("mSmooth")})
setGeneric("mEstimateVelocity", function(points, expCond){standardGeneric("mEstimateVelocity")})
setGeneric("mEstimateAcceleration", function(points, expCond){standardGeneric("mEstimateAcceleration")})
setGeneric("mIDT", function(points, expCond, markPoints){standardGeneric("mIDT")})
setGeneric("mIVT", function(points, expCond, markPoints){standardGeneric("mIVT")})
setGeneric("mAdaptiveNH", function(points, expCond, markPoints){standardGeneric("mAdaptiveNH")})
setGeneric("Plot", function(points, type, markers){standardGeneric("Plot")})
#end_Trajectory_generic_methods_declaration_block



#begin_Monocular_generic_methods_declaration_block
setGeneric("mSetConditions", function(self, conditions){standardGeneric("mSetConditions")})
#end_Monocular_generic_methods_declaration_block




#begin_Binocular_generic_methods_declaration_block
setGeneric("mSetConditions", function(self, conditions){standardGeneric("mSetConditions")})
#end_Binocular_generic_methods_declaration_block



#########################################################################################
                      #generic_methods_declaration_section_end
#########################################################################################


#########################################################################################
                      #class_declaration_section_start
#########################################################################################

## Objects of class "Parser" are intended for specifying settings for initial raw data reading from text files
## it can be our "Own" default data parser or a function specified by a user (through 'funname' and 'funpath' params)
## It is supposed that data parsing method realization is specified for "rawDataTable" class
setClass("Parser",
         representation(name = "character",
                        settings = "list"
                        ),
         prototype(name = "Own",
                   settings = list(funname = NA,
                                   funpath = NA,
                                   sep = "\t",
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

setClass("EventDetector",
         representation(algorithm = "character",
                        settings = "list"
                        ),
         prototype(
         )
         )

setClass("Filter",
         representation(name = "character",
                        settings = "list",
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

# Using headerLines and settings@dataFormat it is sometimes possible to figure out some additional settings
# We need a method which scans headerLines and returns useful settings if possible
setClass("RawDataRecord",
         representation(filepath = "character",
                        headerLines = "list",
                        samples = "data.frame",
                        settings = "DataSettings",
                        subjectCode = "character",
                        expID = "numeric",
                        trialID = "numeric"
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

setClass("Experiments",
         representation(experimentsData = "list" # list of "ExperimentData" objects
                        ),
         prototype(
         )
         )

setClass("MetaExperiment",
         representation(name = "character"
                        description = "character",
                        author = "character",
                        commonSettings = "DataSettings",
                        filterRoutine = "FilterRoutine",
                        eventDetector = "EventDetector",
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
         representation(pupilData = "data.frame", # fields: "PUPX", ["PUPY"]
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
                                             smoothPursuit = "Smooth pursuit"
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
                        name = "character"
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








####################################################################################
                        #methods_realization_section_start
####################################################################################

#begin_AOI_generic_methods_realization_block
setMethod("mPxArea",  "AOI",                                   
          function(characteristics){                         

            
          return(someNumber)
          }
)


setMethod("mEvalAngularSize",  "AOI",                                   
          function(characteristics, expCond){                         
            
            
            return(someList)
          }
)
#end_AOI_generic_methods_realization_block



#begin_AOISet_generic_methods_realization_block
setMethod("mAddAOI",  "AOISet",                                   
          function(self, area){                         
            
            
            return(self)
          }
)


setMethod("mRemoveAOI",  "AOISet",                                   
          function(self, name){                         
            
            
            return(self)
          }
)



setMethod("mUpdateAOI",  "AOISet",                                   
          function(self, area, name, id){                         
            
            
            return(self)
          }
)


setMethod("mGetNames",  "AOISet",                                   
          function(self){                         
            
            
            return(someCharacterVector)
          }
)
#end_AOISet_generic_methods_realization_block



#begin_AOIList_generic_methods_realization_block
setMethod("mAddSet",  "AOIList",                                   
          function(self, AOISet){                         
            
            
            return(self)
          }
)


setMethod("mRemoveSet",  "AOIList",                                   
          function(self, AOISetId){                         
            
            
            return(self)
          }
)



setMethod("mUpdateSet",  "AOIList",                                   
          function(self, AOISetId, AOISet){                         
            
            
            return(self)
          }
)


setMethod("mCreateGrid",  "AOIList",                                   
          function(self, w, h, dimX, dimY){                         
            
            
            return(self)
          }
)
#end_AOIList_generic_methods_realization_block



#begin_Stimulus_generic_methods_realization_block
setMethod("mEvalDimensions",  "Stimulus",                                   
          function(path){                         
            
            
            return(someList)
          }
)


setMethod("mEvalAngularSize",  "Stimulus",                                   
          function(characteristics, expCond){                         
            
            
            return(someList)
          }
)



setMethod("mDrawAOISet",  "Stimulus",                                   
          function(AOISet){                         
            
            
            return(someGraphics)
          }
)


setMethod("mDrawHeatMap",  "Stimulus",                                   
          function(type, points, fixations, AOISet){                         
            
            
            return(someGraphics)
          }
)
#end_Stimulus_generic_methods_realization_block



#begin_MetaExperiment_generic_methods_realization_block
setMethod("mLoadStimuli",  "MetaExperiment",                                   
          function(self, path){                         
            
            
            return(self)
          }
)


setMethod("mLoadAOIL",  "MetaExperiment",                                   
          function(self, AOI){                         
            
            
            return(self)
          }
)



setMethod("mSetTAS",  "MetaExperiment",                                   
          function(self, TAS){                         
            
            
            return(self)
          }
)


setMethod("mUpdateExpInf",  "MetaExperiment",                                   
          function(self, name, description, author){                         
            
            
            return(self)
          }
)


setMethod("mSetExpCond",  "MetaExperiment",                                   
          function(self, conditions){                         
            
            
            return(self)
          }
)
#end_MetaExperiment_generic_methods_realization_block



#begin_ExperimentData_generic_methods_realization_block
setMethod("mLoadData",  "ExperimentData",                                   
          function(self, folder, dataSource)
          {                         
            
            
#            self@subjectData <- 
#            self@pFactors <- 
            return(self)
          }
)


setMethod("mAddSubject",  "ExperimentData",                                   
          function(self, name){                         
            
            
            return(self)
          }
)



setMethod("mRemoveSubject",  "ExperimentData",                                   
          function(self, name){                         
            
            
            return(self)
          }
)


setMethod("mSelectSubjectsByFactors",  "ExperimentData",                                   
          function(self, logicalExpression){                         
            
            
            return(self)
          }
)


setMethod("mEvalStatistics",  "ExperimentData",                                   
          function(self){                         
            
            
            return(self)
          }
)


setMethod("mExportStatistics",  "ExperimentData",                                   
          function(self, fileName, format){                         
            
            
            return(someFile)
          }
)


setMethod("mEvalDistanceMatrix",  "ExperimentData",                                   
          function(self){                         
            
            
            return(someMatrix)
          }
)
#end_ExperimentData_generic_methods_realization_block



#begin_Subject_generic_methods_realization_block
setMethod("mAddTrajData",  "Subject",                                   
          function(trajData){                         
            
            
            return(someList)
          }
)


setMethod("mRemoveTrajData",  "Subject",                                   
          function(id){                         
            
            
            return(someList)
          }
)


setMethod("mSelectDataByTrials",  "Subject",                                   
          function(TrialId){                         
            
            
            return(someList)
          }
)
#end_Subject_generic_methods_realization_block



#begin_EventList_generic_methods_realization_block
setMethod("mEventOverallStatistic",  "EventList",                                   
          function(trajectory, event, statistic){                         
            
            
            return(someDataFrame)
          }
)


setMethod("mEventAOIStatistic",  "EventList",                                   
          function(trajectory, event, statistic, AOISetId){                         
            
            
            return(someDataFrame)
          }
)
#end_EventList_generic_methods_realization_block



#begin_Trajectory_generic_methods_realization_block
setMethod("mFindArtefats",  "Trajectory",                                   
          function(points, situationParam, markPoints){                         
            
            
            return(someDataFrame)
          }
)


setMethod("mSummary",  "Trajectory",                                   
          function(points){                         
            
            
            return(someList)
          }
)


setMethod("mAOISummary",  "Trajectory",                                   
          function(AOISetId){                         
            
            
            return(someList)
          }
)


setMethod("mSmooth",  "Trajectory",                                   
          function(points, method){                         
            
            
            return(someDataFrame)
          }
)


setMethod("mEstimateVelocity",  "Trajectory",                                   
          function(points, expCond){                         
            
            
            return(someDataFrame)
          }
)


setMethod("mEstimateAcceleration",  "Trajectory",                                   
          function(points, expCond){                         
            
            
            return(someDataFrame)
          }
)


setMethod("mIDT",  "Trajectory",                                   
          function(points, expCond, markPoints){                         
            
            
            return(someEventList)
          }
)


setMethod("mIVT",  "Trajectory",                                   
          function(points, expCond, markPoints){                         
            
            
            return(someEventList)
          }
)


setMethod("mAdaptiveNH",  "Trajectory",                                   
          function(points, expCond, markPoints){                         
            
            
            return(someDataFrame)
          }
)


setMethod("Plot",  "Trajectory",                                   
          function(points, type, markers){                         
            
            
            return(someGraphics)
          }
)
#end_Trajectory_generic_methods_realization_block



#begin_Monocular_generic_methods_realization_block
setMethod("mSetConditions",  "Monocular",                                   
          function(self, conditions){                         
            
            
            return(someList)
          }
)
#end_Monocular_generic_methods_realization_block



#begin_Binocular_generic_methods_realization_block
setMethod("mSetConditions",  "Binocular",                                   
          function(self, conditions){                         
            
            
            return(someList)
          }
)
#end_Binocular_generic_methods_realization_block



####################################################################################
                        #methods_realization_section_end
####################################################################################