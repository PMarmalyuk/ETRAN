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

setClass("AOI",
         representation(name = "character",
                        type = "factor",
                        shape = "factor",
                        characteristics = "list",
                        description = "character"
         ),
         prototype(     name = "Not defined",
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
         representation(id = "numeric",
                        name = "character",
                        description = "character",
                        path = "character",
                        dimensions = "list"
         ),
         prototype(id = -1,
                   name = "Not defined",
                   description = "Not defined",
                   path = "Not defined"
         )
)



setClass("MetaExperiment",
         representation(name = "character",
                        description = "character",
                        author = "character",
                        expCond = "list",
                        stimuli = "list",
                        AOIL = "list",
                        TAS = "data.frame"
         ),
         prototype(name = "Not defined",
                   description = "Not defined",
                   author = "Not defined"
           
         )
)



setClass("ExperimentData",
         representation(subjectData = "list",
                        pFactors = "data.frame"
         ),
         prototype(
         )
)



setClass("Subject",
         representation(name = "character",
                        trajData = "list"
         ),
         prototype(name = "Not defined"
         )
)



setClass("EventList",
         representation(saccades = "data.frame",
                        glissades = "data.frame",
                        fixations = "data.frame",
                        blinks = "data.frame",
                        artefacts = "data.frame"
         ),
         prototype(
         )
)


setClass("Trajectory",
         representation(points = "data.frame", # fields: "time", "frame", "PORX", "PORY", ["PUPX"], ["PUPY"], "filterMark", "eventMark"
                        eye = "character", # "left" or "right"
                        pupilData = "character" # "none", "circle", "ellipse"
         ),
         prototype(
         )
)



setClass("Monocular",
         representation(expCond = "list", # timeUnitsExponential, pupilDataType, pupilDataUnits, screenDistance etc.
                        eyeData = "Trajectory",
                        events = "EventList",
                        TrialId = "numeric"
         ),
         prototype(TrialId = -1
         )
)




setClass("Binocular",
         representation(expCond = "list",
                        leftEyeData = "Trajectory",
                        rightEyeData = "Trajectory",
                        leftEvents = "EventList",
                        rightEvents = "EventList",
                        TrialId = "numeric"
         ),
         prototype(TrialId = -1
         )
)

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
          function(self, folder, dataSource){                         
            
            
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