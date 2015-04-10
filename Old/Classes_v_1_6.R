# Classes v 1.6
####################
#class_declarations#
####################
classesFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Classes"
setwd(classesFolder)
source("extFunctionsClasses.R")
source("optionsAndSettingsClasses.R")
source("baseEyeDataClasses.R")
source("baseClasses.R")
source("listsAndTablesClasses.R")

##############################
#generic_methods_declarations#
##############################
setGeneric("addExperiment", function(self, expObject){standardGeneric("addExperiment")})

######################
#methods_realizations#
######################
setMethod("addExperiment",  "Experiments",                                   
          function(self, expObject)
          {                         
            expCnt <- length(self@expList$ids)
            if (expCnt == 0) 
            {
              self@expList$ids <- 1
              self@expList$experiments <- expObject
              return(self)
            }
            newID <- tail(self@expList$ids, n = 1) + 1
            self@expList$ids <- c(self@expList$ids, newID)
            self@expList$experiments <- c(self@expList$experiments, expObject)
            return(self)
          }
)