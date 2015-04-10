setwd("F:\\Институт\\Проекты\\EyeTrackingPackage\\")

rawSettings <- new(Class = "RawDataSettings")
rawSettings@eyeTrackerModel <- "HED"
rawSettings@eyeTrackerType <- "Tower"
expConditions <- new(Class = "Conditions")
expConditions@conditions <- list(eye = "both", 
                                 sampleRate = 50,
                                 eyeToScreenSurfaceDistance = 100, # in mm
                                 frameResolution = c(640, 480), # px
                                 pupilShape = "circle",
                                 pupilDataType = "radius",
                                 timeUnits = 1E-6, # mus
                                 distanceUnits = 1E-3, #mm
                                 pupilSizeUnits = "px")
exp <- new(Class = "Experiment", 
           name = "MyFirstExperiment", 
           description = "This is my first eyetracking experiment", 
           instruction = "file.doc",
           experimenters = c("Marmalyuk P.A., Zhegallo A.V."))
exp@conditions <- expConditions
expList <- new(Class = "Experiments")
expList <- addExperiment(expList, exp)



