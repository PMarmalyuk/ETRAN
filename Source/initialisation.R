# install.packages("grDevices")
# install.packages("jpeg")
# library(grDevices)
library(signal)
library(data.table)
library(ggplot2)
library(ks)
library(MASS)
library(data.table)
library(jpeg)
library(dplyr)
source("Functions\\Analysis\\readCSVData.R", local = T)
print("readCSVData.R")
source("Functions\\Analysis\\filters.R", local = T)
print("filters.R")
source("Functions\\Analysis\\smoothers.R", local = T)
print("smoothers.R")
source("Functions\\Analysis\\angularPositions.R", local = T)
print("angularPositions.R")
source("Functions\\Analysis\\velocities.R", local = T)
print("velocities.R")
source("Functions\\Analysis\\oculomotorEventDetectors.R", local = T)
print("oculomotorEventDetectors.R")
source("Functions\\Analysis\\miscFunctions.R", local = T)
print("miscFunctions.R")
source("Functions\\Analysis\\subFunctions.R", local = T)
print("subFunctions.R")
source("Functions\\Analysis\\subFunctionsInit.R", local = T)
print("subFunctionsInit.R")
source("Functions\\Analysis\\movingWindow.R", local = T)
print("movingWindow.R")
source("Functions\\Analysis\\selectors.R", local = T)
print("selectors.R")
source("Functions\\Analysis\\analyzers.R", local = T)
print("analyzers.R")
source("Functions\\Analysis\\visualisations.R", local = T)
print("visualisations.R")


################## DATA READING SETTINGS ################## 
headerKeys <- list(subjectCode = list(type = "character", 
                                      keyString = "Subject"),
                   fs = list(type = "numeric", 
                             keyString = "Sample Rate"),
                   headDistance = list(type = "numeric", 
                                       keyString = "Head Distance [mm]"),
                   stimulusSize = list(type = "numeric", 
                                       keyString = "Stimulus Dimension [mm]"))
columnsPositions <- list(commonData = list(time = 1),
                         leftEyeData = list(porx = 10,
                                            pory = 11,
                                            pupSizeX = 6,
                                            pupSizeY = 7,
                                            rawx = 4,
                                            rawy = 5,
                                            crx = 8,
                                            cry = 9),
                         rightEyeData = NA,
                         commonEvents = list(trial = list(column = 3,
                                                          type = "TrialEvents")),
                         leftEvents = NA,
                         rightEvents = NA)

################## DEFAULT CONSTANTS ################## 
smoothingFunctions <- list(movAvgSmoother = movAvgSmoother, 
                           medianSmoother = medianSmoother, 
                           savGolSmoother = savGolSmoother)
detectionFunctions <- list(IVT = IVT, IDT = IDT, ANH = ANH)

filterMarkerNames <- list(okMarker = "Ok", 
                          gapMarker = "Gap",
                          outMarker = "Out",
                          bliMarker = "Blink")

detectorMarkerNames <- list(fixMarker = "Fixation", 
                            sacMarker = "Saccade",
                            gliMarker = "Glissade",
                            smpMarker = "Smooth Pursuit",
                            gapMarker = "Gap")

settingsList <- list(filterSettings = list(interpolateShort = T,
                                           blinkDetection = T,
                                           minGapDuration = 0.02,
                                           smoothLen = 3),
                     smoothingSettings = list(fl = 33, 
                                              forder = 2),
                     velocitySettings = list(velType          = "analytical",
                                             fl               = 13),
                     IDTSettings      = list(angular = T,
                                             DT = 0.5,
                                             durT = 0.03),
                     IVTSettings      = list(angular = T,
                                             VT            = 30),
                     ANHSettings      = list(angular = T,
                                             maxSaccadeVel  = 1000, # deg/s 
                                             maxSaccadeAcc  = 100000, # deg/s^2
                                             minSaccadeDur  = 0.01, # seconds
                                             minFixationDur = 0.04)
)

