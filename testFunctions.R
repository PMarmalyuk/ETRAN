setwd("F:/Институт/Проекты/EyeTrackingPackage/Git/EyeTrackingProject/Shiny")
source("Functions\\miscFunctions.R", local = T)
source("Functions\\dataLoaders.R", local = T)
source("Classes\\optionsAndSettingsClasses.R", local = T)
source("Classes\\baseEyeDataClasses.R", local = T)
source("Classes\\baseClasses.R", local = T)
source("Classes\\listsAndTablesClasses.R", local = T)
source("Functions\\dataParsers.R", local = T)
source("Functions\\filters.R", local = T)
source("Functions\\smoothers.R", local = T)
source("Functions\\detectors.R", local = T)
source("Functions\\eventAnalyzersNew.R", local = T)
source("Functions\\estimatorsNew.R", local = T)
source("Methods\\Methods_v_1_7.R", local = T)
library(data.table)
library(signal)
rawSett <- new(Class = "ReadSettings")
folder <- "F:/Институт/Проекты/EyeTrackingPackage/Data/TestData"
records <- new(Class = "RawDataRecords")
loader <- createLoader(name = "Standard Loader", fun = createRawDataRec, 
                       settings = list(rawSettings = rawSett))
rawRecords <- addRawDataRecords(self = records, 
                            filesList = dir(path = folder, all.files = F, full.names = T, recursive = F),
                            loader = loader)
dataF <- new(Class = "AvailableDataFields")
dataF@availableFields <- list(time = 1, trial = 3, frame = NA, stimname = NA, smptype = 2, 
                              lporx = 10, lpory = 11, rporx = NA, rpory = NA, 
                              lpupxsize = 6, lpupysize = 7, rpupxsize = NA, rpupysize = NA,
                              leftAdditionalFields = list(lrawx = 4, lrawy = 5), rightAdditionalFields = NA)
hKeys <- new(Class = "HeaderKeys")
conditions <- new(Class = "Conditions")
conditions@conditions$screenDistance <- 80
conditions@conditions$screenResolution <- c(1280, 1024)
conditions@conditions$screenSize <- c(33.7, 27)
conditions@conditions$timeUnits <- 1E-6
parser <- createParser(name = "Core Parser", fun = coreParser, 
                       settings = list(dataFields = dataF, 
                                       headerKeys = hKeys, 
                                       sampleKey = "SMP", 
                                       sep = "\t",
                                       conditions = conditions))
rec <- parseDataRecord(self = rawRecords@rawDataRecordsList$rawDataRecords[[1]], parser = parser)
dataRec <- new(Class = "DataRecord", expID = 1, subjectID = 1, trialID = 1, eyesDataObject = rec$eyesDataObjects[[1]])

# Event Detection test
smoother <- createSmoother("Standard", fun = coreSmoother, settings = list(subfun = medianFilt, fl = 33))

filter <- createFilter(name = "Standard", fun = coreFilter, settings = list(subfun = standardFilter, 
                                                                            screenResolution = conditions@conditions$screenResolution, 
                                                                            interpolate = F,
                                                                            filterMarkerNames = new(Class = "FilterMarkers")@markerNames))
detector <- createDetector("Standard", fun = coreDetector, settings = list(subfun = IVT,
                                                                           postProcess = F,
                                                                           VT = 15,
                                                                           velType = "analytical",
                                                                           sampleRate = 500,
                                                                           fl = 33,
                                                                           angular = T,
                                                                           screenDistance = 100,
                                                                           screenResolution = c(1280, 1024),
                                                                           screenSize = c(33.7, 27),
                                                                           MaxTBetFix = 0.075,
                                                                           MaxDistBetFix = 0.5,
                                                                           minFixLen = 0.05,
                                                                           maxGapLen = 0.07,
                                                                           maxVel = 1000,
                                                                           maxAccel = 1000000,
                                                                           classifyGaps = F))
res <- detectEvents(dataRec, filter, smoother, detector)
dataRec@eyesDataObject@leftFilterMarkers <- res@eyesDataObject@leftFilterMarkers
dataRec@eyesDataObject@rightFilterMarkers <- res@eyesDataObject@rightFilterMarkers
dataRec@eyesDataObject@leftEventMarkers <- res@eyesDataObject@leftEventMarkers
dataRec@eyesDataObject@rightEventMarkers <- res@eyesDataObject@rightEventMarkers

# Event Analysis test
# subFuns <- getSubfunctions(self = subFunctions, operation = "Event Analysis")
# sfToApply <- subFuns@subFunctionsList$subFunctions
source('Functions\\DataRecordSubFunctions.R', local = T)
source('Functions\\GeneralEventSubFunctions.R', local = T)
source('CoreSubFunctionsInit.R', local = T)
source("Functions\\eventAnalyzersNew.R", local = T)
subFunctionsBodies <- subFunctions@subFunctionsList$subFunctions
eventFactors <- new(Class = "AvailableFactors")
analyzer <- createAnalyzer(name = "Standard", fun = coreEventAnalyzer,
                           settings = list(operation = "Oculomotor Events Analysis", 
                                           subFunctions = subFunctionsBodies, 
                                           eventFactors = eventFactors))
eventAnalysisResult <- eventAnalyzer(dataRec, analyzer)
eventAnalysisResult$dataRec@analysisResults$eventFactorsData
eventAnalysisResult$eventFactors
# data <- eventAnalysisResult$dataRec@analysisResults$eventData@factorsDataList
# data[sapply(eventAnalysisResult$dataRec@analysisResults$eventData@factorsDataList$owner, identical, c("Event", "Gap")),]
# eventAnalysisResult$intFctrs@availableFactors$varName[eventAnalysisResult$intFctrs@availableFactors$id == 3]


evd <- eventAnalysisResult$dataRec@analysisResults$eventData
evd@factorsDataList[1:10,]
evd2 <- deleteFactorValue(evd, owner = c("Event", "Fixation"), ownerID = list(EventGroup = 1), factorID = 3, eye = "left")
evd2@factorsDataList[1:10,]

# Estimators test
source('Functions\\DataRecordSubFunctions.R', local = T)
source('CoreSubFunctionsInit.R', local = T)
source("Functions\\estimatorsNew.R", local = T)
estimator <- createEstimator(name = "Standard", fun = coreEstimator,
                             settings = list(subFunctions = subFunctions@subFunctionsList$subFunctions))
dataRec <- estimateParams(self = dataRec, estimator = estimator)

# Scanpath try
fix <- res@analysisResults$leftEventData@fixations@fixations
sac <- resEvents@analysisResults$leftEventData$s
df <- getDataFrame(resEvents@eyesDataObject, eye = "left")
sac$length[which(is.nan(sac$asymmetry))]
sac$peakAcceleration[which(is.nan(sac$asymmetry))]
df[df$eventGroup == sac$eventGroup[which(is.nan(sac$asymmetry))][15],]
plot(fix$positionX[10:20], fix$positionY[10:20], cex = fix$duration[10:20]/(max(fix$duration[10:20])), pch = 16, type = "b")

## FACTORS LOADING TESTS ##
availFactors <- new(Class = "AvailableFactors")
### FOR EXAMPLE USER HAS ALREADY SPECIFIED SOME FACTORS ###
age <- new(Class = "Factor", varName = "Age", description = "Age of subject", type = "integer", owner = "Subject")
numOfChildren <- new(Class = "Factor", varName = "numOfChildren", description = "How many children in family", type = "integer", owner = "Subject")
height <- new(Class = "Factor", varName = "Height", description = "Height of subject", type = "numeric", owner = "Subject")
sex <- new(Class = "Factor", varName = "Sex", description = "Sex of subject", type = "factor", levels = c("Male", "Female"), owner = "Subject")
objectsCount <- new(Class = "Factor", varName = "objectsCount", description = "Number of objects in stimulus", type = "integer", owner = "Stimulus")
availFactors <- addFactorDefinition(availFactors, height)
availFactors <- addFactorDefinition(availFactors, age)
availFactors <- addFactorDefinition(availFactors, sex)
availFactors <- addFactorDefinition(availFactors, numOfChildren)
availFactors <- addFactorDefinition(availFactors, objectsCount)
### AND ALSO SOME FACTORS VALUES HAS BEEN SPECIFIED ###
factorsData <- new(Class = "FactorsData")
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 1, factorID = 1, value = 179, replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 2, factorID = 1, value = 180, replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 3, factorID = 1, value = 165, replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 4, factorID = 1, value = 190, replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 1, factorID = 3, value = "Male", replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 2, factorID = 3, value = "Male", replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 3, factorID = 3, value = "Male", replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 4, factorID = 3, value = "Male", replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", ownerID = 15, factorID = 3, value = "Male", replace = T)
factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Stimulus", ownerID = 1, factorID = 7, value = 10, replace = T)

res <- loadFactorsData(file = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Factors\\Subjects.txt", 
                       header = T, sep = "\t", dec = ",", encoding = "UTF-8")
varCnt <- length(res$names)
### THESE SHOULD BE SPECIFIED BY A USER THROUGH GUI ###
owners <- rep("Subject", varCnt)
varNames <- c("Height", "academicLevel", "Sex", "CognitiveStyle1")
colnames(res$data) <- c("id", varNames)
descriptions <- c("Height", "academicLevel", "Sex", "CognitiveStyle1")
types <- c("integer", "ordFactor", "factor", "numeric")
levels <- list("NA", c("Student", "PostDoc Student", "PhD"), c("Male", "Female"), "NA")
### FACTORS LOADING ###
for (i in 1:varCnt)
{
  f <- new(Class = "Factor", 
           varName = varNames[i], 
           description = descriptions[i], 
           type = types[i],
           levels = levels[[i]],
           owner = owners[i])
  availFactors <- addFactorDefinition(availFactors, factor = f)
}
availFactors #NEW FACTORS LOADED

### ADDING NEW DATA
obsCnt <- nrow(res$data)
for (i in 1:obsCnt)
{
  ownerID <- res$data$id[i]
  records <- res$data
  for (j in 2:ncol(records))
  {
    factorName <- colnames(res$data)[j]
    factorID <- getFactorIDByName(self = availFactors, factorName = factorName)
    value <- res$data[i,j]
    factorsData <- addFactorValue(self = factorsData, availableFactors = availFactors, owner = "Subject", 
                                  ownerID = ownerID, factorID = factorID, value = value, replace = T) 
  }
}
nrow(factorsData@stimuliFactors)

df2 <- asDataFrame(factorsData, owner = "Stimulus", availFactors)


