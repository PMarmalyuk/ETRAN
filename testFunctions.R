setwd("F:/Институт/Проекты/EyeTrackingPackage/Git/EyeTrackingProject/Shiny")
source("Functions\\miscFunctions.R", local = T)
source("Functions\\dataLoaders.R", local = T)
source("Classes\\optionsAndSettingsClasses.R", local = T)
source("Classes\\baseEyeDataClasses.R", local = T)
source("Classes\\baseClasses.R", local = T)
source("Classes\\listsAndTablesClasses.R", local = T)
source("Classes\\Events\\filterEvent.R", local = T)
source("Classes\\Events\\oculomotorEvent.R", local = T)
source("Classes\\Events\\AOIEvent.R", local = T)
source("Classes\\Events\\syncEvent.R", local = T)
source("Classes\\Events\\frameEvent.R", local = T)
source("Classes\\Events\\windowEvent.R", local = T)
source("Classes\\representations.R", local = T)
source("Functions\\dataParsers.R", local = T)
source("Functions\\filters.R", local = T)
source("Functions\\smoothers.R", local = T)
source("Functions\\detectors.R", local = T)
source("Functions\\eventSelector.R", local = T)
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

evMarksDefs <- new(Class = "EventMarkersDefinitions")
evMarksDefs@definitions <- append(evMarksDefs@definitions, new(Class = "EventMarkersDefinition",
                                                               eventClass = "FilterEvent",
                                                               eventTypesIDs = c(1,2,3),
                                                               typesMarkers = c("OK", "GAP", "ARTIFACT")))
evMarksDefs@definitions <- append(evMarksDefs@definitions, new(Class = "EventMarkersDefinition",
                                                               eventClass = "OculomotorEvent",
                                                               eventTypesIDs = c(1,2,3,4,5,6,7),
                                                               typesMarkers = c("FIXATION", "SACCADE", 
                                                                                "GLISSADE", "SMOOTH_PURSUIT",
                                                                                "GAP", "ARTIFACT", "BLINK")))

detectors <- list(ids = c(1, 2, 3), 
                  detectors = list(noFilt = createFilter(id = 1, name = "Standard", fun = coreFilter, description = "Test Filter",
                                                         settings = list(subfun = noFilter)),
                                   standardFilt = createFilter(id = 2, name = "Standard", fun = coreFilter, description = "Test Filter",
                                                               settings = list(subfun = standardFilter, 
                                                                               screenResolution = conditions@conditions$screenResolution, 
                                                                               interpolate = F)),
                                   standardDetector = createDetector(id = 3, name = "Standard", fun = coreDetector,description = "Test Detector",
                                                                     settings = list(subfun = IDT,
                                                                                     postProcess = F,
                                                                                     VT = 15,
                                                                                     velType = "analytical",
                                                                                     sampleRate = 500,
                                                                                     fl = 33,
                                                                                     angular = T,
                                                                                     durationThreshold = 30,
                                                                                     dispersionThreshold = 0.5,
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
                  )
)


dataRec <- dataFilter(self = dataRec, filter = detectors$detectors[[2]])
table(dataRec@eyesDataObject@leftEventsMarkers$filterMarkers@markers)

dataRec <- detectEvents(dataRec, filter = detectors$detectors[[2]], smoother, detector = detectors$detectors[[3]])
table(dataRec@eyesDataObject@leftEventsMarkers$oculomotorEventMarkers@markers)

summary(factor(dataRec@eyesDataObject@leftEventsMarkers$filterMarkers@markers))

shannon.entropy <- function(p)
{
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <- p[p>0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}

# Event Analysis test
# subFuns <- getSubfunctions(self = subFunctions, operation = "Event Analysis")
# sfToApply <- subFuns@subFunctionsList$subFunctions
source('Functions\\DataRecordSubFunctions.R', local = T)
source('Functions\\GeneralEventSubFunctions.R', local = T)
source('CoreSubFunctionsInit.R', local = T)
source("Functions\\eventAnalyzersNew.R", local = T)
subFunctionsBodies <- subFunctions@subFunctionsList$subFunctions
factorsDef <- new(Class = "FactorsDefinitions", 
                    factorsDef = list(),
                    ids = numeric())
analyzer <- createAnalyzer(name = "Standard", fun = coreEventAnalyzer,
                           settings = list(detectorID = 3,
                                           subFunctions = subFunctionsBodies, 
                                           factorsDef = factorsDef))
eventAnalysisResult <- eventAnalyzer(dataRec, analyzer)
dataRec <- eventAnalysisResult$dataRec
fd <- dataRec@analysisResults$eventFactorsData@factorsData
shannon.entropy(table(unlist(fd$value[fd$factorID == 1])))
eventAnalysisResult$factorsDef
eventAnalysisResult$dataRec@eyesDataObject@leftEventsMarkers$oculomotorEventMarkers
fd$owner[1:10]
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


