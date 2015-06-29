rawSett <- new(Class = "ReadSettings")
folder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\TestData"
records <- new(Class = "RawDataRecords")


loader <- createLoader(name = "Standard Loader", fun = createRawDataRec, 
                       settings = list(rawSettings = rawSett))
records <- addRawDataRecords(self = records, 
                            filesFolder = folder,
                            loader = loader)
dataF <- new(Class = "AvailableDataFields")
dataF@availableFields <- list(time = 1, trial = 3, frame = NA, stimname = NA, smptype = 2, 
                              lporx = 10, lpory = 11, rporx = NA, rpory = NA, 
                              lpupxsize = 6, lpupysize = 7, rpupxsize = NA, rpupysize = NA,
                              leftAdditionalFields = list(lrawx = 4, lrawy = 5), rightAdditionalFields = NA)
hKeys <- new(Class = "HeaderKeys")
conditions <- new(Class = "Conditions")
conditions@conditions$screenDistance <- 80
conditions@conditions$screenDim <- c(1280, 1024)
conditions@conditions$screenSize <- c(33.7, 27)
conditions@conditions$timeUnits <- 1E-6
parser <- createParser(name = "Core Parser", fun = coreParser, 
                       settings = list(dataFields = dataF, 
                                       headerKeys = hKeys, 
                                       sampleKey = "SMP", 
                                       sep = "\t",
                                       conditions = conditions))
rec <- parseDataRecord(self = records@rawDataRecordsList$rawDataRecords[[1]], parser = parser)
dataRec <- new(Class = "DataRecord", expID = 1, subjectID = 1, trialID = 1, eyesDataObject = rec$eyesDataObjects[[1]])

smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = list(subfun = medianFilt, fl = 3))
rec2 <- dataSmoother(dataRec, smoother)
t <- rec2@eyesDataObject@time@time
x <- rec2@eyesDataObject@leftEyeSamples@eyeData$porx
plot(x~t, type = "l", col = 2)

dataRec <- new(Class = "DataRecord", expID = 1, subjectID = 1, trialID = 1, eyesDataObject = rec$eyesDataObjects[[1]])
smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = list(subfun = medianFilt, fl = 15))
filter <- createFilter(name = "Standard", fun = coreFilter, settings = list(interpolate = T))
detector <- createDetector("Standard", fun = coreDetector, settings = list(subfun = IVT,
                                                                           postProcess = F,
                                                                           VT = 15,
                                                                           angular = T,
                                                                           screenDist = 100,
                                                                           screenDim = c(1280, 1024),
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
par(mfrow = c(1,1))
period <- 1:10000
lev <- c(res@eyesDataObject@leftEventMarkers@markerNames$fixation, 
         res@eyesDataObject@leftEventMarkers@markerNames$saccade, 
         res@eyesDataObject@leftEventMarkers@markerNames$gap, 
         res@eyesDataObject@leftEventMarkers@markerNames$artifact)
t <- (res@eyesDataObject@time@time-res@eyesDataObject@time@time[1])[period]
x2 <- res@eyesDataObject@leftEyeSamples@eyeData$porx[period]
y2 <- res@eyesDataObject@leftEyeSamples@eyeData$pory[period]
ev <- factor(res@eyesDataObject@leftEventMarkers@eventMarkers, levels = lev)[period]
plot(x2, y2, col = ev)
plot(x2~t, col = ev, type = "l")


# Scanpath try
fix <- res@analysisResults$leftEventData@fixations@fixations
sac <- resEvents@analysisResults$leftEventData$s
df <- getDataFrame(resEvents@eyesDataObject, eye = "left")
sac$length[which(is.nan(sac$asymmetry))]
sac$peakAcceleration[which(is.nan(sac$asymmetry))]
df[df$eventGroup == sac$eventGroup[which(is.nan(sac$asymmetry))][15],]

plot(fix$positionX[10:20], fix$positionY[10:20], cex = fix$duration[10:20]/(max(fix$duration[10:20])), pch = 16, type = "b")


# Event analyzers test

f1 <- new(Class = "SubFunction", fun = getValCode, name = "Validity Code", description = "Get validity code of event",
    applyTo = "EventData", event = list("Fixation", "Saccade"), settings = list())
f2 <- new(Class = "SubFunction", fun = getOnOffSetDuration, name = "Onset, offset and duration", description = "Get onset, offset and duration of event",
          applyTo = "EventData", event = list("Fixation", "Saccade"), settings = list())
f3 <- new(Class = "SubFunction", fun = getStartEndPositionsXY, name = "Start and End PositionsXY", description = "Get start, end positions of event",
          applyTo = "EventData", event = list("Fixation", "Saccade"), settings = list(angular = F))
f4 <- new(Class = "SubFunction", fun = getCenterOfMassXY, name = "Center of Mass of Fixation", description = "Get start, end positions of event",
          applyTo = "EventData", event = list("Fixation"), settings = list(angular = F))
f5 <- new(Class = "SubFunction", fun = getDispersionXYAndRadius, name = "DispersionXY and radius", description = "Get dispersion(X), dispersion(Y) and radius of fixation",
          applyTo = "EventData", event = list("Fixation"), settings = list(angular = T, screenDist = 100,
                                                                     screenDim = c(1280, 1024),
                                                                     screenSize = c(33.7, 27)))
f6 <- new(Class = "SubFunction", fun = getPupilMeanAndSD, name = "Pupil mean and sd", description = "Get position(X) and position(Y) of fixation",
          applyTo = "EventData", event = list("Fixation", "Saccade"), settings = list())
          
          
analyzer <- createAnalyzer(name = "Standard", fun = coreAnalyzer, 
                           settings = list(subFunctions = list(f1, f2, f3, f4, f5, f6)))
dataRec <- eventAnalyzer(self = dataRec, analyzer = analyzer)

dataRec@analysisResults$leftEventData@saccades@saccades




# Estimators test
f7 <- new(Class = "SubFunction", fun = trajDurationEstimator, name = "Duration of a record", description = "Get duration of a record",
          applyTo = "EyesData", event = list("-"), settings = list())
f8 <- new(Class = "SubFunction", fun = trajLengthEstimator, name = "Length of a record", description = "Get length of a record",
          applyTo = "EyesData", event = list("-"), settings = list(angular = T, screenDist = 100,
                                                                   screenDim = c(1280, 1024),
                                                                   screenSize = c(33.7, 27)))
dataRec@statistics$left <- list()
dataRec@statistics$right <- list()
estimator <- createEstimator(name = "Standard Estimator", fun = coreEstimator,
                             settings = list(subFunctions = list(f7, f8)))
res <- estimateParams(self = dataRec, estimator = estimator)
res@statistics$left
dataRec@statistics$left <- res@statistics$left
dataRec@statistics$right <- res@statistics$right


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

