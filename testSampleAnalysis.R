rawSett <- new(Class = "ReadSettings")
rawDataRecords <- new(Class = "RawDataRecords")
folder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\TestData"
records <- new(Class = "RawDataRecords")
recordsAll <- addRawDataRecords(self = records, filesFolder = folder,  readSettings = rawSett, useExt = F, extFun = F)
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

records <- lapply(recordsAll@rawDataRecordsList$rawDataRecords, FUN = parseDataRecord, parser = parser)
DataSmp <- new(Class = "DataSample")
for (i in 1:length(records))
{
  dataRec <- new(Class = "DataRecord", expID = 1, subjectID = i, trialID = 1, eyesDataObject = records[[i]]$eyesDataObjects[[1]])
  DataSmp <- addDataRecord(DataSmp, dataRec)
}
DataSmp@keys

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
analyzer <- createAnalyzer(name = "Standard", fun = coreAnalyzer, 
                           settings = list(subfun = IVT, postProcess = F, angular = T, screenDist = 100, screenDim = c(1280, 1024), screenSize = c(33.7, 27)))
estimator <- createEstimator(name = "Standard Estimator", fun = coreEstimator,
                             settings = list(subfun = trajLengthEstimator,
                                             applyTo = "EyesData",
                                             angular = T,
                                             screenDist = 100,
                                             screenDim = c(1280, 1024),
                                             screenSize = c(33.7, 27)))
estimator2 <- createEstimator(name = "Standard Estimator", fun = coreEstimator,
                              settings = list(subfun = trajDurationEstimator,
                                              applyTo = "EyesData"))
dataRecords <- list()
for (i in 1:length(records))
{
  dataRec <- new(Class = "DataRecord", expID = 1, subjectID = i, trialID = 1, eyesDataObject = records[[i]]$eyesDataObjects[[1]])
  res <- detectEvents(dataRec, filter, smoother, detector)
  dataRec@eyesDataObject@leftFilterMarkers <- res@eyesDataObject@leftFilterMarkers
  dataRec@eyesDataObject@rightFilterMarkers <- res@eyesDataObject@rightFilterMarkers
  dataRec@eyesDataObject@leftEventMarkers <- res@eyesDataObject@leftEventMarkers
  dataRec@eyesDataObject@rightEventMarkers <- res@eyesDataObject@rightEventMarkers
  lev <- c(res@eyesDataObject@leftEventMarkers@markerNames$fixation, 
           res@eyesDataObject@leftEventMarkers@markerNames$saccade, 
           res@eyesDataObject@leftEventMarkers@markerNames$gap, 
           res@eyesDataObject@leftEventMarkers@markerNames$artifact)
  t <- (res@eyesDataObject@time@time-res@eyesDataObject@time@time[1])
  x2 <- res@eyesDataObject@leftEyeSamples@eyeData$porx
  y2 <- res@eyesDataObject@leftEyeSamples@eyeData$pory
  ev <- factor(res@eyesDataObject@leftEventMarkers@eventMarkers, levels = lev)
  plot(x2, y2, col = ev)
  dataRec <- eventAnalyzer(self = dataRec, analyzer = analyzer)
  dataRec@statistics$left <- list()
  dataRec@statistics$right <- list()
  res <- estimateParams(self = dataRec, estimator = estimator)
  res <- estimateParams(self = res, estimator = estimator2)
  dataRec@statistics$left <- res@statistics$left
  dataRec@statistics$right <- res@statistics$right
  dataRecords <- append(dataRecords, dataRec)
}
