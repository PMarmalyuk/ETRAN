rawSett <- new(Class = "ReadSettings")
rawDataRecords <- new(Class = "RawDataRecords")
folder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI"
file <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI\\Marmalyuk_Yuriev_problem_solving_Budanov_gr3_Psy_1241_Trial001 Samples.txt"
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
rec <- parseDataRecord(self = recordsAll@rawDataRecordsList$rawDataRecords[[1]], parser = parser)
dataRec <- new(Class = "DataRecord", expID = 1, subjectID = 1, trialID = 1, eyesDataObject = rec$eyesDataObjects[[1]])

smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = list(type = "Median", fl = 3))
rec2 <- dataSmoother(dataRec, smoother)
t <- rec2@eyesDataObject@time@time
x <- rec2@eyesDataObject@leftEyeSamples@eyeData$porx
plot(x~t, type = "l", col = 2)

dataRec <- new(Class = "DataRecord", expID = 1, subjectID = 1, trialID = 1, eyesDataObject = rec$eyesDataObjects[[1]])
smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = list(type = "Median", fl = 15))
filter <- createFilter(name = "Standard", fun = coreFilter, settings = list(interpolate = T))
detector <- createDetector("Standard", fun = coreDetector, settings = list(algorithm = "I-VT",
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

period <- 1:1000
lev <- c(events@eyesDataObject@leftEventMarkers@markerNames$fixation, 
         events@eyesDataObject@leftEventMarkers@markerNames$saccade, 
         events@eyesDataObject@leftEventMarkers@markerNames$gap, 
         events@eyesDataObject@leftEventMarkers@markerNames$artifact)
t <- (res@eyesDataObject@time@time-events@eyesDataObject@time@time[1])[period]
x2 <- res@eyesDataObject@leftEyeSamples@eyeData$porx[period]
y2 <- res@eyesDataObject@leftEyeSamples@eyeData$pory[period]
ev <- factor(res@eyesDataObject@leftEventMarkers@eventMarkers, levels = lev)[period]
plot(x2, y2, col = ev)
plot(x2~t, col = ev)

