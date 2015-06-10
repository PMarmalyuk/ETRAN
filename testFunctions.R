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
parser <- createParser(name = "Core Parser", fun = coreParser, settings = list(dataFields = dataF, headerKeys = hKeys, sampleKey = "SMP", sep = "\t"))
rec <- parseDataRecord(self = recordsAll@rawDataRecordsList$rawDataRecords[[1]], parser = parser)

dataRec <- new(Class = "DataRecord", expID = 1, subjectID = 1, trialID = 1, eyesDataObject = rec$eyesDataObjects[[1]])
dataRec@eyesDataObject@conditions@conditions$screenDistance <- 80
dataRec@eyesDataObject@conditions@conditions$screenDim <- c(1280, 1024)
dataRec@eyesDataObject@conditions@conditions$screenSize <- c(33.7, 27)
dataRec@eyesDataObject@conditions@conditions$timeUnits <- 1E-6

smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = list(type = "Median", fl = 3))
rec2 <- dataSmoother(dataRec, smoother)
t <- rec2@eyesDataObject@time@time
x <- rec2@eyesDataObject@leftEyeSamples@eyeData$porx
points(x~t, type = "l", col = 2)

filter <- createFilter(name = "Standard", fun = coreFilter, settings = list(interpolate = T))
rec3 <- dataFilter(rec2, filter = filter)

t2 <- rec3@eyesDataObject@time@time
x2 <- rec3@eyesDataObject@leftEyeSamples@eyeData$porx
points(x2~t2, type = "l", col =2, pch =2)

t1 <- Sys.time()
rec <- parseDataRecord(self = recordsAll@rawDataRecordsList$rawDataRecords[[1]], parser = parser)
dataRec <- new(Class = "DataRecord", expID = 1, subjectID = 1, trialID = 1, eyesDataObject = rec$eyesDataObjects[[1]])
dataRec@eyesDataObject@conditions@conditions$timeUnits <- 1E-6
smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = list(type = "Median", fl = 15))
dataRec <- dataSmoother(dataRec, smoother)
filter <- createFilter(name = "Standard", fun = coreFilter, settings = list(interpolate = T))
rec2 <- dataFilter(dataRec, filter = filter)
filt <- new(Class = "FilterMarkers", filterMarkersData = rec2@eyesDataObject@leftFilterMarkers@filterMarkersData)

events <- coreDetector(DataRecord = rec2, 
                       settings = 
                         list(algorithm = "I-VT", 
                              VT = 30,
                              angular = T,
                              screenDist = 70,
                              screenDim = c(1280, 1024),
                              screenSize = c(33.7, 27),
                              MaxTBetFix = 0.075,
                              MaxDistBetFix = 0.5,
                              minFixLen = 0.05,
                              maxGapLen = 0.07,
                              maxVel = 1000,
                              maxAccel = 1000000,
                              classifyGaps = F))
period <- 5000:10000
lev <- c(events@eyesDataObject@leftEventMarkers@markerNames$fixation, 
         events@eyesDataObject@leftEventMarkers@markerNames$saccade, 
         events@eyesDataObject@leftEventMarkers@markerNames$gap, 
         events@eyesDataObject@leftEventMarkers@markerNames$artifact)
t <- (dataRec@eyesDataObject@time@time-events@eyesDataObject@time@time[1])[period]*events@eyesDataObject@conditions@conditions$timeUnits
x2 <- dataRec@eyesDataObject@leftEyeSamples@eyeData$porx[period]
y2 <- dataRec@eyesDataObject@leftEyeSamples@eyeData$pory[period]
ev <- factor(events@eyesDataObject@leftEventMarkers@eventMarkersData, levels = lev)[period]
plot(x2, y2, col = ev)
plot(x2~t, col = ev)