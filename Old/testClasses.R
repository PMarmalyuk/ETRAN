classesFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Git\\EyeTrackingProject\\Classes"
setwd(classesFolder)
source("extFunctionsClasses.R")
source("optionsAndSettingsClasses.R")
source("baseEyeDataClasses.R")
source("baseClasses.R")
source("listsAndTablesClasses.R")

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
experiments <- new(Class = "Experiments")
experiments <- addExperiment(experiments, exp)





tr1 <- new(Class = "Trial",
           id = 1,
           expID = experiments@expList$ids[[1]],
           name = "My first trial",
           description = "This is my first trial",
           conditions = experiments@expList$experiments[[1]]@conditions
           )

tr2 <- new(Class = "Trial",
           id = 5,
           expID = experiments@expList$ids[[1]],
           name = "My second trial",
           description = "This is my second trial",
           conditions = experiments@expList$experiments[[1]]@conditions
           )

trials <- new(Class = "Trials")
trials <- addTrial(trials, tr2)

subjectCodes <- c("Subj1", "Subj2", "Subj3")

aoi1 <- new(Class = "AOI",
           name = "Zone 1",
           type = "classic",
           shape = "rect",
           dispositionData = list(c(x = 1, y = 1), c(x = 100, y = 100)))

aoi2 <- new(Class = "AOI",
            name = "Zone 2",
            type = "classic",
            shape = "rect",
            dispositionData = list(c(x = 101, y = 101), c(x = 200, y = 200)))

aoi3 <- new(Class = "AOI",
            name = "Zone 3",
            type = "classic",
            shape = "rect",
            dispositionData = list(c(x = 301, y = 301), c(x = 400, y = 400)))

aoiset <- new(Class = "AOISet",
              name = "My zones",
              AOIs = list(aoi1, aoi2))

aoiset <- addAOI(aoiset, AOIObject = aoi3)

aoimset <- new(Class = "AOIMultiSet",
               name = "My first multi set of AOIs",
               orderIndices = 1,
               AOISets = list(aoiset)
               )

aoimset <- addAOISet(aoimset, aoiset, orderIndex = 10)
msets <- new(Class = "AOIMultiSets")
msets <- addAOIMultiSet(msets, aoimset)

aoiset@AOIs == aoiset@AOIs

subj1 <- new(Class = "Subject",
             code = "Pablo")

subj2 <- new(Class = "Subject",
             code = "Mari")
subjects <- new(Class = "Subjects")
subjects <- addSubject(subjects, subj1)

obj <- new(Class = "EyesData")
datarec1 <- new(Class = "DataRecord", 
               expID = 1,
               subjectID = 1,
               trialID = 1,
               object = list(obj),
               parameters = list(trialCnt = length(trials@trialsList)),
               objectClass = class(obj)[[1]])

datarec2 <- new(Class = "DataRecord", 
                expID = 1,
                subjectID = 1,
                trialID = 1,
                object = list(obj),
                parameters = list(trialCnt = length(trials@trialsList)),
                objectClass = class(obj)[[1]])

datarec3 <- new(Class = "DataRecord", 
                expID = 1,
                subjectID = 2,
                trialID = 1,
                object = list(obj),
                parameters = list(trialCnt = length(trials@trialsList)),
                objectClass = class(obj)[[1]])

sample <- new(Class = "DataSample")

sample <- addDataRecord(sample, dataRecord = datarec2)
printDataSampleKeys(sample)

age <- new(Class = "Factor", varName = "Age", description = "Age of subject", type = "integer", owner = "subject")
height <- new(Class = "Factor", varName = "Height", description = "Height of subject", type = "numeric", owner = "subject")
sex <- new(Class = "Factor", varName = "Sex", description = "Sex of subject", type = "factor", levels = c("Male", "Female"), owner = "subject")
academicLevel <- new(Class = "Factor", varName = "academicLevel", description = "Academic Level of subject", type = "ordFactor", levels = c("Student", "PostDoc Student", "PhD"), owner = "subject")

availFactors <- new(Class = "AvailableFactors")
availFactors <- addFactorDefinition(availFactors, height)
availFactors <- addFactorDefinition(availFactors, academicLevel)
availFactors <- addFactorDefinition(availFactors, age)
availFactors <- addFactorDefinition(availFactors, sex)
availFactors@availableFactors

rawSett <- new(Class = "ReadSettings")
rawDataRecords <- new(Class = "RawDataRecords")
folder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI"
file <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI\\Marmalyuk_Yuriev_problem_solving_Budanov_gr3_Psy_1241_Trial001 Samples.txt"
records <- new(Class = "RawDataRecords")
# record <- addRawDataRecord(self = rawDataRecords, filepath = file, readSettings = rawSett, useExt = F, extFun = F)
recordsAll <- addRawDataRecords(self = records, filesFolder = folder,  readSettings = rawSett, useExt = F, extFun = F)

dataF <- new(Class = "AvailableDataFields")
dataF@availableFields <- list(time = 1, trial = 3, frame = NA, stimname = NA, smptype = 2, 
                              lporx = 10, lpory = 11, rporx = NA, rpory = NA, 
                              lpupxsize = 6, lpupysize = 7, rpupxsize = NA, rpupysize = NA,
                              leftAdditionalFields = list(lrawx = 4, lrawy = 5), rightAdditionalFields = NA)
hKeys <- new(Class = "HeaderKeys")
rec <- parseDataRecord(self = recordsAll@rawDataRecordsList$rawDataRecords[[1]], dataFields = dataF, headerKeys = hKeys, sep = "\t")
plot(rec$eyesDataObjects[[1]]@leftEyeSamples@eyeData, type = "l")

dataRec <- new(Class = "DataRecord", expID = 1, subjectID = 1, trialID = 1, eyesDataObject = rec$eyesDataObjects[[1]])
dataRec@eyesDataObject@conditions@conditions$screenDistance <- 80
dataRec@eyesDataObject@conditions@conditions$screenDim <- c(1280, 1024)
dataRec@eyesDataObject@conditions@conditions$screenSize <- c(33.7, 27)
dataRec@eyesDataObject@conditions@conditions$timeUnits <- 1E-6

filter <- createFilter(name = "Standard", fun = coreFilter, settings = list(maxVel = 1000, maxAccel = 100000, angular = T))
detector <- createDetector(name = "I-VT", fun = IVTDetector, settings = list(VT = 100, angular = T))
rec2 <- dataFilter(self = dataRec, filter = filter)
filt <- rec2@eyesDataObject@leftFilterMarkers@filterMarkersData
detectEvents(self = rec2, detector = detector)
