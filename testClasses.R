classesFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Classes"
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
folder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI"
file <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI\\Drozdov Samples.txt"
t1 <- proc.time()
recordsAll <- addRawDataRecord(self = rawDataRecords, filepath = file, readSettings = rawSett, useExt = F, extFun = F)
t2 <- proc.time()
t2-t1
newRawDataRec <- createRawDataRec(filePath = file, readSettings = rawSett, useExt = F, extFun = F)

t1 <- proc.time()
recordsAll <- addRawDataRecords(self = recordsAll, filesFolder = folder,  readSettings = rawSett, useExt = F, extFun = F)
t2 <- proc.time()
t2-t1

headerLine <- recordsAll@rawDataRecordsList$rawDataRecords[[1]]@headerLines[7]
headerLine <- gsub(" ", "", headerLine, fixed = TRUE)
headerLine <- gsub("\t", "", headerLine, fixed = TRUE)
headerLine <- gsub("##", "", headerLine, fixed = TRUE)

recordsAll@rawDataRecordsList$fileNumbers

