shinyServer(function(input, output, session) {
  ### INITIALISATION ###
  ## Attach packages
  library(devtools)
  library(DT)
  library(htmlwidgets)
  library(D3TableFilter)
  library(shinydashboard)
  
  ## Creating session objects
  source('Initialize.R', local = T)
  
  
  f1 <- new(Class = "SubFunction", fun = getValCode, name = "valCode", description = "Validity code",
            applyTo = "EventData", event = c("Fixation", "Saccade"), settings = list())
  f2 <- new(Class = "SubFunction", fun = getOnOffSetDuration, name = "onOffsetDuration", description = "Onset, offset and duration",
            applyTo = "EventData", event = c("Fixation", "Saccade"), settings = list())
  f3 <- new(Class = "SubFunction", fun = getStartEndPositionsXY, name = "startEndPositionsXY", description = "Start, end positions",
            applyTo = "EventData", event = c("Fixation", "Saccade"), settings = list(angular = F))
  f4 <- new(Class = "SubFunction", fun = getCenterOfMassXY, name = "centerOfMass", description = "Center of mass",
            applyTo = "EventData", event = c("Fixation"), settings = list(angular = F))
  f5 <- new(Class = "SubFunction", fun = getDispersionXYAndRadius, name = "DispersionXY and radius", description = "Dispersion(X), dispersion(Y) and radius",
            applyTo = "EventData", event = c("Fixation"), settings = list(angular = F))
  f6 <- new(Class = "SubFunction", fun = getPupilMeanAndSD, name = "Pupil mean and sd", description = "Position(X) and position(Y)",
            applyTo = "EventData", event = c("Fixation", "Saccade"), settings = list())
  
  ## Creating reactive values
  vals <- reactiveValues(expList = new(Class = "Experiments"),
                         subjectsList = new(Class = "Subjects"),
                         trialsList = new(Class = "Trials"),
                         rawDataFilesList = data.frame(fileNames = NA, colnames = c("FileNames"))[-1,],
                         rawDataRecordsList = new(Class = "RawDataRecords", rawDataRecordsList = list()),
                         tempAvFlds = data.frame(Position = c(1, 3, NA, NA, 2, 10, 11, NA, NA, 6, 7, NA, NA),
                                                 row.names = c("Time", "Trial", "Frame", "Stimuli Name", "Sample Type",
                                                               "PORX (Left Eye)", "PORY (Left Eye)", "PORX (Right Eye)", "PORY (Right Eye)",
                                                               "Horis. Pupil Size (Left Eye)", "Vert. Pupil Size (Left Eye)",
                                                               "Horis. Pupil Size (Right Eye)", "Vert. Pupil Size (Right Eye)")),
                         ## not used
                         tempAddLAvFlds = data.frame(),
                         tempAddRAvFlds = data.frame(),
                         ##
                         avFlds = new(Class = "AvailableDataFields"),
                         hdrKeys = new(Class = "HeaderKeys"),
                         ## temp constants
                         smpKey = "SMP",
                         parserSep = "\t",
                         ##
                         tempDataRecords = data.frame(Id = numeric(), File = character(), Experiment = character(), Trial = character(), Subject = character(), Associated = logical()),
                         dataSample = new(Class = "DataSample", keys = data.frame(expID = numeric(), 
                                                                                  subjectID = numeric(), 
                                                                                  trialID = numeric()),
                                          DataRecordsList = list()),
                         
                         activeDataRecord = new(Class = "DataRecord"),
                         filteredDataSample = new(Class = "DataSample", keys = data.frame(expID = numeric(), 
                                                                                          subjectID = numeric(), 
                                                                                          trialID = numeric()),
                                                  DataRecordsList = list()),
                         
                         dataSampleUpdateFlags = data.frame(Id = numeric(), Flag = numeric()),
                         dataSampleFilterFlags = data.frame(Id = numeric(), Flag = numeric()),
                         dataSampleFilters = list(ids = numeric(), filters = list(), groups = numeric()),
                         
                         tempExtLoaderFun = NA,
                         tempExtLoaderSettings = NA,
                         extLoaders = new(Class = "Loaders", loadersList = list(ids = list(), loaders = list())),
                         extParsers = new(Class = "Parsers", parsersList = list(ids = list(), parsers = list())),
                         extFilters = new(Class = "Filters", filtersList = list(ids = list(), filters = list())), 
                         extSmoothers = new(Class = "Smoothers", smoothersList = list(ids = list(), smoothers = list())), 
                         defaultLoader = new(Class = "Loader", name = "Core Loader", fun = createRawDataRec, 
                                             settings = list(rawSettings = new(Class = "ReadSettings")))
                         )

  # OBSERVERS #
  
  observe({
    dataSample <- vals$dataSample
    recordsWithSubjIds <- which(!is.na(dataSample@keys$subjectID))
    isolate(codes <- sapply(dataSample@keys$subjectID[recordsWithSubjIds], getSubjectCodeById, self = vals$subjectsList))
    isolate(vals$tempDataRecords$Subject[recordsWithSubjIds] <- codes)
    recordsWithTrialsIds <- which(!is.na(dataSample@keys$trialID) & !is.na(dataSample@keys$expID))
    isolate(names <- sapply(recordsWithTrialsIds,
                            FUN = function(x) {getTrialNameById(vals$trialsList, 
                                                                expID = dataSample@keys$expID[x], 
                                                                trialID = dataSample@keys$trialID[x])}))
    isolate(vals$tempDataRecords$Trial[recordsWithTrialsIds] <- names)
  })

  ## reactive expList updating corresponding selectInputs
  observe({
    vars <- getExperimentsNames(vals$expList)
    updateSelectInput(session = session, inputId = "trialExpName", choices = vars)
    updateSelectInput(session = session, inputId = "expNameForParser", choices = vars)
    updateSelectInput(session = session, inputId = "expNameForAnalysis", choices = vars)
  })
  
  ## reactive expNameForAnalysis updating corresponding selectInput
  observe({
    if (input$expNameForAnalysis != "")
    {
      expID <- getExperimentIdByName(vals$expList, input$expNameForAnalysis)
      vars <- getTrialsNames(vals$trialsList, expID = expID)
      updateSelectInput(session = session, inputId = "trialNameForAnalysis", choices = vars)
    }
  })
  
  ## reactive extLoaders, extParsers, extFilters, extSmoothers, etc. updating corresponding selectInputs
  observe({
    vars <- sapply(vals$extLoaders@loadersList$loaders, FUN = function(x) {x@name})
    updateSelectInput(session = session, inputId = "extLoader", choices = vars)
  })
  
  observe({
    vars <- sapply(vals$extParsers@parsersList$parsers, FUN = function(x) {x@name})
    updateSelectInput(session = session, inputId = "extParser", choices = vars)
  })
  
  observe({
    vars <- sapply(vals$extFilters@filtersList$filters, FUN = function(x) {x@name})
    updateSelectInput(session = session, inputId = "extFilterForDetector", choices = vars)
  })
  
  observe({
    vars <- sapply(vals$extSmoothers@smoothersList$smoothers, FUN = function(x) {x@name})
    updateSelectInput(session = session, inputId = "extSmootherForDetector", choices = vars)
  })
  
  ## reactive tempDataRecordsList
  observe({
    if (is.null(input$tempDataRecordsList_rows_selected)) {return()}
    else
    {
      expName <- vals$tempDataRecords$Experiment[input$tempDataRecordsList_rows_selected]
      expID <- getExperimentIdByName(vals$expList, name = expName)
      updateSelectizeInput(session, 'availableTrials', choices = getTrialsNames(vals$trialsList, exp = expID), server = TRUE)
      
      row <- which(vals$tempDataRecords$Id == input$tempDataRecordsList_rows_selected)
      updateTextInput(session, inputId = 'selectedCode', value = vals$tempDataRecords$Subject[row])
      updateTextInput(session, inputId = 'selectedTrial', value = vals$tempDataRecords$Trial[row])
      
    }
  })
  
  ## reactive subjectsList
  observe({
    updateSelectizeInput(session, 'availableSubjects', choices = getSubjectCodes(vals$subjectsList), server = TRUE)
  })

  # EVENT HANDLERS #
  ## Settings tab
  ### Experiments subtab
  expAddEvent <- observeEvent(input$addExp, {
    name <- isolate(input$expName)
    expNames <- getExperimentsNames(vals$expList)
    if (name %in% expNames) {warning("Experiment with this name already exists"); return()}
    
    
    expDate <- isolate(input$expDate)
    description <- isolate(input$expDescription)
    experimenters <- isolate(input$expExperimenters)
    newExp <- new(Class = "Experiment", name = name, expDate = as.character(expDate), description = description, experimenters = experimenters)
    vals$expList <- addExperiment(vals$expList, newExp)
  })
  expViewEvent <- observe({
    selRows <- input$experimentsList_rows_selected
    if (length(selRows) != 0)
    {
      df <- isolate(asDataFrame(vals$expList))
      exp <- df[which(rownames(df) == selRows),]
      updateTextInput(session, inputId = "expName", value = exp$Name)
      updateDateInput(session, inputId = "expDate", value = exp$Date)
      updateTextInput(session, inputId = "expDescription", value = exp$Description)
      updateTextInput(session, inputId = "expExperimenters", value = exp$Experimenters)
      
      #       cond <- getConditionsById(vals$expList, id = exp$Id)
      #       updateSelectInput(session, inputId = "expEye", selected = cond@conditions$eye)
      #       if (cond@conditions$timeUnits == 1E-6) {tUn <- "microseconds"}
      #       if (cond@conditions$timeUnits == 1E-3) {tUn <- "milliseconds"}
      #       if (cond@conditions$timeUnits == 1) {tUn <- "seconds"}
      #       updateSelectInput(session, inputId = "expTimeUnits", selected = tUn)
      #       if (!is.na(cond@conditions$pupilSizeUnits))
      #       {
      #         updateCheckboxInput(session, inputId = "pupilDataExist", value = T)
      #         updateSelectInput(session, inputId = "expPupSizeUnits", selected = cond@conditions$pupilSizeUnits)
      #         updateSelectInput(session, inputId = "expPupShape", selected = cond@conditions$pupilShape)
      #       }
      #       else
      #       {
      #         updateCheckboxInput(session, inputId = "pupilDataExist", value = F)
      #       }
      #       updateNumericInput(session, inputId = "expSampleRate", value = cond@conditions$sampleRate)
      #       updateNumericInput(session, inputId = "expScreenDist", value = cond@conditions$screenDistance)
      #       updateNumericInput(session, inputId = "expScreenSizeX", value = cond@conditions$screenSize[1])
      #       updateNumericInput(session, inputId = "expScreenSizeY", value = cond@conditions$screenSize[2])
      #       updateNumericInput(session, inputId = "expScreenDimX", value = cond@conditions$screenDim[1])
      #       updateNumericInput(session, inputId = "expScreenDimY", value = cond@conditions$screenDim[2])
    }
    
  })
  expUpdateEvent <- observeEvent(input$updateExp, {
    selRows <- input$experimentsList_rows_selected
    if (length(selRows) != 0)
    {
      df <- asDataFrame(vals$expList)
      id <- df[which(rownames(df) == selRows),1]
      name <- isolate(input$expName)
      expDate <- isolate(input$expDate)
      description <- isolate(input$expDescription)
      experimenters <- isolate(input$expExperimenters)
      newExp <- new(Class = "Experiment", name = name, expDate = as.character(expDate), description = description, experimenters = experimenters)
      vals$expList <- updateExperimentById(self = vals$expList, id = id, expObject = newExp)
    }
  })
  expDeleteEvent <- observeEvent(input$delExp, {
    selRows <- as.numeric(input$experimentsList_rows_selected)
    if (length(selRows) != 0)
    { 
      df <- isolate(asDataFrame(vals$expList))
      idsToDel <- df[which(rownames(df) %in% intersect(rownames(df), selRows)),1]
      vals$expList <- delExperimentsById(self = vals$expList, ids = idsToDel)
    }
  })
  ### Subjects subtab
  subjAddEvent <- observeEvent(input$addSubj, {
    code <- isolate(input$subjCode)
    subjCodes <- getSubjectCodes(vals$subjectsList)
    if (code %in% subjCodes) {warning("Subject with this code already exists"); return()}
    fullname <- isolate(input$subjFullname)
    birthdate <- isolate(input$subjBirthdate)
    newSubj <- new(Class = "Subject", code = code, fullname = fullname, birthdate = as.character(birthdate))
    vals$subjectsList <- addSubject(vals$subjectsList, newSubj)
  })
  subjViewEvent <- observe({
    selRows <- input$subjectsList_rows_selected
    if (length(selRows) != 0)
    {
      df <- isolate(asDataFrame(vals$subjectsList))
      subject <- df[which(rownames(df) == selRows),]
      updateTextInput(session, inputId = "subjCode", value = subject$Code)
      updateTextInput(session, inputId = "subjFullname", value = subject$Fullname)
      updateDateInput(session, inputId = "subjBirthdate", value = subject$Birthdate)
    }
  })
  subjUpdateEvent <- observeEvent(input$updateSubj, {
    selRows <- input$subjectsList_rows_selected
    if (length(selRows) != 0)
    {
      df <- asDataFrame(vals$subjectsList)
      id <- df[which(rownames(df) == selRows),1]
      code <- isolate(input$subjCode)
      fullname <- isolate(input$subjFullname)
      birthdate <- isolate(input$subjBirthdate)
      newSubj <- new(Class = "Subject", code = code, fullname = fullname, birthdate = as.character(birthdate))
      vals$subjectsList <- updateSubjectById(self = vals$subjectsList, id = id, subjObject = newSubj)
    }
  })
  subjDeleteEvent <- observeEvent(input$delSubj, {
    selRows <- as.numeric(input$subjectsList_rows_selected)
    if (length(selRows) != 0)
    { 
      df <- asDataFrame(vals$subjectsList)
      idsToDel <- df[which(rownames(df) %in% intersect(rownames(df), selRows)),1]
      vals$subjectsList <- delSubjectsById(self = vals$subjectsList, ids = idsToDel)
    }
  })
  ### Trials subtab
  trialAddEvent <- observeEvent(input$addTrial, {
    if (is.null(input$trialExpName) | input$trialExpName == "") {warning("Experiment should be specified!"); return()}
    name <- input$trialName
    expID <- getExperimentIdByName(self = vals$expList, name = input$trialExpName)
    trialsNames <- getTrialsNames(vals$trialsList, expID = expID)
    if (name %in% trialsNames) {warning("Trial with this name already exists"); return()}
    description <- input$trialDescription
    newTrial <- new(Class = "Trial", expID = expID, name = name, description = description)
    vals$trialsList <- addTrial(vals$trialsList, newTrial)
  })
  trialViewEvent <- observe({
    selRows <- input$trialsList_rows_selected
    if (length(selRows) != 0)
    {
      df <- isolate(asDataFrame(vals$trialsList))
      trial <- df[which(rownames(df) == selRows),]
      expName <- isolate(getExperimentNameById(vals$expList, trial$ExpID))
      updateSelectInput(session, inputId = "trialExpName", selected = expName)
      updateTextInput(session, inputId = "trialName", value = trial$Name)
      updateTextInput(session, inputId = "trialDescription", value = trial$Description)
    }
  })
  trialUpdateEvent <- observeEvent(input$updateTrial, {
    selRows <- input$trialsList_rows_selected
    if (length(selRows) != 0)
    {
      df <- asDataFrame(vals$trialsList)
      id <- df[which(rownames(df) == selRows),1]
      expID <- isolate(getExperimentIdByName(vals$expList, input$trialExpName))
      name <- isolate(input$trialName)
      description <- isolate(input$trialDescription)
      newTrial <- new(Class = "Trial", expID = expID, name = name, description = description)
      vals$trialsList <- updateTrialById(self = vals$trialsList, id = id, trialObject = newTrial)
    }
  })
  trialDeleteEvent <- observeEvent(input$delTrial, {
    selRows <- as.numeric(input$trialsList_rows_selected)
    if (length(selRows) != 0)
    { 
      df <- asDataFrame(vals$trialsList)
      idsToDel <- df[which(rownames(df) %in% intersect(rownames(df), selRows)),1]
      vals$trialsList <- delTrialsById(self = vals$trialsList, ids = idsToDel)
    }
  })
  
  ### Stimuli tab
  
  ### Objects Linkage tab
  
  
  ### ? External Loaders and Parsers subtab
  addExtLoaderFunEvent <- observeEvent(input$addExtLoaderFun, {
    funFile <- choose.files(default = getwd(), caption = "Choose .R file with Loader Function", multi = F, filters = "R")
    vals$tempExtLoaderFun <- funFile
   })
  addExtLoaderSettingsEvent <- observeEvent(input$addExtLoaderSettings, {
    settingsFile <- choose.files(default = getwd(), caption = "Choose .R file with Loader Settings", multi = F, filters = "R")
    vals$tempExtLoaderSettings <- settingsFile
  })
  addExtLoaderEvent <- observeEvent(input$addExtLoader, {
    name <- isolate(input$extLoaderName)
    funFile <- vals$tempExtLoaderFun
    settingsFile <- vals$tempExtLoaderSettings
    validate(
      need(!is.na(funFile) & funFile != "", "Please select a file with loader function!")
    )
    validate(
      need(!is.na(settingsFile) & settingsFile != "", "Please select a file with loader settings!")
    )
      fun <- source(funFile)$value
      settings <- source(settingsFile)$value
      newLoader <- new(Class = "Loader", name = name, fun = fun, settings = settings)
      vals$extLoaders <- addLoader(self = vals$extLoaders, loaderObject = newLoader)
  })
  
  ## Data Import Tab
  ### R Session subtab
  loadSession <- observeEvent(input$loadSession,{
    try(
      {
        sessionFile <- choose.files(multi = F)
        load(sessionFile)
        nms <- names(valuesList)
        for (i in 1:length(valuesList))
        {
          el <- nms[i]
          vals[[el]] <- valuesList[[i]]
        }
      }, silent = T
    )  
  })
  
  ### Gaze Data subtab
  rawDataDirAddEvent <- observeEvent(input$loadFilesFromDir, {
    rawFilesDir <- choose.dir(default = getwd(), caption = "Select a folder with raw gaze datafiles")
    fileNames <- list.files(path = rawFilesDir, pattern = NULL, all.files = FALSE,
               full.names = TRUE, recursive = FALSE,
               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
    if (input$useExtLoader)
    {
      if (is.na(input$extLoader)) {return(NULL)}
      ### TO DO
    }
    else
    {
      files <- as.list(fileNames)
      settings <- isolate(list(rawSettings = new(Class = "ReadSettings", readSettings = list(encoding = input$encoding, 
                                                                                             sep = input$sep,
                                                                                             dec = input$dec,
                                                                                             skip = input$skip,
                                                                                             comment.char = input$commchar,
                                                                                             header = input$header))))
      loader <- new(Class = "Loader", name = "Core Loader", fun = createRawDataRec, settings = settings)
      vals$rawDataRecordsList <- addRawDataRecords(vals$rawDataRecordsList, filesList = fileNames, loader = loader)
    }
  })
  # TO DO: ext loader
  rawDataFilesAddEvent <- observeEvent(input$loadSeparateFilesFromDir, {
    fileNames <- choose.files(default = getwd(), caption = "Select files with raw gaze data")
    if (input$useExtLoader)
    {
      if (is.na(input$extLoader)) {return(NULL)}
      ### TO DO
    }
    else
    {
      files <- as.list(fileNames)
      settings <- isolate(list(rawSettings = new(Class = "ReadSettings", readSettings = list(encoding = input$encoding, 
                                                                                             sep = input$sep,
                                                                                             dec = input$dec,
                                                                                             skip = input$skip,
                                                                                             comment.char = input$commchar,
                                                                                             header = input$header))))
      loader <- new(Class = "Loader", name = "Core Loader", fun = createRawDataRec, settings = settings)
      vals$rawDataRecordsList <- addRawDataRecords(vals$rawDataRecordsList, filesList = fileNames, loader = loader)
    }
  })
  rawRecsDelEvent <- observeEvent(input$deleteSelectedRawRec, {
    if (length(input$rawDataRecordsList_rows_selected) != 0) 
    {    
      idsToDel <- as.numeric(input$rawDataRecordsList_rows_selected)
      vals$rawDataRecordsList <- delRawDataRecordsById(self = vals$rawDataRecordsList, ids = idsToDel)
    }
  })
  rawDataFilesClearEvent <- observeEvent(input$clearRawRecList, {
    vals$rawDataRecordsList <- new(Class = "RawDataRecords", rawDataRecordsList = list())
  })
  saveAvDataFieldsEvent <- observeEvent(input$saveAvDataFields, {
    x <- vals$tempAvFlds[,1]
    names(x) <- c("time", "trial", "frame", "stimname", "smptype",
                  "lporx", "lpory", "rporx", "rpory",
                  "lpupxsize", "lpupysize", "rpupxsize", "rpupysize")
    x <- c(x, list(leftAdditionalFields = NA, rightAdditionalFields = NA))
    isolate(vals$avFlds@availableFields <- x)
  })
  parseFilesEvent <- observeEvent(input$parseFiles, {
    if (input$useExtParser)
    {
      if (is.na(input$extParser)) {return(NULL)}
      else
      {
        stop("External parser is not implemented!")
      }
    }
    else
    {
      if (!is.null(input$expNameForParser) & input$expNameForParser != "")
      {
        expName <- isolate(input$expNameForParser)
        expID <- isolate(getExperimentIdByName(vals$expList, name = input$expNameForParser))
        eye <- switch(EXPR = isolate(input$expEye), 
                      'Left Eye' = "left",
                      'Right Eye' = "right",
                      'Binocular' = "both")
          
        sampleRate <- isolate(input$expSampleRate)
        timeUnits <- switch(EXPR = isolate(input$expTimeUnits), 
                            microseconds = as.numeric(1E-6),
                            milliseconds = as.numeric(1E-3),
                            seconds = as.numeric(1))
        if (input$pupilDataExist)
        {
          pupSizeUnits <- isolate(input$expPupSizeUnits)
          pupShape <- isolate(input$expPupShape)
        }
        else
        {
          pupSizeUnits <- NA
          pupShape <- NA
        }
        screenDist <- isolate(input$expScreenDist)
        scrSizeX <- isolate(input$expScreenSizeX)
        scrSizeY <- isolate(input$expScreenSizeY)
        scrDimX <- isolate(input$expScreenDimX)
        scrDimY <- isolate(input$expScreenDimY)
        cond <- new(Class = "Conditions", conditions = list(eye = eye, sampleRate = sampleRate, timeUnits = timeUnits, 
                                                            pupilSizeUnits = pupSizeUnits, pupilShape = pupShape, screenDistance = screenDist, 
                                                            screenSize = c(scrSizeX, scrSizeY), screenDim = c(scrDimX, scrDimY)))
      }
      else
      {
        expID <- NA
        cond <- new(Class = "Conditions")
      }
      parser <- createParser(name = "Core Parser", fun = coreParser, 
                             settings = list(dataFields = vals$avFlds, 
                                             headerKeys = vals$hdrKeys, 
                                             sampleKey = vals$smpKey, 
                                             sep = vals$parserSep,
                                             conditions = cond))
      records <- lapply(vals$rawDataRecordsList@rawDataRecordsList$rawDataRecords, FUN = parseDataRecord, parser = parser)
      eyesDataObjects <- unlist(lapply(records, FUN = function(x) {x$eyesDataObjects}), recursive = F)
      filePaths <- basename(unlist(lapply (records, FUN = function(x) {x$filePath})))
      
      subjectCodes <- unlist(lapply(records, FUN = function(x) {x$subjectCode}))
      trials <- unlist(lapply(records, FUN = function(x) {x$trials}))

      
      numRec <- length(eyesDataObjects)
      step <- 1/numRec
      withProgress(message = 'Parsing Data', value = step, {
        for (i in 1:numRec)
        {
          rec <- new(Class = "DataRecord", 
                     expID = expID, 
                     subjectID = as.numeric(NA), 
                     trialID = as.numeric(NA), 
                     eyesDataObject = eyesDataObjects[[i]])
          vals$dataSample <- addDataRecord(vals$dataSample, rec)
          Id <- tail(vals$dataSample@ids, 1)
          # Adding flags for the new data record
          vals$dataSampleUpdateFlags <- rbind(vals$dataSampleUpdateFlags, data.frame(Id = Id, Flag = 0))
          vals$dataSampleFilterFlags <- rbind(vals$dataSampleFilterFlags, data.frame(Id = Id, Flag = 0))
          incProgress(step, detail = paste("Parsing Raw Data Record Number", i))
        }
        vals$tempDataRecords <- rbind(vals$tempDataRecords, data.frame(Id = tail(vals$dataSample@ids, n = numRec),
                                                                       File = filePaths,
                                                                       Experiment = rep(expName, numRec),
                                                                       Trial = trials,
                                                                       Subject = subjectCodes,
                                                                       stringsAsFactors = F))
        
          
      })
    }
    vals$rawDataRecordsList <- new(Class = "RawDataRecords", rawDataRecordsList = list())
  })
  
  ### Set Associations tab panel
  ### TO DO: acceptAllAssociationsEvent, changeSelectedSubjectEvent, changeSelectedTrialEvent
  acceptAllAssociationsEvent <- observeEvent(input$acceptAllAssociations, {
    tempdr <- vals$tempDataRecords
    dataSample <- vals$dataSample
    ## Scanning all data records
    for (i in 1:nrow(dataSample@keys))
    {
      ## if a record has no SubjectID associated with
      if (is.na(dataSample@keys$subjectID[i]))
      {
        ## then trying to get temporal subject code, add it and associate
        recID <- dataSample@ids[i]
        tempSubject <- trimws(tempdr$Subject[[which(tempdr$Id == recID)]], "both")
        if (!is.na(tempSubject) & tempSubject != "")
        {
          recIDs <- tempdr$Id[which(trimws(tempdr$Subject, "both") == tempSubject)]
          subjId <- isolate(getSubjectIdByCode(vals$subjectsList, tempSubject))
          if (is.null(subjId))
          {
            ## Creating a new subject with undefined slots except code
            newSubj <- new(Class = "Subject", code = tempSubject, birthdate = as.character(NA), fullname = as.character(NA))
            ## Adding the subject into subjectsList
            isolate(vals$subjectsList <- addSubject(vals$subjectsList, subjObject = newSubj))
            ## Getting the new subject's id
            subjId <- tail(vals$subjectsList@subjectsList$ids, n = 1)
            ## Updating all dataRecords related to the subject
            for (id in recIDs)
            {
              dataRec <- dataSample@DataRecordsList[[which(dataSample@ids == id)]]
              dataRec@subjectID <- subjId
              dataSample <- updateDataRecord(dataSample, id = id, dataRecord = dataRec)
            }    
          }
          else
          {
            warning(paste("The subject with code", tempSubject, "exists! Using Id", subjId))
            for (id in recIDs)
            {
              dataRec <- dataSample@DataRecordsList[[which(dataSample@ids == id)]]
              dataRec@subjectID <- subjId
              dataSample <- updateDataRecord(dataSample, id = id, dataRecord = dataRec)
            }
          }
        }
      }
      if (is.na(dataSample@keys$trialID[i]))
      {
        recID <- dataSample@ids[i]
        tempTrial <- trimws(tempdr$Trial[[which(tempdr$Id == recID)]], "both")
        expID <- dataSample@keys$expID[i]
        if (!is.na(tempTrial) & tempTrial != "" & !is.na(expID))
        {
          recIDs <- tempdr$Id[which(trimws(tempdr$Trial, "both") == tempTrial)]
          trialId <- isolate(getTrialIdByName(vals$trialsList, expID = expID, name = tempTrial))
          if (is.null(trialId))
          {
            ## Creating a new trial with undefined slots except code and expID
            newTrial <- new(Class = "Trial", name = tempTrial, expID = expID, description = as.character(NA))
            ## Adding the trial into subjectsList
            vals$trialsList <- addTrial(vals$trialsList, trialObject = newTrial)
            ## Getting the new trial's id
            trialId <- tail(vals$trialsList@trialsList$ids, n = 1)
            ## Updating all dataRecords related to the subject
            for (id in recIDs)
            {
              dataRec <- dataSample@DataRecordsList[[which(dataSample@ids == id)]]
              dataRec@trialID <- trialId
              dataSample <- updateDataRecord(dataSample, id = id, dataRecord = dataRec)
            }
          }
          else
          {
            warning(paste("The trial with name", tempTrial, "exists! Using Id", trialId))
            for (id in recIDs)
            {
              dataRec <- dataSample@DataRecordsList[[which(dataSample@ids == id)]]
              dataRec@trialID <- trialId
              dataSample <- updateDataRecord(dataSample, id = id, dataRecord = dataRec)
            }
          }
        }
      }
    }
    vals$dataSample <- dataSample
  })
  
  addSelectedSubjectEvent <- observeEvent(input$addSelectedSubject, {
    if (!is.null(input$selectedCode)) 
    {
      recID <- input$tempDataRecordsList_rows_selected
      codeFromTable <- vals$tempDataRecords$Subject[[which(vals$tempDataRecords$Id == recID)]]
      newCode <- codeFromTable
      codeFromUser <- trimws(input$selectedCode, "both")
      if (codeFromUser != "" & codeFromTable != codeFromUser) 
      { 
        newCode <- codeFromUser
      }
      if (input$affectAllRecordsCode)
      {
        ## Find all records Ids with selected codeFromTable
        recIDs <- vals$tempDataRecords$Id[which(vals$tempDataRecords$Subject == codeFromTable)]
      }
      else
      {
        recIDs <- recID
      }
      ## Getting a subject id with entered code (it may exist)
      subjId <- getSubjectIdByCode(vals$subjectsList, newCode)
      ## If it is not exist, adding it to Subjects list and update DataSample
      if (is.null(subjId))
      {
        ## Creating a new subject with undefined slots except code
        newSubj <- new(Class = "Subject", code = codeFromUser, birthdate = as.character(NA), fullname = as.character(NA))
        ## Adding the subject into subjectsList
        vals$subjectsList <- addSubject(vals$subjectsList, subjObject = newSubj)
        ## Getting the new subject's id
        subjId <- tail(vals$subjectsList@subjectsList$ids, n = 1)
        ## Updating all dataRecords related to the subject
        for (id in recIDs)
        {
          dataRec <- vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == id)]]
          dataRec@subjectID <- subjId
          vals$dataSample <- updateDataRecord(vals$dataSample, id = id, dataRecord = dataRec)
        }
      }
      else
      {
        warning(paste("The subject with code", newCode, "exists! Using Id", subjId))
        for (id in recIDs)
        {
          dataRec <- vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == id)]]
          dataRec@subjectID <- subjId
          vals$dataSample <- updateDataRecord(vals$dataSample, id = id, dataRecord = dataRec)
        }
      }
    }
})
  addSelectedTrialEvent <- observeEvent(input$addSelectedTrial, {
    if (!is.null(input$selectedTrial)) 
    {
      recID <- input$tempDataRecordsList_rows_selected
      expName <- vals$tempDataRecords$Experiment[input$tempDataRecordsList_rows_selected]
      expID <- getExperimentIdByName(vals$expList, name = expName)
      nameFromTable <- vals$tempDataRecords$Trial[[which(vals$tempDataRecords$Id == recID)]]
      newName <- nameFromTable
      nameFromUser <- trimws(input$selectedTrial, "both")
      if (nameFromUser != "" & nameFromTable != nameFromUser) 
      { 
        newName <- nameFromUser
      }
      if (input$affectAllRecordsTrial)
      {
        ## Find all records Ids with selected nameFromTable
        recIDs <- vals$tempDataRecords$Id[which(vals$tempDataRecords$Trial == nameFromTable & vals$tempDataRecords$Experiment == expName)]
      }
      else
      {
        recIDs <- recID
      }
      ## Getting a trial id with entered name (it may exist)
      trialId <- getTrialIdByName(self = vals$trialsList, expID = expID, name = newName)
      ## If it is not exist, adding it to Trials list and update DataSample
      if (is.null(trialId))
      {
        ## Creating a new trial with undefined slots except code and expID
        newTrial <- new(Class = "Trial", name = newName, expID = expID, description = as.character(NA))
        ## Adding the trial into subjectsList
        vals$trialsList <- addTrial(vals$trialsList, trialObject = newTrial)
        ## Getting the new trial's id
        trialId <- tail(vals$trialsList@trialsList$ids, n = 1)
        ## Updating all dataRecords related to the subject
        for (id in recIDs)
        {
          dataRec <- vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == id)]]
          dataRec@trialID <- trialId
          vals$dataSample <- updateDataRecord(vals$dataSample, id = id, dataRecord = dataRec)
        }
      }
      else
      {
        warning(paste("The trial with name", newName, "exists! Using Id", trialId))
        for (id in recIDs)
        {
          dataRec <- vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == id)]]
          dataRec@trialID <- trialId
          vals$dataSample <- updateDataRecord(vals$dataSample, id = id, dataRecord = dataRec)
        }
      }
    }
  })
  changeSelectedSubjectEvent <- observeEvent(input$changeSelectedSubject, {
    selSubject <- input$availableSubjects
    if (!is.null(selSubject) & selSubject != "")
    {
      recID <- input$tempDataRecordsList_rows_selected
      codeFromTable <- vals$tempDataRecords$Subject[[which(vals$tempDataRecords$Id == recID)]]
      subjId <- getSubjectIdByCode(vals$subjectsList, selSubject)
      if (input$affectAllRecordsCode2)
      {
        ## Find all records Ids with selected codeFromTable
        recIDs <- vals$tempDataRecords$Id[which(vals$tempDataRecords$Subject == codeFromTable)]
      }
      else
      {
        recIDs <- recID
      }
      for (id in recIDs)
      {
        dataRec <- vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == id)]]
        dataRec@subjectID <- subjId
        vals$dataSample <- updateDataRecord(vals$dataSample, id = id, dataRecord = dataRec)
      }
    }
  })
  changeSelectedTrialEvent <- observeEvent(input$changeSelectedTrial, {
    selTrial <- input$availableTrials
    if (!is.null(selTrial) & selTrial != "")
    {
      recID <- input$tempDataRecordsList_rows_selected
      expName <- vals$tempDataRecords$Experiment[input$tempDataRecordsList_rows_selected]
      expID <- getExperimentIdByName(vals$expList, name = expName)
      nameFromTable <- vals$tempDataRecords$Trial[[which(vals$tempDataRecords$Id == recID)]]
      trialId <- getTrialIdByName(vals$trialsList, expID = expID, name = selTrial)
      if (input$affectAllRecordsTrial2)
      {
        ## Find all records Ids with selected nameFromTable
        recIDs <- vals$tempDataRecords$Id[which(vals$tempDataRecords$Trial == nameFromTable & vals$tempDataRecords$Experiment == expName)]
      }
      else
      {
        recIDs <- recID
      }
      for (id in recIDs)
      {
        dataRec <- vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == id)]]
        dataRec@trialID <- trialId
        vals$dataSample <- updateDataRecord(vals$dataSample, id = id, dataRecord = dataRec)
      }
    }
      
  })
  
  ## Analyze & Graphics Tab
  observe({
    if (!is.null(input$dataRecordsSelectionTable_rows_selected))
    {
      isolate(dataSample <- vals$dataSample)
      selectedRecord <- dataSample@DataRecordsList[[which(dataSample@ids == input$dataRecordsSelectionTable_rows_selected)]]
      if (length(selectedRecord@eyesDataObject@leftEventMarkers) != 0 | length(selectedRecord@eyesDataObject@rightEventMarkers) != 0)
      {
        updateSelectInput(session, 'markPoints', choices = c("No markers", "Filter markers", "Event markers"))
      }
      else
      {
        updateSelectInput(session, 'markPoints', choices = c("No markers", "Filter markers"))
      }
      if (selectedRecord@eyesDataObject@conditions@conditions$eye == "both")
      {
        updateRadioButtons(session, 'eyeForPlot', choices = c("left", "right", "both"))
      }
      if (selectedRecord@eyesDataObject@conditions@conditions$eye == "left")
      {
        updateRadioButtons(session, 'eyeForPlot', choices = c("left"))
      }
      if (selectedRecord@eyesDataObject@conditions@conditions$eye == "right")
      {
        updateRadioButtons(session, 'eyeForPlot', choices = c("right"))
      }
      maxT <- floor(max(selectedRecord@eyesDataObject@time@time))
      updateSliderInput(session, 'timeRange', value = c(0,maxT), min = 0, max = maxT)
      vals$activeDataRecord <- selectedRecord
    }
  })
  
  sample <- reactive(
    {
      vals$filteredDataSample@keys <<- vals$dataSample@keys[vals$dataSampleFilterFlags$Flag == 0,]
      vals$filteredDataSample@ids <<- vals$dataSample@ids[vals$dataSampleFilterFlags$Flag == 0]
      vals$filteredDataSample@DataRecordsList <<- vals$dataSample@DataRecordsList[vals$dataSampleFilterFlags$Flag == 0]
      asDataFrame(vals$filteredDataSample)
    }
  )
  
  observe({
    if (input$expNameForAnalysis == "")
    {
      isolate(vals$dataSampleFilterFlags$Flag <- rep(0, nrow(vals$dataSample@keys)))
    }
    else
    {
      expID <- isolate(getExperimentIdByName(vals$expList, name = input$expNameForAnalysis))
      isolate(vals$dataSampleFilterFlags$Flag <- rep(0, nrow(vals$dataSample@keys)))
      isolate(vals$dataSampleFilterFlags$Flag[vals$dataSample@keys$expID != expID] <- 1)
    }
  })
  
#     if (input$expNameForAnalysis == "")
#     {
#       vals$dataSampleFilterFlags$Flag <- rep(0, nrow(vals$dataSample@keys))
#     }
#     else 
#     {
#       expID <- isolate(getExperimentIdByName(vals$expList, name = input$expNameForAnalysis))
#       if (input$trialNameForAnalysis == "")
#       {
#         isolate(vals$dataSampleFilterFlags$Flag <- rep(0, nrow(vals$dataSample@keys)))
#         vals$dataSampleFilterFlags$Flag[vals$dataSample@keys$expID != expID] <- 1
#       }
#       else
#       {
#         trialID <- getTrialIdByName(vals$trialsList, name = input$trialNameForAnalysis)
#         isolate(vals$dataSampleFilterFlags$Flag <- rep(0, nrow(vals$dataSample@keys)))
#         vals$dataSampleFilterFlags$Flag[vals$dataSample@keys$expID != expID & vals$dataSample@keys$trialID != trialID] <- 1
#       }
#     }
#   })

  
  
  ### Gaze Data subtab
  
  ### Event Detection subtab

  ## Events detection

  ## TO DO: validate inputs
  observeEvent(input$detectEventsForSelected, 
          {
            ## Filter setup
            if (input$filterForDetector == 'External Filter')
            {
              if (is.na(input$extFilterForDetector)) {return()}
              ## Filter using external extFilter
            } else if (input$filterForDetector == 'No filter')
            {
              settings <- list(subfun = noFilter, interpolate = F)
              filter <- createFilter(name = "Standard", fun = coreFilter, settings = settings)
            } else if (input$filterForDetector == 'Core Filter')
            {
              settings <- list(subfun = standardFilter, screenDim = c(input$filterScrDimX, input$filterScrDimY), interpolate = input$interpolate)
              filter <- createFilter(name = "Standard", fun = coreFilter, settings = settings)
            }
            if (input$smootherForDetector == 'External Smoother')
            {
              if (is.na(input$extSmootherForDetector)) {return()}
              ## Smooth using external extSmoother
            } else if (input$smootherForDetector == 'No smoother')
            {
              settings <- list(subfun = noSmoother)
              smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = settings)
            } else if (input$smootherForDetector == 'Core Smoother')
            {
              if (input$smootherSubFun == "Moving Average")
              {
                subfun <- movAvgFilt
                settings <- list(subfun = subfun, fl = input$fl)
              }
              if (input$smootherSubFun == "Moving Average")
              {
                subfun <- medianFilt
                settings <- list(subfun = subfun, fl = input$fl)
              }
              if (input$smootherSubFun == "Savitzky-Golay")
              {
                subfun <- savGolFilt
                settings <- list(subfun = subfun, fl = input$fl, forder = input$forder, dorder = input$dorder)
              }
              smoother <- createSmoother("Standard", fun = coreSmoother, settings = settings)
            }
            
            ## Detector setup
            if (input$useExtDetector)
            {
              if (is.na(input$extDetector)) {return(NULL)}
              ## detect using extDetector
            }
            else
            {
              if (!is.na(input$detector) & input$detector != "")
              {
                if (input$detector == "IVT")
                {
                  settings <- list(subfun = IVT,
                                   postProcess = input$postProcess,
                                   VT = input$vt,
                                   angular = input$angular,
                                   screenDist = input$detectorScreenDist,
                                   screenDim = c(input$detectorScreenDimX, input$detectorScreenDimY),
                                   screenSize = c(input$detectorScreenSizeX, input$detectorScreenSizeY),
                                   MaxTBetFix = input$maxTBetFix/1000,
                                   MaxDistBetFix = input$maxDistBetFix,
                                   minFixLen = input$minFixLen/1000,
                                   maxGapLen = input$maxGapLen/1000,
                                   maxVel = input$maxVel,
                                   maxAccel = input$maxAccel,
                                   classifyGaps = input$classifyGaps)
                  detector <- createDetector("Standard", fun = coreDetector, settings = settings)
                } else
                  if (input$detector == "IDT")
                  {
                    print("IDT")
                  } else
                    if (input$detector == "Adaptive IDT")
                    {
                      print("Adaptive IDT")
                    }
              }
            }
            recIDs <- isolate(input$dataRecordsSelectionTable_rows_selected)
            ## Events detection
            for (recID in recIDs)
            {
              print(paste("Record with ID", recID, "is under processing"))
              res <- detectEvents(self = vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]], filter, smoother, detector)
              vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@leftFilterMarkers <<- res@eyesDataObject@leftFilterMarkers
              vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@rightFilterMarkers <<- res@eyesDataObject@rightFilterMarkers
              vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@leftEventMarkers <<- res@eyesDataObject@leftEventMarkers
              vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@rightEventMarkers <<- res@eyesDataObject@rightEventMarkers
              print(table(vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@leftEventMarkers@eventMarkers))
            }
            print("Done")
          }
  )


  ## Data Export Tab
  ### Save R Session
  saveSession <- observeEvent(input$saveSession,{
    valuesList <- reactiveValuesToList(vals)
    try(
      {
        save(valuesList, file = file.choose(new = T))
      }, silent = T
    )
  })
  



                                       
  ### OUTPUTS ###
  ### Experiments table
  output$experimentsList <- DT::renderDataTable(server = FALSE, {
    datatable(asDataFrame(vals$expList), selection = "single", rownames = F)
  })
  
  ### Subjects table
  output$subjectsList <- DT::renderDataTable(server = FALSE, {
    datatable(asDataFrame(vals$subjectsList), selection = "single", rownames = F)
  })
  
  ### Trials table
  output$trialsList <- DT::renderDataTable(server = FALSE, {
    datatable(asDataFrame(vals$trialsList), selection = "single", rownames = F)
  })
  
  
  ### Raw files table
  output$rawFileList <- DT::renderDataTable(server = FALSE, {
    if (is.null(vals$rawDataFilesList)) return()
    datatable(vals$rawDataFilesList)
  })
  
  ### Raw data records table
  output$rawDataRecordsList <- DT::renderDataTable(server = FALSE, {
    if (is.null(vals$rawDataRecordsList)) return()
    datatable(asDataFrame(vals$rawDataRecordsList), rownames = F)
  })
  
  ### Data records table
  output$tempDataRecordsList <- DT::renderDataTable(server = FALSE, {
    if (is.null(vals$tempDataRecords)) return()
    datatable(vals$tempDataRecords, selection = "single", rownames = F)
  })
  
  output$dataRecordsSelectionTable <- DT::renderDataTable(server = FALSE,{
    smp <- sample()
    if (is.null(smp)) return()
    datatable(smp, selection = "single", rownames = F)
  })
  
  ### External loaders table
  output$extLoadersList <- DT::renderDataTable(server = FALSE, {
      datatable(asDataFrame(vals$extLoaders), selection = "single")
  })
  
  ## SubFunctions Table
  output$selectParamsToEvaluate <- DT::renderDataTable(server = FALSE, {
    datatable(asDataFrame(subFunctions), selection = "single")
  })
  
  

  output$selectedLoaderFunFile <- renderText({
    vals$tempExtLoaderFun
  })
  
  output$selectedLoaderSetFile <- renderText({
    vals$tempExtLoaderSettings
  })

  output$avFields <- renderD3tf({
    
    # Define table properties. See http://tablefilter.free.fr/doc.php
    # for a complete reference
    tableProps <- list(
      btn_reset = F,
      sort = F,
      grid = F
    );
    
    observe({
      if(is.null(input$avFields_edit)) return(NULL);
      edit <- input$avFields_edit;
      
      isolate({
        id <- edit$id; row <- as.integer(edit$row); col <- as.integer(edit$col); val <- edit$val;
        # validate input 
        if(is.na(suppressWarnings(as.numeric(val))) & !(val == ""))
        {
          oldval <- vals$tempAvFlds[row, col];
          rejectEdit(session, tbl = "avFields", row = row, col = col, id = id, value = oldval);
          return(NULL);
        }
        if(any(!is.na(vals$tempAvFlds[,1])))
        {
          if (any(vals$tempAvFlds[which(!is.na(vals$tempAvFlds[,1])),1] == val))
          {
            oldval <- vals$tempAvFlds[row, col];
            rejectEdit(session, tbl = "avFields", row = row, col = col, id = id, value = oldval);
            return(NULL);
          }
        }
        # accept edits
        vals$tempAvFlds[row, col] <- as.numeric(val);
        val = round(as.numeric(val), 1)
        # confirm edits
        confirmEdit(session, tbl = "avFields", row = row, col = col, id = id, value = val);
      })
    })
    
    df <- isolate(vals$tempAvFlds)
    d3tf(df,
         tableProps = tableProps,
         showRowNames = T,
         edit = c("col_1"),
         tableStyle = "table table-bordered");
  })
  
  output$addAvFields <- renderD3tf({
    d3tf(data.frame(a = 1, b = 2),
         tableProps = list(btn_reset = F,
                           sort = F,
                           grid = F),
         showRowNames = T,
         tableStyle = "table table-bordered")

  })
  
  output$headerKeys <- renderD3tf({
    d3tf(data.frame(a = 1, b = 2),
         tableProps = list(btn_reset = F,
                           sort = F,
                           grid = F),
         showRowNames = T,
         tableStyle = "table table-bordered")
    
  })
  
  ### XY plot
  output$XYplot <- renderPlot({
    x <- vals$dataSample@DataRecordsList[[1]]@eyesDataObject@leftEyeSamples@eyeData$porx
    y <- vals$dataSample@DataRecordsList[[1]]@eyesDataObject@leftEyeSamples@eyeData$pory    
    plot(x, y)
  })
  
  output$xtplot <- renderPlot({
    settings <- list(subfun = noFilter, interpolate = F)
    filter <- createFilter(name = "Standard", fun = coreFilter, settings = settings)
    settings <- list(subfun = movAvgFilt, fl = 3)
    smoother <- createSmoother("Standard", fun = coreSmoother, settings = settings)
    plotXt(vals$activeDataRecord, eye = input$eyeForPlot, filter = filter, smoother = smoother, 
           period = input$timeRange, type = input$typeForPlotXt, angular = input$angularPlots, markerType = input$markerType,
           pointsColor = input$pointsColor)
  })
  
  
  session$onSessionEnded(
    function() {
      cat("closed")
    })
})