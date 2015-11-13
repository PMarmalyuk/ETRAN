shinyServer(function(input, output, session) {
  ### INITIALISATION ###
  ## Attach packages
  # library(devtools)
  # library(DT)
  # library(htmlwidgets)
  # library(D3TableFilter)
  # library(shinydashboard)
  
  ## Creating session objects
  source('Initialize.R', local = T)
  source('CoreSubFunctionsInit.R', local = T)
  ## Creating reactive values
  
  vals <- reactiveValues(expList = new(Class = "Experiments"),
                         subjectsList = new(Class = "Subjects"),
                         trialsList = new(Class = "Trials"),
                         fileNames = c(),
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
                         defaultFilterMarkersNames = new(Class = "FilterMarkers")@markerNames,
                         defaultEventMarkersNames = new(Class = "EventMarkers")@markerNames,
                         tempDataRecords = data.frame(Id = numeric(), File = character(), Experiment = character(), Trial = character(), Subject = character(), Associated = logical()),
                         dataSample = new(Class = "DataSample", keys = data.frame(expID = numeric(), 
                                                                                  subjectID = numeric(), 
                                                                                  trialID = numeric()),
                                          DataRecordsList = list()),
                         filteredDataSample = new(Class = "DataSample", keys = data.frame(expID = numeric(), 
                                                                                          subjectID = numeric(), 
                                                                                          trialID = numeric()),
                                                  DataRecordsList = list()),
                         
                         # dataSampleUpdateFlags = data.frame(Id = numeric(), Flag = numeric()),
                         dataSampleFilterFlags = data.frame(Id = numeric(), Flag = logical()),
                         #dataSampleFilters = list(ids = numeric(), filters = list(), groups = numeric()),
                         
                         tempExtLoaderFun = NA,
                         tempExtLoaderSettings = NA,
                         extLoaders = new(Class = "Loaders", loadersList = list(ids = list(), loaders = list())),
                         extParsers = new(Class = "Parsers", parsersList = list(ids = list(), parsers = list())),
                         extFilters = new(Class = "Filters", filtersList = list(ids = list(), filters = list())), 
                         extSmoothers = new(Class = "Smoothers", smoothersList = list(ids = list(), smoothers = list())), 
                         defaultLoader = new(Class = "Loader", name = "Core Loader", fun = createRawDataRec, 
                                             settings = list(rawSettings = new(Class = "ReadSettings"))),
                         # factors = factors,
                         recordsFactorsData = new(Class = "FactorsData"),
                         subFunctions = subFunctions,
                         activeSubFunctionsIDs = subFunctions@subFunctionsList$ids
                         )

  activeFilter <- reactive(
    {
      defFMNames <- vals$defaultFilterMarkersNames
      settings <- list(subfun = noFilter, interpolate = F, filterMarkerNames = defFMNames)
      filter <- createFilter(name = "Standard", fun = coreFilter, settings = settings)
      if (input$filterType == 'External Filter')
      {
        if (is.na(input$extFilter)) {return(filter)}
        ## TO DO: Filter using external extFilter
      } else if (input$filterType == 'No filter')
      {
        settings <- list(subfun = noFilter, interpolate = F, filterMarkerNames = defFMNames)
        filter <- createFilter(name = "Standard", fun = coreFilter, settings = settings)
      } else if (input$filterType == 'Core Filter')
      {
        settings <- list(subfun = standardFilter, interpolate = input$interpolate, filterMarkerNames = defFMNames)
        if (!input$useExpConditions)
        {
          settings <- append(settings, list(screenResolution = c(input$filterScrResX, input$filterScrResY)))
        }
        else
        {
          rec <- activeDataRecord()
          conditions <- isolate(rec@eyesDataObject@conditions@conditions)
          if (input$useExpConditions)
          {
            filter@settings$screenResolution <- conditions$screenResolution
          }
        }
        filter <- createFilter(name = "Standard", fun = coreFilter, settings = settings)
      }
      filter
    }
  )

  # TO DO: external smoother
  activeSmoother <- reactive({
    if (is.null(input$smootherType))
    {
      smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = list(subfun = noSmoother))
    }  
    if (input$smootherType == 'External Smoother')
    {
      if (is.na(input$extSmoother)) {return(smoother)}
      ## Smooth using external extSmoother
    } else if (input$smootherType == 'No smoother')
    {
      smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = list(subfun = noSmoother))
    } else if (input$smootherType == 'Core Smoother')
    {
      if (input$smootherSubFun == "Moving Average")
      {
        settings <- list(subfun = movAvgFilt, fl = input$fl)
      }
      if (input$smootherSubFun == "Moving Median")
      {
        settings <- list(subfun = medianFilt, fl = input$fl)
      }
      if (input$smootherSubFun == "Savitzky-Golay")
      {
        settings <- list(subfun = savGolFilt, fl = input$fl, forder = input$forder)
      }
      smoother <- createSmoother("Standard", fun = coreSmoother, settings = settings)
    }
    smoother
  })

  activeDataRecord <- reactive({
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
      
      isolate(updateSliderInput(session, 'timeRange', value = c(0,maxT), min = 0, max = maxT))
      
      return(selectedRecord)
    } else return(NULL)
  })
    
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
    updateSelectInput(session = session, inputId = "extFilter", choices = vars)
  })
  
  observe({
    vars <- sapply(vals$extSmoothers@smoothersList$smoothers, FUN = function(x) {x@name})
    updateSelectInput(session = session, inputId = "extSmoother", choices = vars)
  })
  
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
  
  observe({
    updateSelectizeInput(session, 'availableSubjects', choices = getSubjectCodes(vals$subjectsList), server = TRUE)
  })
  
  ## updateFlags mechanism
#   observe({
#     updateFlags <- vals$dataSampleUpdateFlags
#     subFuns <- isolate(vals$subFunctions)
#     if (nrow(updateFlags) == 0) {return(NULL)}
#     for (i in 1:nrow(updateFlags))
#     {
#       dataRecID <- updateFlags$Id[i]
#       isolate(dataRec <- vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == dataRecID)]])
#       if (updateFlags$Flag[i] == 1)
#       {
#         isolate(subFuns <- getSubfunctions(self = subFuns, operation = "Record Analysis"))
#         isolate(activeSubFuns <- vals$activeSubFunctionsIDs)
#         sfToApply <- subFuns@subFunctionsList$subFunctions[subFuns@subFunctionsList$ids %in% activeSubFuns]
#         estimator <- createEstimator(name = "Standard", fun = coreEstimator,
#                                      settings = list(subFunctions = sfToApply))
#         print("Evaluating data record's parameters")
#         res <- estimateParams(self = dataRec, estimator = estimator)
#         isolate(vals$recordsFactorsData <- addParamsValues(recordsFactorsData = vals$recordsFactorsData, 
#                                                            recID = dataRecID, 
#                                                            availableFactors = vals$factors, 
#                                                            estimatorResult = res))
#       }
#       if (updateFlags$Flag[i] == 2)
#       {
#         isolate(subFuns <- getSubfunctions(self = subFuns, operation = "Event Analysis"))
#         isolate(activeSubFuns <- vals$activeSubFunctionsIDs)
#         sfToApply <- subFuns@subFunctionsList$subFunctions[subFuns@subFunctionsList$ids %in% activeSubFuns]
#         isolate(analyzer <- createAnalyzer(name = "Standard", fun = coreEventAnalyzer, 
#                                    settings = list(subFunctions = sfToApply,
#                                                    availableFactors = vals$factors)))
#         print("Evaluating events parameters")
#         dataRec <- eventAnalyzer(dataRec, analyzer)
#         print("Evaluating events parameters descriptive statistics")
#       }
#       isolate(vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == dataRecID)]] <- dataRec)
#       isolate(vals$dataSampleUpdateFlags$Flag[i] <- 0)
#     }
#   })

  # EVENT HANDLERS #
  expAddEvent <- observeEvent(input$memoryInfo,{
    print(object.size(reactiveValuesToList(vals)))
    
  })
  
  
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
  # loadFilesFromDir & loadSeparateFilesFromDir events handlers
  
  files1 <- observe({
    if (input$loadFilesFromDir)
    {
      rawFilesDir <- choose.dir(default = getwd(), caption = "Select a folder with raw gaze datafiles")
      isolate(vals$fileNames <<- append(vals$fileNames, list.files(path = rawFilesDir, pattern = NULL, all.files = FALSE,
                              full.names = TRUE, recursive = FALSE,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)))
    }
  })
  
  files2 <- observe({
    if (input$loadSeparateFilesFromDir)
    {
      isolate(vals$fileNames <<- append(vals$fileNames, choose.files(default = getwd(), caption = "Select files with raw gaze data")))
    }
  })
  
#   filesToLoad <- reactive({
#     
#     print(input$loadFilesFromDir)
#     print(input$loadSeparateFilesFromDir)
#     if (input$loadFilesFromDir | input$loadSeparateFilesFromDir)
#     {
#       if (input$loadFilesFromDir)
#       {
#         print("Load Files From Directory")
#         rawFilesDir <- choose.dir(default = getwd(), caption = "Select a folder with raw gaze datafiles")
#         fileNames <- list.files(path = rawFilesDir, pattern = NULL, all.files = FALSE,
#                                 full.names = TRUE, recursive = FALSE,
#                                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
#       }
#       if (input$loadSeparateFilesFromDir)
#       {
#         print("Load Separate Files")
#         fileNames <- choose.files(default = getwd(), caption = "Select files with raw gaze data")
#       }
#       return(fileNames)
#     }
# 
#   })
#   
#   # set vals$fileNames
#   observe({
#     files <- filesToLoad()
#     isolate(vals$fileNames <- files)
#   })
  
  deleteSelectedFilesEvent <- observeEvent(input$deleteSelectedFiles, {
    if (length(input$filesList_rows_selected) != 0) 
    {    
      idsToDel <- as.numeric(input$filesList_rows_selected)
      vals$fileNames <- vals$fileNames[-idsToDel]
    }
  })
  clearFilesListEvent <- observeEvent(input$clearFilesList, {
    vals$fileNames <- c()
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
    if (input$useExtLoader)
    {
      if (is.na(input$extLoader)) {return(NULL)}
      else
      {
        stop("External loader is not implemented!")
      }
    }
    else
    {
      loaderSettings <- isolate(list(rawSettings = new(Class = "ReadSettings", readSettings = list(encoding = input$encoding, 
                                                                                             sep = input$sep,
                                                                                             dec = input$dec,
                                                                                             skip = input$skip,
                                                                                             comment.char = input$commchar,
                                                                                             header = input$header))))
      loader <- new(Class = "Loader", name = "Core Loader", fun = createRawDataRec, settings = loaderSettings)
      rawDataRecords <- new(Class = "RawDataRecords")
      rawDataRecords <- addRawDataRecords(rawDataRecords, filesList = vals$fileNames, loader = loader)
      
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
      scrResX <- isolate(input$expScreenResX)
      scrResY <- isolate(input$expScreenResY)
      conditions <- new(Class = "Conditions", conditions = list(eye = eye, sampleRate = sampleRate, timeUnits = timeUnits, 
                                                          pupilSizeUnits = pupSizeUnits, pupilShape = pupShape, screenDistance = screenDist, 
                                                          screenSize = c(scrSizeX, scrSizeY), screenResolution = c(scrResX, scrResY)))
      avFields <- vals$avFlds 
      # vals$avFlds
      parser <- createParser(name = "Core Parser", fun = coreParser, 
                             settings = list(dataFields = vals$avFlds, 
                                             headerKeys = vals$hdrKeys, 
                                             sampleKey = vals$smpKey, 
                                             sep = vals$parserSep,
                                             conditions = conditions))
      
      records <- lapply(rawDataRecords@rawDataRecordsList$rawDataRecords, FUN = parseDataRecord, parser = parser)
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
          # vals$dataSampleUpdateFlags <- rbind(vals$dataSampleUpdateFlags, data.frame(Id = Id, Flag = 0))
          vals$dataSampleFilterFlags <- rbind(vals$dataSampleFilterFlags, data.frame(Id = Id, Flag = T))
          incProgress(step, detail = paste("Parsing Raw Data Record Number", i))
        }
      })
      vals$tempDataRecords <- rbind(vals$tempDataRecords, data.frame(Id = tail(vals$dataSample@ids, n = numRec),
                                                                     File = filePaths,
                                                                     Experiment = rep(expName, numRec),
                                                                     Trial = trials,
                                                                     Subject = subjectCodes,
                                                                     stringsAsFactors = F))
      vals$fileNames <- c()
    }
  })
  
  ### Set Associations tab panel
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
  
  ## Forming data sample for output (applying complex filter)
  sample <- reactive(
    {
      vals$filteredDataSample@keys <- vals$dataSample@keys[vals$dataSampleFilterFlags$Flag,]
      vals$filteredDataSample@ids <- vals$dataSample@ids[vals$dataSampleFilterFlags$Flag]
      vals$filteredDataSample@DataRecordsList <- vals$dataSample@DataRecordsList[vals$dataSampleFilterFlags$Flag]
      asDataFrame(vals$filteredDataSample)
    }
  )
  
  ## TO DO: finish trial filter, test both filters and debug
  observe({
    if (input$expNameForAnalysis == "")
    {
      isolate(vals$dataSampleFilterFlags$Flag <- rep(T, nrow(vals$dataSample@keys)))
    }
    else
    {
#       expID <- isolate(getExperimentIdByName(vals$expList, name = input$expNameForAnalysis))
#       if (input$trialNameForAnalysis == "")
#       {
#         isolate(vals$dataSampleFilterFlags$Flag <- rep(T, nrow(vals$dataSample@keys)))
#         vals$dataSampleFilterFlags$Flag[vals$dataSample@keys$expID != expID] <- F
#       }
#       else
#       {
#         trialID <- getTrialIdByName(vals$trialsList, name = input$trialNameForAnalysis, expID)
#         isolate(vals$dataSampleFilterFlags$Flag <- rep(T, nrow(vals$dataSample@keys)))
#         vals$dataSampleFilterFlags$Flag[vals$dataSample@keys$expID != expID & vals$dataSample@keys$trialID != trialID] <- F
#       }
      expID <- isolate(getExperimentIdByName(vals$expList, name = input$expNameForAnalysis))
      isolate(vals$dataSampleFilterFlags$Flag <- rep(T, nrow(vals$dataSample@keys)))
      ## It seems that next line is incorrect (has logical error)
      isolate(vals$dataSampleFilterFlags$Flag[vals$dataSample@keys$expID != expID] <- F)
    }
  })

  ## Events detection
  ## TO DO: validate inputs
  observeEvent(input$detectEventsForSelected, 
          {
            ## Detector setup
            if (input$useExternalDetector)
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
                                   MaxTBetFix = input$maxTBetFix/1000,
                                   MaxDistBetFix = input$maxDistBetFix,
                                   minFixLen = input$minFixLen/1000,
                                   maxGapLen = input$maxGapLen/1000,
                                   maxVel = input$maxVel,
                                   maxAccel = input$maxAccel,
                                   classifyGaps = input$classifyGaps,
                                   velType = ifelse(input$velocityEstimationMethod == "Finite Differentiation", "finDiff", "analytical"),
                                   fl = input$fLength)
                  detector <- createDetector("Standard", fun = coreDetector, settings = settings)
                } else
                  if (input$detector == "IDT")
                  {
                    settings <- list(subfun = IDT,
                                     angular = input$angular,
                                     dispersionThreshold = input$dispT,
                                     durationThreshold = input$durT
                                     )
                    detector <- createDetector("Standard", fun = coreDetector, settings = settings)
                  } else
                    if (input$detector == "Adaptive IVT")
                    {
                      settings <- list(subfun = ANH,
                                       angular = input$angular,
                                       maxSaccadeVel = input$ANHmaxSaccadeVel,
                                       maxSaccadeAcc = input$ANHmaxSaccadeAcc,
                                       minSaccadeDur = input$ANHminSaccadeDur/1000,
                                       minFixationDur = input$ANHminFixationDur/1000,
                                       velType = ifelse(input$velocityEstimationMethod == "Finite Differentiation", "finDiff", "analytical"),
                                       fl = input$fLength)
                      detector <- createDetector("Standard", fun = coreDetector, settings = settings)
                    }
              }
            }
            recIDs <- isolate(input$dataRecordsSelectionTable_rows_selected)
            ## Events detection
            for (recID in recIDs)
            {
              print(paste("Record with ID", recID, "is under processing"))
              dataRecord <- isolate(vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]])
              conditions <- dataRecord@eyesDataObject@conditions@conditions
              filterForDetector <- activeFilter()
              smootherForDetector <- activeSmoother()
              if (input$useExpConditions)
              {
                filterForDetector@settings$screenResolution <- conditions$screenResolution
              }
              detector@settings <- append(detector@settings, conditions)
              res <- detectEvents(self = dataRecord, filter = filterForDetector, smoother = smootherForDetector, detector = detector)
              vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@leftFilterMarkers <<- res@eyesDataObject@leftFilterMarkers
              vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@rightFilterMarkers <<- res@eyesDataObject@rightFilterMarkers
              vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@leftEventMarkers <<- res@eyesDataObject@leftEventMarkers
              vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@rightEventMarkers <<- res@eyesDataObject@rightEventMarkers
              print("Samples distribution by event types:")
              print(table(vals$dataSample@DataRecordsList[[which(vals$dataSample@ids == recID)]]@eyesDataObject@leftEventMarkers@eventMarkers))
              # vals$dataSampleUpdateFlags$Flag[vals$dataSampleUpdateFlags$Id %in% recID] <- 2
            }
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

  ### Files to load table
  output$filesList <- DT::renderDataTable(server = FALSE, {
    files <- vals$fileNames
    datatable(data.frame(Filename = files), rownames = F)
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
  
  ## SubFunctions Tables
  output$subFunctions <- DT::renderDataTable(server = FALSE, {
    sfList <- getSubfunctions(self = vals$subFunctions)
    sfListDF <- asDataFrame(sfList)[,-c(4,5,6)]
    sfListDF <- cbind(sfListDF, list(Active = ifelse(any(sfListDF$Id == vals$activeSubFunctionsIDs), T, F)))
    datatable(sfListDF, selection = "single", rownames = F)
  })
  
  output$subFunctions2 <- DT::renderDataTable(server = FALSE, {
    sfList <- getSubfunctions(self = vals$subFunctions)
    sfListDF <- asDataFrame(sfList)[,-c(4,5,6)]
    sfListDF <- cbind(sfListDF, list(Active = ifelse(any(sfListDF$Id == vals$activeSubFunctionsIDs), T, F)))
    datatable(sfListDF, selection = "single", rownames = F)
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
#   output$XYplot <- renderPlot({
#     x <- vals$dataSample@DataRecordsList[[1]]@eyesDataObject@leftEyeSamples@eyeData$porx
#     y <- vals$dataSample@DataRecordsList[[1]]@eyesDataObject@leftEyeSamples@eyeData$pory    
#     plot(x, y)
#   })
  
  xtPlot <- reactive({
    rec <- activeDataRecord()
    if (is.null(rec)) return(NULL)
    
    rec <- dataFilter(rec, activeFilter())
    rec <- dataSmoother(rec, activeSmoother())
    settings <- list(plotType = input$typeForPlotXt,
                     eye = input$eyeForPlot,
                     period = input$timeRange,
                     markerType = input$markerType,
                     angular = input$angularPlots,
                     pointsColor = input$pointsColor,
                     velType = ifelse(input$velEstMethodForPlot == "Finite Differentiation", "finDiff", "analytical"),
                     fl = input$filterLengthForPlot)
    plotXt(rec, settings)
  })
  
  output$xtplot <- renderPlot({
    xtPlot()
  })
  
  ## Dynamic UI Components
  output$subFunctionSettings <- renderUI({
    UIList <- list()
    row <- input$eventSubFunctions_rows_selected
    if (is.null(row) | length(row) > 1) return(NULL)
    subFuns <- isolate(vals$subFunctions)
    subFun <- subFuns@subFunctionsList$subFunctions[[which(subFuns@subFunctionsList$ids == row)]]
    # TO DO: construct input form for selected SubFunction settings
    # now it is just an example
    if (row == 2)#(length(subFun@settings) != 0)
    {
      UIList <- append(UIList, list(checkboxInput("angular", "Angular?", value = F)))
    }
    UIList <- append(UIList, list(actionButton("updateSettings", "Update Settings")))
    do.call(tagList, UIList)
  })
  
  
  session$onSessionEnded(
    function() {
      cat("closed")
    })
})