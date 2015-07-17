shinyServer(function(input, output, session) {
  ### INITIALISATION ###
  ## Attach packages
  library(devtools)
  library(DT)
  library(htmlwidgets)
  library(D3TableFilter)
  library(shinydashboard)
  
  ## Creating session objects
  source('Initialize.R')
  

  
  ## Creating reactive values
  vals <- reactiveValues(expList = new(Class = "Experiments"),
                         rawDataFilesList = data.frame(fileNames = NA, colnames = c("FileNames"))[-1,],
                         rawDataRecordsList = new(Class = "RawDataRecords", rawDataRecordsList = list()),
                         tempAvFlds = data.frame(Position = c(1, 3, NA, NA, 2, 10, 11, NA, NA, 6, 7, NA, NA),
                                                 row.names = c("Time", "Trial", "Frame", "Stimuli Name", "Sample Type",
                                                               "PORX (Left Eye)", "PORY (Left Eye)", "PORX (Right Eye)", "PORY (Right Eye)",
                                                               "Horis. Pupil Size (Left Eye)", "Vert. Pupil Size (Left Eye)",
                                                               "Horis. Pupil Size (Right Eye)", "Vert. Pupil Size (Right Eye)")),
                         tempAddLAvFlds = data.frame(),
                         tempAddRAvFlds = data.frame(),
                         avFlds = new(Class = "AvailableDataFields"),
                         hdrKeys = new(Class = "HeaderKeys"),
                         smpKey = "SMP",
                         parserSep = "\t",
                         tempDataRecords = data.frame(),
                         dataSample = new(Class = "DataSample", keys = data.frame(expID = numeric(), subjectID = numeric(), trialID = numeric()),
                                          DataRecordsList = list()),
                         tempExtLoaderFun = NA,
                         tempExtLoaderSettings = NA,
                         extLoaders = new(Class = "Loaders", loadersList = list(ids = list(), loaders = list())),
                         extParsers = new(Class = "Parsers", parsersList = list(ids = list(), parsers = list())),
                         extFilters = new(Class = "Filters", filtersList = list(ids = list(), filters = list())), 
                         extSmoothers = new(Class = "Smoothers", smoothersList = list(ids = list(), smoothers = list())), 
                         defaultLoader = new(Class = "Loader", name = "Core Loader", fun = createRawDataRec, 
                                             settings = list(rawSettings = new(Class = "ReadSettings"))))

  observe({
    vars <- getExperimentsNames(vals$expList)
    updateSelectInput(session = session, inputId = "expNameForParser", choices = vars)
  })
  
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
  
  ### EVENT HANDLERS ###
    ## Save Session button
    saveSession <- observeEvent(input$saveSession,{
                                  valuesList <- reactiveValuesToList(vals)
                                  try(
                                    {
                                      save(valuesList, file = file.choose(new = T))
                                    }, silent = T
                                  )
                              })
  ## Load Session Data button
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

  ## Settings tab
  ### Experiments subtab
  expAddEvent <- observeEvent(input$addExp, {
    name <- isolate(input$expName)
    expNames <- getExperimentsNames(vals$expList)
    if (name %in% expNames) {warning("Experiment with this name already exists"); return()}
    
    
    expDate <- isolate(input$expDate)
    description <- isolate(input$expDescription)
    experimenters <- isolate(input$expExperimenters)
    eye <- isolate(input$expEye)
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
    newExp <- new(Class = "Experiment", name = name, expDate = as.character(expDate), description = description, experimenters = experimenters,
                  conditions = new(Class = "Conditions", conditions = list(eye = eye, sampleRate = sampleRate, timeUnits = timeUnits, 
                                   pupilSizeUnits = pupSizeUnits, pupilShape = pupShape, screenDistance = screenDist, 
                                   screenSize = c(scrSizeX, scrSizeY), screenDim = c(scrDimX, scrDimY))))
    vals$expList <- addExperiment(vals$expList, newExp)
  })
  expViewEvent <- observeEvent(input$viewExp, {
    selRows <- input$experimentsList_rows_selected
    if (length(selRows) != 0)
    {
      df <- asDataFrame(vals$expList)
      exp <- df[which(rownames(df) == selRows),]
      cond <- getConditionsById(vals$expList, id = exp$Id)
      updateTextInput(session, inputId = "expName", value = exp$Name)
      updateDateInput(session, inputId = "expDate", value = exp$Date)
      updateTextInput(session, inputId = "expDescription", value = exp$Description)
      updateTextInput(session, inputId = "expExperimenters", value = exp$Experimenters)
      updateSelectInput(session, inputId = "expEye", selected = cond@conditions$eye)
      if (cond@conditions$timeUnits == 1E-6) {tUn <- "microseconds"}
      if (cond@conditions$timeUnits == 1E-3) {tUn <- "milliseconds"}
      if (cond@conditions$timeUnits == 1) {tUn <- "seconds"}
      updateSelectInput(session, inputId = "expTimeUnits", selected = tUn)
      if (!is.na(cond@conditions$pupilSizeUnits))
      {
        updateCheckboxInput(session, inputId = "pupilDataExist", value = T)
        updateSelectInput(session, inputId = "expPupSizeUnits", selected = cond@conditions$pupilSizeUnits)
        updateSelectInput(session, inputId = "expPupShape", selected = cond@conditions$pupilShape)
      }
      else
      {
        updateCheckboxInput(session, inputId = "pupilDataExist", value = F)
      }
      updateNumericInput(session, inputId = "expSampleRate", value = cond@conditions$sampleRate)
      updateNumericInput(session, inputId = "expScreenDist", value = cond@conditions$screenDistance)
      updateNumericInput(session, inputId = "expScreenSizeX", value = cond@conditions$screenSize[1])
      updateNumericInput(session, inputId = "expScreenSizeY", value = cond@conditions$screenSize[2])
      updateNumericInput(session, inputId = "expScreenDimX", value = cond@conditions$screenDim[1])
      updateNumericInput(session, inputId = "expScreenDimY", value = cond@conditions$screenDim[2])
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
      eye <- isolate(input$expEye)
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
      newExp <- new(Class = "Experiment", name = name, expDate = as.character(expDate), description = description, experimenters = experimenters,
                    conditions = new(Class = "Conditions", conditions = list(eye = eye, sampleRate = sampleRate, timeUnits = timeUnits, 
                                                                             pupilSizeUnits = pupSizeUnits, pupilShape = pupShape, screenDistance = screenDist, 
                                                                             screenSize = c(scrSizeX, scrSizeY), screenDim = c(scrDimX, scrDimY))))
      vals$expList <- updateExperimentById(self = vals$expList, id = id, expObject = newExp)
    }
  })
  expDeleteEvent <- observeEvent(input$delExp, {
    selRows <- input$experimentsList_rows_selected
    if (length(selRows) != 0)
    { 
      df <- asDataFrame(vals$expList)
      rowsToDel <- as.numeric(input$experimentsList_rows_selected)
      idsToDel <- df[which(rownames(df) %in% intersect(rownames(df), rowsToDel)),1]
      vals$expList <- delExperimentsById(self = vals$expList, ids = idsToDel)
    }
  })
  
  
  ### External Loaders and Parsers subtab
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
  rdaFileChooseEvent <- eventReactive(input$LoadRDA, {
    rdaFilepath <- choose.files(default = getwd(), caption = "Select RDA file", multi = F, filters = "RData")
    sessionDataFile <<- rdaFilepath
    load(sessionDataFile)
    print(ls())
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
      
    }
    else
    {
      if (!is.null(input$expNameForParser) & input$expNameForParser != "")
      {
        expID <- isolate(getExperimentIdByName(vals$expList, name = input$expNameForParser))
        cond <- getConditionsById(vals$expList, expID)
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
      uniqueSubjCodes <- unique(subjectCodes)
      subjectIds <- 1:length(uniqueSubjCodes)
      
      trials <- unlist(lapply(records, FUN = function(x) {x$trials}))
      uniqueTrialCodes <- unique(trials)
      trialsIds <- 1:length(uniqueTrialCodes)
      
      expIDs <- rep(expID, length(eyesDataObjects))
      vals$tempDataRecords <- data.frame(filePaths, expIDs, subjectCodes, trials)
      
      numRec <- length(trials)
      step <- 1/numRec
      withProgress(message = 'Parsing Data', value = step, {
        for (i in 1:length(eyesDataObjects))
        {
          subjId <- subjectIds[which(uniqueSubjCodes == subjectCodes[i])]
          trialId <- trialsIds[which(uniqueTrialCodes == trials[i])]
          rec <- new(Class = "DataRecord", expID = as.numeric(expIDs[i]), subjectID = subjId, trialID = trialId, 
                     eyesDataObject = eyesDataObjects[[i]])
          vals$dataSample <- addDataRecord(vals$dataSample, rec)
          incProgress(step, detail = paste("Parsing Raw Data Record Number", i))
        }
      })
    }
  })
  
  ## Analyze & Graphics Tab
  ### Gaze Data subtab
  
  ### Event Detection subtab
  detectEventsEvent <- observeEvent(input$detectEvents,{
    ## Filter setup
    if (input$filterForDetector == 'External Filter')
    {
      if (is.na(input$extFilterForDetector)) {return()}
      ## Filter using external extFilter
    }
    else
    if (input$filterForDetector == 'No filter')
    {
      settings <- list(subfun = noFilter, interpolate = F)
      filter <- createFilter(name = "Standard", fun = coreFilter, settings = settings)
    } else
    if (input$filterForDetector == 'Core Filter')
    {
      settings <- list(subfun = standardFilter, screenDim = c(input$filterScrDimX, input$filterScrDimY), interpolate = input$interpolate)
      filter <- createFilter(name = "Standard", fun = coreFilter, settings = settings)
    }
    
    ## Smoother setup
    if (input$smootherForDetector == 'External Smoother')
    {
      if (is.na(input$extSmootherForDetector)) {return()}
      ## Smooth using external extSmoother
    } else
    if (input$smootherForDetector == 'No smoother')
    {
      settings <- list(subfun = noSmoother)
      smoother <- createSmoother(name = "Standard", fun = coreSmoother, settings = settings)
    } else
    if (input$smootherForDetector == 'Core Smoother')
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
    ## Data Records selection
    if (length(input$dataRecordsForDetector_rows_selected) != 0)
    {
      subsample <- input$dataRecordsForDetector_rows_selected
    }
    else
    {
      subsample <- 1:length(vals$dataSample)
    }
    ## Events detection
    for (i in subsample)
    {
      res <- detectEvents(vals$dataSample@DataRecordsList[[1]], filter, smoother, detector)
      #vals$dataSample@eyesDataObjectsList[[i]]@eyesDataObject@leftFilterMarkers <- res@eyesDataObject@leftFilterMarkers
#       vals$dataSample@eyesDataObjectsList[[i]]@eyesDataObject@rightFilterMarkers <- res@eyesDataObject@rightFilterMarkers
#       vals$dataSample@eyesDataObjectsList[[i]]@eyesDataObject@leftEventMarkers <- res@eyesDataObject@leftEventMarkers
#       vals$dataSample@eyesDataObjectsList[[i]]@eyesDataObject@rightEventMarkers <- res@eyesDataObject@rightEventMarkers
    }
    print("Events were detected")
  })
                                       
  ### OUTPUTS ###
  ### Experiments table
  output$experimentsList <- DT::renderDataTable(server = FALSE, {
    datatable(asDataFrame(vals$expList), selection = "single", rownames = F)
  })
  
  ### Raw files table
  output$rawFileList <- DT::renderDataTable({
    if (is.null(vals$rawDataFilesList)) return()
    datatable(vals$rawDataFilesList)
  })
  
  ### Raw data records table
  output$rawDataRecordsList <- DT::renderDataTable({
    if (is.null(vals$rawDataRecordsList)) return()
    datatable(asDataFrame(vals$rawDataRecordsList), rownames = F)
  })
  
  ### Data records table
  output$tempDataRecordsList <- DT::renderDataTable({
    if (is.null(vals$tempDataRecords)) return()
    vals$tempDataRecords
  })
  
  output$dataRecordsForDetector <- DT::renderDataTable({
    if (is.null(vals$dataSample)) return()
    vals$dataSample@keys
  })
  
  ### External loaders table
  output$extLoadersList <- DT::renderDataTable(server = FALSE, {
      datatable(asDataFrame(vals$extLoaders), selection = "single")
  })

  output$dataRecordsSelectionTable <- DT::renderDataTable(server = FALSE, {
    Codes <- c("Pablo", "Grinch", "Borislav")
    datatable(data.frame(Code = Codes, stringsAsFactors = F), selection = "single")
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
        id <- edit$id;row <- as.integer(edit$row);col <- as.integer(edit$col);val <- edit$val;
        # validate input 
        if(is.na(suppressWarnings(as.numeric(val))))
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
  
  
  session$onSessionEnded(
    function() {
    cat("closed")
  })
})