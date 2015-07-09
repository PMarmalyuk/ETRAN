if (!require("devtools")) install.packages('devtools')
if (!require("DT")) install.packages('DT', repos = 'http://cran.rstudio.com')
if (!require("htmlwidgets")) install.packages("htmlwidgets")
if (!require("D3TableFilter")) devtools::install_github("ThomasSiegmund/D3TableFilter")
if (!require("shinydashboard")) install.packages("shinydashboard")

shinyServer(function(input, output, session) {
  ### INITIALISATION ###
  ## Attach packages
  library(devtools)
  library(DT)
  library(htmlwidgets)
  library(D3TableFilter)
  library(shinydashboard)
  
  ## Creating session objects
  source('Initializer\\Initialize.R', local=F)
  
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
                         dataSample = new(Class = "DataSample", keys = data.frame(expID = numeric(), subjectID = numeric(), trialID = numeric()),
                                          eyesDataObjectsList = list(), analysisResultsList = list(), statisticsList = list()),
                         tempExtLoaderFun = NA,
                         tempExtLoaderSettings = NA,
                         extLoaders = new(Class = "Loaders", loadersList = list(ids = list(), loaders = list())),
                         extParsers = new(Class = "Parsers", parsersList = list(ids = list(), parsers = list())),
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
  
  ### EVENT HANDLERS ###

  ## Settings tab
  ### Experiments subtab
  expAddEvent <- observeEvent(input$addExp, {
    name <- isolate(input$expName)
    expDate <- isolate(input$expDate)
    description <- isolate(input$expDescription)
    experimenters <- isolate(input$expExperimenters)
    eye <- isolate(input$expEye)
    sampleRate <- isolate(input$expSampleRate)
    timeUnits <- isolate(input$expTimeUnits)
    pupSizeUnits <- isolate(input$expPupSizeUnits)
    pupShape <- isolate(input$expPupShape)
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
      updateSelectInput(session, inputId = "expTimeUnits", selected = cond@conditions$timeUnits)
      updateSelectInput(session, inputId = "expPupSizeUnits", selected = cond@conditions$pupilSizeUnits)
      updateSelectInput(session, inputId = "expPupShape", selected = cond@conditions$pupilShape)
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
      timeUnits <- isolate(input$expTimeUnits)
      pupSizeUnits <- isolate(input$expPupSizeUnits)
      pupShape <- isolate(input$expPupShape)
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
      rowsToDel <- input$experimentsList_rows_selected
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
    vals$rawDataFilesList <- rbind(vals$rawDataFilesList, data.frame(fileNames = fileNames, stringsAsFactors = F))
  })
  rawDataFilesAddEvent <- observeEvent(input$loadSeparateFilesFromDir, {
    fileNames <- choose.files(default = getwd(), caption = "Select files with raw gaze data")
    vals$rawDataFilesList <- rbind(vals$rawDataFilesList, data.frame(fileNames = fileNames, stringsAsFactors = F))
  })
  rawDataFilesDelSelEvent <- observeEvent(input$deleteSelectedRows, {
    if (length(input$rawFileList_rows_selected) != 0) 
    {    
      df <- vals$rawDataFilesList
      rowsToDel <- input$rawFileList_rows_selected
      df <- data.frame(isolate(df[-(which(rownames(df) %in% intersect(rownames(df), rowsToDel))),]))
      colnames(df) <- c("fileNames")
      vals$rawDataFilesList <- df
    }
  })
  rawDataFilesClearEvent <- observeEvent(input$clearFileList, {
    df <- data.frame(fileNames = NA)[-1,]
    df <- as.data.frame(df)
    colnames(df) <- c("fileNames")
    vals$rawDataFilesList <- df
  })
  
  rawDataFilesLoadEvent <- observeEvent(input$loadRawDataFiles, {
    if (input$useExtLoader)
    {
      if (is.na(input$extLoader)) {return(NULL)}
      
    }
    else
    {
      files <- as.list(vals$rawDataFilesList$fileNames)
      settings <- isolate(list(rawSettings = new(Class = "ReadSettings", readSettings = list(encoding = input$encoding, 
                                                                                     sep = input$sep,
                                                                                     dec = input$dec,
                                                                                     skip = input$skip,
                                                                                     comment.char = input$commchar,
                                                                                     header = input$header))))
      loader <- new(Class = "Loader", name = "Core Loader", fun = createRawDataRec, settings = settings)
      vals$rawDataRecordsList <- addRawDataRecords(vals$rawDataRecordsList, filesList = files, loader = loader)
    }
  })
  
  saveAvDataFieldsEvent <- observeEvent(input$saveAvDataFields, {
    x <- vals$tempAvFlds[,1]
    names(x) <- c("time", "trial", "frame", "stimname", "smptype",
                  "lporx", "lpory", "rporx", "rpory",
                  "lpupxsize", "lpupysize", "rpupxsize", "rpupysize")
    x <- c(x, list(leftAdditionalFields = NA, rightAdditionalFields = NA))
    isolate(vals$avFlds@availableFields <- x)
    print(vals$avFlds)
  })
  
  parseFilesEvent <- observeEvent(input$parseFiles, {
    if (input$useExtParser)
    {
      if (is.na(input$extParser)) {return(NULL)}
      
    }
    else
    {
      if (!is.null(input$expNameForParser))
      {
        expID <- isolate(getExperimentIdByName(vals$expList, name = input$expNameForParser))
        cond <- getConditionsById(vals$expList, expID)
      }
      else
      {
        cond <- new(Class = "Conditions")
      }
      parser <- createParser(name = "Core Parser", fun = coreParser, 
                             settings = list(dataFields = vals$avFlds, 
                                             headerKeys = vals$hdrKeys, 
                                             sampleKey = vals$smpKey, 
                                             sep = vals$parserSep,
                                             conditions = cond))
      records <- lapply(vals$rawDataRecordsList@rawDataRecordsList, FUN = parseDataRecord, parser = parser)
      print(lapply(records, FUN = function(x) {x$subjectCode}))
    }
  })
  
  ### OUTPUTS ###
  ### Experiments table
  output$experimentsList <- DT::renderDataTable(server = FALSE, {
    datatable(asDataFrame(vals$expList), selection = "single")
    
  })
  ### Raw files table
  output$rawFileList <- DT::renderDataTable({
    if (is.null(vals$rawDataFilesList)) return()
    datatable(vals$rawDataFilesList)
  })
  ### Raw data records table
  output$rawDataRecordsList <- DT::renderDataTable({
    if (is.null(vals$rawDataRecordsList)) return()
    asDataFrame(vals$rawDataRecordsList)
  })
  
  ### Data records table
  output$dataRecordsList <- DT::renderDataTable({
    if (is.null(vals$dataSample)) return()
    vals$dataSample@keys
  })
  
  ### External loaders table
  output$extLoadersList <- DT::renderDataTable(server = FALSE, {
      datatable(asDataFrame(vals$extLoaders), selection = "single")
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
  
  output$addLAvFields <- renderD3tf({
    d3tf(data.frame(a = 1, b = 2),
         tableProps = list(btn_reset = F,
                           sort = F,
                           grid = F),
         showRowNames = T,
         tableStyle = "table table-bordered")

  })
  output$addRAvFields <- renderD3tf({
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