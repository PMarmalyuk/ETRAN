sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Settings", icon = icon("line-chart"), tabName = "Settings",
             menuSubItem("Experiment", tabName = "Experiment", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("External Modules", tabName = "ExtModules", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Subjects", tabName = "Subjects", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Trials", tabName = "Trials", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Stimuli", tabName = "Stimuli", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Areas of interest", tabName = "AOIs", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)
    ),
    menuItem("Data Import", tabName = "dataImport", icon = icon("file"),
             fluidPage(
               br(),
               actionButton('loadSession', "Load Session Data", class = "btn btn-large btn-block")  
             ),
             menuSubItem("Gaze Data", tabName = "rawDataImport", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Factors Data", tabName = "factorsDataImport", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)
    ),
    menuItem("Analysis & Graphics", icon = icon("line-chart"), tabName = "Analysis",
             menuSubItem("Gaze Data", tabName = "GazeData", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Event Detection", tabName = "EventDetection", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Parameters Estimation", tabName = "ParamEstimation", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Statistical Analysis", tabName = "StatAnalysis", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Clustering", tabName = "Clustering", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)),
    menuItem("Data Export", icon = icon("line-chart"), tabName = "Functions",
             fluidPage(
               br(),
               actionButton('saveSession', "Save Current Session", class = "btn btn-large btn-block")  
             ),
             menuSubItem("Gaze Data", tabName = "ExportRaw", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Event Data", tabName = "ExportEvents", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Parameters", tabName = "ExportParams", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)
    )
  )
)

body <- dashboardBody(
  tabItems(
    ## Experiment tab content
    tabItem(tabName = "Experiment",
            fluidPage(
              fluidRow(  
                box(
                  title = "Available Experiments", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  DT::dataTableOutput('experimentsList'),
                  textOutput('testtext')
                )),
              fluidRow(
                box(
                  status = "primary", solidHeader = TRUE,
                  collapsible = F, width = 12, 
                  fluidRow(
                    column(3,actionButton('addExp', label = "Add New", class = "btn btn-large btn-block")),
                    column(3,actionButton('viewExp', label = "View Selected", class = "btn btn-large btn-block")),
                    column(3,actionButton('updateExp', label = "Update Selected", class = "btn btn-large btn-block")),
                    column(3,actionButton('delExp', label = "Delete Selected", class = "btn btn-large btn-block"))
                  )
                )),
              fluidRow(
                box(
                  status = "primary", solidHeader = TRUE,
                  collapsible = F, width = 4,
                  textInput(inputId = 'expName', "Experiment Name", value = "My Experiment"),
                  dateInput(inputId = 'expDate', "Experiment Date"),
                  tags$textarea(id = "expDescription", rows=4, cols=27, "My experiment description"),
                  br(),
                  tags$textarea(id = "expExperimenters", rows=4, cols=27, "Experimenters")
                ),
                box(
                  status = "primary", solidHeader = TRUE,
                  collapsible = F, width = 4,
                  selectInput(inputId = 'expEye', label = "Recording Mode", choices = c("Left Eye", "Right Eye", "Binocular"), multiple = F, selected = 1),
                  numericInput(inputId = 'expSampleRate', label = "Sample Rate (Hz)", value = NA, min = 0, step = 1),
                  selectInput(inputId = 'expTimeUnits', label = "Time Unit", choices = c("microseconds", "milliseconds", "seconds"), multiple = F, selected = 1),
                  checkboxInput('pupilDataExist', label = "Pupil Data Exist?"),
                  conditionalPanel(condition = "input.pupilDataExist",
                                   selectInput(inputId = 'expPupSizeUnits', label = "Pupil Size Unit", choices = c("px", "mm", "cm"), multiple = F, selected = 1),
                                   selectInput(inputId = 'expPupShape', label = "Pupil Shape", choices = c("circle", "ellipse"), multiple = F, selected = 1)
                                   )
              ),
                box(
                  status = "primary", solidHeader = TRUE,
                  collapsible = F, width = 4,
                  numericInput('expScreenDist', "Screen Distance (cm)", value = NA, min = 1, step = 1),
                  numericInput('expScreenSizeX', "Screen Size X (cm)", value = NA, min = 1, step = 1),
                  numericInput('expScreenSizeY', "Screen Size Y (cm)", value = NA, min = 1, step = 1),
                  numericInput('expScreenDimX', "Screen Dimension X (px)", value = NA, min = 1, step = 1),
                  numericInput('expScreenDimY', "Screen Dimension Y (px)", value = NA, min = 1, step = 1)
                )
              )
            )
    ),
    tabItem(tabName = "Subjects",
            h2("Subjects tab content")
    ),
    tabItem(tabName = "Trials",
            h2("Trials tab content")
    ),
    tabItem(tabName = "Stimuli",
            h2("Stimuli tab content")
    ),
    tabItem(tabName = "AOIs",
            tabsetPanel(selected = 1, type = "tabs", position = "left",
                        tabPanel("Create", title = "Create your AOIs"),
                        tabPanel("Edit", title = "Edit your AOIs"),
                        tabPanel("View", title = "View your AOIs")
            )
    ),
    ## External Loaders and Parsers subtab
    tabItem(tabName = "ExtModules",
            tabsetPanel(selected = 1, id = "ExtModulesPanel", type = "tabs", position = "left", 
                        tabPanel(id = "loadersConfig", title = "Loaders",
                                 fluidRow(
                                   box(
                                     title = "Available External Data Loaders", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = 12,
                                     DT::dataTableOutput('extLoadersList')
                                   )
                                 ),
                                 fluidRow(
                                   box(
                                     status = "primary", solidHeader = TRUE,
                                     collapsible = F, width = 12, 
                                     actionButton('addExtLoader', label = "Add New"),
                                     actionButton('viewLdr', label = "View Selected"),
                                     actionButton('updateLdr', label = "Update Selected"),
                                     actionButton('delLdr', label = "Delete Selected")
                                   )),
                                 fluidRow(
                                   box(
                                     status = "primary", solidHeader = TRUE,
                                     collapsible = F, width = 12,
                                     textInput('extLoaderName', "Enter Loader Name:"),
                                     actionButton('addExtLoaderFun', "Load Function"),
                                     textOutput(outputId ='selectedLoaderFunFile'),
                                     actionButton('addExtLoaderSettings', "Load Settings"),
                                     textOutput(outputId ='selectedLoaderSetFile')
                                   )
                                 )
                        ),
                        tabPanel(id = "ParsersConfig", title = "Parsers",
                                 DT::dataTableOutput('parsersList')
                        ),
                        tabPanel(id = "FiltersConfig", title = "Filters"
                        ),
                        tabPanel(id = "DetectorsConfig", title = "Detectors"
                        ),
                        tabPanel(id = "DetectorsConfig", title = "Subfunctions"
                        )
            )
    ),
    tabItem(tabName = "loadRDA",
            actionButton("LoadRDA", "Upload RDA file..."),
            textOutput("text1")
    ),
    tabItem(tabName = "rawDataImport",
            tabsetPanel(selected = 1, id = "importTabSetPanel", type = "tabs", position = "left",
                        tabPanel(id = "rawRecords", title = "Load Datafiles",
                                 fluidRow(
                                   box(title = "Data Files", status = "primary", solidHeader = TRUE,
                                       collapsible = F, width = 12,
                                       checkboxInput('useExtLoader', "Use External Loader"),
                                       conditionalPanel(condition = "!input.useExtLoader",
                                                        fluidPage(
                                                          fluidRow(
                                                            column(3,
                                                                   selectInput("sep", "Separator:",
                                                                               c("Tab" = "\t",
                                                                                 "Comma" = ",",
                                                                                 "Semicolon" = ";")),
                                                                   selectInput("dec", "Decimal point:",
                                                                               c("Point" = ".",
                                                                                 "Comma" = ","))
                                                            ),
                                                            column(3,
                                                                   selectInput("encoding", "Encoding:",
                                                                                c("UTF-8" = "utf8",
                                                                                  "CP1251" = "cp1251",
                                                                                  "ASCII" = "ascii")),
                                                                   checkboxInput('header', "Header", value = T)
                                                                   ),
                                                            column(3,
                                                                   numericInput('skip', "Lines to skip", value = 20, min = 0, step = 1),
                                                                   textInput('commchar', "Comment Char", value = "#")
                                                            )
                                                          )
                                                        )),
                                       conditionalPanel(
                                         condition = "input.useExtLoader",
                                         selectizeInput(
                                           'extLoader', label = NULL, choices = NULL,
                                           options = list(
                                             placeholder = 'Please choose external loader',
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )
                                         )
                                       ),
                                       fluidRow(
                                         column(6, actionButton("loadFilesFromDir", "Directory...", class = "btn btn-large btn-block")),
                                         column(6, actionButton("deleteSelectedRawRec", "Delete Selected", class = "btn btn-large btn-block"))
                                       ),
                                       fluidRow(
                                         column(6, actionButton("loadSeparateFilesFromDir", "Files...", class = "btn btn-large btn-block")),
                                         column(6, actionButton("clearRawRecList", "Clear List", class = "btn btn-large btn-block"))
                                       ),
                                       br(),
                                       DT::dataTableOutput('rawDataRecordsList'),
                                       style = "overflow-x: scroll;"
                                       
                                   )
                                 )
                        ),
                        tabPanel(id = "parseFiles", title = "Parse files",
                                 fluidRow(
                                   fluidPage(checkboxInput('useExtParser', "Use External Parser")),
                                   conditionalPanel(condition = "!input.useExtParser",
                                                    box(
                                                      title = "Core Parser Settings", status = "primary", solidHeader = TRUE,
                                                      collapsible = T, width = 12,
                                                      box(title = "Configure Data Fields", status = "primary", solidHeader = TRUE,
                                                          collapsible = T, collapsed = T, width = 12,
                                                          fluidPage(
                                                            actionButton("saveAvDataFields", "Save Data Fields", class = "btn btn-large btn-block"),
                                                            fluidRow(
                                                              column(4,
                                                                     "Set columns positions presented in dataset:"),
                                                              column(4,
                                                                     checkboxInput("hasAddAvFields", label = "Add Data Fields"))
                                                            ),
                                                            fluidRow(
                                                              column(4,
                                                                     d3tfOutput('avFields', height = "auto")
                                                              ),
                                                              column(4,
                                                                     conditionalPanel(
                                                                       condition = "input.hasAddAvFields",
                                                                       radioButtons(inputId = "eyeAddAvFlds", label = "Select Eye", choices = c("Left", "Right")),
                                                                       textInput("nameAddField", "Specify Name:", value = NULL),
                                                                       numericInput("posAddField", "Specify Position:", value = 1, min = 1),
                                                                       actionButton("addAddField", "Add Data Field", class = "btn btn-large btn-block")
                                                                     )
                                                              ),
                                                              column(4,
                                                                     conditionalPanel(
                                                                       condition = "input.hasAddAvFields",
                                                                       d3tfOutput('addAvFields', height = "auto") 
                                                                     )
                                                                     )
                                                            )
                                                          )
                                                      ),
                                                      box(title = "Configure Header Keys", status = "primary", solidHeader = TRUE,
                                                          collapsible = T, collapsed = T, width = 12,
                                                          textInput("keyName", "Specify Key Name:", value = NULL),
                                                          textInput("keyString", "Specify Key String:", value = NULL),
                                                          actionButton("addKey", "Add Key"),
                                                          d3tfOutput('headerKeys', height = "auto")
                                                      ),
                                                      box(title = "Additional Settings", status = "primary", solidHeader = TRUE,
                                                          collapsible = T, collapsed = T, width = 12,
                                                          textInput("sampleKey", "Specify Correct Sample Flag:", value = "SMP"),
                                                          selectInput("sepForParser", "Separator:",
                                                                      c("Tab" = "\t",
                                                                        "Comma" = ",",
                                                                        "Semicolon" = ";"))
                                                      )
                                                    )),
                                   conditionalPanel(condition = "input.useExtParser",
                                                    box(title = "External Parser", status = "primary", solidHeader = TRUE,
                                                        collapsible = T, width = 12,
                                                        selectInput('extParser', "Choose External Parser:",
                                                                    choices = NULL)
                                                    )
                                   )
                                 ),
                                 fluidRow(
                                   box(
                                     title = "Parse Raw Data Records", status = "primary", solidHeader = TRUE,
                                     collapsible = T, width = 12,
                                     fluidRow(
                                       column(6,
                                              selectizeInput(
                                                inputId = 'expNameForParser', label = NULL, choices = NULL,
                                                options = list(
                                                  placeholder = 'Please select an experiment',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                )
                                              )
                                       ),
                                       column(6,
                                              conditionalPanel(condition = "input.expNameForParser != ''",
                                                               actionButton("parseFiles", "Parse Raw Data Records..."))
                                              
                                       )
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   box(title = "Parsed Data Records", status = "primary", solidHeader = TRUE,
                                       collapsible = T, width = 12,
                                       DT::dataTableOutput('tempDataRecordsList'),
                                       style = "overflow-x: scroll;"
                                   )
                                 )
                        ),
                        tabPanel(id = "assosiateRecords", title = "Set Associations",
                                 h2("Yo")
                        )
                        )),
    tabItem(tabName = "factorsDataImport",
            h2("factorsDataImport tab content")
    ),
    tabItem(tabName = "GazeData",
            h2("GazeData tab content")
    ),
    ## Analysis tab content
    tabItem(tabName = "EventDetection",
            fluidRow(
              box(title = "Filtering Settings", status = "primary", solidHeader = TRUE,
                  collapsible = T, width = 6,
                  selectInput(inputId = 'filterForDetector', label = "Select Filter:", choices = c("No filter", "Core Filter", "External Filter")),
                  conditionalPanel(condition = "input.filterForDetector == 'Core Filter'",
                                   numericInput('filterScrDimX', label = "Max X Coordinate", value = 1280, min = 0),
                                   numericInput('filterScrDimY', label = "Max Y Coordinate", value = 1024, min = 0),
                                   actionButton('useExpConditions1', "Use Values Specified in Experiment Conditions"),
                                   checkboxInput('interpolate', label = "Interolate Gaps?")
                  ),
                  conditionalPanel(condition = "input.filterForDetector == 'External Filter'",
                                   selectInput('extFilterForDetector', label = "Choose Ext Filter", choices = NULL)
                  )
              ),
              box(title = "Smoothing Settings", status = "primary", solidHeader = TRUE,
                  collapsible = T, width = 6,
                  selectInput(inputId = 'smootherForDetector', label = "Select Smoother:", choices = c("No smoother", "Core Smoother", "External Smoother")),
                  conditionalPanel(condition = "input.smootherForDetector == 'Core Smoother'",
                                   selectInput(inputId = 'smootherSubFun', label = "Select Method:", 
                                               choices = c("Moving Average", "Moving Median", "Savitzky-Golay")),
                                   numericInput(inputId = "fl", label = "Window width:", value = 3, min = 3),
                                   conditionalPanel(condition = "input.smootherSubFun == 'Savitzky-Golay'",
                                                    numericInput(inputId = "forder", label = "Filter Order:", value = 2, min = 2),
                                                    numericInput(inputId = "dorder", label = "Drivative Order:", value = 1, min = 1)
                                   )
                  ),
                  conditionalPanel(condition = "input.smootherForDetector == 'External Smoother'",
                                   selectInput('extSmootherForDetector', label = "Choose Ext Smoother", choices = NULL)
                  )
              )
            ),
            fluidRow(
              box(title = "Detection Settings", status = "primary", solidHeader = TRUE,
                  collapsible = T, width = 12,
                  checkboxInput('useExtDetector', label = "Use External Detector?"),
                  conditionalPanel(condition = "input.useExtDetector",
                                   selectInput('extDetector', label = "Choose External Detector:", choices = c())
                  ),
                  conditionalPanel(condition = "!input.useExtDetector",
                                   selectInput('detector', label = "Choose Detection Algorithm:", choices = c("IVT", "IDT", "Adaptive IDT")),
                                   conditionalPanel(condition = "input.detector == 'IVT'",
                                                    numericInput('vt', label = "Velocity Threshold (px/angles per second)", value = 30, min = 0, step = 1)
                                   ),
                                   conditionalPanel(condition = "input.detector == 'IDT'",
                                                    numericInput('dt', label = "Dispersion Threshold (px/angles)", value = 12345, min = 0, step = 1)
                                   ),
                                   conditionalPanel(condition = "input.detector == 'Adaptive IDT'",
                                                    numericInput('adadt', label = "Dispersion Threshold (px/angles)", value = 12345, min = 0, step = 1)
                                   ),
                                   checkboxInput('angular', label = "Use Angular Velocities?"),
                                   conditionalPanel(condition = "input.angular",
                                                    fluidRow(
                                                      column(12,
                                                      box(title = "Settings For Angular Velocity Estimation", status = "primary", solidHeader = TRUE,
                                                          collapsible = T, width = 12,
                                                          actionButton('useExpConditions2', "Use Values Specified in Experiment Conditions"),
                                                          numericInput('detectorScreenDist', "Screen Distance (cm)", value = 70, min = 1, step = 1),
                                                          numericInput('detectorScreenSizeX', "Screen Size X (cm)", value = 33.7, min = 1, step = 1),
                                                          numericInput('detectorScreenSizeY', "Screen Size Y (cm)", value = 27, min = 1, step = 1),
                                                          numericInput('detectorScreenDimX', "Screen Dimension X (px)", value = 1280, min = 1, step = 1),
                                                          numericInput('detectorScreenDimY', "Screen Dimension Y (px)", value = 1024, min = 1, step = 1)
                                                      )                                                      
                                                    )
                                                    )

                                   ),
                                   checkboxInput('postProcess', label = "Enable Events Post-processing?"),
                                   conditionalPanel(condition = "input.postProcess",
                                                    conditionalPanel(condition = "input.detector == 'IVT'",
                                                                     box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                                         collapsible = T, width = 12,
                                                                         numericInput('maxTBetFix', "Maximum Time Between Fixations (ms)", value = 75, min = 1, step = 1),
                                                                         numericInput('maxDistBetFix', "Maximum Distance Between Fixations (px/angles)", value = 0.5, min = 0, step = 1),
                                                                         numericInput('maxGapLen', "Maximum Gap Duration (ms)", value = 75,  min = 1, step = 1),
                                                                         numericInput('maxVel', "Maximum Velocity (px/angles per second)",value = 1000,  min = 1, step = 1),
                                                                         numericInput('maxAccel', "Maximum Accelleration",value = 1000000, min = 1, step = 1),
                                                                         numericInput('minFixLen', "Minimum Fixation Duration (ms)",value = 50,  min = 1, step = 1),
                                                                         checkboxInput('classifyGaps', label = "Classify Gaps?")
                                                                     )

                                                    ),
                                                    conditionalPanel(condition = "input.detector == 'IDT'",
                                                                     box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                                         collapsible = T, width = 12,
                                                                         h2("Some params for IDT")
                                                                     )
                                                                     
                                                    ),
                                                    conditionalPanel(condition = "input.detector == 'Adaptive IDT'",
                                                                     box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                                         collapsible = T, width = 12,
                                                                         h2("Some params for AdaIDT")
                                                                     )
                                                                     
                                                    )
                                   )
                                   
                  )
              )
            ),
            DT::dataTableOutput('dataRecordsForDetector'),
            actionButton('detectEvents', label = "Detect Events for Selected Records")
    ),
    tabItem(tabName = "ParamEstimation",
            h2("ParamEstimation tab content")
    ),
    tabItem(tabName = "StatAnalysis",
            h2("StatAnalysis tab content")
    ),
    tabItem(tabName = "Clustering",
            h2("Clustering tab content")
    ),
    # Export tab content
    tabItem(tabName = "ExportRaw",
            h2("ExportRaw tab content")
    ),
    tabItem(tabName = "ExportEvents",
            h2("ExportEvents tab content")
    ),
    tabItem(tabName = "ExportParams",
            h2("ExportParams tab content")
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "ETRAN Shiny App v0.1", titleWidth = 100),
  sidebar,
  body
)