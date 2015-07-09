library(shiny)
library(shinydashboard)
library(DT)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Settings", icon = icon("line-chart"), tabName = "Settings",
             menuSubItem("Experiment", tabName = "Experiment", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("External Loaders and Parsers", tabName = "DataImportSettings", href = NULL, newtab = TRUE,
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
             menuSubItem("R Session", tabName = "loadRDA", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Gaze Data", tabName = "rawDataImport", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Factors Data", tabName = "factorsDataImport", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)
    ),
    menuItem("Analysis", icon = icon("line-chart"), tabName = "Analysis",
             menuSubItem("Event Detection", tabName = "EventDetection", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Parameters Estimation", tabName = "ParamEstimation", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Statistical Analysis", tabName = "StatAnalysis", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Clustering", tabName = "Clustering", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)),
    menuItem("Visuzalisations", icon = icon("line-chart"), tabName = "Visuzalisations",
             menuSubItem("Raw Data", tabName = "RawDataVis", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("EventData", tabName = "EventDataVis", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Distributions", tabName = "DistributionsVis", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Dependencies", tabName = "DependenciesVis", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Clusters", tabName = "ClustersVis", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)),
    
    menuItem("Functions", icon = icon("line-chart"), tabName = "Functions",
             menuSubItem("Explore", tabName = "ExploreFun", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Edit", tabName = "EditFun", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)),
    menuItem("Data Export", icon = icon("line-chart"), tabName = "Functions",
             menuSubItem("Session", tabName = "LoadSession", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
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
                  DT::dataTableOutput('experimentsList')
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
                  selectInput(inputId = 'expPupSizeUnits', label = "Pupil Size Unit", choices = c("px", "mm", "cm"), multiple = F, selected = 1),
                  selectInput(inputId = 'expPupShape', label = "Pupil Shape", choices = c("circle", "ellipse"), multiple = F, selected = 1)
                ),
                box(
                  status = "primary", solidHeader = TRUE,
                  collapsible = F, width = 4,
                  numericInput('expScreenDist', "Screen Distance (cm)", value = NA, min = 0, step = 1),
                  numericInput('expScreenSizeX', "Screen Size X (cm)", value = NA, min = 0, step = 1),
                  numericInput('expScreenSizeY', "Screen Size Y (cm)", value = NA, min = 0, step = 1),
                  numericInput('expScreenDimX', "Screen Dimension X (px)", value = NA, min = 0, step = 1),
                  numericInput('expScreenDimY', "Screen Dimension Y (px)", value = NA, min = 0, step = 1)
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
    tabItem(tabName = "DataImportSettings",
            tabsetPanel(selected = 1, id = "importTabSetPanel", type = "tabs", position = "left", 
                        tabPanel(id = "loadersConfig", title = "Configure External Loaders",
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
                        tabPanel("ParsersConfig", title = "Configure Parsers",
                                   DT::dataTableOutput('parsersList')
                                 )
    )
    ),
    tabItem(tabName = "loadRDA",
            actionButton("LoadRDA", "Upload RDA file..."),
            textOutput("text1")
    ),
    tabItem(tabName = "rawDataImport",
            tabsetPanel(selected = 1, id = "importTabSetPanel", type = "tabs", position = "left",
                        tabPanel(id = "rawFiles", title = "Load files",
                                 fluidRow(
                                   box(title = "Data Files List", status = "primary", solidHeader = TRUE,
                                       collapsible = T, width = 12,
                                       DT::dataTableOutput('rawFileList'),
                                       style = "overflow-x: scroll;"
                                   )
                                 ),
                                 fluidRow(
                                   box(status = "primary", solidHeader = TRUE,
                                       collapsible = F, width = 12,
                                       actionButton("loadFilesFromDir", "Directory..."),
                                       actionButton("loadSeparateFilesFromDir", "Files..."),
                                       actionButton("deleteSelectedRows", "Delete Selected"),
                                       actionButton("clearFileList", "Delete All"),
                                       actionButton("loadRawDataFiles", "Load Data")
                                       
                                   )
                                 ),
                                 fluidRow(
                                   box(status = "primary", solidHeader = TRUE,
                                       collapsible = F, width = 12,
                                       checkboxInput('useExtLoader', "Use External Loader"),
                                       conditionalPanel(condition = "!input.useExtLoader",
                                                        fluidPage(
                                                          fluidRow(
                                                            column(4,
                                                                   selectInput("encoding", "Encoding:",
                                                                               c("UTF-8" = "utf8",
                                                                                 "CP1251" = "cp1251",
                                                                                 "ASCII" = "ascii")),
                                                                   selectInput("sep", "Separator:",
                                                                               c("Tab" = "\t",
                                                                                 "Comma" = ",",
                                                                                 "Semicolon" = ";")),
                                                                   selectInput("dec", "Decimal point:",
                                                                               c("Point" = ".",
                                                                                 "Comma" = ","))
                                                            ),
                                                            column(4,
                                                                   numericInput('skip', "Lines to skip", value = 20, min = 0, step = 1),
                                                                   textInput('commchar', "Comment Char", value = "#"),
                                                                   checkboxInput('header', "Header", value = T)
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
                                       )
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
                                                  checkboxInput("hasLAddAvFields", label = "Add Left Eye Fields")),
                                           column(4, 
                                                  checkboxInput("hasRAddAvFields", label = "Add Right Eye Fields"))
                                         ),
                                         fluidRow(
                                           column(4,
                                                  d3tfOutput('avFields', height = "auto")
                                           ),
                                           column(4,
                                                  conditionalPanel(
                                                    condition = "input.hasLAddAvFields",
                                                    textInput("nameLAddField", "Specify Name:", value = NULL),
                                                    numericInput("posLAddField", "Specify Position:", value = 1, min = 1),
                                                    actionButton("addLAddField", "Add Left Eye Field", class = "btn btn-large btn-block"),
                                                    d3tfOutput('addLAvFields', height = "auto")  
                                                  )
                                           ),
                                           column(4,
                                                  conditionalPanel(
                                                    condition = "input.hasRAddAvFields",
                                                    textInput("nameRAddField", "Specify Name:", value = NULL),
                                                    numericInput("posRAddField", "Specify Position:", value = 1, min = 1),
                                                    actionButton("addRAddField", "Add Right Eye Field", class = "btn btn-large btn-block"),
                                                    d3tfOutput('addRAvFields', height = "auto")  
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
                                              actionButton("parseFiles", "Parse Raw Data Records...")
                                       )
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   box(title = "Loaded Raw Data Records", status = "primary", solidHeader = TRUE,
                                       collapsible = T, width = 6,
                                       DT::dataTableOutput('rawDataRecordsList'),
                                       style = "overflow-x: scroll;"
                                   ),
                                   box(title = "Parsed Data Records", status = "primary", solidHeader = TRUE,
                                       collapsible = T, width = 6,
                                       DT::dataTableOutput('dataRecordsList'),
                                       style = "overflow-x: scroll;"
                                   )
                                 )
            ))),
    tabItem(tabName = "factorsDataImport",
            h2("factorsDataImport tab content")
    ),
    ## Analysis tab content
    tabItem(tabName = "EventDetection",
            h2("EventDetection tab content")
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

    ## Visuzalisations tab content       
    tabItem(tabName = "RawDataVis",
            h2("RawDataVis tab content")
    ),
    tabItem(tabName = "EventDataVis",
            h2("EventDataVis tab content")
    ),
    tabItem(tabName = "DistributionsVis",
            h2("DistributionsVis tab content")
    ),
    tabItem(tabName = "DependenciesVis",
            h2("DependenciesVis tab content")
    ),
    tabItem(tabName = "ClustersVis",
            h2("ClustersVis tab content")
    ),
    
    ## Functions tab content       
    tabItem(tabName = "ExploreFun",
            h2("ExploreFun tab content")
    ),
    tabItem(tabName = "EditFun",
            h2("EditFun tab content")
    ),
    
    ## Export tab content
    tabItem(tabName = "SaveSession",
            h2("SaveSession tab content")
    ),
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