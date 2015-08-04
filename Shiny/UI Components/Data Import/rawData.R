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
                                                               selectInput("sep", "Separator:",
                                                                           c("Tab" = "\t",
                                                                             "Comma" = ",",
                                                                             "Semicolon" = ";")),
                                                               selectInput("dec", "Decimal point:",
                                                                           c("Point" = ".",
                                                                             "Comma" = ",")),
                                                               selectInput("encoding", "Encoding:",
                                                                           c("UTF-8" = "utf8",
                                                                             "CP1251" = "cp1251",
                                                                             "ASCII" = "ascii")),
                                                               checkboxInput('header', "Header", value = T),
                                                               numericInput('skip', "Lines to skip", value = 20, min = 0, step = 1),
                                                               textInput('commchar', "Comment Char", value = "#")
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
                                                  box(title = "Configure Conditions", status = "primary", solidHeader = TRUE,
                                                    collapsible = T, collapsed = T, width = 12,
                                                    selectInput(inputId = 'expEye', label = "Recording Mode", choices = c("Left Eye", "Right Eye", "Binocular"), multiple = F, selected = 1),
                                                    numericInput(inputId = 'expSampleRate', label = "Sample Rate (Hz)", value = NA, min = 0, step = 1),
                                                    selectInput(inputId = 'expTimeUnits', label = "Time Unit", choices = c("microseconds", "milliseconds", "seconds"), multiple = F, selected = 1),
                                                    checkboxInput('pupilDataExist', label = "Pupil Data Exist?"),
                                                    conditionalPanel(condition = "input.pupilDataExist",
                                                                     selectInput(inputId = 'expPupSizeUnits', label = "Pupil Size Unit", choices = c("px", "mm", "cm"), multiple = F, selected = 1),
                                                                     selectInput(inputId = 'expPupShape', label = "Pupil Shape", choices = c("circle", "ellipse"), multiple = F, selected = 1)
                                                    ),
                                                    numericInput('expScreenDist', "Screen Distance (cm)", value = NA, min = 1, step = 1),
                                                    numericInput('expScreenSizeX', "Screen Size X (cm)", value = NA, min = 1, step = 1),
                                                    numericInput('expScreenSizeY', "Screen Size Y (cm)", value = NA, min = 1, step = 1),
                                                    numericInput('expScreenDimX', "Screen Dimension X (px)", value = NA, min = 1, step = 1),
                                                    numericInput('expScreenDimY', "Screen Dimension Y (px)", value = NA, min = 1, step = 1)
                                                  ),
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
                             )
                    ),
                    tabPanel(id = "assosiateRecords", title = "Set Associations",
                             fluidRow(
                               box(title = "Parsed Data Records", status = "primary", solidHeader = TRUE,
                                   collapsible = T, width = 12,
                                   DT::dataTableOutput('tempDataRecordsList'),
                                   style = "overflow-x: scroll;",
                                   fluidRow(
                                     column(2,
                                            actionButton("acceptAllAssociations", label = "Accept All", class = "btn btn-large btn-block")
                                            ),
                                     column(5,
                                            fluidRow(
                                              column(6,
                                                     textInput("selectedCode", label = "Subject Code"),
                                                     checkboxInput('affectAllRecordsCode', label = "Affect all records with this subject code", value = T),
                                                     actionButton("addSelectedSubject", label = "Add Selected", class = "btn btn-large btn-block")
                                                     ),
                                              column(6,
                                                     textInput("selectedTrial", label = "Trial Name"),
                                                     checkboxInput('affectAllRecordsTrial', label = "Affect all records with this trial name", value = T),
                                                     actionButton("addSelectedTrial", label = "Add Selected", class = "btn btn-large btn-block")
                                              )
                                            )
                                     ),
                                     column(5,
                                            fluidRow(
                                              column(6,
                                                     selectizeInput("availableSubjects", label = "Choose Subject", multiple = F, choices = NULL, width = '100%'),
                                                     checkboxInput('affectAllRecordsCode2', label = "Affect all records with this subject code", value = T),
                                                     actionButton("changeSelectedSubject", label = "Change Code", class = "btn btn-large btn-block")
                                                     ),
                                              column(6,
                                                     selectizeInput("availableTrials", label = "Choose Trial", multiple = F, choices = NULL, width = '100%'),
                                                     checkboxInput('affectAllRecordsTrial2', label = "Affect all records with this trial name", value = T),
                                                     actionButton("changeSelectedTrial", label = "Change Name", class = "btn btn-large btn-block")
                                                     )
                                            )
                                     )
                                   )
                               )
                             )
                    )
        )
)