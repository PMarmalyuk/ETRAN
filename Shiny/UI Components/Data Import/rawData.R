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
        )
)