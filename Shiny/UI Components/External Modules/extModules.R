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
)