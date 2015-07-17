sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Experimental Settings", icon = icon("line-chart"), tabName = "Settings",
             menuSubItem("Experiments List", tabName = "Experiment", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Subjects List", tabName = "Subjects", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Trials List", tabName = "Trials", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Stimuli List", tabName = "Stimuli", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Objects Linkage", tabName = "Linkage", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)
    ),
    menuItem("Data Import", tabName = "dataImport", icon = icon("file"),
             menuSubItem("ETRAN Data", tabName = "etranDataImport", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Gaze Data", tabName = "rawDataImport", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL),
             menuSubItem("Factors Data", tabName = "factorsDataImport", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = NULL)
    ),
    menuItem("AOI Editor", tabName = "AOIEditor", icon = shiny::icon("angle-double-right")),
    menuItem("Data Analysis", icon = icon("line-chart"), tabName = "Analysis"),
    menuItem("External Modules", tabName = "ExtModules", icon = shiny::icon("angle-double-right")),
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

## Experimental Settings tab content
experimentsSubTab <- source("UI Components\\Experimental Settings\\experiments.R")$value
subjectsSubTab <- source("UI Components\\Experimental Settings\\subjects.R")$value
trialsSubTab <- source("UI Components\\Experimental Settings\\trials.R")$value
stimuliSubTab <- source("UI Components\\Experimental Settings\\stimuli.R")$value

## Data import tab content
ETRANDataSubTab <- source("UI Components\\Data Import\\ETRANData.R")$value
rawDataSubTab <- source("UI Components\\Data Import\\rawData.R")$value
factorsDataSubTab <- source("UI Components\\Data Import\\factorsData.R")$value

## AOI Editor Tab content
AOIEditorTab <- source("UI Components\\AOI Editor\\AOIEditor.R")$value

## Data Analysis tab content
# GazeEventsData
dataAnalysisTab <- source("UI Components\\Data Analysis\\dataAnalysis.R")$value

# StatAnalysis
# Clustering

## External Modules Tab Content
extModulesTab <- source("UI Components\\External Modules\\extModules.R")$value


body <- dashboardBody(
  tabItems(
    ## Experimental Settings tab content
    experimentsSubTab,
    subjectsSubTab,
    trialsSubTab,
    stimuliSubTab,
    
    ## Data Import Tab Content
    ETRANDataSubTab,
    rawDataSubTab,
    factorsDataSubTab,
    
    ## AOI Editor Tab content
    AOIEditorTab,
  
    ## External Modules Tab Content
    extModulesTab,
    
    ## Data Analysis tab content
    dataAnalysisTab,
    
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