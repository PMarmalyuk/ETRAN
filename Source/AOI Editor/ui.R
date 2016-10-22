library(shiny)
source("SharedData.R")

# Define UI for dataset viewer application
shinyUI(
  
  fluidPage(
    tags$head(tags$script(src='CustomJs/fabric.js')),
    #GUI and server interaction JS
    tags$head(tags$script(src='CustomJs/globals.js')),
    #GUI and server interaction JS
    tags$head(tags$script(src='CustomJs/fabric_extras.js')),
    #GUI and server interaction JS
    tags$head(tags$script(src='CustomJs/stim.js')),
    #GUI and server interaction JS
    tags$head(tags$script(src='CustomJs/AOI.js')),
    #GUI and server interaction JS
    tags$head(tags$script(src='CustomJs/AOI_sets.js')),
    #dual list multiple select library
    
    tags$head(tags$script(src='CustomJs/jquery.bootstrap-duallistbox.js')),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="../CustomJs/bootstrap-duallistbox.css" ) ) ,
    
    
    sidebarPanel(  
      id="ImgCollectionPannel",
      #Stimul editor container  
      tags$div(id='LoadImgContainerClicker',HTML(paste("<h4>Image collection editor</h4>"))),
      tags$div(id = 'LoadImgContainer', class= "clickerItem", HTML(paste(
        fileInput('stimImgShnFileSource','Choose stimul file'),
        textInput('stimNameShnInput','Enter name for this stimul',''),
        numericInput('stimWidthShnInput','You could change width parameter',0),
        numericInput('stimHeightShnInput','You could change Height parameter',0),
        
        fluidRow(actionButton('saveStim','Load',align='center'), 
                 actionButton('repStim','Replace',align='center'),
                 actionButton('delStim','Delate',align='center'), align='center' 
        ),
        
        selectInput('stimNameList','Existing image names', choices = pictSelectorOptions()),
        hiddenInput('imgMetaDataStorage'),
        sep='',collapse='') )
      ),
      
      #AOI editor container
      tags$div(id='CreateAOIContainerClicker',HTML(paste("<h4>Area of interest</H4>"))),
      tags$div(id ='CreateAOIContainer', class= "clickerItem", HTML(paste( 
        
        radioButtons('aoigroup','', choices = list("rect","circle"), selected = NULL, inline = TRUE, width = NULL),
        actionButton('AddAOI','NewAOI'),
        actionButton('drawPolygon','Draw polygon'),
        tags$div(id ='snglAOIparams', HTML(paste( 
          textInput('AOINameInput','Enter name for this stimul'),
          actionButton('RenameAOI','Apply new name'),sep='', collapse=''))
        ),
        sep='', collapse='' ))
      ),
      
      #AOI set editor container    
      tags$div(id='CreateSetContainerClicker',HTML(paste("<h4>Area sets</H4>"))),
      tags$div(id = 'CreateSetContainer', class= "clickerItem", HTML(paste( 
        # 
        textInput('AOISetNameInput','Name of selected set'), 
        fluidRow(actionButton('repSet','Apply changes',align='center'),
                 actionButton('delSet','Delate set',align='center'),align='center'
        ),
        
        selectInput('AOIsetList','Existing sets', choices = setSelectorOptions()),
        sep='', collapse='' ))
      )
    ), 
    
    #Main panel Canvas and AOI multiple select
    mainPanel( tags$canvas(id = "imgEditorCanvas", width = "300", height = "350"), img(id='hiddenImgBuffer'),
               tags$div(id = 'SelectContainer', class= "sltrCont", HTML(paste(
                 '<select multiple="multiple" size="5" class="AOImltS" name="duallistbox"></select>',
                 sep='',collapse='' ))      
               )
    )
  ) 
 
)
