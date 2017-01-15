library(shiny)

# Define UI for dataset viewer application
shinyUI(
  
  fluidPage(
    tags$head(tags$script(src='CustomJs/jquery.js')),
    tags$head(tags$script(src='CustomJs/d3_v3.js')),
    tags$head(tags$script(src='CustomJs/d3.stretched.chord.js')),
    tags$head(tags$script(src='CustomJs/d3.layout.chord.sort.js')),
    tags$head(tags$script(src='CustomJs/doChordLayout.js')),
    tags$head(tags$script(src='CustomJs/math.js')),
    tags$head(tags$script(src='CustomJs/html2canvas.js')),
    #GUI and server interaction JS
    tags$head(tags$script(src='CustomJs/svgFromData.js')),

    
    #tags$head(tags$script(src='CustomJs/jquery.bootstrap-duallistbox.js')),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="../CustomJs/chordLayoutStyle.css" ) ) ,
    

    
    #Main panel Canvas and AOI multiple select
    mainPanel( tags$div(id = "chordChart"),
               actionButton('saveChartBtn','Save chart as...',align='center')
    )
  ) 
 
)
