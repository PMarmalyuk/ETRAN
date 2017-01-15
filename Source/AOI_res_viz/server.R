library(shiny)


shinyServer(
  
  function(input, output, session) {
    #clientEnvfrMatrix<-.GlobalEnv$clientEnvfrMatrix
    #load full AOI collection to client
    observeEvent(input$waitFrData,{
      print("call for frData")
      msg<-list()
      if(is.matrix(clientEnvfrMatrix)){
        msg$data<-clientEnvfrMatrix
        msg$legend<-colnames(clientEnvfrMatrix)
        session$sendCustomMessage('loadFrMatrixHandler', msg)    } 
      else {print(paste("empty request data"))}
    })          
    
    
  }
  )
