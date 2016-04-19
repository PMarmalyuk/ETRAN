library(shiny)
source("SharedData.R")

shinyServer(
  
  function(input, output, session) {
    #utility function creates a list of the template that is used to represent image data
    createClientImgSource<-function(input){
      #indirect check on the source of the call, 
      #if no image uid presented in input data, creates a description 
      #from the template provided by the alias library, and input data (e.g. create new image)
      if ( !inputValueRecived(input$stimId) ){
        localImgSource<-als$stmo(
          imgDataURI= input$imgSrc, 
          name=input$stimNameShnInput, 
          imgWidth = as.numeric(input$stimWidthShnInput), 
          imgHeight = as.numeric(input$stimHeightShnInput) )
        #if image uid presented, load it's data from collection, and replace slots
      } else {  
        localImgSource<-als$ebu(clientEnvStimListBuffer,input$stimId)
        localImgSource$imgDataURI<-input$imgSrc
        localImgSource$name<-input$stimNameShnInput
        localImgSource$imgWidth<-as.numeric(input$stimWidthShnInput)
        localImgSource$imgHeight<-as.numeric(input$stimHeightShnInput)
      }
      
      return(localImgSource)
    }
    
    #check for "value not undefined" just bit more readable (profile porpouse of check) then if(length(....))
    inputValueRecived<-function(valueToCheck){return(length(valueToCheck))}
    
    #"On stimulList value change" reload stimul information according to choosen option
    observeEvent(input$stimNameList,{
      #get uid of selected element 
      selected_id<-input$stimNameList
      #load element by uid from STIMUL COLLECTION
      selectedElement<-als$ebu(clientEnvStimListBuffer,selected_id)
      #if collection element succesfully loaded
      if(length(selectedElement)){
        #send msg with replacement data to client
        msg<-list(src=selectedElement$imgDataURI, pictId=selected_id, name=selectedElement$name, imgWidth = 0, imgHeight = 0, isnew=FALSE)
        print("CALL replace data in stimul editor/loadOK")
        session$sendCustomMessage('replaceImgCallbackHandler', msg) }    
      #if couldn't find selected item in stimul collection print error, send resetIO msg
      if(!length(selectedElement)){ 
        msg<-"clearInputs"
        session$sendCustomMessage('resetInputsImgCallbackHandler', msg)
        print("clear call no stimul data found by passed uid") }      })
    
    
    #"On AOIList value change" reload AOI information according to choosen option, equal to commented above     
    observeEvent(input$AOIsetList,{
      
      selected_id<-input$AOIsetList
      selectedElement<-als$ebu(clientEnvAOI_setListBuffer,selected_id)
      
      if(length(selectedElement)){
        setData<-DataFromAOIset(selectedElement,clientEnvAOIListBuffer)
        #get uid's of AOI in selected set
        uids<-lapply(setData,function(x)  x$uid  )
        #get collection numbers of AOI in selectd set
        numbr<-lapply(setData,function(x) als$nbu(clientEnvAOIListBuffer, x$uid)  )
        #send name, uids and numbers of piced set to client (uids temporary not used)
        msg<-list(name=selectedElement$name,ids=as.vector(numbr), uids=as.vector(uids))
        
        print("CALL replace data in AOI set editor/loadOK")
        session$sendCustomMessage('replaceAOI_setCallbackHandler', msg) }    
      
      if(!length(selectedElement)){ 
        msg<-"clearInputs"
        session$sendCustomMessage('resetInputsAOI_setCallbackHandler', msg)
        print("clear call") }      })
    
    
    
    #On press load stimul button
    observeEvent(input$saveStim,{
      if(!inputValueRecived(input$stimId)){
        #create formal image description 
        localImgSource<-createClientImgSource(input)
        #add img to collection
        clientEnvStimListBuffer<<-als$acol(clientEnvStimListBuffer,localImgSource)
        .GlobalEnv$stm<-clientEnvStimListBuffer
        #update image selector options
        inputOptionsValue<-pictSelectorOptions()
        updateSelectInput(session, 'stimNameList', choices = inputOptionsValue, 
                          selected = inputOptionsValue[getNumberByUID(clientEnvStimListBuffer,clientEnvStimListBuffer$lastId)])} 
      else {print(paste("can not save with not empty ID=",input$stimId))}      })
    
    observeEvent(input$repStim,{
      
      replaceAtThisID<-as.numeric(input$stimId)
      print( replaceAtThisID)
      
      if(inputValueRecived(replaceAtThisID) ){
        localImgSource<-createClientImgSource(input)
        str(localImgSource)
        clientEnvStimListBuffer<<-als$rcol(clientEnvStimListBuffer,localImgSource)
        .GlobalEnv$stm<-clientEnvStimListBuffer 
        inputOptionsValue<-pictSelectorOptions()
        updateSelectInput(session, 'stimNameList', choices = inputOptionsValue, 
                          selected = inputOptionsValue[getNumberByUID(clientEnvStimListBuffer,replaceAtThisID)])
      } else {print(paste("could not replace with id NULL, recived Id value=",replaceAtThisID))}  })
    
    observeEvent(input$delStim,{
      delateAtThisID<-input$stimId
      if(inputValueRecived(delateAtThisID)){
        clientEnvStimListBuffer<<-als$dcol(clientEnvStimListBuffer,delateAtThisID)
        .GlobalEnv$stm<-clientEnvStimListBuffer 
        inputOptionsValue<-pictSelectorOptions()
        updateSelectInput(session, 'stimNameList', choices = inputOptionsValue, 
                          selected = inputOptionsValue[1])
      } else {print(paste("could not delate with id NULL, recived Id value=",delateAtThisID))}  })   
    
    # Raw AOI objects operations
    observeEvent(input$newAOIObj,{
      AOIObj<-input$newAOIObj
      if(inputValueRecived(AOIObj)){
        print("recive add AOI request")
        
        AOIo<-als$aoio(name=AOIObj$name, shape=AOIObj$shape, dispositionData=AOIObj$data)
        clientEnvAOIListBuffer<<-als$acol(clientEnvAOIListBuffer,AOIo)
        .GlobalEnv$aoi<-clientEnvAOIListBuffer
        
      } else {print(paste("no valid AOI data recived",replaceAtThisID))}  })
    
    observeEvent(input$repAOIObj,{
      AOIObj<-input$repAOIObj
      if(inputValueRecived(AOIObj$id)){
        print("recive rep AOI request")
        AOIo <- als$ebu(clientEnvAOIListBuffer,AOIObj$id)
        if (!is.null(AOIo)){
          
          AOIo$name<-AOIObj$name
          AOIo$shape<-AOIObj$shape
          AOIo$dispositionData<-AOIObj$data
          
          clientEnvAOIListBuffer<<-als$rcol(clientEnvAOIListBuffer,AOIo)
          .GlobalEnv$aoi<-clientEnvAOIListBuffer
        } else {print(paste("could not find element with uid ",AOIObj$id))}
      } else {print(paste("no valid AOI data recived"))}  })
    
    observeEvent(input$delAOIObj,{
      AOIObjId<-input$delAOIObj
      if(inputValueRecived(AOIObjId)){
        print("recive del AOI request")
        clientEnvAOIListBuffer<<-als$dcol(clientEnvAOIListBuffer,AOIObjId)
        .GlobalEnv$aoi<-clientEnvAOIListBuffer
      } else {print(paste("no valid AOI data recived"))}  })
    
    
    observeEvent(input$newSet,{
      AOIids<-input$newSet$setData
      if(inputValueRecived(AOIids)){
        print("recive add AOIset request")
        
        AOISO<-als$aseto(name=input$newSet$name, UIDs=AOIids)
        clientEnvAOI_setListBuffer<<-als$acol(clientEnvAOI_setListBuffer,AOISO)
        .GlobalEnv$aoi_set<-clientEnvAOI_setListBuffer
        
        updSetSelector(session, clientEnvAOI_setListBuffer$lastId)
      } else {print(paste("no valid AOI data recived",replaceAtThisID))}  })
    
    
    observeEvent(input$repSetMs,{
      SetObj<-input$repSetMs
      print("recive rep AOIset request")
      if(inputValueRecived(SetObj$uid)){
        print("recive rep Set")
        Seto <- als$ebu(clientEnvAOI_setListBuffer,SetObj$uid)
        if (!is.null(Seto)){
          
          Seto$name<-SetObj$name
          Seto$UIDs<-SetObj$setData
          
          clientEnvAOI_setListBuffer<<-als$rcol(clientEnvAOI_setListBuffer,Seto)
          .GlobalEnv$aoi_set<-clientEnvAOI_setListBuffer
          
          updSetSelector(session,SetObj$uid)            
          
        } else {print(paste("could not find element with uid ",SetObj$uid))}
      } else {print(paste("no valid set data recived"))}  })
    
    
    
    observeEvent(input$delSetMs,{
      SetObjId<-input$delSetMs$uid
      if(inputValueRecived(SetObjId)){
        print("recive del AOIset request")
        clientEnvAOI_setListBuffer<<-als$dcol(clientEnvAOI_setListBuffer,SetObjId)
        .GlobalEnv$aoi_set<-clientEnvAOI_setListBuffer
        
        updSetSelector(session, clientEnvAOI_setListBuffer$lastId)
      } else {print(paste("no valid set data recived"))}  })
    
    #load full AOI collection to client
    observeEvent(input$waitForAOI,{
      print("call for AOI")
      msg<-list()
      if(clientEnvAOIListBuffer$lastId>0){
        msg$data<-lapply(clientEnvAOIListBuffer$data,function(x) list(rType=x$shape, rData=x$dispositionData, name=x$name, rId=x$uid)     )
        msg$nextId<-clientEnvAOIListBuffer$lastId+1
        session$sendCustomMessage('loadAOICollectionCallbackHandler', msg)    } 
      else {print(paste("empty request data"))}
    })          
    
    
  })
