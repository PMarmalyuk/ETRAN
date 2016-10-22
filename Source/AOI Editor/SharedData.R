clientEnvStimListBuffer<-stm
clientEnvAOIListBuffer<-aoi
clientEnvAOI_setListBuffer<-aoi_set

hiddenInput <- function(id){
  div(class='form-group shiny-input-container', HTML(paste('<input id="',id,'" type="hidden" class="form-control shiny-hidden-input" value="">',sep='',collapse='')))}

pictSelectorOptions<-function(){ return(setNames(als$uids(clientEnvStimListBuffer), als$nams(clientEnvStimListBuffer)))}

setSelectorOptions<-function(){return(setNames(als$uids(clientEnvAOI_setListBuffer), als$nams(clientEnvAOI_setListBuffer)))}

updSetSelector<-function(session,sn){
  inputOptionsValue<-setSelectorOptions()
  updateSelectInput(session, 'AOIsetList', choices = inputOptionsValue, 
                    selected = inputOptionsValue[getNumberByUID(clientEnvAOI_setListBuffer,sn)])}