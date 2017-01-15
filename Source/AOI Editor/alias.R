source("routines.R", chdir=T)
source("collections.R", chdir=T)
source("private.R", chdir=T)
source("prototype.R", chdir=T)
source("AOI.R", chdir=T)
source("AOISet.R", chdir=T)


als<-list()

als$enm$ALIAS<-ls(als)
#enumerator
als$enm<-list()
als$enm$cln<-list("STIMUL_COLLECTION",
                   "AOI_COLLECTION",
                   "BINDING_COLLECTION",
                   "SETS_COLLECTION",
                   "TAS",
                   "TRIAL")
als$enm$cln<-setNames(als$enm$cln,als$enm$cln)

#collection accesors aliases
als$new<-createCollection
als$acol<-addCollectionElement
als$rcol<-repCollectionElement
als$dcol<-delCollectionElement

#collection getters aliases
als$ebn<-getElementByName
als$ebu<-getElementByUID
als$nbu<-getNumberByUID
als$sbu<-getSignatureByUID


als$uids<-getUIDs
als$nams<-getNames


#aoi aliases
als$aoio<- AOIstructure

als$elp<-ellipseStructure
als$plg<-polyhedronStructure




#selectors aliases
als$slr$cl<-function(val){eval.parent(substitute(ls()[sapply(ls(),function(x) is.list(get(x)) && !is.null(get(x)$cls) && get(x)$cls==as.character(val))]))}
als$slr$nm<-function(val){eval.parent(substitute(ls()[sapply(ls(),function(x) is.list(get(x)) && !is.null(get(x)$name) && get(x)$name==as.character(val))]))}

als$shst<-function(collection){return (collection$state_history)}
als$sgns<-function(){return (als$signatureCollection$HASH_E)}
#
als$nch<-function(o,n){chainInfo[match(o,als$chainInfo$linked),"lcoked"]<-n}

als$scl<-signatureCollection
als$chi<-chainInfo

#stimul aliases
als$stmo<- stimulStructure

#sets aliases
als$aseto<- AOISetStructure

showFrequencyChart<-function(m){
  clientEnvfrMatrix<<-m
  #options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  runApp(appDir = "../AOI_res_viz", launch.browser = getOption("shiny.launch.browser", interactive()))
}