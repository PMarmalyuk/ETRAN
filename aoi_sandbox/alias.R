source("routines.R")
source("collections.R")
source("private.R")
source("prototype.R")
source("AOI.R")
source("AOISet.R")
source("stimul.R")

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