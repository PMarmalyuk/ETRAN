library(digest)
integrityErrorMsg<-"Unhandled data modification was made, by external function call\
integrity safe data modification, use only data acess functions provided by 'collection' library\
'addCollectionElement', 'repCollectionElement', delCollectionElement\
you could manualy disable 'unhandled data modification' restrictions,\
by setting collection$integrityControl=FALSE"

inc <- function(x)
{
  eval.parent(substitute(x <- x + 1))
  return(x)
}

check_integrity<-function(){
  if(get("collection", parent.frame())$integrityControl){
    
    real_state<-digest(get("collection", parent.frame())$data)
    stored_state<-get("collection", parent.frame())$state
    
    if(real_state==stored_state){
    
      return("stateOk") } else {eval.parent(parse(text="stop(integrityErrorMsg)"))
                                return("stateError")}
    } else {return("operation was made with no integrity control")}
}

set_validationcode<-function(){
  eval.parent(parse(text="collection$state_history[[length(collection$state_history)+1]]<-collection$state
                          collection$state<-digest(collection$data)"))
}

new_element_pos<-function(){
  position<-length(get("collection", parent.frame())$data)+1
  return(position)
}


createCollection<-
  function (name="default", cls="collection", data=list(), signature=list(), lastId=0, integrityControl=TRUE, state=digest(list()), state_history=list()){
    return( sapply(ls(), function(x) get(x)   ))
  }


getUIDs<- function(collection){check_integrity()
                                return(sapply(collection$data, function(x) x$uid))}


getNames<- function(collection){check_integrity()
                                return(sapply(collection$data, function(x) x$name))}


getNumberByUID<- function(collection, UID){check_integrity()
                                            return(   head(which(getUIDs(collection) == UID), 1)   )}

getNumberByName<- function(collection, name){check_integrity()
                                            condition<-sapply(collection$data, function(element) element$name == name)
                                            return(   head(which(getUIDs(collection) == name), 1)   )}

getElementByUID<-function(collection,UID){check_integrity()
                                          condition<-sapply(collection$data, function(element) element$uid == UID)
                                          if(sum(condition)){return(collection$data[condition][[1]])} else {return (NULL)}}

getElementByName<-function(collection,name){check_integrity()
                                            condition<-sapply(collection$data, function(element) element$name == name)
                                            if(length(condition)){return(collection$data[condition][[1]])} else {return (NULL)}}
addCollectionElement<-
  function(collection,element){
    #print(collection)
    #print(element)
    check_integrity()
    element$uid<-inc(collection$lastId)
    collection$data[[new_element_pos()]]<-element
    set_validationcode()
    
    #entry<-signElement(collection,element)
    #eval.parent(substitute(als$signatureCollection<-rbind(als$signatureCollection,entry)))
    return (collection)
  }

repCollectionElement<-function(collection, element){
  check_integrity()
  
  c_pos<-als$nbu(collection, element$uid)
  sgn<-collection$signature[c_pos]
  s_pos<-match(sgn,als$sgns())
  
  collection$data[[c_pos]]<-element
  set_validationcode()
  
  signElement(collection,element)
  #eval.parent(substitute(als$signatureCollection[s_pos,]<-entry))
  #substitute(als$signatureCollection[s_pos,]<-entry))
  
  return(collection)
}

delCollectionElement<- function(collection, UID){
  check_integrity()
  collection$data<- collection$data[-getNumberByUID(collection,UID)]
  set_validationcode()
  return(collection)
}



signElement<-function(collection,element){
  pos<-getNumberByUID(collection,element$uid)
  sgn<-digest(element)
  entry<-data.frame(HASH_E=sgn,uid=element$uid,collection=collection$name, stringsAsFactors = FALSE)
  eval.parent(substitute(collection$signature[[pos]]<-sgn))
  return(entry)

#   sgn<-
#   ifelse(!(sgn %in% signatureCollection$HASH_E),
#                                 signatureCollection<-rbind(signatureCollection,c(sgn,collection$name,element$uid))   )
}


getSignatureByUID<- function(collection, UID){check_integrity()
  return(   collection$signature[getNumberByUID(collection,UID)]  )}

chaining_inf<-data.frame(locked=character(0),locker=character(0))

signatureCollection<-data.frame(HASH_E=character(0), uid=numeric(0), collection=character(0), stringsAsFactors = FALSE)
chainInfo<-data.frame(locked=character(0), locker=character(0), uid=numeric(0), collection=character(0), stringsAsFactors = FALSE)


#temporary disablsed due to performance and safety reasons
# 
getSignatures<- function(collection){
                              check_integrity()
                              return(unlist(collection$signature))}
# 
# registredSignatures<-function(){
#   eval.parent(substitute(lapply(als$slr$cl("collection"), function(element) getSignatures(get(element) ))   ) )}
# 
# signatureOwner<-
#   function(signatureVal){
#     eval.parent(substitute(als$slr$cl("collection")[!is.na(sapply(registredSignatures(),function(x)  match(signatureVal,x)))]))}
# 
# getNumberBySignature<- function(collection, signature){check_integrity()
#   return(   head(which(getSignatures(collection) == signature), 1)   )}
