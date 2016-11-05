AOIsetDataByIndex<-function(AOIset, indexList){                         
  AOIset$UIDs <- indexList
  return(AOIset)
}

DataFromAOIset<-function(AOIset, AOI_COLLECTION){                         
  condition<-sapply(AOI_COLLECTION$data, function(element) element$uid %in% AOIset$UIDs)
  return(AOI_COLLECTION$data[condition])
}
