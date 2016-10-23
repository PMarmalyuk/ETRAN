library(shiny)
source("alias.R", chdir=T)


stm<-als$new(als$enm$cln$STIMUL_COLLECTION)
aoi<-als$new(als$enm$cln$AOI_COLLECTION)
aoi_set<-als$new(als$enm$cln$SETS_COLLECTION)

runApp()
