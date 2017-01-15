library(shiny)
library(data.table)
setwd("~/ETRAN-final/Source/AOI Editor")
source("alias.R", local = T)
#install.packages("data.table")

stm<-als$new(als$enm$cln$STIMUL_COLLECTION)
aoi<-als$new(als$enm$cln$AOI_COLLECTION)
aoi_set<-als$new(als$enm$cln$SETS_COLLECTION)

runApp()
