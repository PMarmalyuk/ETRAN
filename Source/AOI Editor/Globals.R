library(shiny)
library(data.table)
source("alias.R", chdir=T)
install.packages("data.table")

stm<-als$new(als$enm$cln$STIMUL_COLLECTION)
aoi<-als$new(als$enm$cln$AOI_COLLECTION)
aoi_set<-als$new(als$enm$cln$SETS_COLLECTION)

runApp()

