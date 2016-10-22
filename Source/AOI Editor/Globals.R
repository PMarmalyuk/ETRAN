setwd("D:\\RGNF_Repo\\AOI_sandbox")
library(shiny)
source("alias.R")


stm<-als$new(als$enm$cln$STIMUL_COLLECTION)
aoi<-als$new(als$enm$cln$AOI_COLLECTION)
aoi_set<-als$new(als$enm$cln$SETS_COLLECTION)

runApp()
