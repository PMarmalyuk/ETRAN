
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
shinyUI(fluidPage(
  navbarPage("Eyetracking analyzer",
             windowTitle="Eyetracking analyzer",
             footer="created by IT MGPPU",
             tabPanel("Данные 1",
                      fluidRow(
                        column(12,
                               navlistPanel("Загрузка данных",
                                 tabPanel("Загрузка 1",
                                          plotOutput("distPlot"),
                                          sliderInput("bins",
                                                      "Number of bins:",
                                                      min = 1,
                                                      max = 50,
                                                      value = 30)
                                          ),
                                 tabPanel("Загрузка 2",
                                          plotOutput("map"),
                                          selectInput("var", 
                                                      label = "Choose a variable to display",
                                                      choices = c("Percent White", "Percent Black",
                                                                  "Percent Hispanic", "Percent Asian"),
                                                      selected = "Percent White"),
                                          
                                          sliderInput("range", 
                                                      label = "Range of interest:",
                                                      min = 0, max = 100, value = c(0, 100))),
                                 "Header B",
                                 tabPanel("Загрузка 3", 
                                          
                                          plotOutput("plot"),
                                          
                                          textInput("symb", "Symbol", "SPY"),
                                          
                                          dateRangeInput("dates", 
                                                         "Date range",
                                                         start = "2013-01-01", 
                                                         end = as.character(Sys.Date())),
                                          
                                          actionButton("get", "Get Stock"),
                                          
                                          br(),
                                          br(),
                                          
                                          checkboxInput("log", "Plot y axis on log scale", 
                                                        value = FALSE),
                                          
                                          checkboxInput("adjust", 
                                                        "Adjust prices for inflation", value = FALSE)),
                                 tabPanel("Загрузка 4"),
                                 tabPanel("Загрузка 5"),
                                 widths = c(3, 9))
                        )
                      )),
             tabPanel("Фильтры",
                      fluidRow(
                        column(12,
                               navlistPanel("Header",
                                            tabPanel("Фильтр 1"),
                                            tabPanel("Фильтр 2"),
                                            "Header B",
                                            tabPanel("Фильтр 3"),
                                            tabPanel("Фильтр 4"),
                                            tabPanel("Фильтр 5"),
                                            widths = c(3, 9))
                        )
                      )),
             navbarMenu("More",
                        tabPanel("Sub-Component A"),
                        tabPanel("Sub-Component B"))
  )
))

