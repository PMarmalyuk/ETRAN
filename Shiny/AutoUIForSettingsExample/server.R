getElementName <- function(x)
{
  return(names(x)[1])
}

createElementId <- function(uiFrameName, x)
{
  elName <- getElementName(x)
  return(paste0(uiFrameName,".", gsub(" ", "", tolower(elName), fixed = TRUE)))
}

createNumInput <- function(uiFrameName, x)
{
  lbl <- getElementName(x)
  id <- createElementId(uiFrameName, x)
  uiElement <- numericInput(inputId=id, label = lbl, value = x[[1]])
  return(uiElement)
}

createBoolInput <- function(uiFrameName, x)
{
  lbl <- getElementName(x)
  id <- createElementId(uiFrameName, x)
  uiElement <- checkboxInput(inputId=id, label = lbl, value = x[[1]])
  return(uiElement)
}

createCharInput <- function(uiFrameName, x)
{
  lbl <- getElementName(x)
  id <- createElementId(uiFrameName, x)
  uiElement <- textInput(inputId=id, label = lbl, value = x[[1]])
  return(uiElement)
}

createFactorInput <- function(uiFrameName, x)
{
  lbl <- getElementName(x)
  id <- createElementId(uiFrameName, x)
  uiElement <- selectInput(inputId=id, label = lbl, choices = levels(x[[1]]), selected = x[[1]])
  return(uiElement)
}

createInput <- function(uiFrameName, x)
{
  uiElementClass <- class(x[[1]])[1]
  if (uiElementClass %in% c("integer", "numeric")) 
  {
    return(createNumInput(uiFrameName, x))
  } else
  if (uiElementClass == "logical")
  {
    return(createBoolInput(uiFrameName, x))
  }
  else
  if (uiElementClass == "character")
  {
    return(createCharInput(uiFrameName, x))
  }
  else
  if (uiElementClass  %in% c("factor", "ordered"))
  {
    return(createFactorInput(uiFrameName, x))
  } else return(NULL)
}

shinyServer(function(input, output, session) {
 
  reactVals <- reactiveValues(settings = list())
  ## instead of input$generateUI button clock we can trigger this code when 
  ## somebody selects a function from a selectInput (e.g. detector with its own settings)
  ## use cases:
  ## 1. when we use activeDetector() it should be able to be updated with new settings
  ## 2. when we select a subFunction it should be able to be updated with new settings
  ## 3. when we want to set settings for external plug-in
  observeEvent(input$generateUI, {
  reactVals$settings <<- list('A integer' = as.integer(10), 
                              'B numeric' = as.numeric(pi), 
                              'D logical' = TRUE,
                              'Algorithm' = factor(x = "IVT", levels = c("IVT", "IDT", "ANH")),
                              'F ordinal' = factor(x = "University Student",  levels = c("Schoolchild", "College Student", "University Student", "PhD Student"), ordered = T))
  
  })
  
  output$uiFrame <- renderUI(
      {
        settings <- reactVals$settings
        uiList <- lapply(seq_along(settings), FUN = function(idx) {createInput(uiFrameName = "uiFrameSettings", x = settings[idx])})
        do.call(tagList, uiList)
      })
  
  
  replaceSettingValue <- function(setting, newValue)
  {
    uiElementClass <- class(setting[[1]])[1]
    if (uiElementClass == "integer") 
    {
      return(as.integer(newValue))
    } else
    if (uiElementClass == "numeric") 
    {
      return(as.numeric(newValue))
    } else
    if (uiElementClass == "logical")
    {
      return(as.logical(newValue))
    }
    else
    if (uiElementClass == "character")
    {
      return(as.character(newValue))
    }
    else
    if (uiElementClass == "factor")
    {
      return(factor(newValue, levels = levels(setting[[1]])))
    }
    else
    if (uiElementClass == "ordered")
    {
      return(factor(newValue, levels = levels(setting[[1]]), ordered = T))
    } else return(NULL)
  }
  
  observeEvent(input$saveSettings,
    {
      setts <- reactVals$settings
      
      for (i in 1:length(setts))
      {
        elName <- getElementName(setts[i])
        elId <- createElementId("uiFrameSettings",setts[i])
        reactVals$settings[[`elName`]] <<- replaceSettingValue(setts[i], input[[`elId`]])
      }
    }
  )
})
