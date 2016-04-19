coreEstimator <- function(DataRecord, settings)
{
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  eye <- conditions$eye
  factorsDef <- settings$factorsDef
  mainFactorsData <- settings$mainFactorsData
  dataRecIdentifier <- list(expID = DataRecord@expID, subjectID = DataRecord@subjectID, trialID = DataRecord@trialID)
  
  if (eye == "left")
  {

  }
  if (eye == "right")
  {
    
  }
  if (eye == "both")
  {
    
  }
  return(list(dataRec = DataRecord, mainFactorsData = mainFactorsData, factorsDef = factorsDef))
}

# modifyList()
