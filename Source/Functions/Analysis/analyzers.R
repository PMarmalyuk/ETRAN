# SubFunctions evaluator --------------------------------------------------
evaluateSubFunctions <- function(ETD, eye, locations = NA, 
                                 events = NA, subFunctions, 
                                 excludeFiltered = T, 
                                 okMarker = "Ok")
{
  if (all(is.na(locations)))
  {
    locations <- data.frame(eye = NA, 
                            event = NA, 
                            group = NA, 
                            start = 1, 
                            end = length(ETD$commonData$time))
  }
  else
  {
    if (all(!is.na(events)))
    {
      locations <- locations[locations$event %in% events, ]
    }
  }
  dataSubsets <- getDataSubsets(ETD = ETD, 
                                eye = eye, 
                                locations = locations, 
                                excludeFiltered = excludeFiltered, 
                                okMarker = okMarker)
  res <- lapply(dataSubsets, FUN = function(x)
  {
    res <- lapply(subFunctions, FUN = function(y) 
    {
      settings <- append(y$settings, ETD$settings)
      y$fun(x, settings)
    })
    unlist(res, recursive = F)
  })
  res <- rbindlist(res)
  res <- data.frame(locations, res)
  return(res)
}
