trajDurationEstimator <- function(data, settings)
{
  t <- data$time
  return(list('Trajectory Duration' = as.numeric(tail(t, 1) - t[1])))
}

trajLengthEstimator <- function(data, settings)
{
  x <- data$porx
  y <- data$pory
  angular <- settings$angular
  conditions <- settings$conditions
  if (angular)
  {
    screenDist <- settings$screenDist
    screenDim <- settings$screenDim
    screenSize <- settings$screenSize
    pos <- calcAngPos(x = x, y = y, 
                      screenDist = settings$conditions$screenDistance, 
                      screenResolution = settings$conditions$screenResolution, 
                      screenSize = settings$conditions$screenSize)
    xAng <- pos$xAng
    yAng <- pos$yAng
    dxs <- xAng[-1] - xAng[-length(xAng)]
    dys <- yAng[-1] - yAng[-length(yAng)]
  } else
  {
    dxs <- x[-1] - x[-length(x)]
    dys <- y[-1] - y[-length(y)]
  }
  return(list('Trajectory Length'= as.numeric(sum(sqrt(dxs^2 + dys^2)))))
}

eventCounter <- function(data, settings)
{
  if (length(data$eventMarkers) == 0 | length(data$eventGroups) == 0) {return(NULL)}
  eventTypes <- unique(data$eventMarkers[-which(is.na(data$eventMarkers) | is.null(data$eventMarkers))])
  tab <- table(data$eventMarkers, data$eventGroups)
  res <- lapply(eventTypes, FUN = function(x) {
    eventRow <- tab[which(rownames(tab) == x),]
    eventCnt <- list(as.integer(length(which(eventRow != 0))))
    names(eventCnt) <- paste(x, "Count")
    return(eventCnt)
  })
  return(unlist(res, recursive = F))
}