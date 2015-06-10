gapMarkers <- c(rep(0,5), rep(1,5), rep(0,5), rep(1,5))

t <- c(1,2,3,4,6,10,11,12,15,36,37,38,39,40,41,42,43,44,45,46)

x <- rnorm(length(gapMarkers))*t
y <- rnorm(length(gapMarkers))*t


x <- ifelse(gapMarkers == 1, 0, x)
y <- ifelse(gapMarkers == 1, 0, y)
gapMarks <- data.frame(firstGap = gapMarkers[-length(gapMarkers)], secondGap = gapMarkers[-1])
transitions <- apply(gapMarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
group <- c(1,cumsum(transitions)+1)
data <- data.frame(t, x, y, group)
plot(data$x~t, type = "b")
lastGroup <- group[length(group)]
gapGroups <- unique(group[which(gapMarkers == 1)])
data2 <- lapply(unique(group), FUN = function(x)
{
  if (x %in% gapGroups) 
  {
    if (x != 1) {groupBeforeGap <- data[data$group == x - 1,1:3]} else {groupBeforeGap <- NA}
    if (x != lastGroup) {groupAfterGap <- data[data$group == x + 1,1:3]} else {groupAfterGap <- NA}
    if (is.na(groupBeforeGap) && !is.na(groupAfterGap)) {lastSample <- groupAfterGap[1,1:3]; firstSample <- lastSample}
    if (is.na(groupAfterGap) && !is.na(groupBeforeGap)) {lastSample <- groupBeforeGap[nrow(groupBeforeGap),1:3]; firstSample <- lastSample}
    if (!is.na(groupBeforeGap) && !is.na(groupAfterGap)) {firstSample <- groupBeforeGap[nrow(groupBeforeGap),1:3]; lastSample <- groupAfterGap[1,1:3]}
    smpCnt <- nrow(data[data$group == x,])
    dPos <- c(lastSample$x - firstSample$x, lastSample$y - firstSample$y)
    if (identical(dPos, c(0,0)))
    {
      newX <- rep(lastSample$x, smpCnt)
      newY <- rep(lastSample$y, smpCnt)
    }
    else
    {
      ts <- data[data$group == x,1]
      scalingFactors <- (lastSample$t-ts)/(lastSample$t-firstSample$t)
      newX <- firstSample$x*scalingFactors
      newY <- firstSample$y*scalingFactors
    }
    data[data$group == x,2] <- newX
    data[data$group == x,3] <- newY
    return(data[data$group == x,])
  }
  else 
  {
    return(data[data$group == x,])
  }
}
)
data3 <- do.call("rbind", data2)
points(data3$x~t, type = "l", col = 2)
  