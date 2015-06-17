
simpleIVT <- function(t, x, y, filterMarkers, settings)
{
  VT <- settings$VT
  angular <- settings$angular
  screenDist <- settings$screenDist
  screenDim <- settings$screenDim
  screenSize <- settings$screenSize
  MaxTBetFix <- settings$MaxTBetFix
  MaxDistBetFix <- settings$MaxDistBetFix
  minFixLen <- settings$minFixLen
  maxGapLen <- settings$maxGapLen 
  maxVel <- settings$maxVel
  maxAccel <- settings$maxAccel
  classifyGaps <- settings$classifyGaps
  
  # 1. Velocities and accelerations estimation
  if (!angular)
  {
    vel <- calcPxVel(t, x, y)
    vel$vels <- as.numeric(smooth(vel$vels, kind = "3"))
  } 
  else
  {
    vel <- calcAngVel(t, x, y, screenDist, screenDim, screenSize)
    vel$vels <- as.numeric(smooth(vel$vels, kind = "3"))
  }
  accels <- c(((vel$vels[-1]-vel$vels[-length(vel$vels)])/vel$dts[-length(vel$dts)]), 0)
  # 2. Classification stage: getting raw event markers
  gapMarkers <- ifelse(filterMarkers@filterMarkers != filterMarkers@markerNames$ok, "Gap", "Not gap")
  rawEventMarkers <- ifelse(gapMarkers[-length(gapMarkers)] == "Gap", "Gap", ifelse(vel$vels <= VT, "Fixation", "Saccade"))
  evmarks <- data.frame(firstEv = rawEventMarkers[-length(rawEventMarkers)], secondEv = rawEventMarkers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  evm <- new(Class = "EventMarkers")
  evm@eventMarkers <- rawEventMarkers
  evm@eventGroups <- group
  return(evm)
}

# TO DO: implement post processing
# TO DO: evaluate closeness of fixations in space by center of their mass
IVT <- function(t, x, y, filterMarkers, settings)
{
  VT <- settings$VT
  angular <- settings$angular
  screenDist <- settings$screenDist
  screenDim <- settings$screenDim
  screenSize <- settings$screenSize
  MaxTBetFix <- settings$MaxTBetFix
  MaxDistBetFix <- settings$MaxDistBetFix
  minFixLen <- settings$minFixLen
  maxGapLen <- settings$maxGapLen 
  maxVel <- settings$maxVel
  maxAccel <- settings$maxAccel
  classifyGaps <- settings$classifyGaps
  
  # 1. Velocities and accelerations estimation
  if (!angular)
  {
    vel <- calcPxVel(t, x, y)
    vel$vels <- as.numeric(smooth(vel$vels, kind = "3"))
  } 
  else
  {
    vel <- calcAngVel(t, x, y, screenDist, screenDim, screenSize)
    vel$vels <- as.numeric(smooth(vel$vels, kind = "3"))
  }
  accels <- c(((vel$vels[-1]-vel$vels[-length(vel$vels)])/vel$dts[-length(vel$dts)]), 0)
  # 2. Classification stage: getting raw event markers
  gapMarkers <- ifelse(filterMarkers@filterMarkers != filterMarkers@markerNames$ok, "Gap", "Not gap")
  rawEventMarkers <- ifelse(gapMarkers[-length(gapMarkers)] == "Gap", "Gap", ifelse(vel$vels <= VT, "Fixation", "Saccade"))
  evmarks <- data.frame(firstEv = rawEventMarkers[-length(rawEventMarkers)], secondEv = rawEventMarkers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  evm <- new(Class = "EventMarkers")
  evm@eventMarkers <- rawEventMarkers
  evm@eventGroups <- group
  return(evm)
}



## CORE DETECTOR ##
# This detector uses specified method (I-VT, I-DT, Ada-NH) to detect oculomotor events
# and also it uses eventGroupsAnalyser function to extract events' parameters
coreDetector <- function(DataRecord, settings)
{
  scrDim <- DataRecord@eyesDataObject@conditions@conditions$screenDim
  scrSize <- DataRecord@eyesDataObject@conditions@conditions$screenSize
  dist <- DataRecord@eyesDataObject@conditions@conditions$screenDist
  t <- DataRecord@eyesDataObject@time@time
  
  algorithm <- settings$algorithm
  if (algorithm == "I-VT")
  {
    if (settings$postProcess)
    {
      fun <- IVT
    }
    else
    {
      fun <- simpleIVT
    }
    # Settings for I-VT algorithm:
    ## VT, angular, screenDist, screenDim, screenSize, MaxTBetFix, MaxDistBetFix, 
    ## minFixLen, maxGapLen, maxVel, maxAccel, classifyGaps
  }
  if (algorithm == "I-VT")
  {
    fun <- simpleIVT
    # Settings for I-VT algorithm:
    ## VT, angular, screenDist, screenDim, screenSize, MaxTBetFix, MaxDistBetFix, 
    ## minFixLen, maxGapLen, maxVel, maxAccel, classifyGaps
  }
  if (algorithm == "I-DT")
  {
    fun <- IDT
  }
  if (algorithm == "Ada-NH")
  {
    fun <- AdaNH
  }
  
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    filterMarkers <- DataRecord@eyesDataObject@leftFilterMarkers
    res <- fun(t = t, x = leftX, y = leftY, filterMarkers, settings)
    DataRecord@eyesDataObject@leftEventMarkers <- res
    #DataRecord@analysisResults$leftEventData <- res$eventGroups
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    filterMarkers <- DataRecord@eyesDataObject@rightFilterMarkers
    res <- fun(t = t, x = rightX, y = rightY, filterMarkers, settings)
    DataRecord@eyesDataObject@rightEventMarkers <- res
    #DataRecord@analysisResults$rightEventData <- eventGroupsAnalyser(res$eventGroups)
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    leftFilterMarkers <- DataRecord@eyesDataObject@leftFilterMarkers
    rightFilterMarkers <- DataRecord@eyesDataObject@rightFilterMarkers
    resLeft <- IVT(t = t, x = leftX, y = leftY, leftFilterMarkers, VT = VT, angular = angular, screenDist = dist, screenDim = scrDim, screenSize = scrSize)
    resRight <- IVT(t = t, x = rightX, y = rightY, rightFilterMarkers, VT = VT, angular = angular, screenDist = dist, screenDim = scrDim, screenSize = scrSize)
    DataRecord@eyesDataObject@leftEventMarkers <- resLeft
    #DataRecord@analysisResults$leftEventData <- eventGroupsAnalyser(resLeft$eventGroups)
    DataRecord@eyesDataObject@rightEventMarkers <- resRight
    #DataRecord@analysisResults$rightEventData <- eventGroupsAnalyser(resRight$eventGroups)
  }
  return(DataRecord)
}