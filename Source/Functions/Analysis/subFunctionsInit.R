# Core Sub Functions Initialization
subFunctions <- list()
## Parameters of events or other parts of trajectory
subFunctions <- append(subFunctions, 
                       list(smpCnt = list(fun = getSmpCnt)))
subFunctions <- append(subFunctions, 
                       list(onset = list(fun = getOnset)))
subFunctions <- append(subFunctions, 
                       list(offset = list(fun = getOffset)))
subFunctions <- append(subFunctions, 
                       list(duration = list(fun = getDuration)))
subFunctions <- append(subFunctions, 
                       list(xAxisOrientation = 
                              list(fun = getXAxisOrientation)))
subFunctions <- append(subFunctions, 
                       list(pupilMeanAndSD = 
                              list(fun = getPupilMeanAndSD)))
subFunctions <- append(subFunctions, 
                       list(startEndPositionsXY = 
                              list(fun = getStartEndPositionsXY,
                                   settings = list(angular = F))))
subFunctions <- append(subFunctions, 
                       list(centerOfMassXY = 
                              list(fun = getCenterOfMassXY,
                                   settings = list(angular = F))))
subFunctions <- append(subFunctions, 
                       list(gazePointsArea = 
                              list(fun = getGazePointsArea,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(dispersionXYAndRadius = 
                              list(fun = getDispersionXYAndRadius,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(amplitude = list(fun = getAmplitudes,
                                settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(pathLength = 
                              list(fun = getPathLength,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(curvature = 
                              list(fun = getCurvature,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(peakVelocity = 
                              list(fun = getPeakVelocity,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(peakAcceleration = 
                              list(fun = getPeakAcceleration,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(peakDeceleration = 
                              list(fun = getPeakDeceleration,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(asymmetry = 
                              list(fun = getAsymmetry,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(meanVelocity = 
                              list(fun = getMeanVelocity,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(meanAcceleration = 
                              list(fun = getMeanAcceleration,
                                   settings = list(angular = T))))
subFunctions <- append(subFunctions, 
                       list(meanDeceleration = 
                              list(fun = getMeanDeceleration,
                                   settings = list(angular = T))))