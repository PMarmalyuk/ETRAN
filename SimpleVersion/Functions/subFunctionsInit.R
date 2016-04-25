# Core Sub Functions Initialization
subFunctions <- list()
## Parameters of events or other parts of trajectory
subFunctions <- append(subFunctions, list(startIdx = list(fun = getStartIdx)))
subFunctions <- append(subFunctions, list(endIdx = list(fun = getEndIdx)))
subFunctions <- append(subFunctions, list(smpCnt = list(fun = getSmpCnt)))
subFunctions <- append(subFunctions, list(onset = list(fun = getOnset)))
subFunctions <- append(subFunctions, list(offset = list(fun = getOffset)))
subFunctions <- append(subFunctions, list(duration = list(fun = getDuration)))
subFunctions <- append(subFunctions, list(xAxisOrientation = list(fun = getXAxisOrientation)))
subFunctions <- append(subFunctions, list(pupilMeanAndSD = list(fun = getPupilMeanAndSD)))

subFunctions <- append(subFunctions, list(startEndPositionsXY = list(fun = getStartEndPositionsXY,
                                                                     settings = list(angular = F))))
subFunctions <- append(subFunctions, list(centerOfMassXY = list(fun = getCenterOfMassXY,
                                                                settings = list(angular = F))))

subFunctions <- append(subFunctions, list(gazePointsArea = list(fun = getGazePointsArea,
                                                                settings = list(angular = F))))

subFunctions <- append(subFunctions, list(fixationPointsArea = list(fun = getFixationPointsArea,
                                                                    settings = list(angular = T))))
subFunctions <- append(subFunctions, list(dispersionXYAndRadius = list(fun = getDispersionXYAndRadius,
                                                                       settings = list(angular = T))))
subFunctions <- append(subFunctions, list(amplitude = list(fun = getAmplitude,
                                                           settings = list(angular = T))))
subFunctions <- append(subFunctions, list(pathLengthAndCurvature = list(fun = getPathLengthAndCurvature,
                                                                        settings = list(angular = T))))
subFunctions <- append(subFunctions, list(peakVelAcDecelAndAsymmetry = list(fun = getPeakVelAcDecelAndAsymmetry,
                                                                            settings = list(angular = T,
                                                                                            velType = "analytical",
                                                                                            fl = 33))))
subFunctions <- append(subFunctions, list(meanVelAcDecel = list(fun = getMeanVelAcDecel,
                                                                settings = list(angular = T,
                                                                                velType = "analytical",
                                                                                fl = 33))))
subFunctions <- append(subFunctions, list(repres = list(fun = getRepres)))
