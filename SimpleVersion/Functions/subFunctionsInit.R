# Core Sub Functions Initialization
subFunctions <- list()
## Parameters of events or other parts of trajectory
subFunctions <- append(subFunctions, list(startIdx = list(fun = getStartIdx)))
subFunctions <- append(subFunctions, list(endIdx = list(fun = getEndIdx)))
subFunctions <- append(subFunctions, list(onset = list(fun = getOnset)))
subFunctions <- append(subFunctions, list(offset = list(fun = getOffset)))
subFunctions <- append(subFunctions, list(duration = list(fun = getDuration)))
subFunctions <- append(subFunctions, list(repres = list(fun = getRepres)))
# subFunctions <- append(subFunctions, pointsArea = list(fun = getPointsArea))

# subFunctions <- append(subFunctions, startEndPositionsXY = list(fun = getStartEndPositionsXY))
# subFunctions <- append(subFunctions, centerOfMassXY = list(fun = getCenterOfMassXY))
# subFunctions <- append(subFunctions, dispersionXYAndRadius = list(fun = getDispersionXYAndRadius))
# subFunctions <- append(subFunctions, pupilMeanAndSD = list(fun = getPupilMeanAndSD))
# subFunctions <- append(subFunctions, amplitude = list(fun = getAmplitude))
# subFunctions <- append(subFunctions, pathLengthAndCurvature = list(fun = getPathLengthAndCurvature))
# subFunctions <- append(subFunctions, onOffSetDuration = list(fun = getPeakVelAcDecelAndAsymmetry))
# subFunctions <- append(subFunctions, onOffSetDuration = list(fun = getXAxisOrientation))