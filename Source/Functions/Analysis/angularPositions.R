# Calculates angular height and width in degrees of the eye position 
# relative to the screen left corner (default) or the given reference point
calcAngPos <- function(x, y, settings, refPoint = c(0,0))
{
  d <- settings$headDistance
  w <- settings$screenSize[1]
  h <- settings$screenSize[2]
  wPx <- settings$screenResolution[1]
  hPx <- settings$screenResolution[2]
  xAng <- (180/pi)*atan(((x-refPoint[1])/(d*wPx/w)))
  yAng <- (180/pi)*atan(((y-refPoint[2])/(d*hPx/h)))
  return(list(xAng = xAng, yAng = yAng))
}

calculateAngPos <- function(ETD)
{
  mode <- etd$settings$mode
  if (mode == "left" | mode == "binocular")
  {
    angLeft <- calcAngPos(x = ETD$leftEyeData$porx, 
                          y = ETD$leftEyeData$pory,
                          settings = ETD$settings) 
    ETD$leftEyeData <- modifyList(x = ETD$leftEyeData, 
                                  val = list(xAng = angLeft$xAng,
                                             yAng = angLeft$yAng))
  }
  
  if (mode == "right" | mode == "binocular")
  {
    angRight <- calcAngPos(x = ETD$rightEyeData$porx, 
                           y = ETD$rightEyeData$pory,
                           settings = ETD$settings) 
    ETD$leftEyeData <- modifyList(x = ETD$rightEyeData, 
                                  val = list(xAng = angRight$xAng,
                                             yAng = angRight$yAng))
  }
  return(ETD)
}