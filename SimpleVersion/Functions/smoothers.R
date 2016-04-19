# Savitzky-Golay Smoother
savGolSmoother <- function(x, smoothingSettings)
{
  fl <- smoothingSettings$fl
  forder <- smoothingSettings$forder
  sgolayfilt(x = x, p = forder, n = fl, m = 0, ts = 1)
}

# Running median smoother
medianSmoother <- function(x, smoothingSettings)
{
  k <- smoothingSettings$fl
  runmed(x, k)
}

# Moving average smoother
# Note: first and last fl%/%2 samples are equal to the first and last fl%/%2 samples from x
movAvgSmoother <- function(x, smoothingSettings)
{
  fl <- smoothingSettings$fl
  filt <- rep(1/fl, fl)
  xsmoothed <- stats::filter(x, filter = filt, method = "convolution")
  xsmoothed[1:(fl%/%2)] <- x[1:(fl%/%2)]
  xsmoothed[(length(x)-(fl%/%2)+1):length(x)] <- x[(length(x)-(fl%/%2)+1):length(x)]
  xsmoothed
}

## CORE SMOOTHER ##
dataSmoother <- function(ETD, smoother, smoothingSettings)
{
  mode <- etd$settings$mode
  if (mode == "left" | mode == "binocular")
  {
    ETD$leftEyeData$porx <- smoother(ETD$leftEyeData$porx, smoothingSettings)
    ETD$leftEyeData$pory <- smoother(ETD$leftEyeData$pory, smoothingSettings)
  }
  if (mode == "right" | mode == "binocular")
  {
    ETD$rightEyeData$porx <- smoother(ETD$rightEyeData$porx, smoothingSettings)
    ETD$rightEyeData$pory <- smoother(ETD$rightEyeData$pory, smoothingSettings)
  }
  return(ETD)
}
