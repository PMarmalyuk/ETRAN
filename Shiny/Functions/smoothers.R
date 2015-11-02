
## SMOOTHERS ##
noSmoother <- function(x, settings)
{
  return(x)
}

# Savitzky-Golay Smoother
savGolFilt <- function(x, settings)
{
  fl <- settings$fl
  forder <- settings$forder
  sgolayfilt(x = x, p = forder, n = fl, m = 0, ts = 1)
}

# Running median smoother
medianFilt <- function(x, settings)
{
  k <- settings$fl
  runmed(x, k)
}

# Moving average smoother
# Note: first and last fl%/%2 samples are equal to the first and last fl%/%2 samples from x
movAvgFilt <- function(x, settings)
{
  fl <- settings$fl
  filt <- rep(1/fl, fl)
  xsmoothed <- stats::filter(x, filter = filt, method = "convolution")
  xsmoothed[1:(fl%/%2)] <- x[1:(fl%/%2)]
  xsmoothed[(length(x)-(fl%/%2)+1):length(x)] <- x[(length(x)-(fl%/%2)+1):length(x)]
  xsmoothed
}

## CORE SMOOTHER ##
coreSmoother <- function(DataRecord, settings)
{
  smoother <- settings$subfun

  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- smoother(DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx, settings)
    DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory <- smoother(DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory, settings)
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx <- smoother(DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx, settings)
    DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory <- smoother(DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory, settings)
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- smoother(DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx, settings)
    DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory <- smoother(DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory, settings)
    DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx <- smoother(DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx, settings)
    DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory <- smoother(DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory, settings)
  }
  return(DataRecord)
}
