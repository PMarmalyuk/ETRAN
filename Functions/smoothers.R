
## SMOOTHERS ##
# ----------------------------------------------------------------------
# Savitzky-Golay Algorithm
# ----------------------------------------------------------------------
# T2 <- sav.gol(T, fl, forder=4, dorder=0);
#
# Polynomial filtering method of Savitzky and Golay
# See Numerical Recipes, 1992, Chapter 14.8, for details.
#
# x      = vector of signals to be filtered
#          (the derivative is calculated for each ROW)
# fl     = filter length (for instance fl = 51..151)
# forder = filter order (2 = quadratic filter, 4= quartic)
# dorder = derivative order (0 = smoothing, 1 = first derivative, etc.)
# 
# Note from Pablo: there is a problem with this smoother - 
# it may produce negative coordinates due to overfitting!

savGolFilt <- function(x, settings)
{
  #   *** PseudoInvers of a Matrix ***
  #   using singular value decomposition
  pinv <- function (A)
  {
    s <- svd(A)
    s$v %*% diag(1/s$d) %*% t(s$u)
  }
  
  fl <- settings$fl
  forder <- settings$forder
  dorder <- settings$dorder
  
  m <- length(x)
  dorder <- dorder + 1
  
  # -- calculate filter coefficients --
  fc <- (fl-1)/2                          # index: window left and right
  X  <- outer(-fc:fc, 0:forder, FUN="^")  # polynomial terms and coefficients
  Y  <- pinv(X);                          # pseudoinverse
  
  # -- filter via convolution and take care of the end points --
  T2 <- convolve(x, rev(Y[dorder,]), type="o")    # convolve(...)
  T2 <- T2[(fc+1):(length(T2)-fc)]
}
# ----------------------------------------------------------------------

# Simple running median smoother
medianFilt <- function(x, settings)
{
  k <- settings$fl
  runmed(x, k)
}

# Simple running/moving average smoother
# Note: first and last fl%/%2 samples are equal to the first and last fl%/%2 samples from x
movAvgFilt <- function(x, settings)
{
  fl <- settings$fl
  filt <- rep(1/fl, fl)
  xsmoothed <- stats::filter(x, filter = filt, method = "convolution")
  xsmoothed[1:(fl%/%2)] <- x[1:(fl%/%2)]
  xsmoothed[(length(x)-(fl%/%2)+1):length(x)] <- x[(length(x)-(fl%/%2)+1):length(x)]
  as.numeric(xsmoothed)
}



## CORE SMOOTHER ##
coreSmoother <- function(DataRecord, settings)
{
  type <- settings$type
  
  if (type == "Median") {smoother <- medianFilt}
  if (type == "MovAvg") {smoother <- movAvgFilt}
  if (type == "SavGol") {smoother <- savGolFilt}
  
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
