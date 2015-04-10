source("Parser_v1.R")
## Available data examples
# data1 <- read.samples(filepath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI\\Kiryukhin Samples.txt",
#                      sep = "\t", skip = 20, comment.char = "#", header = T)
# data2 <- read.samples(filepath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI\\Long\\Kuravsky.pilot Samples.txt",
#                      sep = "\t", skip = 20, comment.char = "#", header = T)
# data3 <- read.samples(filepath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Remote Interactive Minds\\s706t1.txt",
#                      sep = ";", skip = 0, comment.char = "", header = T)
# data4 <- read.samples(filepath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI\\Marmalyuk_Yuriev_problem_solving_Atrashenko__1229_Trial001 Samples.txt",
#                      sep = "\t", skip = 46, comment.char = "#", header = T)

## Supported fields order:
## "Time", "Type", "Trial" , "Frame", "Stimulus.Name", 
## "LPOR.X", "LPOR.Y", "RPOR.X", "RPOR.X",
## "LPupil.X.Size", "LPupil.Y.Size", "RPupil.X.Size", "RPupil.Y.Size"
dataLoadSettings <- list(sep = "\t", skip = "20", comment.char = "#",
                         header = T, SMPString = "SMP", delLeadFrameData = F, 
                         field.names =  list(time = "Time", type = "Type", trial = "Trial", frame = "Frame", stimname = "-", 
                                lporx = "L POR X [px]", lpory = "L POR Y [px]", rporx = "-", rpory = "-",
                                lpupsizex = "L Dia X [px]", lpupsizey = "L Dia Y [px]", rpupsizex = "-", rpupsizey = "-"),
                         field.order = NA) 
#list(time = 1, type = 2, trial = 3, frame = 14, stimname = 0,
# lporx = 10, lpory = 11, rporx = 0, rpory = 0, 
# lpupsizex = 6, lpupsizey = 7, rpupsizex = 0, rpupsizey = 0)
dataLoadSettingsSet <- T

alldata <- parser(filepath = "F:/Институт/Проекты/EyeTrackingPackage/Data/Head-mounted SMI/Kiryukhin Samples.txt",
               datafileReadScript = "own",
               settings = dataLoadSettings)


# Example of using external data loading script
## data <- parser(filepath = "F:/Институт/Проекты/EyeTrackingPackage/Data/Head-mounted SMI/Kiryukhin Samples.txt",
##               datafileReadScript = "F:/Институт/Проекты/EyeTrackingPackage/extDataLoad.R")

plot(df$LPOR.Y~df$Time, type = "l")

## interpolationMethod can be "none", "nn" (piecewise constant), "linear"
## units can be "px", "mm" and "deg"
## speed is measured in units/second, 
## accel.: units/second^2
filterSettings <- list(interpolationMethod = "none", 
                       stimDim = c(752, 480), 
                       headDist = 500, 
                       maxSpeed = 100, 
                       maxAccel = 100, 
                       units = "px")

filter <- function(df, settings)
{
  ## find and mark zero gaze points
    
  ## find and mark gaze points outside of stimulus
  
  ## estimate speed and mark points with speed higher than threshold
  
  ## estimate accel. and mark points with accel. higher than threshold
  
  ## perform interpolation if necessary
}

smoother <- function(df, settings)
{
  ## use Savitzky-Golay to smooth the trajectory
}

