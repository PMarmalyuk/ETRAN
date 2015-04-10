source("Classes_v_1_4.R")
source("Methods_v_1_4.R")
source("Functions.R")

## Available data examples
# "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI\\Kiryukhin Samples.txt"
#  sep = "\t", skip = 20, comment.char = "#", header = T)
# "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI\\Long\\Kuravsky.pilot Samples.txt"
#  sep = "\t", skip = 20, comment.char = "#", header = T)
# "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Remote Interactive Minds\\s706t1.txt"
#  sep = ";", skip = 0, comment.char = "", header = T
# "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI\\Marmalyuk_Yuriev_problem_solving_Atrashenko__1229_Trial001 Samples.txt"
#  sep = "\t", skip = 46, comment.char = "#", header = T

parser <- new("Parser")
folderPath <- "F:/Институт/Проекты/EyeTrackingPackage/Data/Head-mounted SMI/"
rawDataTable <- new("RawDataTable")
rawDataTable <- mLoadRecords(self =  rawDataTable, path = folderPath, parser = parser)


# Example of using external data loading script
## data <- parser(filepath = "F:/Институт/Проекты/EyeTrackingPackage/Data/Head-mounted SMI/Kiryukhin Samples.txt",
##               datafileReadScript = "F:/Институт/Проекты/EyeTrackingPackage/extDataLoad.R")



