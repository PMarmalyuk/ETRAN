loadData <- function(filepath)
{
  print("I am here!")
  data <- read.csv(filepath, sep = "\t")
  data
}