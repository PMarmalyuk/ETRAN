##### Function Interface Description #####
# character 'filepath' sets a full path to a datafile
# character 'sep' is the field separator character
# integer 'skip' is the number of lines of the input file to skip before beginning to read data values
# character 'comment.char' is a character vector of length one containing a single character or an empty string. Use "" to turn off the interpretation of comments altogether (the default).
# logical 'header' indicates if there is a header in a datafile
# character 'SMPString'
# logical 'delLeadFrameData'

## if there's header specified then function uses 'field.names' to construct rawdata object ready to be a "traj" object
## if there's no header, then function uses 'field.order' to construct rawdata object ready to be a "traj" object
## if there's no header and 'field.order' is not specified then function treats a datafile format as default (see documentation)

# character vector 'field.names' 
# integer(0) vector 'field.order'

## Supported fields order:
## "Time", "Type", "Trial" , "Frame", "Stimulus.Name", 
## "LPOR.X", "LPOR.Y", "RPOR.X", "RPOR.X",
## "LPupil.X.Size", "LPupil.Y.Size", "RPupil.X.Size", "RPupil.Y.Size"

read.samples <- function(filepath, sep = "", skip = 0, comment.char = "", header)
{
  if (file.exists(filepath))
  {
    asIsData <- read.csv(filepath, sep = sep, skip = skip, comment.char = comment.char, header = header,
                        blank.lines.skip = T, 
                        check.names = F,
                        stringsAsFactors = F)
  }
  else
  {
    stop("Datafile not found!")
  }
  asIsData
}

parser <- function(filepath, datafileReadScript = "own", fun = loadData, Settings)
{
  if (datafileReadScript == "own")
  {
    ## check if data loading settings were set
    if (dataLoadSettingsSet)
    {
      asIsData <- read.samples(filepath,
                              sep = Settings$sep,
                              skip = Settings$skip,
                              comment.char = Settings$comment.char,
                              header = Settings$header)
      N <- nrow(asIsData)
      rawdata <- data.frame(Time = new(Class = "numeric", rep(NA, N)),
                            Type = new(Class = "character", rep(NA, N)),
                            Trial = new(Class = "integer", rep(NA, N)),
                            Frame = new(Class = "integer", rep(NA, N)),
                            Stimulus.Name = new(Class = "character", rep(NA, N)),
                            LPOR.X = new(Class = "numeric", rep(NA, N)),
                            LPOR.Y = new(Class = "numeric", rep(NA, N)),
                            RPOR.X = new(Class = "numeric", rep(NA, N)),
                            RPOR.Y = new(Class = "numeric", rep(NA, N)),
                            LPupil.X.Size = new(Class = "numeric", rep(NA, N)),
                            LPupil.Y.Size = new(Class = "numeric", rep(NA, N)),
                            RPupil.X.Size = new(Class = "numeric", rep(NA, N)),
                            RPupil.Y.Size = new(Class = "numeric", rep(NA, N)))
      
      ## Creating the availableFields flags according to the settings specified by user
      availableFields <- list(
        times = logical(),
        sample.types = logical(),
        trials = logical(),
        frames = logical(),
        stimuli.names = logical(),
        lporx = logical(),
        lpory = logical(),
        rporx = logical(),
        rpory = logical(),
        lpupilxsize = logical(),
        lpupilysize = logical(),
        rpupilxsize = logical(),
        rpupilysize = logical()
      )
      if (!is.na(Settings$field.names)) 
      {
        availableFields[] <- (Settings$field.names != "-")
      }
      if (is.na(Settings$field.names) && !is.na(Settings$field.order))
      {
        availableFields[] <- (Settings$field.order != 0)
      }

      ## Filling the rawdata object with all data available (different setting cases)
      if ( Settings$header && !is.na(Settings$field.names) )
      {
        j <- 0
        for (i in 1:length(availableFields))
        {
          if (availableFields[[i]]) 
          {
            j <- j + 1
            rawdata[ , i] <- asIsData[ , which(colnames(asIsData) == Settings$field.names[j])]
          }
        }
      }
      if ( (!Settings$header && !is.na(Settings$field.order)) |
           (Settings$header && is.na(Settings$field.names) && !is.na(Settings$field.order)) )
      {
        j <- 0
        for (i in 1:length(availableFields))
        {
          if (availableFields[[i]]) 
          {
            j <- j + 1
            rawdata[ , i] <- asIsData[ , Settings$field.order[j]]
          }
        }
      }
      if ( (!Settings$header && is.na(Settings$field.order)) |
           (Settings$header && is.na(Settings$field.names) && is.na(Settings$field.order)) )
      {
        j <- 0
        for (i in 1:length(availableFields))
        {
          if (availableFields[[i]]) 
          {
            j <- j + 1
            rawdata[ , i] <- asIsData[ , j]
          }
        }
      }

      ## Cleaning the data 
      ### Deleting rows with Type = MSG 
      if (availableFields$sample.types)
      {
        rawdata <- rawdata[rawdata[ , 2] == Settings$SMPString, ]
      }
      
      ### Deleting starting rows when Frame is incorrect
      if (availableFields$frames & Settings$delLeadFrameData)
      {
        #
      }
      
      ## Splitting the dataframe by Trial or by StimulusName field information
      if (availableFields$trials)
      {
        trialsData <- split(rawdata, rawdata$Trial)
      }
      if (!availableFields$trials & availableFields$stimuli.names)
      {
        trialsData <- split(rawdata, rawdata$Stimulus.Name)
      }
      
      ## Returning the monocular/binocular trajectory object with empty EventList slot and ExpConditions filled (if possible)
    
      
    }
    else
    {
      ## ask user to provide data loading settings once and restart data loading process
      stop("Please, set data loading settings!")
    }
  }
  # user can write his/her own script (*.R) and use it to load data
  if (datafileReadScript != "own")
  {
    # check if the specified source exists
    if (file.exists(datafileReadScript))
    {
      source(datafileReadScript)
      trialsData <- fun(filepath)
    }
    else
    {
      warning("Script not found!")
    }
  }
  trialsData
}

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
                         header = T, SMPString = "SMP", delLeadFrameData = F, field.names =  
                         list(time = "Time", type = "Type", trial = "Trial", frame = "Frame", stimname = "-", 
                              lporx = "L POR X [px]", lpory = "L POR Y [px]", rporx = "-", rpory = "-",
                              lpupsizex = "L Dia X [px]", lpupsizey = "L Dia Y [px]", rpupsizex = "-", rpupsizey = "-"),
                         field.order = NA) 
                        #list(time = 1, type = 2, trial = 3, frame = 14, stimname = 0,
                        # lporx = 10, lpory = 11, rporx = 0, rpory = 0, 
                        # lpupsizex = 6, lpupsizey = 7, rpupsizex = 0, rpupsizey = 0)
dataLoadSettingsSet <- T

data <- parser(filepath = "F:/Институт/Проекты/EyeTrackingPackage/Data/Head-mounted SMI/Kiryukhin Samples.txt",
       datafileReadScript = "own",
       Settings = dataLoadSettings)

data <- parser(filepath = "F:/Институт/Проекты/EyeTrackingPackage/Data/Head-mounted SMI/Kiryukhin Samples.txt",
               datafileReadScript = "F:/Институт/Проекты/EyeTrackingPackage/extDataLoad.R")

