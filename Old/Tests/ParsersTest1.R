##### Function Interface Description #####
# character 'filepath' sets a full path to a datafile
# character 'sep' is the field separator character
# integer 'skip' is the number of lines of the input file to skip before beginning to read data values
# character 'comment.char' is a character vector of length one containing a single character or an empty string. Use "" to turn off the interpretation of comments altogether (the default).
# logical 'header' indicates if there is a header in a datafile

## if there's header specified then function uses 'field.names' to construct rawdata object ready to be a "traj" object
## if there's no header, then function uses 'field.order' to construct rawdata object ready to be a "traj" object
## if there's no header and 'field.order' is not specified then function treats a datafile format as default (see documentation)

# logical 'times' indicates if there is a timestamp field in a datafile

# logical 'sample.types' indicates if there is a sample type field in a datafile (used in SMI datafiles)
## in SMI datafiles there is usually the "Type" field containing labels indicating a type of a sample
## for example, SMP is an ordinary sample, but MSG is a sample with a message such as stimulus change command

# logical 'trials' indicates if there is a trial counter field in a datafile

# logical 'frames' indicates if there is a video frame counter field in a datafile 
## this parameter is used with HED-like eyetrackers and video stimuli (not supported so far)

# logical 'stimuli.names' indicates if there is a stimuli name field in a datafile 
## if no trial counter field: used instead of a trial counter field to split a record into parts 
## if there is trial counter field: used as a source of information useful for trial number and stimulus file linkage

# character 'eye' specifies which eye was recorded in monocular mode ("left" or "right")

# logical 'binocular' indicates if recording was performed in a binocular mode

# logical 'pupil.data' indicates if there is a pupil size data fields in a datafile

# character 'pupil.data.type' specifies which data is available for pupil's size:
## 'pupil.data.type' = "radius": pupil's radius
## 'pupil.data.type' = "diameter": pupil's diameter
## 'pupil.data.type' = "ellipse.radius": pupil's horisontal and vertical axes half-length
## 'pupil.data.type' = "ellipse.diameter": pupil's horisontal and vertical axes length

# character vector 'field.names',
## its elements should be specified in a predefined order, giving names for all required fields:
## 1. time field name 2. type field name 3. trial field name 4. frame field name 5. stimuli names field name
## 6. then 2 (monocular mode) or 4 (binocular mode) names of x and y POR fields:
### default monocular ('eye' = "left): left x POR field name, left y POR field name
### monocular and 'eye' = "right": right x POR field name, right y POR field name
### binocular: left x POR field name, left y POR field name, right x POR field name, right y POR field name
## 7. then 1, 2 or 4 names of pupil.data fields:
### default monocular ('eye' = "left) and 'pupil.data.type' = "circle": L Size field name
### default monocular ('eye' = "left) and 'pupil.data.type' = "ellipse": L H Size name and L V Size names
### monocular and 'eye' = "right": the same cases but for right eye
### binocular and 'pupil.data.type' = "circle": L Size and then R Size names
### binocular and 'pupil.data.type' = "ellipse": L H Size and L V Size and then R H Size and R V Size names

# 'field.names' EXAMPLES:
### 
### DATAFILE HEADER: 
#### Time  Type  Trial  L Raw X [px]  L Raw Y [px]	L Dia X [px]	L Dia Y [px]	L CR1 X [px]	L CR1 Y [px]	L POR X [px]	L POR Y [px]
### SETTING PARAMETERS: 
#### ..., header = T, times = T, ..., sample.types = T, trials = T, frames = F, stimuli.names = F, 
#### eye = "left", binocular = F, pupil.data = T, pupil.data.type = "ellipse.diameter",
#### field.names = c("Time", "Type", "Trial", "Frame", "L POR X [px]", "L POR Y [px]", "L Dia X [px]", "L Dia Y [px]")

# integer vector 'field.order',
## its elements should set datafile fields' indices for all required fields:
## 1. time field index 2. type field index 3. trial field index 4. frame field index 5. stimuli names field index
## 6. then 2 (monocular mode) or 4 (binocular mode) indices of x and y POR fields:
### default monocular ('eye' = "left): L X POR index, L Y POR index
### monocular and 'eye' = "right": R X POR index, R Y POR index
### binocular: L X POR index, L Y POR index, R X POR index, R Y POR index
## 7. then 1, 2 or 4 indices of pupil.data fields:
### default monocular ('eye' = "left) and 'pupil.data.type' = "circle": L Size field index
### default monocular ('eye' = "left) and 'pupil.data.type' = "ellipse": L H Size and L V Size indices
### monocular and 'eye' = "right": the same cases but for right eye
### binocular and 'pupil.data.type' = "circle": L Size and then R Size indices
### binocular and 'pupil.data.type' = "ellipse": L H Size and L V Size and then R H Size and R V Size indices

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

parser <- function(filepath, datafile.read.script = "own", fun, header,
                   times, sample.types, trials, frames, stimuli.names,
                   eye, binocular, pupil.data, pupil.data.type,
                   field.names, field.order)
{
  if (datafile.read.script == "own")
  {
    ## check if data loading settings were set
    if (dataLoadSettingsSet)
    {
      asIsData <- read.samples(filepath,
                              sep = dataLoadSettings$sep,
                              skip = dataLoadSettings$skip,
                              comment.char = dataLoadSettings$comment.char,
                              header = header)
      N <- nrow(asIsData)
      
      availableFields <- list(time = logical(),
                              type = logical(),
                              trial = logical(),
                              frame = logical(),
                              stimname = logical(),
                              lporx = logical(),
                              lpory = logical(),
                              rporx = logical(),
                              rpory = logical(),
                              lpupilxsize = logical(),
                              lpupilysize = logical(),
                              rpupilxsize = logical(),
                              rpupilysize = logical()
      )
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
      if (!is.na(field.names)) 
      {
        availableFields <- (field.names != "-")
      }
      if (is.na(field.names) & !is.na(field.order))
      {
        availableFields <- (field.order != 0)
      }

      availableFields <- c(times, sample.types, trials, frames, stimuli.names)
      if (binocular) 
      {
        availableFields <- c(availableFields, T, T, T, T)
        if (pupil.data)
        {
          if (pupil.data.type == "circle")
          {
            availableFields <- c(availableFields, T, F, T, F)
          }
          if (pupil.data.type == "ellipse")
          {
            availableFields <- c(availableFields, T, T, T, T)
          }
        }
      }
      else 
      {
        if (eye == "left") 
        {
          availableFields <- c(availableFields, T, T, F, F)
          if (pupil.data.type == "circle")
          {
            availableFields <- c(availableFields, T, F, F, F)
          }
          if (pupil.data.type == "ellipse")
          {
            availableFields <- c(availableFields, T, T, F, F)
          }
        }
        if (eye == "right") 
        {
          availableFields <- c(availableFields, F, F, T, T)
          if (pupil.data.type == "circle")
          {
            availableFields <- c(availableFields, F, F, T, F)
          }
          if (pupil.data.type == "ellipse")
          {
            availableFields <- c(availableFields, F, F, T, T)
          }
        }
      }
      
      ## Filling the rawdata object with all data available (different setting cases)
      if ( header && !is.na(field.names) )
      {
        j <- 0
        for (i in 1:length(availableFields))
        {
          if (availableFields[i]) 
          {
            j <- j + 1
            rawdata[ , i] <- asIsData[ , which(colnames(asIsData) == field.names[j])]
          }
        }
      }
      if ( (!header && !is.na(field.order)) |
           (header && is.na(field.names) && !is.na(field.order)) )
      {
        j <- 0
        for (i in 1:length(availableFields))
        {
          if (availableFields[i]) 
          {
            j <- j + 1
            rawdata[ , i] <- asIsData[ , field.order[j]]
          }
        }
      }
      if ( (!header && is.na(field.order)) |
           (header && is.na(field.names) && is.na(field.order)) )
      {
        j <- 0
        for (i in 1:length(availableFields))
        {
          if (availableFields[i]) 
          {
            j <- j + 1
            rawdata[ , i] <- asIsData[ , j]
          }
        }
      }
      
      ## Cleaning the data 
      ### Deleting rows with Type = MSG 
      ## !!! Should add SMP string intop settings?
      if (sample.types) 
      {
        rawdata <- rawdata[rawdata[ , 2] == "SMP", ]
      }
      
      ### Deleting starting rows when Frame is incorrect
      ## !!! Should add corresponding setting
      if (frames)
      {
        #
      }
      
      ## Splitting the dataframe by Trial or by StimulusName field information
      if (trials)
      {
        trialsData <- split(rawdata, rawdata$Trial)
        print("Yo!")
      }
      if (!trials & stimuli.names)
      {
        trialsData <- split(rawdata, rawdata$Stimulus.Name)
      }
      ## Returning the monocular/binocular trajectory object with empty EventList slot and ExpConditions filled (if possible)
    
      
    }
    else
    {
      ## ask user to provide data loading settings once and restart data loading process 
    }
  }
  # user can write his/her own script (*.R) and use it to load data
  if (datafile.read.script != "own")
  {
    # check if the specified source exists
    if (file.exists(datafile.read.script))
    {
      ## source(datafile.read.script)
      ## rawdata <- fun(mydata.txt)
    }
    else
    {
      warning("Script not found!")
    }
  }
  trialsData
}

dataLoadSettings <- list(sep = "\t", skip = "20", comment.char = "#",
                         header = F,
                         times = T,
                         sample.types = T,
                         trials = T,
                         frames = T,
                         stimuli.names = F,
                         eye = "left",
                         binocular = F,
                         pupil.data = T,
                         pupil.data.type = "ellipse",
                         field.names = NA, #c("Time", "Type", "Trial", "Frame", "L POR X [px]", "L POR Y [px]", "L Dia X [px]", "L Dia Y [px]"),
                         field.order = c(1,2,3,14,10,11,6,7))
dataLoadSettingsSet <- T

fields <- c("Time", "Type", "Trial" , "Frame", "Stimulus.Name", 
            "LPOR.X", "LPOR.Y", "RPOR.X", "RPOR.X", 
            "LPupil.X.Size", "LPupil.Y.Size", "RPupil.X.Size", "RPupil.Y.Size")

## Available data examples
data1 <- read.samples(filepath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI\\Kiryukhin Samples.txt",
                      sep = "\t", skip = 20, comment.char = "#", header = T)
data2 <- read.samples(filepath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Head-mounted SMI\\Long\\Kuravsky.pilot Samples.txt",
                      sep = "\t", skip = 20, comment.char = "#", header = T)
data3 <- read.samples(filepath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Remote Interactive Minds\\s706t1.txt",
                      sep = ";", skip = 0, comment.char = "", header = T)
data4 <- read.samples(filepath = "F:\\Институт\\Проекты\\EyeTrackingPackage\\Data\\Tower-mounted SMI\\Marmalyuk_Yuriev_problem_solving_Atrashenko__1229_Trial001 Samples.txt",
                      sep = "\t", skip = 46, comment.char = "#", header = T)


data <- parser(filepath = "F:/Институт/Проекты/EyeTrackingPackage/Data/Head-mounted SMI/Kiryukhin Samples 2.txt",
       datafile.read.script = "own",
       header = dataLoadSettings$header,
       times = dataLoadSettings$times,
       sample.types = dataLoadSettings$sample.types,
       trials = dataLoadSettings$trials,
       frames = dataLoadSettings$frames,
       stimuli.names = dataLoadSettings$stimuli.names,
       eye = dataLoadSettings$eye,
       binocular = dataLoadSettings$binocular,
       pupil.data = dataLoadSettings$pupil.data,
       pupil.data.type = dataLoadSettings$pupil.data.type,
       field.names = dataLoadSettings$field.names,
       field.order = dataLoadSettings$field.order)

str(data)
