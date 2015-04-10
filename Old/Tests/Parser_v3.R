##### Function Interface Description #####
# Arguments:
##
# Value: a list of
## filename
## subject ID/name
## availableFields list
## trajectories list
## conditions list

# TO DO: 
## 1. automatic subject name, sample rate, screen distance setting by scanning the file
## 2. additional error situations searching and handling
## 3. function testing

parser <- function(filepath, datafileReadScript = "own", fun = loadData, 
                   sep = "\t", skip = 0, comment.char = "#", header = F, 
                   eyeTracker = "Unknown", format = "Unknown", SMPString = NA,
                   pupilDataType1 = NA, pupilDataType2 = NA, pupilDataUnits = NA, 
                   sampleRate = NA, timeUnits = NA, screenDistance = NA, distanceUnits = NA,
                   field.names = NA,
                   field.order = list(time = 1, type = 2, trial = 3, frame = 4, stimname = 5,
                                   lporx = 6, lpory = 7, rporx = 8, rpory = 9, 
                                   lpupxsize = 10, lpupysize = 11, rpupxsize = 12, rpupysize = 13),
                   noFieldMarker = list(name = "-", order = 0),
                   subjectID = NA)
{
  ## check if data loading settings were set
  if (!file.exists(filepath))
  {
    stop("Datafile not found!")
  }
  else
  if (datafileReadScript == "own")
  {
    if (skip > 0)
    {
      # Here we can implement/use a function which tries to evaluate subject name, sample rate and screen distance automatically
    }
    
    # set default fieldsNames and availableFields list elements names
    fieldsNames <- c("time", "type", "trial", "frame", "stimname",
                     "lporx", "lpory", "rporx", "rpory", 
                     "lpupxsize", "lpupysize", "rpupxsize", "rpupysize")
    availableFields <- as.list(rep(T, 13))
    names(availableFields) <- fieldsNames

    # check if field.names or field.order is specified correctly
    if (!is.na(field.names) && !all(names(field.names) == fieldsNames))
    {
      stop("The 'field.names' list is specified incorrectly. The correct order and names of the list elements are the following: 
           time, type, trial, frame, stimname, lporx, lpory, rporx, rpory, lpupxsize, lpupysize, rpupxsize, rpupysize")
    }
    if (!is.na(field.order) && !all(names(field.order) == fieldsNames))
    {
      stop("The 'field.order' list is specified incorrectly. The correct order and names of the list elements are the following: 
           time, type, trial, frame, stimname, lporx, lpory, rporx, rpory, lpupxsize, lpupysize, rpupxsize, rpupysize")
    }
    
        
    # considering different situations with header, field.names and field.order specifications
    # 000
    if (!header && is.na(field.names) && is.na(field.order))
    {
      warning("The 'field.names' and 'field.order' lists aren't specified! 
              Using default data columns order.")
      order <- "default"
    }
    # 001
    if (!header && is.na(field.names) && !is.na(field.order))
    {
      order <- "order"
    }
    # 010
    if (!header && !is.na(field.names) && is.na(field.order))
    {
      warning("The 'field.names' list is specified but there is no header in the data file! 
              Using default data columns order for all specified columns.")
      order <- "default_by_names"
    }
    # 011
    if (!header && !is.na(field.names) && !is.na(field.order))
    {
      warning("The 'field.names' and 'field.order' are specified but 'header' is not! 
              Using 'field.order' as the source of information about data columns positions.")
      order <- "order"
    }
    # 100
    if (header && is.na(field.names) && is.na(field.order))
    {
      warning("The 'field.names' and 'field.order' lists aren't specified! 
              Using default data columns order.")
      order <- "default"
    }
    # 101
    if (header && is.na(field.names) && !is.na(field.order))
    {
      order <- "order"
    }
    # 110
    if (header && !is.na(field.names) && is.na(field.order))
    {
      order <- "names"
    }
    # 111
    if (header && !is.na(field.names) && !is.na(field.order))
    {
      warning("The 'field.names', 'header' and 'field.order' are specified! 
              Using 'field.names' amd 'header' as the source of information about data columns positions.")
      order <- "names"
    }

    ## order cases, determining fieldPositions etc to read data correctly
    if (order == "default")
    {
      useNames <- F
      field.order <- list(time = 1, type = 2, trial = 3, frame = 4, stimname = 5,
                          lporx = 6, lpory = 7, rporx = 8, rpory = 9, 
                          lpupxsize = 10, lpupysize = 11, rpupxsize = 12, rpupysize = 13)
      availableFields <- as.list(rep(T,13))
      availableFieldsNames <- fieldsNames
      fieldPositions <- 1:13
    }
    if (order == "default_by_names")
    {
      useNames <- F
      field.order <- list(time = 1, type = 2, trial = 3, frame = 4, stimname = 5,
                          lporx = 6, lpory = 7, rporx = 8, rpory = 9, 
                          lpupxsize = 10, lpupysize = 11, rpupxsize = 12, rpupysize = 13)
      availableFields <- as.list(field.names != noFieldMarker$names)
      availableFieldsNames <- fieldsNames[as.vector(availableFields, mode = "logical")]
      fieldPositions <- field.order[as.vector(availableFields, mode = "logical")]
    }
    if (order == "order")
    {
      useNames <- F
      availableFields <- as.list(field.order != noFieldMarker$order)
      availableFieldsNames <- fieldsNames[as.vector(availableFields, mode = "logical")]
      fieldPositions <- field.order[as.vector(availableFields, mode = "logical")]
    }
    if (order == "names")
    {
      useNames <- T
      availableFields <- as.list(field.names != noFieldMarker$name)
      availableFieldsNames <- fieldsNames[as.vector(availableFields, mode = "logical")]
      fieldPositions <- c()
    }

    ## reading data from file
    asIsData <- read.csv(filepath, sep = sep,   
                         skip = skip, 
                         comment.char = comment.char, 
                         header = header,
                         blank.lines.skip = T, 
                         check.names = F,
                         stringsAsFactors = F)
    
    ## figuring out fieldPositions by specified header and field.names
    if (useNames)
    {
      for (field in field.names)
      {
        if (field != noFieldMarker$name)
        {
          fieldPositions <- c(fieldPositions, which(colnames(asIsData) == field))
        }
      }
    }
    
    ## Filling the rawdata object with all data available (different setting cases)
    rawdata <- asIsData[ , fieldPositions]
    names(rawdata) <- availableFieldsNames
    
    ## Cleaning the data 
    ### Excluding rows with Type <> settings$SMPString 
    if (availableFields$type && is.na(SMPString))
    {
      warning("There is Type field but SMPString is not specified!
              Using all samples available in the data file.")
      rawdata <- rawdata[rawdata$type == SMPString, ]
      rawdata <- rawdata[ , -which(names(rawdata) == "type")]
    }
    if (availableFields$type && !is.na(SMPString))
    {
      rawdata <- rawdata[rawdata$type == SMPString, ]
      rawdata <- rawdata[ , -which(names(rawdata) == "type")]
    }
    ## Splitting the data and exclude trial column afterwards
    splittedTrialsPoints <- rawdata
    if (availableFields$trial)
    {
      splittedTrialsPoints <- split(rawdata, rawdata$trial)
      splittedTrialsPoints <- lapply(splittedTrialsPoints, FUN = function(x) {x[, -which(colnames(x) == "trial")]})
    }
    else if (availableFields$stimname)
    {
      stimnames <- unique(rawdata$stimname)
      rawdata$stimname <- apply(rawdata$stimname, FUN = function(x) which(stimnames == x))
      names(rawdata$stimname) <- "trial"
      splittedTrialsPoints <- split(rawdata, rawdata$trial)
      splittedTrialsPoints <- lapply(splittedTrialsPoints, FUN = function(x) {x[, -which(colnames(x) == "trial")]})
    }
    
    # figuring out data availability
    eyeDataLeft <- F
    eyeDataRight <- F
    pupilDataLeft <- F
    pupilDataRight <- F
    pupilData <- "none"
    if (availableFields$lporx && availableFields$lpory)
    {
      eyeDataLeft <- T
    }
    if (availableFields$lporx && availableFields$lpory)
    {
      eyeDataRight <- T
    }
    
    if (availableFields$lpupxsize && !availableFields$lpupysize)
    {
      pupilDataLeft <- T
      pupilData <- "circle"
    }
    if (availableFields$lpupxsize && availableFields$lpupysize)
    {
      pupilDataLeft <- T
      pupilData <- "ellipse"
    }
    if (availableFields$rpupxsize && !availableFields$rpupysize)
    {
      pupilDataRight <- T
      pupilData <- "circle"
    }
    if (availableFields$rpupxsize && availableFields$rpupysize)
    {
      pupilDataRight <- T
      pupilData <- "ellipse"
    }
    
    if (eyeDataLeft && eyeDataRight | pupilDataLeft && pupilDataRight)
    {
      binocular <- T
      eye <- "both"
    }
    if (eyeDataLeft && !eyeDataRight | pupilDataLeft && !pupilDataRight)
    {
      binocular <- F
      eye <- "left"
    }
    if (!eyeDataLeft && eyeDataRight | !pupilDataLeft && pupilDataRight)
    {
      binocular <- F
      eye <- "right"
    }
    
    # Filling the fileDataList which is returned by the parser
    availableDataFields <- availableFields[-c(2,3,5)]
    fileDataList <- list(availableDataFields, # +
                         binocular = binocular, # +
                         eye = eye, # +
                         conditions = list(eyeTracker, # +
                                           pupilData, # +
                                           sampleRate, # - should be also figured out automatically
                                           timeUnits, # +
                                           screenDistance, # - should be also figured out automatically
                                           distanceUnits), # +
                         subjectID = subjectID, # -should be also figured out automatically
                         splittedTrialsPoints
                         )
  } 
  else
  # user can write his/her own script (*.R) and use it to load data
  {
    # check if the specified source exists
    if (file.exists(datafileReadScript))
    {
      source(datafileReadScript)
      fileDataList <- fun(filepath)
    }
    else
    {
      warning("Script not found!")
    }
  }
  fileDataList
}
