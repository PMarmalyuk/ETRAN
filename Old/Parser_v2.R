##### Function Interface Description #####
# Arguments:
##
# Value: a list of
## filename
## subject ID/name
## availableFields list
## trajectories list
## conditions list

parser <- function(filepath, datafileReadScript = "own", fun = loadData, settings)
{
  # settings list structure:
  ## sep = "\t", skip = "20", comment.char = "#", 
  ## header = T, eyeTracker = "SMI", SMPString = "SMP", pupilData = "ellipse",
  ## sampleRate = 100, timeUnits = 1E-3, screenDistance = 100, distanceUnits = 1E-2,
  ## field.names = NA,
  ## field.order = c(time = 1, type = 2, trial = 3, frame = 14, stimname = 0,
  ##                lporx = 10, lpory = 11, rporx = 0, rpory = 0, 
  ##                lpupsizex = 6, lpupsizey = 7, rpupsizex = 0, rpupsizey = 0),
  ## noFieldMarkers = list(name = "-", order = 0)
  
  # sep is set? if not use \t with warning
  # skip is set? if not use 0 with warning
  # comment.char is set? if not use "#" with warning
  # header is set? if not use F and field.order with warning;
  # eyeTracker is set? if not use "Unknown"
  # SMPString is set? if not use NA and don't analyze the Type field with warning
  # pupilData is set? if not figure it out depending on available data fields, if no fields - use NA
  # sampleRate is set? if not use NA
  # timeUnits is set? if not use NA
  # screenDistance is set? if not use NA
  # distanceUnits is set? if not use NA
  # field.names is set? if not use field.order; 
  ## if field.order is not set then use default order with warning
  # field.order is set? if not use field.names; 
  ## if field.names is not set then use default order with warning;
  ## if field.names is set but header = F or not set - use default order with warning;
  settings <- list(sep = "\t", skip = 20, comment.char = "#",
                           header = T, eyeTracker = "SMI", SMPString = "SMP", pupilData = "ellipse",
                           sampleRate = 100, timeUnits = 1E-3, screenDistance = 100, distanceUnits = 1E-2,
                           field.names = NA,
                           field.order = c(time = 1, type = 2, trial = 3, frame = 14, stimname = 0,
                                           lporx = 10, lpory = 11, rporx = 0, rpory = 0, 
                                           lpupsizex = 6, lpupsizey = 7, rpupsizex = 0, rpupsizey = 0),
                           noFieldMarkers = list(name = "-", order = 0))
  
  if (is.na(settings$sep) | is.null(settings$sep) | settings$sep == "")
  {
    sep <- "\t"
    warning("Separator parameter is not defined. Using default (tab).")
  }
  else
  {
    sep <- settings$sep
  }
  
  if (is.na(settings$skip) | is.null(settings$skip) | settings$skip == "")
  {
    skip <- 0
    warning("Skip parameter is not defined. Using default (0)")
  }
  
  if (datafileReadScript == "own")
  {
    ## check if data loading settings were set
    if (!file.exists(filepath))
    {
      stop("Datafile not found!")
    }
    else
    {
      if (!dataLoadSettingsSet)
      {
        ## ask user to provide data loading settings once and restart data loading process
        stop("Please, set data loading settings!")
      }
      else
      {
        asIsData <- read.csv(filepath, 
                             sep = settings$sep, 
                             skip = settings$skip, 
                             comment.char = settings$comment.char, 
                             header = settings$header,
                             blank.lines.skip = T, 
                             check.names = F,
                             stringsAsFactors = F)
        

        #N <- nrow(asIsData)
        ## Creating the availableFields flags according to the settings specified by user
        colNames <- c("time", "type", "trial", "frame", "stimname",
                      "lporx", "lpory", "rporx", "rpory", 
                      "lpupilxsize", "lpupilysize", "rpupilxsize", "rpupilysize")
        availableFields <- as.list(as.logical(rep(1,13)))
        names(availableFields) <- colNames
        if (!is.na(settings$field.names)) 
        {
          availableFields <- as.list(settings$field.names != "-")
          colNames <- colNames[as.vector(availableFields, mode = "logical")]
        }
        if (is.na(settings$field.names) && !is.na(settings$field.order))
        {
          availableFields <- as.list(settings$field.order != 0)
          colNames <- colNames[as.vector(availableFields, mode = "logical")]
        }
        ## Filling the rawdata object with all data available (different setting cases)
        fieldPositions <- c()
        ## header = 1, names = 1
        if ( settings$header && !is.na(settings$field.names) )
        {
          for (field in settings$field.names)
          {
            if (field != "-")
            {
              fieldPositions <- c(fieldPositions, which(colnames(asIsData) == field))
            }
          }
        }
        ## header = 0, order = 1 or names = 0, order = 1
        if ( (!settings$header && !is.na(settings$field.order)) |
             (is.na(settings$field.names) && !is.na(settings$field.order)) )
        {
              fieldPositions <- settings$field.order[settings$field.order != 0]
        }
        ## header = 0, names = 1, order = 0
        if ( !settings$header && !is.na(settings$field.names) && is.na(settings$field.order) )
        {
          fieldPositions <- 1:length(availableFields)
        }
        ## names = 0, order = 0
        if ( is.na(settings$field.names) && is.na(settings$field.order) )
        {
          fieldPositions <- 1:13
        }
        rawdata <- asIsData[ , fieldPositions]
        names(rawdata) <- colNames
        
        ## Cleaning the data 
        ### Excluding rows with Type <> settings$SMPString 
        if (availableFields$type)
        {
          rawdata <- rawdata[rawdata$type == settings$SMPString, ]
          rawdata <- rawdata[ , -which(names(rawdata) == "type")]
        }
        splittedTrialsPoints <- rawdata
        if (availableFields$trial)
        {
          splittedTrialsPoints <- split(rawdata, rawdata$trial)
        }
        else if (availableFields$stimname)
        {
          print("I am there!")
          stimnames <- unique(rawdata$stimname)
          rawdata$stimname <- apply(rawdata$stimname, FUN = function(x) which(stimnames == x))
          names(rawdata$stimname) <- "trial"
          splittedTrialsPoints <- split(rawdata, rawdata$trial)
        }
        
        
        
        #if (availableFields$lporx && availableFields$lpory)
        #{
        #  eye <- "left"
        #}
        #if (availableFields$rporx && availableFields$rpory)
        #{
        #  eye <- "right"
        #}
        #if (availableFields$lporx && availableFields$lpory && availableFields$rporx && availableFields$rpory)
        #{
        #  eye <- "both"
        #}
        #trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
        #{
        #  x <- x[ , -which(names(x) == "trial")]
        #  new("oculomotorData", samples = x, eye = eye, pupilData = settings$pupilData)
        #}
        #)
        
     } 
    }
  }
  # user can write his/her own script (*.R) and use it to load data
  if (datafileReadScript != "own")
  {
    # check if the specified source exists
    if (file.exists(datafileReadScript))
    {
      source(datafileReadScript)
      trajList <- fun(filepath)
    }
    else
    {
      warning("Script not found!")
    }
  }
  trajList
}
