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

## Field names and order in trajectory object dataframe:
## "Time", "Frame", "PORX", "PORY", "PupX", "PupY",

parser <- function(filepath, datafileReadScript = "own", fun = loadData, settings)
{
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
        
        
        N <- nrow(asIsData)
        ## Creating the availableFields flags according to the settings specified by user
        availableFields <- c(
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
          rpupilysize = logical())
        
        if (!is.na(settings$field.names)) 
        {
          availableFields[] <- (settings$field.names != "-")
        }
        if (is.na(settings$field.names) && !is.na(settings$field.order))
        {
          availableFields[] <- (settings$field.order != 0)
        }
        
        potentialColNames <- c("Time", "Type", "Trial", "Frame", "Stimulus.Name", 
                      "LPOR.X", "LPOR.Y", "RPOR.X", "RPOR.Y",
                      "LPupil.X.Size", "LPupil.Y.Size", "RPupil.X.Size", "RPupil.Y.Size")
        colNames <- potentialColNames[availableFields]
        #rawdata <- data.frame(Time = new(Class = "numeric", rep(NA, N)),
                          #    Type = new(Class = "character", rep(NA, N)),
                       #       Trial = new(Class = "integer", rep(NA, N)),
                        #      Frame = new(Class = "integer", rep(NA, N)),
                         #     Stimulus.Name = new(Class = "character", rep(NA, N)),
                          #    LPOR.X = new(Class = "numeric", rep(NA, N)),
                           #   LPOR.Y = new(Class = "numeric", rep(NA, N)),
                            #  RPOR.X = new(Class = "numeric", rep(NA, N)),
                             # RPOR.Y = new(Class = "numeric", rep(NA, N)),
                            #  LPupil.X.Size = new(Class = "numeric", rep(NA, N)),
                             # LPupil.Y.Size = new(Class = "numeric", rep(NA, N)),
                              #RPupil.X.Size = new(Class = "numeric", rep(NA, N)),
                              #RPupil.Y.Size = new(Class = "numeric", rep(NA, N)))

        ## Filling the rawdata object with all data available (different setting cases)
        #time = "Time", type = "Type", trial = "Trial", frame = "Frame", stimname = "-", 
        #     lporx = "L POR X [px]", lpory = "L POR Y [px]", rporx = "-", rpory = "-",
        #     lpupsizex = "L Dia X [px]", lpupsizey = "L Dia Y [px]", rpupsizex = "-", rpupsizey = "-"),
        
         if ( settings$header && !is.na(settings$field.names) )
         {
           fieldPositions <- c()
           for (field in settings$field.names)
          {
            if (field != "-")
            {
              fieldPositions <- c(fieldPositions, which(colnames(asIsData) == field))
            }
          }
          rawdata <- asIsData[ , fieldPositions]
          print(head(rawdata))
          names(rawdata) <- colNames
          print(head(rawdata))
        }
        if ( (!settings$header && !is.na(settings$field.order)) |
             (settings$header && is.na(settings$field.names) && !is.na(settings$field.order)) )
        {
          for (i in 1:length(availableFields))
          {
            if (availableFields[[i]]) 
            {
              rawdata[ , i] <- asIsData[ , settings$field.order[i]]
            }
          }
        }
        if ( (!settings$header && is.na(settings$field.order)) |
             (settings$header && is.na(settings$field.names) && is.na(settings$field.order)) )
        {
          for (i in 1:length(availableFields))
          {
            if (availableFields[[i]]) 
            {
              rawdata[ , i] <- asIsData[ , i]
            }
          }
        }
        ## Cleaning the data 
        ### Excluding rows with Type <> settings$SMPString 
        if (availableFields$sample.types)
        {
          rawdata <- rawdata[rawdata[ , 2] == settings$SMPString, ]
        }

        ## Creating the trajList
        ### points = "data.frame", # fields: "time", "frame", "PORX", "PORY", "PUPX", "PUPY", "filterMark", "eventMark"
        ### eye = "character" # "left" or "right"
        ### pupilData = "character", # "none", "circle", "ellipse"
        
        ## Monocular trajList - left eye - NOT FINISHED (need to evaluate and set additional Monocular params)
        if (availableFields$lporx && availableFields$lpory)
        {
          if (settings$pupilData == "none")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              df <- data.frame(time = x$Time, 
                               frame = x$Frame, 
                               PORX = x$LPOR.X, 
                               PORY = x$LPOR.Y, 
                               filterMark = new(Class = "character", rep("-", nrow(x))),
                               eventMark = new(Class = "character", rep("-", nrow(x))))
              traj <- new("Trajectory", points = df, eye = "left", pupilData = settings$pupilData)
              new("Monocular", eyeData = traj)
            })
          }
          if (settings$pupilData == "circle")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              df <- data.frame(time = x$Time, 
                               frame = x$Frame, 
                               PORX = x$LPOR.X, 
                               PORY = x$LPOR.Y,
                               PUPX = x$LPupil.X.Size,
                               filterMark = new(Class = "character", rep("-", nrow(x))),
                               eventMark = new(Class = "character", rep("-", nrow(x))))
              traj <- new("Trajectory", points = df, eye = "left", pupilData = settings$pupilData)
              new("Monocular", eyeData = traj)
            })
          }
          if (settings$pupilData == "ellipse")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              df <- data.frame(time = x$Time, 
                               frame = x$Frame, 
                               PORX = x$LPOR.X, 
                               PORY = x$LPOR.Y,
                               PUPX = x$LPupil.X.Size,
                               PUPY = x$LPupil.Y.Size,
                               filterMark = new(Class = "character", rep("-", nrow(x))),
                               eventMark = new(Class = "character", rep("-", nrow(x))))
              traj <- new("Trajectory", points = df, eye = "left", pupilData = settings$pupilData)
              new("Monocular", eyeData = traj)
            })
          }
        }
        ### Monocular trajList - right eye - NOT FINISHED (need to evaluate and set additional Monocular params)
        if (availableFields$rporx && availableFields$rpory)
        {
          if (settings$pupilData == "none")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              df <- data.frame(time = x$Time, 
                               frame = x$Frame, 
                               PORX = x$RPOR.X, 
                               PORY = x$RPOR.Y, 
                               filterMark = new(Class = "character", rep("-", nrow(x))),
                               eventMark = new(Class = "character", rep("-", nrow(x))))
              traj <- new("Trajectory", points = df, eye = "right", pupilData = settings$pupilData)
              new("Monocular", eyeData = traj)
            })
          }
          if (settings$pupilData == "circle")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              df <- data.frame(time = x$Time, 
                               frame = x$Frame, 
                               PORX = x$RPOR.X, 
                               PORY = x$RPOR.Y,
                               PUPX = x$RPupil.X.Size,
                               filterMark = new(Class = "character", rep("-", nrow(x))),
                               eventMark = new(Class = "character", rep("-", nrow(x))))
              traj <- new("Trajectory", points = df, eye = "right", pupilData = settings$pupilData)
              new("Monocular", eyeData = traj)
            })
          }
          if (settings$pupilData == "ellipse")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              df <- data.frame(time = x$Time, 
                               frame = x$Frame, 
                               PORX = x$RPOR.X, 
                               PORY = x$RPOR.Y,
                               PUPX = x$RPupil.X.Size,
                               PUPY = x$RPupil.Y.Size,
                               filterMark = new(Class = "character", rep("-", nrow(x))),
                               eventMark = new(Class = "character", rep("-", nrow(x))))
              traj <- new("Trajectory", points = df, eye = "right", pupilData = settings$pupilData)
              new("Monocular", eyeData = traj)
            })
          }
        }
        ### Binocular trajList - NOT FINISHED (need to evaluate and set additional Binocular params)
        if (availableFields$lporx && availableFields$lpory && availableFields$rporx && availableFields$rpory)
        {
          if (settings$pupilData == "none")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              dfLeft <- data.frame(time = x$Time, 
                               frame = x$Frame, 
                               PORX = x$LPOR.X, 
                               PORY = x$LPOR.Y, 
                               filterMark = new(Class = "character", rep("-", nrow(x))),
                               eventMark = new(Class = "character", rep("-", nrow(x))))
              dfRight <- data.frame(time = x$Time, 
                                   frame = x$Frame, 
                                   PORX = x$RPOR.X, 
                                   PORY = x$RPOR.Y, 
                                   filterMark = new(Class = "character", rep("-", nrow(x))),
                                   eventMark = new(Class = "character", rep("-", nrow(x))))
              trajLeft <- new("Trajectory", points = dfLeft, eye = "left", pupilData = settings$pupilData)
              trajRight <- new("Trajectory", points = dfRight, eye = "right", pupilData = settings$pupilData)
              new("Binocular", leftEyeData = trajLeft, rightEyeData = trajRight)
            })
          }
          if (settings$pupilData == "circle")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              dfLeft <- data.frame(time = x$Time, 
                                   frame = x$Frame, 
                                   PORX = x$LPOR.X, 
                                   PORY = x$LPOR.Y,
                                   PUPX = x$LPupil.X.Size,
                                   filterMark = new(Class = "character", rep("-", nrow(x))),
                                   eventMark = new(Class = "character", rep("-", nrow(x))))
              dfRight <- data.frame(time = x$Time, 
                                    frame = x$Frame, 
                                    PORX = x$RPOR.X, 
                                    PORY = x$RPOR.Y,
                                    PUPX = x$RPupil.X.Size,
                                    filterMark = new(Class = "character", rep("-", nrow(x))),
                                    eventMark = new(Class = "character", rep("-", nrow(x))))
              trajLeft <- new("Trajectory", points = dfLeft, eye = "left", pupilData = settings$pupilData)
              trajRight <- new("Trajectory", points = dfRight, eye = "right", pupilData = settings$pupilData)
              new("Binocular", leftEyeData = trajLeft, rightEyeData = trajRight)
            })
          }
          if (settings$pupilData == "ellipse")
          {
            trajList <- lapply(splittedTrialsPoints, FUN = function(x) 
            {
              dfLeft <- data.frame(time = x$Time, 
                                   frame = x$Frame, 
                                   PORX = x$LPOR.X, 
                                   PORY = x$LPOR.Y,
                                   PUPX = x$LPupil.X.Size,
                                   PUPY = x$LPupil.Y.Size,
                                   filterMark = new(Class = "character", rep("-", nrow(x))),
                                   eventMark = new(Class = "character", rep("-", nrow(x))))
              dfRight <- data.frame(time = x$Time, 
                                    frame = x$Frame, 
                                    PORX = x$RPOR.X, 
                                    PORY = x$RPOR.Y,
                                    PUPX = x$RPupil.X.Size,
                                    PUPY = x$RPupil.Y.Size,
                                    filterMark = new(Class = "character", rep("-", nrow(x))),
                                    eventMark = new(Class = "character", rep("-", nrow(x))))
              trajLeft <- new("Trajectory", points = dfLeft, eye = "left", pupilData = settings$pupilData)
              trajRight <- new("Trajectory", points = dfRight, eye = "right", pupilData = settings$pupilData)
              new("Binocular", leftEyeData = trajLeft, rightEyeData = trajRight)
            })
          }
        }
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
