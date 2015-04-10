setClass("ReadSettings",
         representation(readSettings = "list"),
         prototype(readSettings = list(encoding = "UTF-8",   
                                       sep = "\t",
                                       skip = 20,
                                       comment.char = "#",
                                       header = T,
                                       splitBy = "Trial") # "Trial" or "Stimname"
         )
)

setClass("AvailableDataFields",
         representation(availableFields = "list"),
         prototype(availableFields = list(time = NA, trial = NA, frame = NA, stimname = NA, smptype = NA,
                                          lporx = NA, lpory = NA, rporx = NA, rpory = NA,
                                          lpupxsize = NA, lpupysize = NA, rpupxsize = NA, rpupysize = NA,
                                          additionalFields = list(lrawx = NA, lrawy = NA))
         )
)

setClass("DataFieldNames",
         representation(fieldNames = "list"),
         prototype(fieldNames = list(time = "Time", trial = "Trial", frame = "Frame", stimname = "StimulusName", smptype = "SampleType",
                                     lporx = "LPORX", lpory = "LPORY", rporx = "RPORX", rpory = "RPORY",
                                     lpupxsize = "LPupilXSize", lpupysize = "LPupilYSize", rpupxsize = "RPupilXSize", rpupysize = "RPupilYSize",
                                     additionalFields = list(lrawx = "LRawX", lrawy = "LRawY"))
         )
)

setClass("HeaderKeys",
         representation(subjectCode = "character",
                        sampleRate = "character",
                        stimDim = "character",
                        headDist = "character",
                        otherKeys = "list"),
         prototype(subjectCode = "Subject",
                   sampleRate = "Sample Rate",
                   stimDim = "Stimulus Dimension",
                   headDist = "Head Distance")
)

setClass("RawDataSettings",
         representation(rawReadSettings = "ReadSettings",
                        availableFields = "AvailableDataFields",
                        fieldNames = "DataFieldNames",
                        headerKeys = "HeaderKeys",
                        eyeTrackerModel = "character",
                        eyeTrackerType = "character" #"tower", "remote" or "head-mounted"
         ),
         prototype()
)

setClass("Conditions",
         representation(conditions = "list"),
         prototype(conditions = list(eye = NA, # "left", "right" or "both"
                                     sampleRate = NA, # frequency in Hz
                                     screenDistance = NA,
                                     pupilShape = NA, #"circle" or "ellipse"
                                     pupilDataType = NA, #"radius" or "diameter"
                                     timeUnits = NA, # e.g. 1E-3 for milliseconds
                                     distanceUnits = NA, # e.g. 1E-2 for millimeters
                                     pupilSizeUnits = NA)) #"px", "mm", cm", etc
)