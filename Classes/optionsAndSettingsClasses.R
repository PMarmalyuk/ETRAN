setClass("ReadSettings",
         representation(readSettings = "list"),
         prototype(readSettings = list(encoding = "UTF-8",   
                                       sep = "\t",
                                       dec = ",",
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
                                          leftAdditionalFields = NA,
                                          rightAdditionalFields = NA)
         )
)

setClass("DataFieldNames",
         representation(fieldNames = "list"),
         prototype(fieldNames = list(time = "Time", trial = "Trial", frame = "Frame", stimname = "StimulusName", smptype = "SampleType",
                                     lporx = "LPORX", lpory = "LPORY", rporx = "RPORX", rpory = "RPORY",
                                     lpupxsize = "LPupilXSize", lpupysize = "LPupilYSize", rpupxsize = "RPupilXSize", rpupysize = "RPupilYSize",
                                     leftAdditionalFields = NA,
                                     rightAdditionalFields = NA)
         )
)

setClass("HeaderKeys",
         representation(keys = "list"),
         prototype(keys = list(subjectCode = "Subject",
                   sampleRate = "Sample Rate",
                   stimDim = "Stimulus Dimension",
                   headDist = "Head Distance"))
)

setClass("RawDataSettings",
         representation(rawReadSettings = "ReadSettings",
                        availableFields = "AvailableDataFields",
                        headerKeys = "HeaderKeys"
         ),
         prototype()
)


setClass("Conditions",
         representation(conditions = "list"),
         prototype(conditions = list(eye = NA, # "left", "right" or "both"
                                     sampleRate = NA, # frequency in Hz
                                     screenDistance = NA,
                                     screenDim = NA,
                                     screenSize = NA, # width and height of the screen / field of view
                                     pupilShape = NA, #"circle" or "ellipse"
                                     pupilDataType = NA, #"radius" or "diameter"
                                     timeUnits = NA, # e.g. 1E-3 for milliseconds
                                     distanceUnits = NA, # e.g. 1E-2 for millimeters
                                     pupilSizeUnits = NA)) #"px", "mm", cm", etc
)