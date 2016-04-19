source("Classes\\optionsAndSettingsClasses.R", local = T)
source("Classes\\baseEyeDataClasses.R", local = T)
source("Classes\\baseClasses.R", local = T)
source("Classes\\listsAndTablesClasses.R", local = T)
source("Classes\\operators.R", local = T)
source("Classes\\representations.R", local = T)

source("Classes\\Events\\events.R", local = T)
source("Classes\\Events\\frameEvent.R", local = T)
source("Classes\\Events\\filterEvent.R", local = T)
source("Classes\\Events\\oculomotorEvent.R", local = T)
source("Classes\\Events\\AOIEvent.R", local = T)
source("Classes\\Events\\syncEvent.R", local = T)
source("Classes\\Events\\windowEvent.R", local = T)

source("Functions\\miscFunctions.R", local = T)
source("Functions\\dataParsers.R", local = T)

source("Functions\\filters.R", local = T)
source("Functions\\smoothers.R", local = T)
source("Functions\\detectors.R", local = T)
source("Functions\\analyzers.R", local = T)
source("Functions\\eventCounter.R", local = T)
source("Functions\\eventSelector.R", local = T)
source("Functions\\estimator.R", local = T)
source("Functions\\subFunctions.R", local = T)
source("Functions\\subFunctionsInit.R", local = T)
source("Methods\\Methods_v_1_7.R", local = T)


eventMarkersDefs <- new(Class = "EventMarkersDefinitions")
eventMarkersDefs@definitions <- append(eventMarkersDefs@definitions, new(Class = "EventMarkersDefinition",
                                                                         eventClass = "FilterEvent",
                                                                         eventTypesIDs = c(1,2,3),
                                                                         typesMarkers = c("OK", "GAP", "ARTIFACT")))
eventMarkersDefs@definitions <- append(eventMarkersDefs@definitions, new(Class = "EventMarkersDefinition",
                                                                         eventClass = "OculomotorEvent",
                                                                         eventTypesIDs = c(1,2,3,4,5,6,7),
                                                                         typesMarkers = c("FIXATION", "SACCADE",
                                                                                          "GLISSADE", "SMOOTH PURSUIT",
                                                                                          "BLINK", "GAP", "ARTIFACT")))
                                                                                          