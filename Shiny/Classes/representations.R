setClass(Class = "Representation", 
         representation = representation())

setClass("AOISequence",
         contains = "Representation",
         representation(sequence = "data.frame", # df with AOI ID and duration
                        AOISetID = "numeric")
)
setClass("AOIStatsVector",
         contains = "Representation",
         representation(type = "character", #freqs, probs, times
                        vector = "numeric", # vector
                        AOISetID = "numeric")
)
setClass("AOITransMatrix",
         contains = "Representation",
         representation(type = "character", #transition freqs or probs, SR, SRnormalized
                        mat = "matrix", # matrix
                        AOISetID = "numeric")
)
setClass("Heatmap",
         contains = "Representation",
         representation(mat = "matrix" # df with x, y and delay columns
         )
)
setClass("ScanPath",
         contains = "Representation",
         representation(path = "data.frame" # df with x, y and delay columns
         )
)


## just for check
a <- new(Class= "AOISequence")
extends(class(a), "Representation")
