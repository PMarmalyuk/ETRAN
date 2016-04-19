createAOI_S_E<-
          function(self, dispositionData){
            if(ArgIntersection(dispositionData,ExpDispDataList$ClassicEllipse)==0){
              
              if(is.numeric(dispositionData$CX) & dispositionData$CX>=0){
                if(is.numeric(dispositionData$CY) & dispositionData$CY>=0){    
                  if(is.numeric(dispositionData$MinAxis) & dispositionData$MinAxis>=0){
                    if(is.numeric(dispositionData$MajAxis) & dispositionData$MajAxis>=0){
                      if(is.numeric(dispositionData$Ang) & dispositionData$Ang>=0){
                      self@dispositionData$CX=dispositionData$CX
                      self@dispositionData$CY=dispositionData$CY
                      self@dispositionData$MinAxis=dispositionData$MinAxis
                      self@dispositionData$MajAxis=dispositionData$MajAxis
                      self@dispositionData$Angle=dispositionData$Angle
                      }
                      else {stop(paste("Angle of Ellipse is either not numeric or less then zero!"))}
                    }
                    else {stop(paste("Major axis of Ellipse is either not numeric or less then zero!"))}
                  }
                  else {stop(paste("Minor axis of Ellipse is either not numeric or less then zero!"))}
                }
                else {stop(paste("Coordinat Y of Ellipse (`CY`) center is either not numeric or less then zero!"))}
              }
              else {stop(paste("Coordinat X of Ellipse (`CX`) center is either not numeric or less then zero!"))}
              
              return(self)
              
            } else {stop(paste("Incorrect `dispositionData` for this type of `AOI`(`Ellipse`), expected ",CharVecToStr(as.character(ExpDispDataList$ClassicEllipse))," but ",
                               CharVecToStr(as.character(names(dispositionData))),"found, check params names and number"))}
          }




createAOI_S_P<-
          function(self, dispositionData){
            if(ArgIntersection(dispositionData,ExpDispDataList$ClassicPolyhedron)==0){
              
              if(is.vertexes(dispositionData$Vertexes)){
                self@dispositionData$Vertexes=dispositionData$Vertexes
              }
              else {stop(paste("Unexpected description of Polyhedron vertexes!"))}
              
              return(self)
            } else {stop(paste("Incorrect `dispositionData` for this type of `AOI`(`Ellipse`), expected ",CharVecToStr(as.character(ExpDispDataList$ClassicPolyhedron))," but ",
                               CharVecToStr(as.character(names(dispositionData))),"found, check params names and number"))}
          }
