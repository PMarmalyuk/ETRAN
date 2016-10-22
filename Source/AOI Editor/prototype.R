AOISetStructure<-
  function(name="default", UIDs=list() ){
    return( sapply(ls(), function(x) get(x)   ))
  }

stimulStructure<-
  function(imgDataURI= "", name= "default", imgWidth = 0, imgHeight = 0, uid=-1){
    return( sapply(ls(), function(x) as.vector(get(x))   ))
  }

AOIstructure<-
  function(name= "", shape= "default", dispositionData = list()){
    return( sapply(ls(), function(x) as.vector(get(x))   ))
  }


bindingobject<-
   function(name="default", imgUID=NULL, AOI_UIDs=list(), setUIDs=list() ){
     return( sapply(ls(), function(x) as.vector(get(x))   ))
  }

polyhedronStructure<-
  function(name="default",shape="Polyhedron", dispositionData=list(Vertexes=list(list(1,1),list(2,2),list(3,3) ) )   ){
    return( sapply(ls(), function(x) as.vector(get(x))   ))
  }

ellipseStructure<-
  function(name="default",shape="Ellipse", dispositionData=list(CX=0,CY=0,MinAxis=0,MajAxis=0,Angle=0)  ){
    return( sapply(ls(), function(x) as.vector(get(x))   ))
  }