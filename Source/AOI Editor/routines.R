#library(microbenchmark)
library(Rcpp)
sourceCpp("D:\\RGNF_Repo\\AOI_sandbox\\CPP\\SPC.cpp")

#Именованная константа, список неоходимых для определения зоны интереса параметров
ExpDispDataList=list(Rect=list("LTX","LTY","Height","Width"),
                     Circle=list("CX","CY","Radius"),
                     Ellipse=list("CX","CY","MinAxis","MajAxis","Angle"),
                     Polyhedron=list("Vertexes")
)

#проверяет является ли список списком вершин
is.vertexes<-function(nList){
  if((is.list(nList))&
     (all(as.logical(lapply(nList,is.numeric_l))))&
     (all(as.logical(sapply(lapply(nList,as.numeric),GETZ))))&
     (length(nList)>=3)
     #если переданный параметр явлется списком, 
     #все значения в этом списке числа
     #все эти числа больше либо равны нулю
     #и вершин больше 2
  ){return(TRUE)
    #то передан список вершин
  } else {return(FALSE)}
    #в противном случае по переданнному списку невозможно построить многогранник
}

#проверяет все ли координаты вершин имеют числовые значения
is.numeric_l<-function(nList){
  if(is.list(nList)){
    return(all(as.logical(lapply(nList,is.numeric))))
  } else return(FALSE)
}