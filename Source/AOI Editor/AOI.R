inAOI<-function(AOI, x, y){ 
  options<-AOI$dispositionData  
  switch(AOI$shape,
         Rectangle={
           if((options$LTX<x)&
              ((options$LTX+options$Width)>x)& #точка между левой и правой границей
              (options$LTY>y)&
              ((options$LTY-options$Height)<y) #точка между верхней и нижней границей
           ){res<-TRUE} else res<-FALSE
         },
         Circle={
           tx<-(options$CX-x)^2
           ty<-(options$CY-y)^2
           if(sqrt(tx+ty)<options$Radius){res<-TRUE} else res<-FALSE
         },
         Ellipse={          
           nx<-(((x*cos(options$Angle))-(y*sin(options$Angle)))-options$CX)^2
           ny<-(((y*cos(options$Angle))+(x*sin(options$Angle)))-options$CY)^2
           
           if((nx/(options$MajAxis^2)+ny/(options$MinAxis^2))<1){res<-TRUE} else res<-FALSE
         },
         Polyhedron={
           xv<-sapply(options,function(a){return(a[[1]])}) #Все координаты х списком
           yv<-sapply(options,function(a){return(a[[2]])}) #Все координаты y списком
           res<-InPolyhedron(xv,yv,x,y)
         }
  )
  return(res)
}
         
#check if x/y trajectory points belongs to specified AOI, 
#retuns T/F vector which values coresponds to trajectory points
seq_in_aoi<-
  function(AOI,cordinats){
    return(mapply(inAOI, x=cordinats$x, y=cordinats$y,MoreArgs=list(AOI=AOI)))
  }

#for single point and list of AOI objects returns list of AOI@name
#for AOIs which contains point. If multiple areas contains specified point
#all their unique names would be added.
tag_point<-
  function(AOIL,x,y){
    #str(AOIL)
    bflag<-mapply(AOIL,FUN=inAOI,MoreArgs=list(x=x,y=y))
    if(sum(bflag)){
      f_area_name<-AOIL[bflag]
      f_area_name<-lapply(f_area_name, function(element) element$name)
      return(unique(f_area_name))
    }
    else{return("_milk")}
  }

#check for each point of trajectory whether it is in one of AOI from specified AOI list
#return vector of AOI@names One point<->One name. For multiple includance, first detected AOI@name prefered.
#Used for AOI detector function to get AOI_lables column
tag_trajectory<-
  function(AOIL,df){
    apply(df,1,function(x) tag_point(AOIL=AOIL,x=x['x'],y=x['y'])[[1]])
  }

#return all detected AOI names for each point of trajectory
mult_tag_trajectory<-
  function(AOIL,df){
    apply(df,1,function(x) tag_point(AOIL=AOIL,x=x['x'],y=x['y']))
  }

#AOI detector
#for x/y pair and given AOI list binds data.frame [x y AOI_NAME]
AOI <-
  function(t,x,y,settings,AOIRawData){
    df<-cbind(t,x,y)
    return(as.data.frame(cbind(df,AOI_NAME=tag_trajectory(AOIL=AOIRawData,df=df)), strigsAsFactors=FALSE))
  }


get_un_AOINames<-
  function(AOIL){
    un_name<-lapply(AOIL, function(element) element@name)
    return(unique(un_name))
  }

#calck freqencies of transitions between AOIs by specified AOI seqence
getFrMatrix<-
  function(tagList){
    tagNames<-unique(tagList)
    frMatrix<-matrix(0,length(tagNames),length(tagNames))
    colnames(frMatrix)<-tagNames
    rownames(frMatrix)<-tagNames
    LA<-length(tagList)-1
    for(i in 1:LA){
      vthis<-toString(tagList[i])
      vnext<-toString(tagList[i+1])
      if (vthis!=vnext){frMatrix[vthis,vnext]<-(frMatrix[vthis,vnext]+1)}
    }
    return (frMatrix)
  }

#calck freqencies of transitions between AOIs by specified AOI seqence
getPrMatrix<-
  function(tagList){
    prMatrix<-getFrMatrix(tagList)
    return (prMatrix/sum(prMatrix))
  }

mergeNeighborAOI<-
  function(tagList){
    LA<-length(tagList)-1
    mergedList<-vector()
    mergedList[length(mergedList)+1]<-toString(tagList[1])
    for(i in 1:LA){
      vthis<-toString(tagList[i])
      vnext<-toString(tagList[i+1])
      if (vthis==vnext){}else{ mergedList[length(mergedList)+1]<-vnext}
    }
    return (mergedList)
  }


subseqPosInAOI<-
  function(tagList,sbseq){
    LS<-length(sbseq)
    LA<-length(tagList)
    psn<-c()
    if(LS>LA){return (c(-1))} 
    else{
      for(i in 1:(LA-LS)){
        newSubset<-tagList[i:(i+LS-1)]
        if (all(newSubset==sbseq)){psn<-c(psn,i)}
      }
      if(length(psn)==0){return (c(-1))}else {return (psn)}
    }
  }
