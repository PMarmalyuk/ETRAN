source("f_alias.R")

aoi<-als$new("AOI_COLLECTION")
aoi<-als$acol(aoi,als$aoio(name="Ellip",shape="Ellipse",
                          dispositionData=list(CX=8,CY=8,MinAxis=2,MajAxis=4,Angle=0)  )   )

aoi<-als$acol(aoi,als$aoio(name="Pol",shape="Polyhedron",
                          dispositionData=list(list(5,5),list(8,6),list(9,1),list(6,2)))  )   

aoi<-als$acol(aoi,als$aoio(name="Pol2",shape="Polyhedron",
                          dispositionData=list(list(list(6,9),list(10,9),list(10,7),list(6,7))) )   )

aoi<-als$acol(aoi,als$aoio(name="Pol3",shape="Polyhedron",
                          dispositionData=list(list(12,3),list(12,6),list(15,5),list(15,3)))  )   


aoi_set<-als$new("SETS_COLLECTION")
aoi_set<-als$acol(aoi_set, AOIsetDataByIndex(AOISetStructure(name="my_set_1"),list(1,2,3,4) ))
aoi_set<-als$acol(aoi_set, AOIsetDataByIndex(AOISetStructure(name="my_set_2"),list(1,4) ))
aoi_set

#create sample trajectory which contain 23 points
trajectory<-data.frame(t=c(1:23),
               x=c(6,7,13,13,14,11,10,10,10,9,7,6,13,7,13,7,13,7,13,7,13,7,13),
               y=c(4,4,4,7,4,5,6,6,6,8,5,5,5,8,4,8,4,8,4,8,4,8,4))

my_set<-als$ebu(aoi_set,1)
#check single point labeling (return list of AOI names which contain given point)
tag_point(AOIL=DataFromAOIset(my_set,aoi),x=0,y=0)#AOI names from AOI set - my_set, contains point (0,0)
tag_point(AOIL=DataFromAOIset(my_set,aoi),x=9,y=8)#AOI names from AOI set - my_set, contains point (9,8)
my_set<-als$ebn(aoi_set,"my_set_1")
tag_point(AOIL=DataFromAOIset(my_set,aoi),x=9,y=8)

#check AOI detector
my_setting<-NULL
AOI(trajectory$t,trajectory$x,trajectory$y,my_setting,DataFromAOIset(my_set,aoi))

#check sequence in AOI function
seq_in_aoi(DataFromAOIset(my_set,aoi)[[1]],trajectory)
seq_in_aoi(DataFromAOIset(my_set,aoi)[[2]],trajectory)
seq_in_aoi(DataFromAOIset(my_set,aoi)[[3]],trajectory)


#get frequencies/probability matrixes
#obtain AOI seq by x/y data (with _milk)
AOISeqByTrajectory<-AOI(trajectory$t,trajectory$x,trajectory$y,my_setting,DataFromAOIset(my_set,aoi))$AOI_NAME
getFrMatrix(AOISeqByTrajectory)
getPrMatrix(AOISeqByTrajectory)
#obtain AOI seq by x/y data (without _milk)
AOISeqByTrajectory<-AOISeqByTrajectory[AOISeqByTrajectory!="_milk"] 
getFrMatrix(AOISeqByTrajectory)
getPrMatrix(AOISeqByTrajectory)



#merge equal neighbors names in string list
mergedAOISeqence<-mergeNeighborAOI(AOISeqByTrajectory)
#set seq to serch
targetSeqence<-c("Pol","Pol3","Ellip","Pol3")
#get positi(on/ons)
subseqPosInAOI(mergedAOISeqence,targetSeqence)
#set another seq to serch
targetSeqence1<-c("Ellip","Pol3")
#get positi(on/ons)
subseqPosInAOI(mergedAOISeqence,targetSeqence1)
