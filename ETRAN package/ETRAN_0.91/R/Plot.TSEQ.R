#
# Plot Time Sequence 
#
Plot.TSEQ <- function(d,d.px=c(0,0),d.r=c(0,0),d.time=c(0,0),d.angle=c(0,0),d.speed=c(0,0),
                      Y.Scale="A",Fix=NULL,Speed=NULL,Plot.X=TRUE,Plot.Y=TRUE,Plot.Pupil=FALSE,
                      Col.X=c("red","red3"),Col.Y=c("blue","blue3"),Col.Pupil=c("black","gray50"),
					  Col.XF="lightsalmon",Col.YF="lightskyblue",Col.Speed="green2",Header=" ") 
{
	if (d$Mode == "Binocular") Plot.TSEQ.bin(d,d.px=d.px,d.r=d.r,d.time=d.time,d.angle=d.angle,d.speed=d.speed,
	                                         Y.Scale=Y.Scale,Fix=Fix,Speed=Speed,
	                                         Plot.X=Plot.X,Plot.Y=Plot.Y,Plot.Pupil=Plot.Pupil,
											 Col.X=Col.X,Col.Y=Col.Y,Col.Pupil=Col.Pupil,
											 Col.XF=Col.XF,Col.YF=Col.YF,Col.Speed=Col.Speed,Header=Header)
    else 			           Plot.TSEQ.mon(d,d.px=d.px,d.r=d.r,d.time=d.time,d.angle=d.angle,d.speed=d.speed,
	                                         Y.Scale=Y.Scale,Fix=Fix,Speed=Speed,
	                                         Plot.X=Plot.X,Plot.Y=Plot.Y,Plot.Pupil=Plot.Pupil,
											 Col.X=Col.X,Col.Y=Col.Y,Col.Pupil=Col.Pupil,
											 Col.XF=Col.XF,Col.YF=Col.YF,Col.Speed=Col.Speed,Header=Header)								 
}

Plot.TSEQ.mon <- function(d,d.px=c(0,0),d.r=c(0,0),d.time=c(0,0),d.angle=c(0,0),d.speed=c(0,0),
                      Y.Scale="A",Fix=NULL,Speed=NULL,
                      Plot.X=TRUE,Plot.Y=TRUE,Plot.Pupil=FALSE,
                      Col.X=c("red","red2"),Col.Y=c("blue","blue3"),Col.Pupil=c("black","black"),
					  Col.XF="lightsalmon",Col.YF="lightskyblue",Col.Speed="green2",Header=" ")
{ 											 
     # Monocular plot now 
     # Selection by time start and end
     #=========================================================================
     X  <- d$X
     Y  <- d$Y
     R  <- d$R
     T  <- d$T
     NV <- d$NV
     if (d.time[2] > 0) {                               
          s.t<-which((T >= d.time[1])&(T <= d.time[2]))
          X<-X[s.t]; Y<-Y[s.t]; T<-T[s.t]; R<-R[s.t]; NV <- NV[s.t]
      }     
     d.len <- length(X)                                 
     if (d.len < 3) {
           stop("Plot.TSEQ: Data selection error");
      }
     #Start time , End time
     T.first<-T[1];  T.last <-T[d.len]                                  
     #
     #Downsampling
     #
     scale <- d.len / 2000
     if (scale >= 2) {
        ns <- as.integer(scale)
        s.ds <- seq(1,d.len,by=ns)
        X  <- X[s.ds]
        Y  <- Y[s.ds]
        R  <- R[s.ds]
        T  <- T[s.ds]
        NV <- NV[s.ds]
     }

	 
     #
     # Pupil dillation limits
     #
     if (d.r[2] == 0) {
         s <- which(R>0)
         d.r[1] = min(R[s])*0.9; d.r[2] = max(R[s])*1.1
     }
     #
     # Coordinate limits
     #
     s.nv <- which (NV == 0)
	 
	 if (length(s.nv) < 3) {
	    stop("Plot.TSEQ: No valid data for plotting");
 	 }	
     X <- X[s.nv]; Y <- Y[s.nv]; R <- R[s.nv]; T <- T[s.nv]
     #
     if (Y.Scale == "A") {
          X  <- X.ang((X - (d$W/2)),scr.res=d$Res,eye.dist=d$Head.Dist) 
          Y  <- X.ang(((d$H/2) - Y),scr.res=d$Res,eye.dist=d$Head.Dist) 
     }


     X.max <- max(X); X.min <-min(X); Y.max <- max(Y); Y.min <- min(Y)
     Lim.min <- min(X.min,Y.min)
     Lim.max <- max(X.max,Y.max)
	 d.Lim   <- abs(Lim.max-Lim.min)*0.03
	 Lim.min <- Lim.min - d.Lim
	 Lim.max <- Lim.max + d.Lim	 	 
     #
     # Pupil dillation resampling
     #
     if (Y.Scale=="A") {
          if ((d.angle[1]==0)&(d.angle[2] == 0)) { 
             d.angle[1]<-Lim.min; d.angle[2]<-Lim.max
           }
          delta.angle <- d.angle[2] - d.angle[1]
          delta.r     <- d.r[2] - d.r[1]
          f.dr        <- delta.angle / delta.r
          R  <- ((R - d.r[1])*f.dr)+d.angle[1]
          Y.lim <- d.angle
     } else {
          if ((d.px[1]==0)&(d.px[2]==0)) {
             d.px[1]<- Lim.min; d.px[2]<-Lim.max
          }
          delta.px    <- d.px[2] - d.px[1]
          delta.r     <- d.r[2] - d.r[1]
          f.dr        <- delta.px / delta.r
          R  <- ((R - d.r[1])*f.dr)+d.px[1]
          Y.lim <- d.px
     }
     #
     # Header 
     #===============================================   
     Lab.X<-"Time (ms)"                               #подпись по оси X
     if (Y.Scale=="A") {
        Lab.Y<-"X - Y position (angle)"               #подпись по оси Y
     } else {
        Lab.Y<-"X - Y position (px)"                  #подпись по оси Y
     }
#==========================================================================
#

#     par(mar=c(4,4,1,2))
     if (Header == " ") {
      plot (T,X,col=Col.X[1],ylim=Y.lim,type="n",axes = FALSE,xlab=Lab.X,ylab=Lab.Y)	 

	 } else {
	  plot (T,X,col=Col.X[1],ylim=Y.lim,type="n",axes = FALSE,xlab=Lab.X,ylab=Lab.Y,main=Header,cex.main=.9) 
     }

     Y.seq <- seq(from=Y.lim[1],to=Y.lim[2],by=(Y.lim[2]-Y.lim[1])/10)
     if (Y.lim[2] < 100) Y.seq <- round(Y.seq,1) else Y.seq <- round(Y.seq,0)

     if ( !is.null(Fix)) {
        FX <- Fix$X
        FY <- Fix$Y
        FS <- Fix$Start
        FE <- Fix$Start+Fix$Dur
		FDISP <- Fix$Disp
        #s  <- which((FS >= T.first)&(FS <= T.last)&(FE <= T.last))
        #FX <- FX[s]; FY <- FY[s]; FS<-FS[s]; FE <-FE[s]; FDISP <- FDISP[s];
        if (Y.Scale == "A") {
            FX <- X.ang((FX - d$W/2),scr.res=d$Res,eye.dist=d$Head.Dist)
            FY <- X.ang((d$H/2 - FY),scr.res=d$Res,eye.dist=d$Head.Dist)  
            FDISP <- X.ang(FDISP,scr.res=d$Res,eye.dist=d$Head.Dist)  			
        }
        nfix<- length(FS)
        for (i in 1: nfix) {
		    rect(FS[i],(FX[i]-FDISP[i]),FE[i],(FX[i]+FDISP[i]),col=Col.XF)
            #lines(c(FS[i],FE[i]),c(FX[i],FX[i]),col=Col.XF,lwd=4,lend=3)
            #lines(c(FS[i],FE[i]),c((FX[i]+FDISP[i]),(FX[i]+FDISP[i])),col=Col.XF,lwd=2,lend=3,lty=3)
            #lines(c(FS[i],FE[i]),c((FX[i]-FDISP[i]),(FX[i]-FDISP[i])),col=Col.XF,lwd=2,lend=3,lty=3)
		    rect(FS[i],(FY[i]-FDISP[i]),FE[i],(FY[i]+FDISP[i]),col=Col.YF)			
            #lines(c(FS[i],FE[i]),c(FY[i],FY[i]),col=Col.YF,lwd=4,lend=3)
            #lines(c(FS[i],FE[i]),c((FY[i]+FDISP[i]),(FY[i]+FDISP[i])),col=Col.YF,lwd=2,lend=3,lty=3)
            #lines(c(FS[i],FE[i]),c((FY[i]-FDISP[i]),(FY[i]-FDISP[i])),col=Col.YF,lwd=2,lend=3,lty=3)			
        }
      }	

     if ( !is.null(Speed)) {
        if ((d.speed[1]==0)&(d.speed[2]==0)) {
           Speed.max <- max(Speed)*1.1;
           Speed.min <- 0
           d.speed[1] <- Speed.min
           d.speed[2] <- Speed.max
        }
        f.speed <- (Y.lim[2]-Y.lim[1]) / (d.speed[2] - d.speed[1])
        Speed   <- ((Speed - d.speed[1])*f.speed)+Y.lim[1]
        if (exists("s.t"))  Speed <- Speed[s.t]
        if (exists("s.ds")) Speed <- Speed[s.ds]
        if (exists("s.nv")) Speed <- Speed[s.nv]		
        points(T,Speed,col=Col.Speed,pch=20,cex=.5)
        Speed.seq <- seq(from=d.speed[1],to=d.speed[2],by=(d.speed[2]-d.speed[1])/10)
        Speed.seq <- round(Speed.seq,0)
        axis(4,Y.seq,labels=Speed.seq)
      }


     if (Plot.X)      points(T,X,col=Col.X[1],pch=20,cex=.5)           
     if (Plot.Y)      points(T,Y,col=Col.Y[1],pch=20,cex=.5)          
     if (Plot.Pupil)  points(T,R,col=Col.Pupil[1],pch=20,cex=.5)    

     T.seq  <-  seq(T.first,T.last,((T.last - T.first)/20))
     if (T.last > 900) T.seq <- round(T.seq,0)
     else              T.seq <- round(T.seq,1)
    
     axis(1,T.seq)
#
     axis(2,Y.seq)

     if (Plot.Pupil & is.null(Speed) ) {
        R.seq <- seq(from=d.r[1],to=d.r[2],by=(d.r[2]-d.r[1])/10)
        R.seq <- round(R.seq,1)
        axis(4,Y.seq,labels=R.seq)
     }
     abline(h=Y.seq,v=T.seq,col="lightgray",lty=3)
}
#
Plot.TSEQ.bin <- function(d,d.px=c(0,0),d.r=c(0,0),d.time=c(0,0),d.angle=c(0,0),d.speed=c(0,0),
                      Y.Scale="A",Fix=NULL,Speed=NULL,Plot.X=TRUE,Plot.Y=TRUE,Plot.Pupil=FALSE,
                      Col.X=c("red","red2"),Col.Y=c("blue","blue3"),Col.Pupil=c("black","black"),Col.XF="lightsalmon",Col.YF="lightskyblue",Col.Speed="green2",Header=" ") 											 
{
     Xl  <- d$Xl; Xr <- d$Xr
     Yl  <- d$Yl; Yr <- d$Yr
     Rl  <- d$Rl; Rr <- d$Rr 
     T  <- d$T
     NV <- d$NV
     if (d.time[2] > 0) {                               
          s.t<-which((T >= d.time[1])&(T <= d.time[2]))
          Xl<-Xl[s.t]; Yl<-Yl[s.t]; T<-T[s.t]; Rl<-Rl[s.t]; NV <- NV[s.t]
		  Xr<-Xr[s.t]; Yr<-Yr[s.t]; Rr<-Rr[s.t]
      }     
     d.len <- length(Xl)   
     if (d.len < 3) {
           stop("Plot.TSEQ: Data selection error");
     }
     #Start time , End time
     T.first<-T[1];  T.last <-T[d.len]                                  
     #
     #Downsampling
     #
     scale <- d.len / 2000
     if (scale >= 2) {
        ns <- as.integer(scale)
        s.ds <- seq(1,d.len,by=ns)
        Xl  <- Xl[s.ds]; Xr  <- Xr[s.ds]
        Yl  <- Yl[s.ds]; Yr  <- Yr[s.ds]
        Rl  <- Rl[s.ds]; Rr  <- Rr[s.ds]
        T  <- T[s.ds]
        NV <- NV[s.ds]
     }
     #
     # Pupil dillation limits
     #
     if (d.r[2] == 0) {
	     R.all <-c(Rl,Rr)
         s <- which(R.all>0)
         d.r[1] = min(R.all[s])*0.9; d.r[2] = max(R.all[s])*1.1
     }
     #
     # Coordinate limits
     #
     s <- which (NV == 0)
	 if (length(s) < 3) {
	    stop("Plot.TSEQ: No valid data for plotting");
 	}		 
     Xl <- Xl[s]; Yl <- Yl[s]; Rl <- Rl[s]; T <- T[s]
	 Xr <- Xr[s]; Yr <- Yr[s]; Rr <- Rr[s]
     #
     if (Y.Scale == "A") {
          Xl  <- X.ang((Xl - (d$W/2)),scr.res=d$Res,eye.dist=d$Head.Dist); Xr  <- X.ang((Xr - (d$W/2)),scr.res=d$Res,eye.dist=d$Head.Dist)  
          Yl  <- X.ang(((d$H/2) - Yl),scr.res=d$Res,eye.dist=d$Head.Dist); Yr  <- X.ang(((d$H/2) - Yr),scr.res=d$Res,eye.dist=d$Head.Dist)
     }


     Xl.max <-  max(Xl); Xl.min <-min(Xl); Yl.max <- max(Yl); Yl.min <- min(Yl)
     Xr.max <-  max(Xr); Xr.min <-min(Xr); Yr.max <- max(Yr); Yr.min <- min(Yr)	 
	 
     Lim.min <- min(Xl.min,Yl.min,Xr.min,Yr.min)
     Lim.max <- max(Xl.max,Yl.max,Xr.max,Yr.max)
	 d.Lim   <- abs(Lim.max-Lim.min)*0.03
	 Lim.min <- Lim.min - d.Lim
	 Lim.max <- Lim.max + d.Lim	 
     #
	 #print(paste(Lim.min,Lim.max))
     # Pupil dillation resampling
     #
     if (Y.Scale=="A") {
          if ((d.angle[1]==0)&(d.angle[2] == 0)) { 
             d.angle[1]<-Lim.min; d.angle[2]<-Lim.max
           }
          delta.angle <- d.angle[2] - d.angle[1]
          delta.r     <- d.r[2] - d.r[1]
          f.dr        <- delta.angle / delta.r
          Rl  <- ((Rl - d.r[1])*f.dr)+d.angle[1]
          Rr  <- ((Rr - d.r[1])*f.dr)+d.angle[1]		  
          Y.lim <- d.angle
     } else {
          if ((d.px[1]==0)&(d.px[2]==0)) {
             d.px[1]<- Lim.min; d.px[2]<-Lim.max
          }
          delta.px    <- d.px[2] - d.px[1]
          delta.r     <- d.r[2] - d.r[1]
          f.dr        <- delta.px / delta.r
          Rl  <- ((Rl - d.r[1])*f.dr)+d.px[1]		  
          Rr  <- ((Rr - d.r[1])*f.dr)+d.px[1]
          Y.lim <- d.px
     }
     #
     #===============================================   
     Lab.X<-"Time (ms)"                               #подпись по оси X
     if (Y.Scale=="A") {
        Lab.Y<-"X - Y position (angle)"               #подпись по оси Y
     } else {
        Lab.Y<-"X - Y position (px)"                  #подпись по оси Y
     }
#==========================================================================
#
     if (Header == " ") {
      plot (T,Xl,col=Col.X[1],ylim=Y.lim,type="n",axes = FALSE,xlab=Lab.X,ylab=Lab.Y)	 

	 } else {
	  plot (T,Xl,col=Col.X[1],ylim=Y.lim,type="n",axes = FALSE,xlab=Lab.X,ylab=Lab.Y,main=Header,cex.main=.9) 
     }

     Y.seq <- seq(from=Y.lim[1],to=Y.lim[2],by=(Y.lim[2]-Y.lim[1])/10)
     if (Y.lim[2] < 100) Y.seq <- round(Y.seq,1) else Y.seq <- round(Y.seq,0)

 #    if ( !is.null(Fix)) {
 #       FX <- Fix$X
 #       FY <- Fix$Y
 #       FS <- Fix$Start
 #       FE <- Fix$Start+Fix$Dur
 #       s  <- which((FS >= T.first)&(FS <= T.last)&(FE <= T.last))
 #       FX <- FX[s]; FY <- FY[s]; FS<-FS[s]; FE <-FE[s]
 #       if (Y.Scale == "A") {
 #           FX <- X.ang(FX - d$W/2)
 #           FY <- X.ang(d$H/2 - FY)      
 #       }
 #       nfix<- length(FS)
 #       for (i in 1: nfix) {
 #           lines(c(FS[i],FE[i]),c(FX[i],FX[i]),col=Col.XF,lwd=6)
 #           lines(c(FS[i],FE[i]),c(FY[i],FY[i]),col=Col.YF,lwd=6)
 #       }
 #     }	

 #    if ( !is.null(Speed)) {
 #       if ((d.speed[1]==0)&(d.speed[2]==0)) {
 #          Speed.max <- max(Speed)*1.1;
 #          Speed.min <- 0
 #          d.speed[1] <- Speed.min
 #          d.speed[2] <- Speed.max
 #       }
 #       f.speed <- (Y.lim[2]-Y.lim[1]) / (d.speed[2] - d.speed[1])
 #       Speed   <- ((Speed - d.speed[1])*f.speed)+Y.lim[1]
 #       if (exists("s.t"))  Speed <- Speed[s.t]
 #       if (exists("s.ds")) Speed <- Speed[s.ds]
 #       points(T,Speed,col=Col.Speed,pch=20,cex=.5)
 #       Speed.seq <- seq(from=d.speed[1],to=d.speed[2],by=(d.speed[2]-d.speed[1])/10)
 #       Speed.seq <- round(Speed.seq,0)
 #       axis(4,Y.seq,labels=Speed.seq)
 #     }

     if (Plot.X)      {
	    points(T,Xl,col=Col.X[1],pch=20,cex=.5)           
		points(T,Xr,col=Col.X[2],pch=20,cex=.5)
	 }	
     if (Plot.Y)      {
	    points(T,Yl,col=Col.Y[1],pch=20,cex=.5)  
	    points(T,Yr,col=Col.Y[2],pch=20,cex=.5) 
     }    
     if (Plot.Pupil)  {
	    points(T,Rl,col=Col.Pupil[1],pch=20,cex=.5)  
	    points(T,Rr,col=Col.Pupil[2],pch=20,cex=.5) 
     }
     T.seq  <-  seq(T.first,T.last,((T.last - T.first)/20))
     if (T.last > 900) T.seq <- round(T.seq,0)
     else              T.seq <- round(T.seq,1)
#    
     axis(1,T.seq)
     axis(2,Y.seq)
     if (Plot.Pupil & is.null(Speed) ) {
        R.seq <- seq(from=d.r[1],to=d.r[2],by=(d.r[2]-d.r[1])/10)
        R.seq <- round(R.seq,1)
        axis(4,Y.seq,labels=R.seq)
     }
     abline(h=Y.seq,v=T.seq,col="lightgray",lty=3)
}
#