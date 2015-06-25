#
# 
# select data for specified experimental situation;
# optional - for specified time interval in experimental situation
# Input D - name of full data set (прочитанный функцией readSMI из пакета eyetracker)
#       fn - exposition numbers
#       d.time - time interval
#       $Mode -  "Left"  get left eye data; 
#                "Right" -  get right eye data; 
#                "Binocular" - get both eyes data;
#                "Average"  - get averaged data; 
#                "Monocular" - get one eye (first in data set) data
#       $Pupil - "RAW" - get RAW pupil data (DX+DY)/2
#                "Mapped" - get mapped pupil data 
#       $EPOS  - 1 - read EPOS & GVEC data from RED family eyetrackers	- only binocular mode
#       
# Output (Monocular and Average mode)
#       $X - Х-coordinate
#       $Y - Y-coordinate
#       $R - Pupil size
#       $T - Time
#       $Name - original background name
#
Sel.SMI <-function(dfname,fn,d.time=c(0,0),Mode="Monocular",Pupil="RAW",EPOS=0)
{
   if ((Mode != "Monocular")&(Mode != "Average")&(Mode != "Binocular")&
       (Mode != "Left")&(Mode != "Right")) {
     print(paste("Sel.SMI: Wrong $Mode argument:",Mode))
	 return
   }
   if ((Pupil != "RAW")&(Pupil != "Mapped")) {
     print(paste("Sel.SMI: Wrong $Pupil argument:",Pupil))
	 return
   }   
   #
   if (EPOS == 1) {
      Mode <- "Binocular"
   }  
   # Select specified experimental situation
   sel     <- which(match(get(dfname)$Fn,fn,nomatch=0)>0)
   # Get data names
   nn      <- names(get(dfname)) 
   #===============================================================================================================================================   
   Name    <- get(dfname)$MsgTxt[fn[1]]                         #берем имя фонового изображения из первого сампла данных, относящегося к данной ЭС
   Subj    <- get(dfname)$Subj                                  #Subject ID
   Time    <- get(dfname)$Time[sel]                             #выбираем время
   T0      <- Time[1]                                           #начальное время
   Time    <- Time - T0                                         #устанавливаем начальное время выборки в 0   
   # Set number of eyes to read   
   neyes <- 1
   if ((Mode == "Binocular")|(Mode == "Average")) neyes <-2
   # Select data in Pupil == RAW mode
   if (Pupil == "RAW") {
      if ((Mode == "Binocular")|(Mode == "Average")|(Mode == "Monocular")) FNAMES <- c("DX","DY","PorX","PorY")
      if (Mode == "Left")  FNAMES <- c("L_DX","L_DY","L_PorX","L_PorY")
      if (Mode == "Right") FNAMES <- c("R_DX","R_DY","R_PorX","R_PorY")
      posdx    <-grep(FNAMES[1],nn,fixed=TRUE)
      if (length(posdx) < neyes) { print("Sel.SMI ERROR: no valid DX data\n"); return(NULL) }	
      posdy    <-grep(FNAMES[2],nn,fixed=TRUE)
      if (length(posdx) < neyes) { print("Sel.SMI ERROR: no valid DY data\n"); return(NULL) }	  
	  posx     <-grep(FNAMES[3],nn,fixed=TRUE)
      if (length(posx) < neyes)  { print("Sel.SMI ERROR: no valid X data\n");  return(NULL) }
      posy     <-grep(FNAMES[4],nn,fixed=TRUE)
      if (length(posy) < neyes)  { print("Sel.SMI ERROR: no valid Y data\n");  return(NULL) }
      # Get data	  
      if (neyes == 1) {
         X   <- get(dfname)[[posx[1]]][sel]                       # выбираем Х - координаты
         Y   <- get(dfname)[[posy[1]]][sel]                       # выбираем Y - координаты
         Dx  <- get(dfname)[[posdx[1]]][sel]                      # выбираем раскрытие зрачка по X
         Dy  <- get(dfname)[[posdy[1]]][sel]                      # выбираем раскрытие зрачка по Y
         R   <- (Dx+Dy)/2                                         #среднее раскрытие зрачка		 
	  }
      if (neyes == 2) {	  
         Xl  <- get(dfname)[[posx[1]]][sel]                       # выбираем Х - координаты
         Yl  <- get(dfname)[[posy[1]]][sel]                       # выбираем Y - координаты
         Dxl <- get(dfname)[[posdx[1]]][sel]                      # выбираем раскрытие зрачка по X
         Dyl <- get(dfname)[[posdy[1]]][sel]                      # выбираем раскрытие зрачка по Y
         Xr  <- get(dfname)[[posx[2]]][sel]                       # выбираем Х - координаты
         Yr  <- get(dfname)[[posy[2]]][sel]                       # выбираем Y - координаты
         Dxr <- get(dfname)[[posdx[2]]][sel]                      # выбираем раскрытие зрачка по X
         Dyr <- get(dfname)[[posdy[2]]][sel]                      # выбираем раскрытие зрачка по Y	
         Rl  <- (Dxl+Dyl)/2                                       #среднее раскрытие зрачка
         Rr  <- (Dxr+Dyr)/2                                       #среднее раскрытие зрачка
       }
   }
   # Select data in Pupil == Mapped mode	 
   if (Pupil == "Mapped")  {
      if ((Mode == "Binocular")|(Mode == "Average")|(Mode == "Monocular")) FNAMES <- c("_DM","PorX","PorY")
      if (Mode == "Left")  FNAMES <- c("L_DM","L_PorX","L_PorY")
      if (Mode == "Right") FNAMES <- c("R_DM","R_PorX","R_PorY")
      posd    <-grep(FNAMES[1],nn,fixed=TRUE)
      if (length(posd) < neyes) { print("Sel.SMI ERROR: no valid D data\n"); return(NULL) }	
	  posx    <-grep(FNAMES[2],nn,fixed=TRUE)
      if (length(posx) < neyes) { print("Sel.SMI ERROR: no valid X data\n"); return(NULL) }
      posy    <-grep(FNAMES[3],nn,fixed=TRUE)
      if (length(posy) < neyes) { print("Sel.SMI ERROR: no valid Y data\n"); return(NULL) }
 
      if (neyes == 1) {
         X   <- get(dfname)[[posx[1]]][sel]                       # выбираем Х - координаты
         Y   <- get(dfname)[[posy[1]]][sel]                       # выбираем Y - координаты
         R   <- get(dfname)[[posd[1]]][sel]                       # выбираем раскрытие зрачка 
 	  }
      if (neyes == 2) {	  
         Xl  <- get(dfname)[[posx[1]]][sel]                       # выбираем Х - координаты
         Yl  <- get(dfname)[[posy[1]]][sel]                       # выбираем Y - координаты
         Rl  <- get(dfname)[[posd[1]]][sel]                       # выбираем раскрытие зрачка 
         Xr  <- get(dfname)[[posx[2]]][sel]                       # выбираем Х - координаты
         Yr  <- get(dfname)[[posy[2]]][sel]                       # выбираем Y - координаты
         Rr  <- get(dfname)[[posd[2]]][sel]                       # выбираем раскрытие зрачка 
       }
   }
   # Select EPOS & GVEC data
   if (EPOS == 1) {
     pos1 <- grep("EPOS",nn,fixed=TRUE)
     if (length(pos1) < 6) { print("Sel.SMI ERROR: no valid EPOS data\n"); return(NULL) }
	 pos2 <- grep("GVEC",nn,fixed=TRUE)
     if (length(pos2) < 6) { print("Sel.SMI ERROR: no valid GVEC data\n"); return(NULL) }	
     LPX <- get(dfname)$L_EPOSX[sel]
     LPY <- get(dfname)$L_EPOSY[sel]
     LPZ <- get(dfname)$L_EPOSZ[sel]
     RPX <- get(dfname)$R_EPOSX[sel]
     RPY <- get(dfname)$R_EPOSY[sel]
     RPZ <- get(dfname)$R_EPOSZ[sel]
     LVX <- get(dfname)$L_GVECX[sel]
     LVY <- get(dfname)$L_GVECY[sel]
     LVZ <- get(dfname)$L_GVECZ[sel]
     RVX <- get(dfname)$R_GVECX[sel]
     RVY <- get(dfname)$R_GVECY[sel]
     RVZ <- get(dfname)$R_GVECZ[sel]	 
   }	 
   if (Mode == "Average") {
   	   X <- (Xl + Xr)/2
	   Y <- (Yl + Yr)/2
	   R <- (Rl + Rr)/2
   }
   # Select by time start and end 
   if (d.time[2] > 0) {   
      sel<-which((Time >= d.time[1])&(Time <= d.time[2]))   
      Time<-Time[sel]                                     #select Time	  
      if (Mode != "Binocular") {
            X<-X[sel]                                     #select X
            Y<-Y[sel]                                     #select Y
            R<-R[sel]                                     #select Pupil

      } else {
            Xl   <-Xl[sel]                                       #выбираем X-координату
            Yl   <-Yl[sel]                                       #выбираем Y-координату
            Rl   <-Rl[sel]                                       #выбираем раскрытие зрачка
            Xr   <-Xr[sel]                                       #выбираем X-координату
            Yr   <-Yr[sel]                                       #выбираем Y-координату
            Rr   <-Rr[sel]                                       #выбираем раскрытие зрачка		
		if (EPOS == 1) {
		    LPX <- LPX[sel]; LPY <- LPY[sel]; LPZ <- LPZ[sel];
			RPX <- RPX[sel]; RPY <- RPY[sel]; RPZ <- RPZ[sel];
			LVX <- LVX[sel]; LVY <- LVY[sel]; LVZ <- LVZ[sel];
			RVX <- RVX[sel]; RVY <- RVY[sel]; RVZ <- RVZ[sel];
        } 		
	  }
	}
   # Get data length
   dlen  <- length(Time) 
   # Set valid data mark   
   NV    <- vector(mode="numeric",length=dlen)                # valid data mark  	
   Area  <- get(dfname)$Area  
   W     <- Area[1]; H    <- Area[2]                             #screen dimensions	
   W.mm  <- Area[3]; H.mm <- Area[4]   
   res.X  <- W / W.mm; res.Y <- H/H.mm; res <- (res.X + res.Y)/2
   Head.Dist <- get(dfname)$HeadDistance
   if (Mode != "Binocular") {
       nv.idx     <- which((R == 0)|(X < 0)|(X > W)|(Y < 0)|(Y > H))
       NV[nv.idx] <- 1;  	  
   } else {
       nv.idx    <- which((Rl == 0)|(Xl < 0)|(Xl > W)|(Yl < 0)|(Yl > H)|
                          (Rr == 0)|(Xr < 0)|(Xr > W)|(Yr < 0)|(Yr > H))
       NV[nv.idx]<-1;  	
   }
   if (Mode != "Binocular") {
	 L<-list(X=X,Y=Y,R=R,T=Time,Name=Name,Subj=Subj,W=W,H=H,fn=fn,NV=NV,Mode=Mode,Res=res,Head.Dist=Head.Dist)       #формируем выходной список
   } else {
    if (EPOS == 1) {
	   L <- list(Xl=Xl,Xr=Xr,Yl=Yl,Yr=Yr,Rl=Rl,Rr=Rr,
	             L_EPOSX=LPX,L_EPOSY=LPY,L_EPOSZ=LPZ,R_EPOSX=RPX,R_EPOSY=RPY,R_EPOSZ=RPZ,
			     L_GVECX=LVX,L_GVECY=LVY,L_GVECZ=LVZ,R_GVECX=RVX,R_GVECY=RVY,R_GVECZ=RVZ,
			     T=Time,Name=Name,Subj=Subj,W=W,H=H,fn=fn,NV=NV,Mode=Mode,Res=res,Head.Dist=Head.Dist)	
    } else {
  	   L <-list(Xl=Xl,Xr=Xr,Yl=Yl,Yr=Yr,Rl=Rl,Rr=Rr,T=Time,Name=Name,Subj=Subj,W=W,H=H,fn=fn,NV=NV,Mode=Mode,Res=res,Head.Dist=Head.Dist)       #формируем выходной список
    }
    return(L)
   } 	
}
#
#
