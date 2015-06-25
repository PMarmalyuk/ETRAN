#
# Основная функция: визуализация на заданном фоне
# Вход: D     - selected data set
#       bgd   - background image
#
Plot.XY<-function(d,bgd,Fix=NULL,Plot.RAW=TRUE,maxdatasize=2000,Col.Points="red",Col.Fix="red3",Col.Fixl="red4",Plot.Circle=FALSE,Plot.Chull=TRUE,Plot.Time=FALSE,FixL.w=2,CnvL.w=1)
{
     h  <- dim(bgd)[1]                                    #background height 
     w  <- dim(bgd)[2]                                    #background width
     dw <- (d$W - w)/2                                    #X displacement
     dh <- (d$H - h)/2                                    #Y displacement
     d.len<-length(d$X)                                   #raw data length 
	 if (!(is.null(Fix)))    f.len<-length(Fix$X)         #number of fixations
#-----------------------------------------------------------------------------------   	 
	 if ((Plot.RAW==TRUE)&&(d.len <= 2)) stop("No enough RAW data to plot")
	 if ((Plot.RAW==FALSE)&&(is.null(Fix))) stop("No fixations data to plot") 
	 if ((Plot.RAW==FALSE)&&(f.len < 1)) stop("No fixations data to plot")
#----- Plot with background --------------------------------------------------------  
 	 par(mar=c(0, 0, 0, 0))                               #minimal margins
     plot(c(1,w),c(1,h),type="n",axes=FALSE,asp=1)      
     rasterImage(bgd,1,1,w,h)                             #background image
	 if (Plot.RAW == TRUE) {
         X    <- d$X                                      #RAW data - X
         Y    <- d$H - d$Y                                #RAW data - Y
         X    <- X-dw                                     #x correction (bgd)
         Y    <- Y-dh                                     #y correction (bgd)
         # data downsampling 
         d.len <- length(X)
         scale <- d.len / maxdatasize
         if (scale >= 2) {
             ns <- as.integer(scale)
             s <- seq(1,d.len,by=ns)
             X <- X[s]
             Y <- Y[s]
         }		 
         points(X,Y,col=Col.Points,cex=.5,pch=20)  #на фоне рисуем движения глаз		 
	 } 
	 if (!is.null(Fix)) {
         fX    <- Fix$X                                     #fixations - X coordinates
         fY    <- d$H - Fix$Y                               #fixations - Y coordinates
		 fD    <- Fix$Disp                                  #fixation dispersion
         fX    <- fX-dw                                     #x correction (bgd)
         fY    <- fY-dh                                     #Y correction (bgd)	
		 if (Plot.Circle) {
		  for (i in 1:f.len) {
		    draw.circle(fX[i],fY[i],fD[i],border=Col.Fix)
		  }
		 }
         if (Plot.RAW && Plot.Chull ) {
		  for (i in 1:f.len) {
		    s <- Fix$s_Start[i]:(Fix$s_Start[i]+Fix$s_Dur[i]-1)
			hdf <- data.frame(X=X[s],Y=Y[s])
			vtx <-chull(hdf)
			vtx<-c(vtx,vtx[1])
			lines(hdf[vtx,],col=Col.Fix,lwd=CnvL.w)
          }		  
		 }
		 if (Plot.Time) {
		  for (i in 1:f.len) {
		      tdur<-round(Fix$Dur[i],0)
			  text(fX[i],fY[i],labels=tdur)
           }
         }		   
         lines (fX,fY,col=Col.Fixl,lwd=FixL.w) 	
     }		 
} 
#
#