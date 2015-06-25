#
#
#
# Вспомогательная функция: 
# преобразование отклонения от центра экрана в пикселях в угловое отклонение в градусах
# расстояние до экрана 58 см; разрешение экрана 38 пикселей на сантиметр
#
X.ang <- function(x,scr.res=38,eye.dist=58,X.max=640)
{
     x[which(x > X.max)]  <-  X.max                        #коррекция верхнего допустимого значения
     x[which(x < -X.max)] <- -X.max                        #коррекция нижнего допустимого значения
     x.sm <- x / scr.res                                   #перевод из пикселей в сантиметры
     x.ang<-(atan(x.sm / eye.dist) / (pi * 2)) * 360       #вычисление углового значения
     x.ang
}
#
#==============================;
# Heatmap build                ;
# Input:                       ;
# D    - selected data set     ;
# w,h  - heatmap dimensions    ;
# bw   - bandwidth             ;
#==============================;
Bld.KS<-function(D,w=D$W,h=D$H,bw=30)
{
    dw <- (D$W - w)/2                                     #сдвиг по горизонтали
    dh <- (D$H - h)/2                                     #сдвиг по вертикали
    X    <- D$X                                           #выбираем Х - координаты
    Y    <- D$Y                                           #выбираем Y - координаты
    Y    <- D$H - Y
    # size correction
    X    <- X - dw
    Y    <- Y - dh
    #формируем набор данных для ядерного сглаживания
    df   <- cbind(X,Y)
    est  <- bkde2D(df, bandwidth=c(bw,bw),gridsize=c(51L,51L),range.x=list(c(1,w),c(1,h)))
    iX   <-est$x1; iY<-est$x2; iZ<-est$fhat[1:50,1:50]
    list(X=iX,Y=iY,Z=iZ)
}
#
Plot.KSbg <- function(D,bgd,mycols=NULL)
{
    h  <- dim(bgd)[1]                                     #bgd height
    w  <- dim(bgd)[2]                                     #bgd width
	if ( is.null(mycols)) {
       mycols <- adjustcolor(topo.colors(40),alpha.f = .4)   #color set for heatmap
       mycols[1] <- "#4C00FF23"                              #transparent first color	
	}   
    op<-par(mar=c(0,0,0,0))
    plot(c(1,w),c(1,h),type="n",axes=FALSE,xlab="",ylab="",xlim=c(1,w),ylim=c(1,h),asp=1)   
	#show background
    rasterImage(bgd,1,1,w,h) 
    #show heatmap
    image(D$X,D$Y,D$Z,col=mycols,add=TRUE)
    par(op)
}
#
# Основная функция: визуализация на заданном фоне
# Вход: D     - selected data set
#       bgd   - background image
#
Plot.XYbg<-function(d,bgd)
{
     h  <- dim(bgd)[1]                                    #background height 
     w  <- dim(bgd)[2]                                    #background width
     dw <- (d$W - w)/2                                    #X displacement
     dh <- (d$H - h)/2                                    #Y displacement
     d.len<-length(d$X)                                   #длина выбранных данных 
     if (d.len > 2) {                                     #если длинна выбранных данных не менее 2 самплов - выполняем визуализацию 
        X    <- d$X                                       #сырые данные - X-координаты
        Y    <- d$H - d$Y                                 #сырые данные - Y-координаты
        X    <- X-dw                                      #коррекция по горизонтали (фон)
        Y    <- Y-dh                                      #коррекция по вертикали (фон)
# data sample subsampling 
      d.len <- length(X)
      scale <- d.len / 2000
      if (scale >= 2) {
        ns <- as.integer(scale)
        s <- seq(1,d.len,by=ns)
        X <- X[s]
        Y <- Y[s]
      }
      par(mar=c(0, 0, 0, 0))                             #минимальные отступы слева, снизу, 0 - сверху, справа
#
#    начальный пустой plot
#
        plot(c(1,w),c(1,h),type="n",axes=FALSE,asp=1)      
        rasterImage(bgd,1,1,w,h)                           #фоновое изображение
        points(X,Y,col="red",cex=.5,pch=20)                #на фоне рисуем движения глаз
     #===================================================  #визуализация выполнена
     } else {                                              #если недостаточно данных для визуализации - выводим plot с сообщением об ошибке 
           plot(0:1,0:1,type="n",axes=FALSE,xlab="",ylab="")
           text(0.5,0.5,"PlotXYbg: data selection error")
     }
} 
#
Plot.Fbg<-function(F,W,H,bgd)
{
     h  <- dim(bgd)[1]                                    #background height 
     w  <- dim(bgd)[2]                                    #background width
     dw <- (W - w)/2                                      #X displacement
     dh <- (H - h)/2                                      #Y displacement
     d.len<-length(F$X)                                   #number of fixations
     if (d.len > 1) {                                     
        X    <- F$X                                       #fixations - X coordinates
        Y    <- H - F$Y                                   #fixations - Y coordinates
        X    <- X-dw                                      #x correction (bgd)
        Y    <- Y-dh                                      #Y correction (bgd)
        par(mar=c(0, 0, 0, 0))                            #минимальные отступы слева, снизу, 0 - сверху, справа
#
#    начальный пустой plot
#
        plot(c(1,w),c(1,h),type="n",axes=FALSE,asp=1)      
        rasterImage(bgd,1,1,w,h)                           #фоновое изображение
        points(X,Y,col="green",cex=1.5,pch=20)             #на фоне рисуем движения глаз
        lines (X,Y,col="green",lwd=1) 	
     #===================================================  #визуализация выполнена
     } else {                                              #если недостаточно данных для визуализации - выводим plot с сообщением об ошибке 
           plot(0:1,0:1,type="n",axes=FALSE,xlab="",ylab="")
           text(0.5,0.5,"Plot.Fbg: data selection error")
     }	 
}
#

#
#