amplitude <- function(x0, y0, x1, y1)
{
  amplitudeX <- abs(x1-x0)
  amplitudeY <- abs(y1-y0)
  amplitude <- sqrt(amplitudeX^2 + amplitudeY^2)
  return(list(amplitudeX = amplitudeX, 
              amplitudeY = amplitudeY, 
              amplitude = amplitude))
}

kurtosis <- function (x, na.rm = FALSE) 
{
  if (any(ina <- is.na(x))) {
    if (na.rm) 
      x <- x[!ina]
    else return(NA)
  }

  n <- length(x)
  x <- x - mean(x)
  r <- n * sum(x^4)/(sum(x^2)^2)
  y <- r * (1 - 1/n)^2 - 3
  y
}

skewness <- function (x, na.rm = FALSE) 
{
  if (any(ina <- is.na(x))) {
    if (na.rm) 
      x <- x[!ina]
    else return(NA)
  }
  n <- length(x)
  x <- x - mean(x)
  y <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
  y <- y * ((1 - 1/n))^(3/2)
  y
}

pointsArea <- function(x, y) {
  df <- data.frame(x = x, y = y)
  df <- df[complete.cases(df),]
  x <- df$x; y <- df$y
  geron <- function(x,y) {
    if ((length(x)!=length(y))|(length(x)!=3))
    {
      warning("Wrong number of vertices.")
      return(NA)
    }
    a <- sqrt((x[2]-x[1])^2+(y[2]-y[1])^2)
    b <- sqrt((x[3]-x[2])^2+(y[3]-y[2])^2)
    c <- sqrt((x[1]-x[3])^2+(y[1]-y[3])^2)
    p <- (a+b+c)/2
    return(sqrt(p*(p-a)*(p-b)*(p-c)))
  }
  area <- 0
  shape <- chull(x,y)
  x <- x[shape]; y <- y[shape]  
  len <- length(x)
  if (len<3) area <- NA 
  else 
  {
    for (i in 2:(len-1)) 
      area <- area + geron(x[c(1,i,i+1)],y[c(1,i,i+1)])
  }
  return(area)
}

pathLength <- function(x, y)
{
  if (length(x) < 2)
  {
    pathLength <- NA
  }
  else
  {
    pathLength <- sum(sqrt((x[-1]-x[-length(x)])^2 + 
                             (y[-1]-y[-length(y)])^2), na.rm = T)
  }
  return(pathLength)
}

orientationAngleXAxis <- function(x0, y0, x1, y1)
{
  dx <- x1 - x0
  dy <- y0 - y1
  orientationXaxis <- atan2(y = dy, x = dx) * (180/pi)
  return(orientationXaxis)
}

orientationsPointsXY <- function(x, y)
{
  points <- data.frame(x = x, y = y)
  pointsStart <- points[-nrow(points),]
  pointsEnd <- points[-1,]
  pointsStartEnd <- data.frame(x0 = pointsStart$x, 
                               y0 = pointsStart$y, 
                               x1 = pointsEnd$x, 
                               y1 = pointsEnd$y)
  sapply(1:nrow(pointsStartEnd), FUN = function(i) {
    orientationAngleXAxis(x0 = pointsStartEnd[i,]$x0,
                          y0 = pointsStartEnd[i,]$y0,
                          x1 = pointsStartEnd[i,]$x1,
                          y1 = pointsStartEnd[i,]$y1)
  })
}

meanAngle <- function(angles, na.rm = T)
{
  if (na.rm)
  {
    angles <- angles[!is.na(angles)]
  }
  sumsin <- sum(sin(angles*(pi/180)))
  sumcos <- sum(cos(angles*(pi/180)))
  meanAngle <- atan2(sumsin, sumcos)
  return(meanAngle*(180/pi))
}

varianceAngle <- function(angles, na.rm = T)
{
  if (na.rm)
  {
    angles <- angles[!is.na(angles)]
  }
  meanAng <- meanAngle(angles, na.rm = na.rm)
  varianceAngle = var(sin(angles*(pi/180))) + 
    var(cos(angles*(pi/180)))
  return(varianceAngle*(180/pi))
}

shannon.entropy <- function(p)
{
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <- p[p>0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}

#Вычисление SR-матрицы
getSR <- function(size, classified, alpha=0.233, gamma=0.255, history=F)
{
  #Вектор полученных AOIs
  if (is.data.frame(classified))
    cv = classified$state #Classified_Vector
  else if (is.vector(classified))
    cv=classified;
  #Длина вектора перемещений
  len=length(cv);
  #Генерируем единичную матрицу I нужного размера
  I = diag(1,size);
  #Инициализируем новую SR-матрицу
  SR = matrix(0,size,size);
  #Переменная, содержащая историю изменений SR-матрицы
  SRHistory = matrix(0,len,size^2);
  #Заполняем матрицу данными и записываем в историю предыдущее значение матрицы
  for (i in 1:(len-1))
  {
    SRHistory[i,] = as.vector(SR);
    SR[,cv[i]] = SR[,cv[i]] +alpha*(I[,cv[i+1]] + gamma*SR[,cv[i+1]]                       
                                    - SR[,cv[i]]);
  }
  SRHistory[len,]=as.vector(SR);
  if (history) 
    #Выдаем полученную историю изменений матрицы
    return(SRHistory)
  else
    return(as.vector(SR)/sum(SR))
}