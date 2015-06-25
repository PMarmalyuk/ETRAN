FixDetect.IDT<-function(X,Y,R,Disp=50,Dur=6,R.min=1,Debug=0,method="LC.orig",SampleRate=50,Max.n_nv=3,dR.max=0.05)
{
  if ((method != "Rd2") && (method != "Rd") && (method != "SG") && (method != "SGm") && (method != "LC.r") && (method != "LC.orig")) stop("Implemented methods: Rd, SG, SGm, LC.r, LC.orig")
  if (! is.numeric(X)) stop("X argument not numeric")
  if (! is.numeric(Y)) stop("Y argument not numeric")
  if (! is.numeric(R)) stop("R argument not numeric")
  if ((length(X) != length(Y))||(length(X) != length(R))) stop("X,Y,R arguments must have the same length");
  
  if ((method == "Rd")||(method == "SG")||(method == "SGm")||(method == "LC.r")) {
      lres = Fixd.IDT.R(X=X,Y=Y,R=R,Th.Disp=Disp,Th.Dur=Dur,R.min=R.min,Max.n_nv=Max.n_nv,dR.max=dR.max,method=method,Debug=Debug)
  } 
  if  (method == "LC.orig") {
      lres = .Call("FixDetectC",X=X,Y=Y,R=R,Dispersion=Disp,MinDur=Dur,Debug=Debug,package="Eyetracking");
  }
  if (method == "Rd2") {
      lres = Fixd.IDT.R2(X=X,Y=Y,R=R,Th.Disp=Disp,Th.Dur=Dur,R.min=R.min,Max.n_nv=Max.n_nv,dR.max=dR.max,Debug=Debug)  
  }
  
  if (SampleRate < 1)  SampleRate <- 1
  SR    <- 1000/SampleRate
  Dur   <- lres$Dur*SR
  Start <- lres$Start*SR  
  res<-data.frame(X=lres$X,Y=lres$Y,R=lres$R,Disp=lres$Disp,Start=Start,Dur=Dur,s_Start=lres$Start,s_Dur=lres$Dur)  
  return(res)  
}
#

Fixd.IDT.R <- function(X,Y,R,Th.Disp=50,Th.Dur=6,R.min=1,method="LC.r",Max.n_nv=3,dR.max=0.05,Debug=0)
{
    len     <- length(R)
	NV      <- Validate.Data(X,Y,R,R.min,dR.max)
	i.start <- 1; i.end   <- i.start+Th.Dur-1; nf <- 0
	maxnf <- len / Th.Dur
	Fix <- matrix(data=0,nrow=maxnf,ncol=2)
	
	while(i.end <= len) {
		s     <- i.start:i.end
		i_nv  <- which(NV[s])  #not valid samples in vindow
	    n_nv  <- length(i_nv)
	    l.fix <- i.end - i.start + 1		 
		if (n_nv > Max.n_nv) {
		    if ((l.fix-n_nv) < Th.Dur) {
			   i.start <- i.start+i_nv[n_nv]; i.end   <- i.start+Th.Dur-1
			   next
			} else { #terminate fixation
			    while(NV[i.end]) { #remove not valid samples from the end of fixation
			       i.end <- i.end-1
				}   
			    nf <- nf+1
                Fix[nf,] <- c(i.start,i.end)	
                i.start  <- i.end + 1; i.end <- i.start + Th.Dur - 1			   
			}
        }			

        Dsp <- Calc.Wind.Disp(X[s],Y[s],method) 
		if (Debug >  0) print(paste("Dbg: nf i.start i.end Dsp ",nf,i.start,i.end,Dsp))		
	    if (Dsp <= Th.Disp) {
		    i.end <- i.end+1; next
		} else {
	        i.end <- i.end-1;  l.fix <- i.end - i.start + 1
		    if ( l.fix >= Th.Dur) {
                 nf <- nf+1
                 Fix[nf,] <- c(i.start,i.end)
		         i.start <- i.end + 1; i.end <- i.start + Th.Dur - 1
				 next
		    } else {
                 #frame shift
		         i.start <- i.start+1;	i.end <- i.start+Th.Dur-1
                 next				 
		    }
        }
	}
	if (nf == 0) {
	   if ((Dsp <= Th.Disp)&&(i.start == 1)) {
	      Fix[1,] <-c(i.start,len)
	      nf <- 1;
	   }
	}   
    if (Debug >  0) print(paste("Detection finished nf=",nf))	
	if (nf == 0) {
       print("Fixd.IDT.R: No fixations detected")
	   DF <- data.frame(X=-1,Y=-1,R=-1,Disp=-1,Start=0,Dur=0)
       return(DF)	
	}
    if (Fix[nf,1] < i.start) {
        i.end  <- len
	    s      <- i.start:i.end
	    l.fix  <- i.end - i.start 
        Dsp    <- Calc.Wind.Disp(X[s],Y[s],method)
		if (Debug >  0) print(paste("Last data:",i.start,i.end,l.fix,Dsp))	
 	    if ((Dsp <= Th.Disp)&(l.fix >= Th.Dur)) {
            nf <- nf+1
            Fix[nf,] <- c(i.start,i.end)
        }
	}  
    #
    Res <- matrix(data=0,nrow=nf,ncol=6)     
    for (i in 1:nf) {
         s     <- Fix[i,1]:Fix[i,2]
         len.f <- Fix[i,2] - Fix[i,1] 
   	     Res[i,1] <- mean(X[s])
         Res[i,2] <- mean(Y[s])
         Res[i,3] <- mean(R[s])
         Res[i,4] <- Calc.Wind.Disp(X[s],Y[s],method)
         Res[i,5] <- Fix[i,1]
         Res[i,6] <- len.f
    }
    DF  <- data.frame(X=Res[,1],Y=Res[,2],R=Res[,3],Disp=Res[,4],Start=Res[,5],Dur=Res[,6])
    return(DF)	
}




Fixd.IDT.R2 <- function(X,Y,R,Th.Disp=50,Th.Dur=6,R.min=1,method="LC.r",Max.n_nv=3,dR.max=0.05,Debug=0)
{
    len     <- length(R)
	NV      <- Validate.Data(X,Y,R,R.min,dR.max)
	i.start <- 1; i.end   <- i.start+Th.Dur-1; nf <- 0
	maxnf <- len / Th.Dur
	is_frameshift <- FALSE
	Fix <- matrix(data=0,nrow=maxnf,ncol=2)
	
	# loop by (X,Y) data samples
	while(i.end <= len) {
		s     <- i.start:i.end
		i_nv  <- which(NV[s])  #not valid samples in vindow
	    n_nv  <- length(i_nv)
	    l.fix <- i.end - i.start + 1	
        # process non valid data samples		
		if (n_nv > Max.n_nv) {
		    if ((l.fix-n_nv) < Th.Dur) {
			   i.start <- i.start+i_nv[n_nv]; i.end   <- i.start+Th.Dur-1
			   next
			} else { #terminate fixation
			    while(NV[i.end]) { #remove not valid samples from the end of fixation
			       i.end <- i.end-1
				}   
			    nf <- nf+1
                Fix[nf,] <- c(i.start,i.end)	
                i.start  <- i.end + 1; i.end <- i.start + Th.Dur - 1			   
			}
        }			
        # calculate fixation dispersion
        RESD <- Calc.Wind.Disp.rd2(X[s],Y[s]) 
		Dsp <- RESD[1]; Flg <- RESD[2]
		if (Debug >  0) print(paste("Dbg: nf i.start i.end Dsp ",nf,i.start,i.end,Dsp))		
	    if (Dsp <= Th.Disp) {
		    i.end <- i.end+1; is_frameshift <- FALSE; next
		} else {
		    if (Flg && !is_frameshift) {
			  i.start <- i.start + 1; is_frameshift <- TRUE; next
			}
	        i.end <- i.end-1;  l.fix <- i.end - i.start + 1
		    if ( l.fix >= Th.Dur) {
                 nf <- nf+1
                 Fix[nf,] <- c(i.start,i.end)
		         i.start <- i.end + 1; i.end <- i.start + Th.Dur - 1
				 next
		    } else {
                 #frame shift
		         i.start <- i.start+1;	i.end <- i.start+Th.Dur-1
                 next				 
		    }
        }
	}
	
	# check, if full data set (X,Y) is the one (unfinished)  fixation
	if (nf == 0) {
	   if ((Dsp <= Th.Disp)&&(i.start == 1)) {
	      Fix[1,] <-c(i.start,len)
	      nf <- 1;
	   }
	}   
	
    if (Debug >  0) print(paste("Detection finished nf=",nf))	
	# check if no fixations detected	
	if (nf == 0) {
       print("Fixd.IDT.R: No fixations detected")
	   DF <- data.frame(X=-1,Y=-1,R=-1,Disp=-1,Start=0,Dur=0)
       return(DF)	
	}
	
	# check if last (X,Y) data can be merged into fixation
    if (Fix[nf,1] < i.start) {
        i.end  <- len
	    s      <- i.start:i.end
	    l.fix  <- i.end - i.start 
        RESD <- Calc.Wind.Disp.rd2(X[s],Y[s]); Dsp <- RESD[1]
		if (Debug >  0) print(paste("Last data:",i.start,i.end,l.fix,Dsp))	
 	    if ((Dsp <= Th.Disp)&(l.fix >= Th.Dur)) {
            nf <- nf+1
            Fix[nf,] <- c(i.start,i.end)
        }
	}  
    #
    Res <- matrix(data=0,nrow=nf,ncol=6)     
    for (i in 1:nf) {
         s     <- Fix[i,1]:Fix[i,2]
         len.f <- Fix[i,2] - Fix[i,1]
   	     Res[i,1] <- mean(X[s])
         Res[i,2] <- mean(Y[s])
         Res[i,3] <- mean(R[s])
		 RESD <- Calc.Wind.Disp.rd2(X[s],Y[s]); Res[i,4] <- RESD[1]
         Res[i,5] <- Fix[i,1]
         Res[i,6] <- len.f
    }
    DF  <- data.frame(X=Res[,1],Y=Res[,2],R=Res[,3],Disp=Res[,4],Start=Res[,5],Dur=Res[,6])
    return(DF)	
}


Validate.Data <-function(X,Y,R,R.min=1,dR.max=0.05) {
    len     <- length(R)
	NV      <- vector(mode="logical",length=len)
	NV[which(R <= R.min)] <- TRUE
	dR      <- abs(diff(R,lag=1))
    idx     <- which(dR > dR.max)
	idx     <- idx+1
	NV[idx] <- TRUE
	return(NV)
}

Calc.Wind.Disp <- function(X,Y,CWD.method)
{
  res<-0
  if (CWD.method == "Rd")    res<-Calc.Wind.Disp.rd(X,Y)
  if (CWD.method == "SGm") {
                             res<-Calc.Wind.Disp.sg(X,Y); res<-.5*res
  }
  if (CWD.method == "SG")    res<-Calc.Wind.Disp.sg(X,Y)  
  if (CWD.method == "LC.r")  res<-Calc.Wind.Disp.lc(X,Y)  
  if (CWD.method == "WTF")   res<-Calc.Wind.Disp.wtf(X,Y)    
  return(res)
}

Calc.Wind.Disp.sg <- function(X,Y)
{
  maxx <- max(X)
  minx <- min(X)
  maxy <- max(Y)
  miny <- min(Y)
  Disp <- abs(maxx - minx) + abs(maxy-miny)
  return(Disp)
}

Calc.Wind.Disp.rd <- function(X,Y)
{
  mx <- mean(X)
  my <- mean(Y)
  rd <- sqrt((X-mx)^2+(Y-my)^2)
  Disp <- max(rd)
  return(Disp)
}


Calc.Wind.Disp.rd2 <- function(X,Y)
{
  mx <- mean(X)
  my <- mean(Y)
  rd <- sqrt((X-mx)^2+(Y-my)^2)
  Disp <- max(rd)
  FLG <- Disp == rd[1]
  return(c(Disp,FLG))
}

Calc.Wind.Disp.lc <- function(X,Y)
{
  l <- length(X); l1 <- l-1
  mx <- mean(X[1:l1])
  my <- mean(Y[1:l1])
  
  rd <- sqrt((X[l]-mx)^2+(Y[l]-my)^2)
  
  #rd <- (abs(mx - X[l]) + abs(my-Y[l]))/2
  
  return(rd)
}

Calc.Wind.Disp.wtf <- function(X,Y)
{
  mx <- mean(X)
  my <- mean(Y)
  rd <- (X-mx)^2+(Y-my)^2
  Disp <- sqrt(sum(rd)/(length(rd)-1))
  return(Disp)
}