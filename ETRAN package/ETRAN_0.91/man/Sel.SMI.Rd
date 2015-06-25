\name{Sel.SMI}
\alias{Sel.SMI}
\title{
Sel.SMI
}
\description{
Select eyetracking data (SMI eyetrackers) by experiment situation number ant time interval.
}
\usage{
Sel.SMI(dfname,fn,d.time=c(0,0),Mode="Monocular",Pupil="RAW",EPOS=0)
}
\arguments{
  \item{dfname}{
	name of R object, contain eyetracking data (must be read from data file by Read.SMI() function.
	}
  \item{fn}{
	number( or vector of numbers) of  experimental situation(s) to select (selection by Fn field of eyetracking data).
	}
  \item{d.time}{
	time interval to select, relative begin of experimental situation; by default d.time=c(0,0), which mean, what full experimental situation selected.
	}
  \item{Mode}{
    "Left"  get left eye data; 
    "Right" -  get right eye data; 
    "Binocular" - get both eyes data;
    "Average"  - get averaged data; 
    "Monocular" - get one eye (first in data set) data	
	}	
  \item{Pupil}{
    "RAW"  get raw pupil data; 
    "Mapped" -  get mapped pupil data; 
	}	
  \item{EPOS}{
    1 - get EPOS & GVEC data from RED eyetrackers (binocular mode)  
   }
}
%%\details{
%%This package is useless but hopefully great for learning the minimal things that should
%%be in a package.
%%}
 

\value{
	In monoclular, left, right or average mode returns a list
	\item{X}{num vector, gaze X coordinates (px).}
	\item{Y}{num vector, gaze Y coordinates (px).}
	\item{R}{num vector, pupil diameter (px).}
	\item{T}{num vector, time from the begin of experimental situation, ms.}
	\item{Name}{experimental situation name (from MsgTxt field of eyetracking data).}
	\item{Subj}{subject name (from Subj field of eyetracking data).}
	\item{W}{screen width (px).}	
	\item{H}{screen height (px).}
	\item{fn}{selected experimental situation number.}		
	\item{NV}{num vector, 0 - related data sample is valid, 1 - related data sample isn't valid.}
	\item{Mode}{mode, as input field}
	\item{Res}{screen resolution px/mm}
	\item{Head.Dist}{Distance to head in mm}
	
	
	In binocular mode returns a list
	\item{Xl}{num vector, gaze X coordinates (left eye, px).}
    \item{Xl}{num vector, gaze X coordinates (right eye, px).}
	\item{Yl}{num vector, gaze Y coordinates (left eye,px).}
	\item{Yr}{num vector, gaze Y coordinates (right eye,px).}	
	\item{Rl}{num vector, pupil diameter (left eye,px or mm).}
	\item{Rr}{num vector, pupil diameter (right eye,px or mm).}	
	\item{T}{num vector, time from the begin of experimental situation, ms.}
	\item{Name}{experimental situation name (from MsgTxt field of eyetracking data).}
	\item{Subj}{subject name (from Subj field of eyetracking data).}
	\item{W}{screen width (px).}	
	\item{H}{screen height (px).}
	\item{fn}{selected experimental situation number.}		
	\item{NV}{num vector, 0 - related data sample is valid, 1 - related data sample isn't valid.}
	\item{Mode}{mode, as input field}
	\item{Res}{screen resolution px/mm}
	\item{Head.Dist}{Distance to head in mm}	

	In binocular mode with EPOS=1 returns a list
	\item{Xl}{num vector, gaze X coordinates (left eye, px).}
    \item{Xl}{num vector, gaze X coordinates (right eye, px).}
	\item{Yl}{num vector, gaze Y coordinates (left eye,px).}
	\item{Yr}{num vector, gaze Y coordinates (right eye,px).}	
	\item{Rl}{num vector, pupil diameter (left eye,px or mm).}
	\item{Rr}{num vector, pupil diameter (right eye,px or mm).}	
	\item{L_EPOSX}{num vector, left eye X EPOS coordinate (mm)}
	\item{L_EPOSY}{num vector, left eye Y EPOS coordinate (mm)}	
	\item{L_EPOSZ}{num vector, left eye Z EPOS coordinate (mm)}	
	\item{R_EPOSX}{num vector, right eye X EPOS coordinate (mm)}
	\item{R_EPOSY}{num vector, right eye Y EPOS coordinate (mm)}	
	\item{R_EPOSZ}{num vector, right eye Z EPOS coordinate (mm)}
	\item{L_GVECX}{num vector, left eye X GVEC coordinate (rad)}
	\item{L_GVECY}{num vector, left eye Y GVEC coordinate (rad)}	
	\item{L_GVECZ}{num vector, left eye Z GVEC coordinate (rad)}	
	\item{R_GVECX}{num vector, right eye X GVEC coordinate (rad)}
	\item{R_GVECY}{num vector, right eye Y GVEC coordinate (rad)}	
	\item{R_GVECZ}{num vector, right eye Z GVEC coordinate (rad)}
	\item{T}{num vector, time from the begin of experimental situation, ms.}
	\item{Name}{experimental situation name (from MsgTxt field of eyetracking data).}
	\item{Subj}{subject name (from Subj field of eyetracking data).}
	\item{W}{screen width (px).}	
	\item{H}{screen height (px).}
	\item{fn}{selected experimental situation number.}		
	\item{NV}{num vector, 0 - related data sample is valid, 1 - related data sample isn't valid.}
	\item{Mode}{mode, as input field}
	\item{Res}{screen resolution px/mm}
	\item{Head.Dist}{Distance to head in mm}	

	
 	}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alexander Zhegallo
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ETFN      <- system.file("eyedata", "Yarbus1.txt", package="ETRAN")
ETDATA    <- Read.SMI(ETFN)
D         <- Sel.SMI("ETDATA",2)
str(D)
#
# same data in binocular mode and EPOS & GVEC data
#
ETDATA    <- Read.SMI(ETFN,EPOS=1)
D         <- Sel.SMI("ETDATA",2,EPOS=1)
str(D)
# calculate subject's distance between pupil's centers
DX <- sqrt((D$R_EPOSX - D$L_EPOSX)^2+(D$R_EPOSY - D$L_EPOSY)^2+(D$R_EPOSZ - D$L_EPOSZ)^2)
s <-which((DX>60)&(DX<80))
hist(DX[s])
#
# Plot horizontal vergence angle
#
ytxt <- "Vergence angle (X)"
xtxt <- "Time (ms)"
Verg_X <- D$L_GVECX - D$R_GVECX
plot  (D$T,(Verg_X/pi)*180,col="blue",pch=20,cex=.5,ylim=c(0,10),xlab=xtxt,ylab=ytxt)
#
#
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
