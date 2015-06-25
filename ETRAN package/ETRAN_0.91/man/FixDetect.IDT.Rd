\name{FixDetect.IDT}
\alias{FixDetect.IDT}
\title{
FixDetect.IDT
}
\description{
Detect fixations in eyetracking data with I-DT (dispersion threshold identification) algorithm.
}
\usage{
FixDetect.IDT(X,Y,R,Disp=50,Dur=6,R.min=1,Debug=0,method="LC.orig",SampleRate=50,Max.n_nv=3,dR.max=0.05)
}
\arguments{
  \item{X}{
	num vector, serial data samples, X gaze coordinate (px).
	}
  \item{Y}{
	num vector, serial data samples, Y gaze coordinate (px).
	}
  \item{R}{
	num vector, serial data samples, Pupil diameter (px).
	}
  \item{Disp}{
	Maximum gaze data dispersion (px).
	}	
  \item{Dur}{
	Minimum fixation duration (number of data samples).
	}	
  \item{Rmin}{
	Threshold pupil diameter (R < Rmin - invalid data).
	}	
  \item{method}{
    method="LC.orig" - LC technologies original algorithm (1989,1995), dispersion is calculated as Euclidian distance from the center of fixation to the next sample, so the total fixation dispersion may be greater, than threshold. \cr
    method="LC.r" - rewritten LC technologies algorithm
	method="Rd" - dispersion calculated as maximum Eucledian distance from the center of fixation \cr
	method="SG" - dispersion calculated as abs(max)X) - min(X)) + abs(max(Y)-min(Y)) as written in Salvucci, D. D., & Goldberg, J. H. (2000). Identifying fixations and saccades in eye-tracking protocols. In Proceedings of the Eye
Tracking Research and Applications Symposium (pp. 71-78). New York: ACM Press. \cr
    method="SGm" - dispersion calculated as dispersion calculated as 0.5*abs(max)X) - min(X)) + abs(max(Y)-min(Y)) , as suggested by P. Blignaut (2009). Fixation identification: The optimum threshold
for a dispersion algorithm. In Attention, Perception, & Psychophysics 2009, 71 (4), 881-895 \cr
  }
  \item{SampleRate}{
	Eyetracker sampling rate (Gz).
	}	
  \item{Max.n_nv}{
	Maximal number of nonvalid data samples in fixation.
	}
  \item{dR.max}{
	Pupil size variation threshold. Data not valid if pupil size variation between samplese greater, than dR.max.
	}
}
%%\details{
%%This package is useless but hopefully great for learning the minimal things that should
%%be in a package.
%%}
\value{
	Returns a data frame
	\item{X}{num vector, fixations X coordinates (px).}
	\item{Y}{num vector, fixations Y coordinates (px).}
	\item{R}{num vector, fixations average pupil diameter (px).}
	\item{Disp}{num vector, fixations dispersion (px).}
	\item{Start}{num vector, fixations start position, ms.}	
	\item{Dur}{num vector, fixations durations, ms.}
	\item{s_Start}{num vector, fixations start position (sample number).}	
	\item{s_Dur}{num vector, fixations durations, (number of samples,).}

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
F         <- FixDetect.IDT(D$X,D$Y,D$R,Disp=60,Dur=10,method="LC.orig",SampleRate=120)

# plot histogram
hist(F$Dur,breaks=30,col="lightgray",main="Fixation durations histogram",xlab="Fixation duration (ms)")
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
