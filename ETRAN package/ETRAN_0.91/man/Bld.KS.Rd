\name{Bld.KS}
\alias{Bld.KS}
\title{
Bld.KS
}
\description{
Build heatmap with kernel smoothing. Require KernSmooth bkde2D function.
}
\usage{
Bld.KS(D,w=D$W,h=D$H,bw=30)
}
\arguments{
  \item{D}{
	eyetracking data (selected by ES.sel() function ).
	}
  \item{w}{
	heatmap width.
	}
  \item{h}{
	heatmap height.
	}
  \item{bw}{
	bandwidth, according bkde2D function.
	}	
}
\details{
 By default w and h set equal to screen size in px. If w and h is less then screen size, heatmap is centre of screen. Greed size is 50x50 cells.
}
\value{
	Returns a list
	\item{X}{matrix, grid cells boundaries (px).}
	\item{Y}{matrix, grid cells boundaries (px).}
	\item{Z}{matrix, grid cells values}
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
library(ETRAN)       
library(jpeg)          
library(KernSmooth)  
BGFN      <- system.file("bgd", "bgd1.jpg", package="ETRAN")
bgd       <- as.raster(readJPEG(BGFN))
ETFN      <- system.file("eyedata", "Yarbus1.txt", package="ETRAN")
ETDATA    <- Read.SMI(ETFN)
D         <- Sel.SMI("ETDATA",2)
HM        <- Bld.KS(D)
             Plot.KSbg(HM,bgd)
#
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
