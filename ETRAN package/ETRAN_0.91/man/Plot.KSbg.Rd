\name{Plot.KSbg}
\alias{Plot.KSbg}
\title{
Plot.KSbg
}
\description{
Plot heatmap data on background.
}
\usage{
Plot.KSbg(D,bgd)
}
\arguments{
  \item{D}{
	list with $X, $Y, $Z components (build by Bld.KS() function).
	}
  \item{bgd}{
	raster background.
	}
}
%%\details{
%%This package is useless but hopefully great for learning the minimal things that should
%%be in a package.
%%}
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
