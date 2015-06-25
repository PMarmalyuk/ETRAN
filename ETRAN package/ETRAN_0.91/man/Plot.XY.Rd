\name{Plot.XY}
\alias{Plot.XY}
\title{
Plot.XY
}
\description{
Plot raw eyetracking data on background.
}
\usage{
Plot.XY(d,bgd,Fix=NULL,Plot.RAW=TRUE,maxdatasize=2000,Col.Points="red",Col.Fix="red3",Col.Fixl="red4",Plot.Circle=FALSE,Plot.Chull=TRUE,Plot.Time=FALSE,FixL.w=2,CnvL.w=1)
}
\arguments{
  \item{d}{
	eyetracking data (selected by Sel...() function ).
	}
  \item{bgd}{
	raster background.
	}
  \item{Fix}{
	Fixations data to plot.
	}	
  \item{Plot.RAW}{
	Plot RAW data.
	}	
  \item{Plot.Circle}{
	Plot fixations as circles.
	}	
  \item{Plot.Chull}{
	Plot convex hull for fixations.
	}	
  \item{maxdatasize}{
	Maximum data size to plot without downsampling.
	}			
  \item{Col.points}{
	Color for raw data.
	}	
  \item{Col.Fix}{
	Color for fixations.
	}	
  \item{Col.Fixl}{
	Color for fixations connection lines.
	}	
  \item{FixL.w}{
	Width of fixations connections lines.
	}	
  \item{CnwL.w}{
	Width of convex hull lines.
	}		
}
\details{
If background size is less then screen size, it interpreted as central part of screen.
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
BGFN      <- system.file("bgd", "bgd1.jpg", package="ETRAN")
bgd       <- as.raster(readJPEG(BGFN))
ETFN      <- system.file("eyedata", "Yarbus1.txt", package="ETRAN")
ETDATA    <- Read.SMI(ETFN)
D         <- Sel.SMI("ETDATA",2)
Plot.XY(D,bgd)
#
F         <- FixDetect.IDT(D$X,D$Y,D$R,Disp=60,Dur=10,method="LC.orig",SampleRate=120)
          Plot.XY(D,bgd,F)
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
