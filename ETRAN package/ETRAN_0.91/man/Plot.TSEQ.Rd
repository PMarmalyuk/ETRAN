\name{Plot.TSEQ}
\alias{Plot.TSEQ}
\title{
Plot.TSEQ
}
\description{
Plot time sequence: X(t), Y(t), Pupil size(T), Speed(T), detected fixations.
Maximum number of data samples to plot: 2000. If the number of data samples is great then 4000, downsampling performed.
Width of rectangles for fixation visualization is 2*fixation dispersion.
Detected fixations and speed visualized only in monocular mode.
}

\usage{
Plot.TSEQ(d,d.px=c(0,0),d.r=c(0,0),d.time=c(0,0),d.angle=c(0,0),d.speed=c(0,0),
                      Y.Scale="A",Fix=NULL,Speed=NULL,Plot.X=TRUE,Plot.Y=TRUE,Plot.Pupil=FALSE,
                      Col.X=c("red","red3"),Col.Y=c("blue","blue3"),Col.Pupil=c("black","gray50"),Col.XF="lightsalmon",Col.YF="lightskyblue",Col.Speed="green2",Header=" ") 
}
\arguments{
  \item{d}{
	eyetracking data (selected by Sel.SMI() function; for other eyetracking data required separate Read... & Sel... functions ).
	}
  \item{d.px}{
	pixel limits (if Y.Scale="P")
	}	
  \item{d.r}{
	pupil dillation limits
	}	
  \item{d.angle}{
	angle limits (if Y.Scale="A")
	}		
  \item{d.speed}{
	speed limits
	}	
  \item{Y.Scale}{
	"A" - angle (degree), "P" - screen pixels
	}		
  \item{Fix}{
	fixation data (only in monocular mode)
	}
  \item{Speed}{
	speed data (only in monocular mode)
	}	
  \item{Plot.X}{
    plot X coordinate data
   }	
  \item{Plot.Y}{
    plot X coordinate data
   }	
  \item{Plot.Pupil}{
    plot pupil dilation data
   }	
  \item{Col.X}{
    color for X coordinate data (monocular & binocular)
   }
  \item{Col.Y}{
    color for X coordinate data (monocular & binocular)
   }
  \item{Col.Pupil}{
    color for X coordinate data (monocular & binocular)
   }   
  \item{Col.XF}{
    color for X fixation data (only monocular)
   }   
  \item{Col.YF}{
    color for Y fixation data (only monocular)
   }      
  \item{Col.Speed}{
    color for speed data (only monocular)
   }   
  \item{Header}{
    plot header
   }   
}
\details{
If Plot.Pupil=TRUE and Speed argument exists - right vertical scale is speed (degree).
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
# data loading & selection
ETFN      <- system.file("eyedata", "Yarbus1.txt", package="ETRAN")
ETDATA    <- Read.SMI(ETFN)
D         <- Sel.SMI("ETDATA",2)
#
# Simple plot with X,Y & pupil data
Hd <- "I.E. Repin <<An Unexpected Visitor>> eye movements"
Plot.TSEQ(D,Plot.Pupil=TRUE,Header=Hd)
#
#
# Plot with detected fixations
# fixation detection
F  <- FixDetect.IDT(D$X,D$Y,D$R,Disp=60,Dur=10,method="LC.orig",SampleRate=120)
# header
Hd <- "I.E. Repin <<An Unexpected Visitor>> eye movements"
# plot
Plot.TSEQ(D,Plot.Pupil=TRUE,Fix=F,Header=Hd)
#
#
# Plot with fixations & calculated speed
# speed calculation
V  <- Calc.Speed(D)
# fixation detection
F  <- FixDetect.IDT(D$X,D$Y,D$R,Disp=60,Dur=10,method="LC.orig",SampleRate=120)
# header
Hd <- "I.E. Repin <<An Unexpected Visitor>> eye movements"
# plot
Plot.TSEQ(D,Plot.Pupil=TRUE,Fix=F,Speed=V,Header=Hd)
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
