\name{Calc.Speed}
\alias{Calc.Speed}
\title{
Calc.Speed
}
\description{
Calculate angular speed (degree) by two points intervals
}
\usage{
Calc.Speed <- function(D)
}
\arguments{
  \item{D}{
	eyetracking data (selected by Sel.SMI() function ).
	}
}
\details{
...
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
ETFN      <- system.file("eyedata", "Yarbus1.txt", package="ETRAN")
ETDATA    <- Read.SMI(ETFN)
D         <- Sel.SMI("ETDATA",2)
V         <- Calc.Speed(D)
plot(D$T,V,type="l")
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
