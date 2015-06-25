\name{ETRAN-package}
\alias{ETRAN-package}
\alias{ETRAN}
\docType{package}
\title{
Package for eyetracking data processing
}
\description{
Package for eyetracking data processing (from SMI High Speed, SMI HED, SMI RED-m)
}
\details{
\tabular{ll}{
Package: \tab ETRAN\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2015-13-06\cr
License: \tab GPL (>= 3)\cr
LazyLoad: \tab no\cr
}
}
\author{
Alexander Zhegallo <\url{http://psyexp.ru}>

Maintainer: Alexander Zhegallo <a.zhegallo@gmail.com>
}

% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
% ~~ the R documentation directory ~~
\keyword{ package }
\seealso{
%%~~ Optional links to other man pages, e.g. ~~
%%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
ETFN      <- system.file("eyedata", "Yarbus1.txt", package="ETRAN")
ETDATA    <- Read.SMI(ETFN)
}
