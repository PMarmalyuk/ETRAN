\name{Read.SMI}
\alias{Read.SMI}
\title{
Read.SMI
}
\description{
Read SensoMotoric Instruments Eyetracker (SMI High Speed, SMI RED-m, SMI HED, etc) txt iView data file (converted from original .IDF data file with  SMI IDFConverter).
C realization increase performance for LARGE data files (from SMI High Speed).
}
\usage{
Read.SMI(filename,ReadRAW=0,EPOS=0,Debug=0)
}
\arguments{
  \item{filename}{
	full name of file to read
	}
  \item{ReadRAW}{
	Set ReadRAW=1 to pass information about eye and pupil coordinates in camera frame
	}
  \item{ReadTrial}{
    Set ReadTrial=1 to pass information about trial field
   }
  \item{EPOS}{
	Set EPOS=1 for RED eyetracker family only to pass information about absolute eye position and gaze vector coordinates
	}
  \item{Debug}{
	Debug=1 - 1st level debug information; Debug=2 - 2nd level debug informaton; Debug=3 terminate after obtaining file structure
	}	
}
%%\details{
%%This package is useless but hopefully great for learning the minimal things that should
%%be in a package.
%%}
\value{
	Returns a list
	\item{Subj}{Subject identification string.}
	\item{N}{number of data samples.}
	\item{SampleRate}{eyetracker frequency (Gz).}
	\item{Area}{num vector, length=4, [1] - screen width (px), [2] - screen height (px), [3] - screen width (mm), [4] - screen height (mm).}
	\item{HeadDistance}{distance to head in mm.}
	\item{MsgTxt}{chr vector, text for MSG lines in data file.}
	\item{MsgTimes}{num vector, time stamps (in ms ) for MSG lines in data file.}
	\item{Time}{num vector, time stamps (in ms) for SMP lines in data file.}
	if ReadTrial=1 set
	\item{Trial}{num vector,trial number.}
    According binocular or monocular registration mode:	
	\item{L_PorX}{num vector, left eye X coordinates(screen px).}
	\item{L_PorY}{num vector, left eye Y coordinates (screen px).}	
	\item{R_PorX}{num vector, right eye X coordinates(screen px).}
	\item{R_PorY}{num vector, right eye Y coordinates (screen px).}	
	\item{L_DX}{num vector, left eye pupil size (X, camera px).}
	\item{L_DY}{num vector, left eye pupil size (Y, camera px).}	
	\item{R_DX}{num vector, right eye pupil size (X, camera px).}
	\item{R_DY}{num vector, right eye pupil size (Y, camera px).}
	\item{L_DM}{num vector, right eye pupil size (mapped to mm).}	
	\item{R_DM}{num vector, right eye pupil size (mapped to mm).}	
	If ReadRAW=1 set (depended of registration mode and eyetracker type)
	\item{L_RAWX}{num vector,left eye pupil center (X, camera px).}
    \item{L_RAWY}{num vector,left eye pupil center (Y, camera px).}
	\item{R_RAWX}{num vector,right eye pupil center (X, camera px).}
    \item{R_RAWY}{num vector,righ eye pupil center (Y, camera px).}	
	\item{L_CRX1}{num vector,left eye corneal reflection 1  (X, camera px).}
    \item{L_CRY1}{num vector,left eye corneal reflection 1  (Y, camera px).}	
	\item{L_CRX2}{num vector,left eye corneal reflection 2  (X, camera px).}
    \item{L_CRY2}{num vector,left eye corneal reflection 2  (Y, camera px).}	
	\item{R_CRX1}{num vector,right eye corneal reflection 1  (X, camera px).}
    \item{R_CRY1}{num vector,right eye corneal reflection 1  (Y, camera px).}	
	\item{R_CRX2}{num vector,right eye corneal reflection 2  (X, camera px).}
    \item{R_CRY2}{num vector,right eye corneal reflection 2  (Y, camera px).}		
	If EPOS=1 set and appropriate data file from RED family eyetrackers given, additional fields:
	\item{L_EPOSX}{num vector, left eye X position relative RED-m camera (mm).}
	\item{L_EPOSY}{num vector, left eye Y position relative RED-m camera (mm).}
	\item{L_EPOSZ}{num vector, left eye Z position relative RED-m camera (mm).}	
	\item{R_EPOSX}{num vector, right eye X position relative RED-m camera (mm).}
	\item{R_EPOSY}{num vector, right eye Y position relative RED-m camera (mm).}
	\item{R_EPOSZ}{num vector, right eye Z position relative RED-m camera (mm).}		
	\item{L_GVECX}{num vector, left eye normalized gaze direction vector, X - coordinate.}
	\item{L_GVECY}{num vector, left eye normalized gaze direction vector, Y - coordinate.}
	\item{L_GVECZ}{num vector, left eye normalized gaze direction vector, Z - coordinate.}	
	\item{R_GVECX}{num vector, right eye normalized gaze direction vector, X - coordinate.}
	\item{R_GVECY}{num vector, right eye normalized gaze direction vector, Y - coordinate.}
	\item{R_GVECZ}{num vector, right eye normalized gaze direction vector, Z - coordinate.}	
	Experimental situation data
    \item{Fn}{num vector, experimental situation (MSG) number for each SMP line.}		
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
str(ETDATA)
# get EPOS data
ETDATA    <- Read.SMI(ETFN,EPOS=1)
str(ETDATA)
# get EPOS & RAW data
ETDATA    <- Read.SMI(ETFN,EPOS=1,ReadRAW=1)
str(ETDATA)
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
