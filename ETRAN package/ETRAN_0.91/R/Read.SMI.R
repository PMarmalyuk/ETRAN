Read.SMI <- function(filename, ReadRAW=0, EPOS=0, ReadTrial=0, Debug=0)
{
   return(.Call("readsmiC",fn = filename, ReadRAW=ReadRAW, ReadEPOS=EPOS, ReadTrial=ReadTrial, Debug=Debug, package = "Eyetracking"));
}