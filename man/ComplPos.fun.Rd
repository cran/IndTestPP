\name{ComplPos.fun}
\alias{ComplPos.fun}


\title{Changes  the format of a vector of the occurrence times in a point process}

\description{It changes the format ofthe vector of the occurrence points in a point process.  It builds a vector  of 
 length T, the length of the observation period, which takes value 0 at  the non occurrence times and 
 the position value,  or 1,  at the occurence times.}

\usage{
ComplPos.fun(pos, T, type='Pos')
}

\arguments{
  \item{pos}{Numeric vector. Occurrence times}
  \item{T}{Integer. Length of the observation period}
  \item{type}{Character string, 'Pos' or 'Bin'. Type of the new format}
}

\details{ This function changes the format of the occurrence points in a point process. The new format is useful
when several point processes, in the same observation period, must be specfied; for example, in function 
\code{\link{NHJ.fun}} or \code{\link{NHD.fun}}, where the occurrence times of   different point processes 
must be   introduced as a matrix. Since the number of occurrences in each process can be different,  
in the new format,  occurrences in each process are specified as a vector  of length T, which takes value  0 
at non occurrence times  and  the time position  (if type='Pos') or 1 (if type='Bin') at the occurrence times.}


\value{
\item{Npos }{Numeric vector of lenght T containing the occurrence times in the new format.}
}




\seealso{\code{\link{NHD.fun}}, \code{\link{NHF.fun}}, \code{\link{NHK.fun}}, \code{\link{NHJ.fun}}}


\examples{
pos<-c(4,15,18,34,36,67,98)
Npos<-ComplPos.fun(pos, T=100)
}

