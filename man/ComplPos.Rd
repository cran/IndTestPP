\name{ComplPos}
\alias{ComplPos}


\title{Changes  format of the vector of occurrence times in a point process}

\description{It changes the format of the vector of occurrence points in a point process.  It builds a vector  of 
 length T, the length of the observation period, which takes value 0 at  the non occurrence times and 
 the position value (type="Pos"),  or 1 (type="Bin"),  at the occurence times.}

\usage{
ComplPos(pos, T, type='Pos')
}

\arguments{
  \item{pos}{Numeric vector. Occurrence times.}
  \item{T}{Integer. Length of the observed period.}
  \item{type}{Character string, 'Pos' or 'Bin'. Type of the new format.}
}

\details{This function changes the format of the occurrence points in a point process. The new format is useful
when several point processes, in the same observed period, must be specfied; for example, in function 
\code{\link{NHJ}} or \code{\link{NHD}}, where the occurrence times of   different point processes 
must be   introduced as a matrix. Since the number of occurrences in each process can be different,  
in the new format,  occurrences in each process are specified as a vector  of length T, which takes value  0 
at non occurrence times  and  the time position  (if type='Pos') or 1 (if type='Bin') at the occurrence times.}


\value{
\item{Npos}{Numeric vector of lenght T containing the occurrence times in the new format.}
}




\seealso{\code{\link{NHD}}, \code{\link{NHF}}, \code{\link{NHK}}, \code{\link{NHJ}}}


\examples{
pos<-c(4,15,18,34,36,67,98)
Npos<-ComplPos(pos, T=100)
}

