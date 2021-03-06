\name{DistObs}
\alias{DistObs}
\alias{calcdist}

\title{Calculates the set of close points and the mean distance in a vector of processes,
 for each point in the first process}

\description{Given  a set of two or three processes, this function calculates the set of close points and 
the mean distance for each point in the  first process.


It calls  the functions  \code{\link{calcdist}}, not intended for the users, and \code{\link{uniongentri}}. 
}


\usage{DistObs(posx, posy, posz=NULL, info = FALSE, PA = FALSE, procName=c('X','Y','Z'),...)}

\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}
  \item{posy}{Numeric vector. Position of the occurrence points in the second process.}
  \item{posz}{Optional. Numeric vector. Position of the occurrence points in the third process.}

   \item{info}{Optional logical flag. If it is TRUE,  information about the generated points is showed  on the screen and
 dotcharts  and bivariate charts of the  occurrence points of the processes are displayed.}

  \item{PA}{Optional logical flag. If it is TRUE, the close point relation  is
broadened by including the previous and the following points to the overlapping
intervals.}

  \item{procName}{Vector of character strings. Labels for the first, second and third processes.}

  \item{...}{Further arguments to  be passed to the function \code{\link{plot}},  if  argument \emph{info="TRUE"}.}
}


\details{
 Given a set of two or three  point proccesses,  for each  point \eqn{t_{x_i}} in the first process of the set,
this function calcultes its set of close points  and the mean distance
 to its close points. The definition of set of close points can be found in  Abaurrea et al. (2015),
 and the  distances  are defined as \eqn{|t_{y_j}-t_{x_i}|} if there are two processes, and 
as \eqn{|t_{y_j}-t_{x_i}|+|t_{z_k}-t_{x_i}|}  if there are three.
}

\value{
 \item{DistTri}{The vector of the mean distances of   points \eqn{t_{x_i}}.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}, 22(1), 127-144.}


\seealso{\code{\link{TestIndNH}}, \code{\link{DistSim}}, \code{\link{uniongentri}}}


\examples{
data(TxBHZ)
dateT<-cbind(TxBHZ$year,TxBHZ$month,TxBHZ$day)  #year, month and day of the month
BivEv<-CPSPPOTevents(N1=TxBHZ$TxH,N2=TxBHZ$TxZ,thres1=36.4, thres2=37.8, date=dateT)
aux<-DistObs(BivEv$Px1, BivEv$Px2, BivEv$Px12,info = TRUE)
}
