\name{uniongentri}
\alias{uniongentri}
\alias{genbiPos}
\alias{gentriPos}

\title{Calculating the set of close points}

\description{This function calculates the set of close points
of each  occurence point in the first process of a vector of two or three processes.}

\usage{uniongentri(posx, posy, posz=NULL,  info = FALSE, PA = FALSE,
	procName=c('X','Y','Z'),...)}



\arguments{
  \item{posx}{Numeric vector. Position of the occurrence points in the first process.}
  \item{posy}{Numeric vector. Position of the occurrence points in the second process.}
  \item{posz}{Optional. Numeric vector. Position of the occurrence points in the third process. Only used when three
processes are involved.}

  \item{info}{Optional. Logical flag. If it is TRUE,  information about the generated points is shown  on the screen and
 dotcharts and bivariate charts of the  occurrence points in the processes are displayed.}

  \item{procName}{Vector of character strings. Names of the processes.}

  \item{PA}{Optional. Logical flag. If it is TRUE, the close point relation  is
broadened by including the previous and the following points to the overlapping
intervals.}

  \item{...}{Further arguments to be passed to the function \code{\link{plot}}  if  \code{info=T}.}
}


\details{A point in a process is close to a point in another
process,  if  their time intervals overlap; the time interval of a point is the interval
between itself and the previous  point in the same process. If there are three processes,  
the set of close points of \eqn{t_{x_k}},\eqn{S_{x_i; xyz}}, is defined as
 the set of  the pairs of points \eqn{(t_{y_j}, t_{z_k})} such that  \eqn{t_{x_i}} is  close to \eqn{t_{y_j}} and \eqn{t_{y_j}} is  close to 
\eqn{t_{z_k}}. If there are  two processes,  \eqn{S_{x_i; xy}} is the set of   points
\eqn{t_{y_j}} such that  \eqn{t_{x_i}} is  close to \eqn{t_{y_j}}.
This definition can be broadened, see  argument \code{PA}, by adding to the set two more points, the previous and the following ones.

The algortihm to calculate the sets of close points  (in the case of three processes) is the following, 
see Abaurrea et al. (2015) for details: First,  given two processes, the pairs of close points  in those
processes are calculated. If the  last
point occurs in the first process,  there is a censored time interval in the second process (the point overlaps a time interval whose occurrence point has not been observed)
and that pair is not considered). This step is performed for all the combinations of  pairs of processes. The basic
 close point relation is commutative,   and only three different  pairs (XY, YZ, XZ) must be considered. 
This is not the case of the broadened  definition, where the six pairs (XY, YX, YZ, ZY, XZ, ZX) must be  calculated.

Once all the pairs of close points are obtained,  the set of close points for each point \eqn{t_{x_i}} is obtained
by concatenating the adequate pairs of points from all the  possible orders of the three  processes: XYZ,
XZY and YXZ for the basic definition, and the six possible permutations for the broadened definition.
The final set of close points of \eqn{t_{x_i}} is  the union of the different pairs  from  all the possible permutations.
}

\value{ A list with elements:
  \item{X}{First elements of the 3-tuples  of points \eqn{(t_{x_i}, t_{y_i}, t_{z_i})} in the sets of close points.}
  \item{iX}{Position  i (=1,2,3....) of the  point \eqn{t_{x_i}} in the first process.}
  \item{Y}{Second elements of the 3-tuples  of points \eqn{(t_{x_i}, t_{y_i}, t_{z_i})} in the sets of close points.}
  \item{iY}{Position  i (=1,2,3....) of the  point \eqn{t_{y_i}} in the second process.}
  \item{Z}{Third elements of the 3-tuples  of points \eqn{(t_{x_i}, t_{y_i}, t_{z_i})} in the sets of close points. 
	It is NULL if \code{posz=NULL}.}
  \item{iZ}{Position i (=1,2,3....) of the  point \eqn{t_{z_i}} in the third process. 
	It is NULL if posz=NULL.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}.}


\seealso{\code{\link{TestIndNH}}, \code{\link{DistSim}}, \code{\link{DistObs}}}

\examples{

set.seed(123)
posx<-sort(runif(20,0,1000))
posy<-sort(runif(25,0,1000))
posz<-sort(runif(40,0,1000))
aux<-uniongentri(posx, posy, posz, info=TRUE)
}
