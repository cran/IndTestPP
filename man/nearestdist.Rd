\name{nearestdist}
\alias{nearestdist}
\alias{pdist}


\title{Distance to the nearest point}

\description{Given the  occurrence points in two  point processes, this function   calculates
for each point in  the first process,  the  distance to the nearest occurrence point in the second process. }

\usage{nearestdist(posx, posy)}



\arguments{
  \item{posx}{Numeric vector. Occurrence times of the  points in the first  point process.}
  \item{posy}{Numeric vector. Occurrence times of the  points in the second  point process.}
}



\details{

The distance between two points \eqn{x_i} and \eqn{y_i} in  a point  process in time, is the  absolute value of their 
difference: \eqn{|x_i-y_i|}.

To obtain the vector of nearest points, this function applies to each point in \code{posx},  
 the function \code{pdist}, which calculates the distance to its nearest point  in  \code{posy}.


}

\value{
Vector  of  the distances to the  nearest point in the second process for each point in the 
first process.
}


\seealso{\code{\link{DutilleulPlot}}}

\examples{
posx<-c(3,8,23,54,57,82)
posy<-c(2,8,14,16,29,32,45,55,65)
nearestdist(posx, posy)
}