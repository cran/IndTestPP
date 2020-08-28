\name{PlotMargP}
\alias{PlotMargP}


\title{Plotting  the  occurrence points  of a vector of point processes}


\description{This function  plots the points in the marginal processes \eqn{N_{1}}, \eqn{N_{2}},..., \eqn{N_{d}}   
of a vector of point processes. 
}

\usage{PlotMargP(listpos, T, date=NULL,axispoints=NULL, tcex=1.2, ...)}

\arguments{
  \item{listpos}{A list of vectors. Each  element of the list  is the vector of the occurrences in a marginal process.}
  \item{T}{Numeric value. The length of the observed period of the processes.}
  \item{date}{Optional. A vector indicating the date of each observation to be used in the axis of the plot.}
  \item{axispoints}{Optional. Numeric vector with the  points in the time index in which axis ticks and labels (from \code{date})
	 will be drawn.}
  \item{tcex}{Optional.  cex argument, see \code{\link{par}}, for the text  labels in the plot.}
  \item{...}{Further arguments to  be passed to the function \code{\link{plot}}.}
}

\details{
The  points in  the  d marginal processes \eqn{N_{1}}, \eqn{N_{2}} ,..., \eqn{N_{d}}   of a  vector 
of point processes are plotted versus the time index. 

If  one of the arguments \code{date} and \code{axispoints} is NULL, default axis are used. 
Otherwise, the values in \code{axispoints} are used as the  points in the time index in which axis ticks 
and labels, from \code{date},  are drawn.
}

\value{A plot.}

\seealso{ \code{\link{PlotMCPSP}}}

\examples{
set.seed(123)
N1<-runif(50,0,5000)
set.seed(124)
N2<-runif(42,0,5000)

PlotMargP(list(N1=N1, N2=N2),T=5000)
}