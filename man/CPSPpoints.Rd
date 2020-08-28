\name{CPSPpoints}
\alias{CPSPpoints}
\alias{marca}

\title{Identifying the  occurrence points  of the indicator processes in a CPSP}


\description{It calculates the  occurrence  points
 in the three indicator processes of a bivariate Common Poisson Shock Process (CPSP), using as input
 the two marginal processes \eqn{N_1} and \eqn{N_2}.
}

\usage{CPSPpoints(N1,N2,date=NULL, dplot=T, pmfrow=c(2,1), axispoints=NULL,...)}

\arguments{
  \item{N1}{Binary vector of the first CPSP marginal process; occurrence points must be marked with 1 and the other with 0.}
  \item{N2}{Binary vector of the second CPSP marginal process; occurrence points must be marked with 1 and the other with 0.}
  \item{date}{Optional. A vector or matrix indicating the date of each observation.}
  \item{dplot}{Optional. A logical flag. If it is TRUE,  the  marginal and indicator processes  are plotted.}
  \item{pmfrow}{Optional.  A vector of  the form (nr, nc) to be supplied as value of the argument \code{mfrow} in  
	\code{\link{par}}. }
  \item{axispoints}{Optional. Numeric vector with the  points in the time index where axis ticks and labels (from the first column in \code{date})
	 have to be drawn.}
  \item{...}{Further arguments to  be passed to  the function \code{\link{plot}}.}
}

\details{
A  bivariate CPSP \eqn{N}   is usually specified by its two marginal,  and possibly dependent, processes
\eqn{N_1} and \eqn{N_2}, which are the  observed processes. However,  \eqn{N} can be  decomposed into 
three independent indicator processes: \eqn{N_{(1)}}, \eqn{N_{(2)}}  
and \eqn{N_{(12)}}, which are the processes of the points  occurring
only in the first  marginal process, only in the second and  in both of them (simultaneous points).
The union of  \eqn{N_{(1)}}  and \eqn{N_{(12)}}, and \eqn{N_{(2)}}  
and \eqn{N_{(12)}}  gives  respectively the two marginal processes. 



 The  points in the  marginal  \eqn{N_{1}}, \eqn{N_{2}}  and indicator  
\eqn{N_{(1)}}, \eqn{N_{(2)}}  and \eqn{N_{(12)}}  processes can be optionally plotted. 
If  \code{date} is NULL, default axis are used. Otherwise, the values in \code{axispoints} are used 
as the  points in the time index where axis ticks and labels, from  the first column in \code{date},  
have to be drawn.  If  \code{axispoints} is NULL, a default grid of points  is built  using  the 
function \code{marca}.
}

\value{A list with components
  \item{Px1}{Vector of the  occurrence points  in \eqn{N_{(1)}}.}
  \item{Px2}{Vector of the  occurrence points  in \eqn{N_{(2)}}.}
  \item{Px12}{Vector of the  occurrence points  in \eqn{N_{(12)}}.}
  \item{N1}{Input argument.}
  \item{N2}{Input argument.}
  \item{date}{Input argument.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2015). A Bootstrap Test of Independence Between Three Temporal Nonhomogeneous Poisson Processes 
and its Application to Heat Wave Modeling. \emph{Environmental and Ecological Statistics}, 22(1), 127-144.
}


\seealso{
\code{\link{CPSPPOTevents}}, \code{\link{PlotMCPSP}}, \code{\link{PlotICPSP}}
}


\examples{

set.seed(123)
X<-as.numeric(runif(100)<0.10)
set.seed(124)
Y<-as.numeric(runif(100)<0.15)

aux<-CPSPpoints(N1=X,N2=Y)
}