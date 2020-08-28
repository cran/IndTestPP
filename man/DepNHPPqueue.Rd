\name{DepNHPPqueue}
\alias{DepNHPPqueue}
\alias{DepNHPPqueueI}
\alias{DepNHPPqueue1}

\title{Generating   dependent point processes by a tandem queueing network}

\description{It  generates \eqn{d} dependent  (homogeneous or nonhomogeneous)  Poisson processes  
 using  \eqn{d-1}  queues in tandem.}

\usage{DepNHPPqueue(lambda, d,   T=NULL,  nEv=NULL,  nserv="infty", Clambda=TRUE,
	            ddist='exp',argd=1,dplot=TRUE, fixed.seed=NULL, ...)}

\arguments{
  \item{lambda}{Numeric value or vector. Intensity  of  the first Poisson process. If  its length is 1,
   homogeneous processes are generated.}
  \item{d}{Integer. Number of dependent processes to be generated.}
  \item{T}{Optional. Positive integer. Length of the period where the point are  going to be generated. 
	Only used in homogeneous processes (if lambda is a constant).}
  \item{nEv}{Optional. Positive integer. Number of points to be generated in the processs. Only used in homogeneous processes.}
  \item{Clambda}{Optional logical  flag. Only used in nonhomogeneous processes. If it is TRUE, the intensity vectors of the generated point 
	 processes is calculated.}
  \item{nserv}{Optional.  Number of servers in the queue system; only two  values are possible: 1 or "infty" if the  number of servers is infinity.}
  \item{ddist}{Character string.  Identification of the probability distribution of the  serving time. It must be one 
	of the  names of the probability distributions available in the stats package, 
	see \code{\link{Distributions}}; for example "gamma", "geom", etc.}. 
  \item{argd}{Optional. A  value, vector or a matrix (with d-1 rows) containing the arguments to be used in the probability distribution  in \code{ddist}; see Details.}
  \item{dplot}{Optional. A logical flag. If it is TRUE,  the  marginal processes  are plotted.}
  \item{fixed.seed}{Optional. An integer or NULL. Value used to set the seed 
in random generation processes; if it is NULL, a random seed is used.}
  \item{...}{Further arguments to  be passed to the function \code{\link{plot}}.}

}

\details{

The generation algorithm is described in Cebrian et al. (2020).
The generation of dependent homogeneous Poisson processes   can be  based on  a  M\ M\ 1 or a M\ M \ \eqn{\infty}  
queue, and the intensity of each output process is equal to the intensity of the input process, see Burke(1956).
The generation of dependent nonhomogeneous Poisson processes  is based on  a  M(t)\ G \ \eqn{\infty}  queue 
 and the intensity of each output process is equal to the convolution  \eqn{\lambda_{out}(t)=\lambda_{inp}(t)*f(t)},  
where  \eqn{\lambda_{inp}(t)} is the input intensity and \eqn{f(t)}  the density function of the
service time, see Keilson and Servi (1994).

In the homogeneous processes, the argument \eqn{\lambda} can be  an integer or
a vector with equal values. In the first case,  the argument \code{nEv} or \code{T} must be specified;  in the second,
the length of the vector determines the length of period where the points are 
generated (as in the nonhomogeneous case).

The  serving times in all the queues must have a probability distribution in the same family (\code{ddist}) 
but the parameters of that distribution can be different in each queue. If the parameters are  the same in all the queues, 
argument \code{argd}  is a numeric value (if there is only one parameter) or a  vector; if the parameters are different in each
queue,  \code{argd}  must be a matrix with (d-1) rows, even if there is only one parameter it must be  a (d-1) \eqn{\times} 1 matrix.

The occurrence times  in each  output process are sorted, so that the output time in row i does not always 
correspond to the input time in the same row.  The number of points in the output process may be lower than  in the input (if some outputs 
have not been observed).

}


\value{A list with elements

  \item{posNH}{A list of  \eqn{d} vectors,  containing the occurrence points in each point process. The name of the 
	elements in the list are N1, N2,..., Nd.}
  \item{lambdaM}{A \eqn{d}-column matrix containing the  intensity vectors of  the \eqn{d} dependent processes,  in nonhomogeneous processes and
	if \code{Clambda=TRUE}. Otherwise, the input argument \code{lambda}.}
}

\references{


Burke, P. J. (1956). The Output of a Queuing System. \emph{Operations Research}. 4(6), 699-704.

Cebrian, A.C., Abaurrea, J.  and Asin, J. (2020).   Testing  independence between two  point processes in time.
 \emph{Journal of Simulation and Computational Statistics}

Keilson, J. Servi, L.D. (1994). Networks of nonhomogeneous
M G \eqn{\infty}. \emph{J. Appl. Probab.}, 31, 157-68.
}

\seealso{\code{\link{IndNHPP}}, \code{\link{DepNHNeyScot}}, 
\code{\link{DepNHPPMarked}}, \code{\link{DepNHCPSP}}  }

\examples{

#Generation of 3 dependent HPPs, with  one server  and exponential service time equal to 10
aux<-DepNHPPqueue(lambda=0.05, d=3,  nEv=25,fixed.seed=123,nserv=1, argd=0.1)
aux$posNH

#Generation of 4 dependent NHPPs, with infinity servers and different mean service times
#at each queue
lambda<-runif(200,0,0.1)
aux<-DepNHPPqueue(lambda=lambda, d=4, fixed.seed=123, argd=cbind(c(0.1, 0.3, 0.1)))
aux$posNH


#Generation of 3 dependent NHPPs, with infinity servers and  Gamma service times 
#with different parameters at each queue
lambda<-runif(200,0,0.1)
aux<-DepNHPPqueue(lambda=lambda, d=3, ddist='gamma',fixed.seed=123, 
                  argd=cbind(c(0.1, 0.3), c(1,2)))
aux$posNH

}
