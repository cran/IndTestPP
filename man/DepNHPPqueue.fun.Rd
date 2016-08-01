\name{DepNHPPqueue.fun}
\alias{DepNHPPqueue.fun}

\title{Generates trajectories of dependent  Poisson processes based on queue simulation}

\description{This function  generates \eqn{d} dependent  (homogeneous or nonhomogeneous) Poisson processes.  
 using  \eqn{d-1}  queues in tandem. }

\usage{DepNHPPqueue.fun(lambda, d, rate = 1,type="NH", T=NULL, nEv ,fixed.seed=NULL)}

\arguments{
  \item{lambda}{Numeric value or vector. Intensity  of  the first PP. If  its length is 1,
   homogeneous PPs are generated.}
  \item{d}{Integer. Number of dependent processes to be generated.}
  \item{rate}{Optional. Numeric value or vector. Parameters (inverse of the mean) of the exponential 
distributions used to generate the time services, see \code{\link{rexp}}. If its length is 1, 
the same parameter is used  to generate  the time services in  the \eqn{d-1}  steps.}
  \item{type}{Optional. Labels "H"  or "NH". Type of the processes,homogeneous or nonhomogeneous.  
If type="H", the length of lambda must be 1.}
  \item{T}{Optional. Positive integer. Length of the period where the point are  going to be generated. 
	Only used if type="H" .}
  \item{nEv}{Optional. Positive integer. Number of points to be generated in the PPs. Only used if type="H".}
  \item{fixed.seed}{Optional. An integer or NULL. Value used to set the seed 
in random generation processes; if it is NULL, a random seed is used.}
}

\details{
The generation of dependent homogeneous PPs is based on  Burke s theorem,  Burke (1956),  
which states that  in  a stationary system formed by a tandem of M M 1  queues   
with  input intensity \eqn{\lambda} and exponential  time services, the input and the output 
processes of the first queue and  the output of the second are three dependent PPs with  the same intensity 
\eqn{\lambda}.  

Points are generated in continuous time, using the  following procedure: first the points  of a PP  and their 
time services are generated, and  their  corresponding 
output times obtained. The output times are the  occurrence times of the second process. 
Using them as input times of the following service and 
generating  new time services, the second output times, which are the occurrence times of the third PP,
are obtained.  The last step is repeated up to obtain \eqn{d} dependent PPs.

Analogously, the generation of dependent NHPPs  is based on  a result by Keilson and Servi (1994), 
which states that the output process from a M(t) G 1  queue is also a PP  with a
NH intensity equal to the convolution  \eqn{\lambda_{out}(t)=\lambda_{inp}(t)*f(t)},  
where  \eqn{\lambda_{inp}(t)} is the input intensity and \eqn{f(t)}  the density function of the
service time, in this case  an exponential distribution.

In the homogeneous processes, the argument \eqn{\lambda} can be  an integer or
a vector with equal values. In the first case,  the argument \code{nEv} must be specified;  in the second,
the length of the vector determines the length of period where the points are 
generated (as in the nonhomogeneous case).

}


\value{A list with elements

  \item{posNHs}{A list of  \eqn{d} vectors,  containing the occurrence points in each PP. The name of the elements of the list are PP1, PP2,..., PPd.}
  \item{lambdaM}{A \eqn{d}-column matrix containing the  intensity vectors of  the \eqn{d} dependent 
processes.}
}

\references{
Abaurrea, J. Asin, J. and Cebrian, A.C. (2014). A Bootstrap Test of Independence Between Three Temporal 
Nonhomogeneous Poisson Processes and its Application to Heat Wave Modeling. 
\emph{Environmental and Ecological Statistics}, 22(1), 127-144.

Burke, P. J. (1956). The Output of a Queuing System. \emph{Operations Research}. 4(6), 699-704.

Keilson, J. Servi, L.D. (1994). Networks of nonhomogeneous
M G \eqn{\infty}. \emph{J. Appl. Probab.}, 31, 157-68.
}

\seealso{\code{\link{IndNHPP.fun}}, \code{\link{DepNHNeyScot.fun}}, 
\code{\link{DepMarkedNHPP.fun}}, \code{\link{DepNHCPSP.fun}}  }

\examples{
#Generation of 3 dependent HPPs, with  mean service time equal to 10
aux<-DepNHPPqueue.fun(lambda=0.05, d=3, rate=0.1, type="H", nEv=25, 
	fixed.seed=123)
aux$posNHs

#Generation of 3 dependent NHPPs, with  mean service time equal to 10
#lambda<-runif(200,0,0.1)

#aux<-DepNHPPqueue.fun(lambda=lambda, d=3, rate=0.1, type="NH")
#aux$posNHs

}
