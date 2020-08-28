\name{IndNHPP}
\alias{IndNHPP}


\title{Generates trajectories of independent Poisson processes}

\description{This function  generates independent  Poisson processes, which can be homogeneous or nonhomogeneous 
depending on the  value of the intensity vectors. }

\usage{IndNHPP(lambdas,dplot=TRUE, fixed.seed=NULL,...)}

\arguments{
  \item{lambdas}{Matrix where each column contains the  intensity vector to generate a Poisson process.}
  \item{dplot}{Optional. A logical flag. If it is TRUE,  the  marginal  processes  are plotted.}
  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}
  \item{...}{Further arguments to  be passed to the function \code{\link{plot}}.}
}


\details{
The number of generated processes is determined by the number of columns of the argument \code{lambdas}. The lenght of the 
period where the processes are generated is  determined by the number of rows of \code{lambdas}.

Homogenous  processes  are  generated if the corresponding intensity vector is  constant (that is if  all the rows 
of the corresponding column are equal).

For the generation algorithm of each Poisson process, see \code{\link{simNHPc}}.
}

\value{

\item{posNHs}{A list of  d vectors, each one containing the time occurrences of  the independent NHPPs. The name of the 
	elements of the list are N1, N2,..., Nd.}

}

\seealso{\code{\link{IndNHNeyScot}}, \code{\link{simNHPc}}}

\examples{
set.seed(123)
lambdas<-cbind(runif(500)/10, rep(0.05,500))

IndNHPP(lambdas=lambdas, fixed.seed=123)

}


