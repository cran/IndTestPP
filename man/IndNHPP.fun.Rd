\name{IndNHPP.fun}
\alias{IndNHPP.fun}


\title{Generates trajectories of independent Poisson processes}

\description{This function  generates independent  Poisson processes (PPs), which can be homogeneous or nonhomogeneous 
depending on the  value of the intensity vectors. }

\usage{IndNHPP.fun(lambdas, fixed.seed=NULL)}

\arguments{
  \item{lambdas}{Matrix where each column contains the  intensity vector to generate a Poisson process}


  \item{fixed.seed}{An integer or NULL. If it is an integer, that is the value  used to set the seed 
in  random  generation processes. It it is NULL, a random seed is used.}
}


\details{
The number of generated processes is determined by the number of columns of the argument \code{lambdas}. The lenght of the 
period where the processes are generated is  determined by the number of rows of \code{lambdas}.

Homogenous  processes  are  generated if the corresponding intensity vector is  constant (that is if  all the rows 
of the corresponding column are equal).

For the generation algorithm of each PP, see \code{\link{simNHPc.fun}}.
}

\value{

posNHs{A list of  d vectors, each one containing the time occurrences of  the independent NHPP. The name of the 
	elements of the list are PP1, PP2...PPd}

}

\seealso{\code{\link{IndNHNeyScot.fun}}, \code{\link{simNHPc.fun}}}

\examples{
set.seed(123)
lambdas<-cbind(runif(500)/10, rep(0.05,500))

IndNHPP.fun(lambdas=lambdas, fixed.seed=123)

}


