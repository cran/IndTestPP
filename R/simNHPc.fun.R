simNHPc.fun <-
function (lambda, fixed.seed = NULL) 
{
    if (length(lambda)==1) stop('lambda must be a vector of length T even if the process is homogeneous')
    T<-length(lambda)
    lambdacum <- cumsum(lambda)
    lastposH <- sum(lambda)
    if (!is.null(fixed.seed))  set.seed(fixed.seed)
    ne<-20*length(lambda)*mean(lambda)
#the expected number of points is multiplied by 20 to guarantee that the whole
#observed period is covered  by the generating process
   

    if (max(abs(diff(lambda)))==0)
    {
	distexp <- rexp(ne, lambda[1])
 	posHaux <- cumsum(distexp)
    	if (sum(distexp)<T) stop('The generated points do not cover the observed period of the  Poisson process')
	posNH <- posHaux[posHaux < T]
     }
    else
    {
	distexp <- rexp(ne, 1)
        posHaux <- cumsum(distexp)
	if (sum(distexp)<lastposH) stop('The generated points do not cover the observed period of the  Poisson process')
	posH <- posHaux[posHaux < lastposH]
	posNH <- as.vector(apply(as.matrix(posH), MARGIN = 1, FUN = buscar, lambda,
        	lambdacum), mode='numeric')
    }

    return(list(posNH = posNH, lambda = lambda,  fixed.seed = fixed.seed))
}
