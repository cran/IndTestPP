simNHPc <-
function (lambda, fixed.seed = NULL, algor='Thinning') 
{
    if (length(lambda)==1) stop('lambda must be a vector of length T even if the process is homogeneous')
    T<-length(lambda)
    if (algor=='Inversion')
    {
    lambdacum <- cumsum(lambda)
    lastposH <- sum(lambda)

    ne<-20*length(lambda)*mean(lambda)
#the expected number of points is multiplied by 20 to guarantee that the whole
#observed period is covered  by the generating process
   

    if (max(abs(diff(lambda)))==0)
    {
     if (!is.null(fixed.seed))  set.seed(fixed.seed)
	distexp <- rexp(ne, lambda[1])
 	posHaux <- cumsum(distexp)
    	if (sum(distexp)<T) stop('The generated points do not cover the observed period of the  Poisson process')
	posNH <- posHaux[posHaux < T]
     }
    else
    {
     if (!is.null(fixed.seed))  set.seed(fixed.seed)
	distexp <- rexp(ne, 1)
        posHaux <- cumsum(distexp)
	if (sum(distexp)<lastposH) stop('The generated points do not cover the observed period of the  Poisson process')
	posH <- posHaux[posHaux < lastposH]
	posNH <- as.vector(apply(as.matrix(posH), MARGIN = 1, FUN = buscar, lambda,
        	lambdacum), mode='numeric')
    }
    }
    if (algor=='Thinning')
    {
    lambdamax<-max(lambda, na.rm=T)
    if (!is.null(fixed.seed))  set.seed(fixed.seed)
    np<-rpois(1, lambda=lambdamax*T)
    if (!is.null(fixed.seed))  set.seed(fixed.seed)
    posaux<-runif(np, 0, T)
    posNH<-sort(posaux[rbinom(length(posaux), 1, prob=(lambda[ceiling(posaux)]/lambdamax))==1])

    }

    return(list(posNH = posNH, lambda = lambda,  fixed.seed = fixed.seed))
}
