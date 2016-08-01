simHPc.fun <-
function (lambda,  nEv, fixed.seed = NULL) 
{

    if (!is.null(fixed.seed)) set.seed(fixed.seed)
    if(length(lambda)>1) stop('lambda must be a number')
    if (is.null(nEv)) stop ('nEv must be specified')
	
    posH<-cumsum(rexp(nEv,lambda))
    T<-posH[nEv]

    return(list(posH = posH, T=T,  fixed.seed = fixed.seed))
}
