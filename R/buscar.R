buscar <-
function (x, lambda, lambdacum) 
{
    posNHi <- sum(lambdacum <= x) 
    if (posNHi>0)    posNH<-(posNHi+(x-lambdacum[posNHi])/lambda[posNHi+1])
    else posNH<-x/lambda[posNHi+1]
	
    return(posNH)
}
