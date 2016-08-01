DepNHCPSP.fun <-
function(lambdai1, lambdai2, lambdai12,fixed.seed=NULL)
{

	if ((length(lambdai1)!=length(lambdai12))||(length(lambdai2)!=length(lambdai12)))
		stop('Length of the three lambda vectors must be the same') 
	posNHi1<-simNHPc.fun(lambda=lambdai1,fixed.seed=fixed.seed)$posNH
	posNHi2<-simNHPc.fun(lambda=lambdai2,fixed.seed=fixed.seed)$posNH
	posNHi12<-simNHPc.fun(lambda=lambdai12,fixed.seed=fixed.seed)$posNH

	posNH1<-sort(c(posNHi1, posNHi12))
	posNH2<-sort(c(posNHi2, posNHi12))
	lambda1<-lambdai1+lambdai12
	lambda2<-lambdai2+lambdai12

	return(list(posNH1=posNH1, posNH2=posNH2,posNHi1=posNHi1,posNHi2=posNHi2,
	posNHi12=posNHi12,lambda1=lambda1, lambda2=lambda2))
}
