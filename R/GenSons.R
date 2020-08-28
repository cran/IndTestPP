GenSons <-
function(posT, lambdaNumP, Tf, sigmaC=1, minC=-1, maxC=1,dist='normal',fixed.seed=NULL)
{
	np<-length(posT)
	if (!is.null(fixed.seed)) set.seed(fixed.seed)
	nP<-rpois(np,lambda=lambdaNumP)
	snP<-sum(nP)
	if (!is.null(fixed.seed)) set.seed(fixed.seed)
      if (dist=='normal') ndis<-rnorm(snP,mean=0, sd=sigmaC) 
	if (dist=='uniform') ndis<-runif(snP,min=minC, max=maxC)
	posTaux<-rep(posT, times=nP)
	pos<-ndis+posTaux
	pos<-sort(pos[(pos>0)&(pos<=Tf)])
	return(list(pos=pos, sC=nP))
}
