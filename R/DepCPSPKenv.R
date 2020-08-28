DepCPSPKenv <-
function(i,lambdaix, lambdaiy, lambdaixy,lambdax, lambday, r=NULL, T=NULL,typeEst, fixed.seed=NULL)
{
	if (!is.null(fixed.seed)) fixed.seed2<-fixed.seed+i
	auxpos<-DepNHCPSP(lambdaiM=cbind(lambdaix, lambdaiy, lambdaixy), d=2 ,dplot=FALSE, 
		fixed.seed=fixed.seed2)$posNH

	Kaux<-NHKaux(lambdaC=lambdax, lambdaD=lambday,T=T, posC=auxpos$N1,
		 posD=auxpos$N2, typeC=rep(1,length(auxpos$N1)), typeD=rep(1,length(auxpos$N2)), r=r, typeEst=typeEst)
	return(Kaux)
}
