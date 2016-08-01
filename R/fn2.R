fn2 <-
function(i,posx, PA=FALSE, NumProcess=2, type='Poisson', dist='normal', 
		lambdaMarg=NULL, lambdaParent=NULL, lambdaNumP=NULL, sigmaC=1, 
		minC=-1, maxC=1)
{
	aux<-DistSim.fun(posx=posx, NumProcess=NumProcess, type=type,  
		dist=dist, lambdaMarg=lambdaMarg, 
		lambdaParent=lambdaParent, lambdaNumP=lambdaNumP, sigmaC=sigmaC, 
		minC=minC, maxC=maxC,  PA=PA, info=FALSE)
	return(aux)
}
