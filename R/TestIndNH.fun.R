TestIndNH.fun <-
function(posx, posy, posz=NULL, NumProcess=2, alpha=0.05, nsim=100,PA=FALSE,cores=1, 
	type='Poisson',lambdaMarg=NULL, lambdaParent=NULL, lambdaNumP=NULL, 
	 dist='normal',sigmaC=1, minC=-1, maxC=1,fixed.seed=NULL)
{
#This function is a version of testINPInhomSep.fun adapted to perform simulations to calculate
#the power of the test

	if (is.null(lambdaMarg)) Tf<-length(lambdaParent)
		else Tf<-dim(lambdaMarg)[1]
	n<-length(posx)
	distObs<-DistObs.fun(posx=posx,posy=posy,posz=posz, Tf=Tf, 
		drawpoints='F', PA=PA)

	cl<-makeCluster(cores)
#	clusterExport(cl, c('miKS.fun','mirank.fun','DistSim.fun','simNHP.fun','buscar','uniongentri.fun',
#		'genbiPos.fun','gentriPos.fun','calcdist.fun','DistObs.fun'))
	clusterExport(cl, c('simNHPc.fun'))
	clusterExport(cl, objects(, envir = .GlobalEnv))

	if (is.null(fixed.seed)) matdist<- parSapply(cl, c(1:nsim), FUN=fn2, posx=posx, NumProcess=NumProcess, 
		PA=PA, type=type, dist=dist,lambdaMarg=lambdaMarg, lambdaParent=lambdaParent,
		lambdaNumP=lambdaNumP, 	sigmaC=sigmaC,minC=minC, maxC=maxC)
	else 	matdist<- parSapply(cl, c(1:nsim), FUN=fn2fix, posx=posx, NumProcess=NumProcess, 
		PA=PA, type=type, dist=dist,lambdaMarg=lambdaMarg, lambdaParent=lambdaParent,
		lambdaNumP=lambdaNumP, 	sigmaC=sigmaC,minC=minC, maxC=maxC, fixed.seed=fixed.seed)

	matdistT<-cbind(distObs,matdist)
	matperT<-parSapply(cl, c(1:length(distObs)), FUN=mirank.fun,mat=matdistT)/(nsim+1)
	KSest<-parSapply(cl, c(1:(nsim+1)), FUN=miKS.fun,mat=matperT)
	stopCluster(cl)

#	print(cbind(KSest,rank(KSest)) )
	KSpv<-1-rank(KSest)[1]/(nsim+1)
	reject<-as.numeric(KSpv<alpha)

	return(list(KSpv=KSpv, reject=reject, KSest=KSest))
}
