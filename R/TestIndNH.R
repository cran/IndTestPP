TestIndNH <-
function(posx, posy, posz=NULL, alpha=0.05, nsim=100,PA=FALSE,cores=1, 
	type='Poisson',lambdaMarg=NULL, lambdaParent=NULL, lambdaNumP=NULL, 
	 dist='normal',sigmaC=1, minC=-1, maxC=1,fixed.seed=NULL)
{

	NumProcess<-2+(is.null(posz)==F) 

	if (is.null(lambdaMarg)) Tf<-length(lambdaParent)
		else Tf<-dim(lambdaMarg)[1]
	n<-length(posx)
	distObs<-DistObs(posx=posx,posy=posy,posz=posz, Tf=Tf, 
		drawpoints='F', PA=PA)

	cl<-makeCluster(cores)
	clusterExport(cl, c('simNHPc'))
	clusterExport(cl, objects(, envir = .GlobalEnv))

	if (is.null(fixed.seed)) matdist<- parSapply(cl, c(1:nsim), FUN=fn2, posx=posx, NumProcess=NumProcess, 
		PA=PA, type=type, dist=dist,lambdaMarg=lambdaMarg, lambdaParent=lambdaParent,
		lambdaNumP=lambdaNumP, 	sigmaC=sigmaC,minC=minC, maxC=maxC)
	else 	matdist<- parSapply(cl, c(1:nsim), FUN=fn2fix, posx=posx, NumProcess=NumProcess, 
		PA=PA, type=type, dist=dist,lambdaMarg=lambdaMarg, lambdaParent=lambdaParent,
		lambdaNumP=lambdaNumP, 	sigmaC=sigmaC,minC=minC, maxC=maxC, fixed.seed=fixed.seed)

	matdistT<-cbind(distObs,matdist)
	matperT<-parSapply(cl, c(1:length(distObs)), FUN=mirank,mat=matdistT)/(nsim+1)
	KSest<-parSapply(cl, c(1:(nsim+1)), FUN=miKS,mat=matperT)
	stopCluster(cl)

	KSpv<-1-rank(KSest)[1]/(nsim+1)
	names(KSpv)<-"p-value"
	reject<-as.numeric(KSpv<alpha)


	return(list(pv=KSpv, reject=reject, est=KSest))
}
