TestIndLS.fun <-
function(posx, posy, posz=NULL,T, alpha=0.05, nTrans=100,PA=FALSE,cores=1, fixed.seed=NULL)
{
	NumProcess<-2+(is.null(posz)==F) 
	n<-length(posx)
	distObs<-DistObs.fun(posx=posx,posy=posy,posz=posz, info='F', PA=PA)

	cl<-makeCluster(cores)
	clusterExport(cl, objects(, envir = .GlobalEnv))

	if (is.null(fixed.seed)==F) set.seed(fixed.seed)

	shi1<-round(runif(nTrans,1,(T-1)))
	
	if(NumProcess==3)
	{
		if (is.null(fixed.seed)==F) set.seed((fixed.seed+1))
		shi2<-round(runif(nTrans,1,(T-1))) 
	}
	else {shi2<-NULL}
	

#	matdist<- sapply( c(1:nTrans), FUN=fn2B, posx=posx, posy=posy, posz=posz,NumProcess=NumProcess,  
	matdist<- parSapply(cl, c(1:nTrans), FUN=fn2B, posx=posx, posy=posy, posz=posz,NumProcess=NumProcess,
		PA=PA, shi1=shi1, shi2=shi2, T=T)

	matdistT<-cbind(distObs,matdist)
	matperT<-parSapply(cl, c(1:length(distObs)), FUN=mirank.fun,mat=matdistT)/(nTrans+1)
	KSest<-parSapply(cl, c(1:(nTrans+1)), FUN=miKS.fun,mat=matperT)

	stopCluster(cl)

	KSpv<-1-rank(KSest)[1]/(nTrans+1)
	reject<-as.numeric(KSpv<alpha)

	return(list(KSpv=KSpv, reject=reject, KSest=KSest))
}
