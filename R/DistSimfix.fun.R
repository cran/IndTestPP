DistSimfix.fun <-
function(posx, NumProcess=2,type='Poisson',
		lambdaMarg=NULL,  #parameter for the 'Poisson' type
		lambdaParent=NULL, lambdaNumP=NULL, dist='normal', sigmaC=1,minC=-1,maxC=1, #parameters for the 'Cluster' type
		PA=FALSE,info=FALSE,fixed.seed=1,...) 
{
#DistSimfix.fun  generates the second and following  processes, and given the first process
#calculates de sets of close points and the corresponding  mean distance
#for each point X of the  first process. The differencewith DistSim is that here  the seed is fixed

	if (length(sigmaC)==1) sigmaC<-c(sigmaC, sigmaC)
	if (length(minC)==1) minC<-c(minC, minC)
	if (length(maxC)==1) Max<-c(maxC, maxC)
	if (length(lambdaNumP)==1) lambdaNumP<-c(lambdaNumP, lambdaNumP)
	if (is.vector(lambdaParent)==TRUE) lambdaParent<-cbind(lambdaParent, lambdaParent)

	if(type=='Poisson')
	{
		posyNH<-simNHPc.fun(lambda=lambdaMarg[,1],fixed.seed=fixed.seed)$posNH
		if (NumProcess==3) poszNH<-simNHPc.fun(lambda=lambdaMarg[,2],
			fixed.seed=(fixed.seed+100000))$posNH
		else poszNH=NULL
	}


	if(type=='PoissonCluster') 
	{
		Tf<-length(lambdaParent)
		posTy<-simNHPc.fun(lambda=lambdaParent[,1],fixed.seed=fixed.seed)$posNH
		posyNH<-GenSons.fun(posTy, lambdaNumP=lambdaNumP[1], dist=dist,sigmaC=sigmaC[1], 
			minC=minC[1], maxC=maxC[1], Tf=Tf,fixed.seed=fixed.seed)
		if (NumProcess==3){
			posTz<-simNHPc.fun(lambda=lambdaParent[,2],fixed.seed=(fixed.seed+100000))$posNH
			poszNH<-GenSons.fun(posTz,lambdaNumP=lambdaNumP[2], dist=dist,sigmaC=sigmaC[2], 
			minC=minC[2], maxC=maxC[2], Tf=Tf,fixed.seed=(fixed.seed+100000))
		} else poszNH=NULL
	}

	DistTri<-DistObs.fun(posx=posx,posy=posyNH,posz=poszNH, info=info,
		 PA=PA,procName=c('ObsX','SimY','SimZ'),...)

	return(DistTri)
}
