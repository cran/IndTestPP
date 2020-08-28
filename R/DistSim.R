DistSim <-
function(posx, NumProcess=2,type='Poisson',
		lambdaMarg=NULL,  #parameter for the 'Poisson' type
		lambdaParent=NULL, lambdaNumP=NULL, dist='normal', sigmaC=1,minC=-1,maxC=1, #parameters for the 'Cluster' type
		PA=FALSE,info=FALSE,...) 
{
#DistSim  generates the second and following  processes, and given the first process
#calculates de sets of close points and the corresponding  mean distance
#for each point X of the  first process

	if (length(sigmaC)==1) sigmaC<-c(sigmaC, sigmaC)
	if (length(minC)==1) minC<-c(minC, minC)
	if (length(maxC)==1) Max<-c(maxC, maxC)
	if (length(lambdaNumP)==1) lambdaNumP<-c(lambdaNumP, lambdaNumP)
	if (is.vector(lambdaParent)==TRUE) lambdaParent<-cbind(lambdaParent, lambdaParent)

	if(type=='Poisson')
	{
		posyNH<-simNHPc(lambda=lambdaMarg[,1])$posNH
		if (NumProcess==3) poszNH<-simNHPc(lambda=lambdaMarg[,2])$posNH
		else poszNH<-NULL
	}


	if(type=='PoissonCluster') 
	{
		Tf<-length(lambdaParent)
		posTy<-simNHPc(lambda=lambdaParent[,1])$posNH
		posyNH<-GenSons(posTy, lambdaNumP=lambdaNumP[1], dist=dist,sigmaC=sigmaC[1], 
			minC=minC[1], maxC=maxC[1], Tf=Tf)
		if (NumProcess==3){
			posTz<-simNHPc(lambda=lambdaParent[,2])$posNH
			poszNH<-GenSons(posTz,lambdaNumP=lambdaNumP[2], dist=dist,sigmaC=sigmaC[2], 
			minC=minC[2], maxC=maxC[2], Tf=Tf)
		} else poszNH=NULL
	}

	DistTri<-DistObs(posx=posx,posy=posyNH,posz=poszNH, info=info,
		 PA=PA,procName=c('ObsX','SimY','SimZ'),...)

	return(DistTri)
}
