DepNHNeyScot <-
function(lambdaParent,d, lambdaNumP=1,  dist='normal',sigmaC=1, minC=-1,
	maxC=1, dplot=TRUE,fixed.seed=NULL,...)
{
	T<-length(lambdaParent)
	if (length(lambdaNumP)==1) lambdaNumP<-rep(lambdaNumP,d)
	if (length(sigmaC)==1) sigmaC<-rep(sigmaC,d)
	if (length(maxC)==1) maxC<-rep(maxC,d)
	if (length(minC)==1) minC<-rep(minC,d)
	if ( ( (length(lambdaNumP)==d)*(length(sigmaC)==d)*(length(minC)==d)*(length(maxC))==d ) ==0) 
		stop('The lenght of the arguments lambdaNumP, sigmaC, minC and maxC must be 1 or d')
	posParent<-simNHPc(lambda=lambdaParent, fixed.seed=fixed.seed)$posNH
	posNH<-list(N1=NULL)
	sizeCl<-list(size1=NULL)
	for (i in c(1:d)) 
	{	if (!is.null(fixed.seed)) fixed.seed<-fixed.seed+1

		gs<-GenSons(posParent, lambdaNumP=lambdaNumP[i], dist=dist, 
		sigmaC=sigmaC[i],minC=minC[i],maxC=maxC[i], Tf=T,fixed.seed=fixed.seed)
		posNH[[i]]<-gs$pos
		sizeCl[[i]]<-gs$sC
	}
	names(posNH)<-paste('N',c(1:d), sep='')
	names(sizeCl)<-paste('size',c(1:d), sep='')
	if (dplot==TRUE) PlotMargP(listpos=posNH,T=T,...)
	return(list(posNH=posNH, sizeCl=sizeCl))
	
}
