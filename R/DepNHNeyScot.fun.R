DepNHNeyScot.fun <-
function(lambdaParent,d, lambdaNumP=1,  dist='normal',sigmaC=1, minC=-1,
	maxC=1, fixed.seed=NULL)
{
#This function generates three dependent  trajectories based on a Neyman-Scott model
#using the same trajectory of the underllying PP to generate d trajectories
#with number of sons Poisson ( with different means to obtain different number of points

	if (length(lambdaNumP)==1) lambdaNumP<-rep(lambdaNumP,d)
	if (length(sigmaC)==1) sigmaC<-rep(sigmaC,d)
	if (length(maxC)==1) maxC<-rep(maxC,d)
	if (length(minC)==1) minC<-rep(minC,d)
	if ( ( (length(lambdaNumP)==d)*(length(sigmaC)==d)*(length(minC)==d)*(length(maxC))==d ) ==0) 
		stop('The lenght of the arguments lambdaNumP, sigmaC, minC and maxC must be 1 or d')
	posParent<-simNHPc.fun(lambda=lambdaParent, fixed.seed=fixed.seed)$posNH
	Tf<-length(lambdaParent)
	posNHs<-list(PP1=NULL)
	for (i in c(1:d)) 
	{	if (!is.null(fixed.seed)) fixed.seed<-fixed.seed+1
		posNHs[[i]]<-GenSons.fun(posParent, lambdaNumP=lambdaNumP[i], dist=dist, 
		sigmaC=sigmaC[i],minC=minC[i],maxC=maxC[i], Tf=Tf,fixed.seed=fixed.seed)
	}
	names(posNHs)<-paste('PP',c(1:d), sep='')
	return(posNHs)
	
}
