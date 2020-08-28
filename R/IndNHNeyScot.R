IndNHNeyScot <-
function(lambdaParent, d,lambdaNumP=1,  dist='normal',sigmaC=1, minC=-1,
	maxC=1, dplot=TRUE, fixed.seed=NULL,...)
{
#This function generates independent  trajectories based on a Neyman-Scott model
#with number of sons Poisson ( with different means to obtain different number of points
#If d=1  a univariate Neyman-Scott process is generated

	if (length(lambdaNumP)==1) lambdaNumP<-rep(lambdaNumP,d)
	if (length(sigmaC)==1) sigmaC<-rep(sigmaC,d)
	if (length(maxC)==1) maxC<-rep(maxC,d)
	if (length(minC)==1) minC<-rep(minC,d)
	if ( ( (length(lambdaNumP)==d)*(length(sigmaC)==d)*(length(minC)==d)*(length(maxC))==d ) ==0) 
		stop('The lenght of the arguments lambdaNumP, sigmaC, minC and maxC must be 1 or d')
	T<-length(lambdaParent)
	posNH<-NULL #list(N1=NULL)
	
	if (!is.null(fixed.seed))
	{for (i in c(1:d)) 
	{
		posParent<-simNHPc(lambda=lambdaParent, fixed.seed=(fixed.seed+i))$posNH
		posNH[[i]]<-GenSons(posParent, lambdaNumP=lambdaNumP[i], dist=dist, sigmaC=sigmaC[i],
			minC=minC[i],maxC=maxC[i], Tf=T,fixed.seed=(fixed.seed+i))$pos
	}
	} else
	{for (i in c(1:d)) 
	{
		posParent<-simNHPc(lambda=lambdaParent)$posNH
		posNH[[i]]<-GenSons(posParent, lambdaNumP=lambdaNumP[i], dist=dist, sigmaC=sigmaC[i],
			minC=minC[i],maxC=maxC[i], Tf=T)$pos
	}
	}
	names(posNH)<-paste('N',c(1:d), sep='')
	if (dplot==TRUE) PlotMargP(listpos=posNH,T=T,...)
	return(posNH)
}
