DutilleulPlot.fun <-
function(posx, posy, lambday, nsim=1000,lenve=c(0.025, 0.975),...)
{


	distObsD<-nearestdist.fun(posx, posy)
	nn<-length(distObsD[complete.cases(distObsD)])
	sec<-c(1:nn)/nn
	quantobs<-quantile(distObsD, probs=sec, na.rm=T)

	matdistD<-sapply(c(1:nsim), FUN=fn3, posx=posx, lambday=lambday)
	matperD<-apply(matdistD, MARGIN=2, FUN=quantile, probs=sec, na.rm=T )
	enve1<-apply(matperD, MARGIN=1, FUN=quantile, probs=lenve[1], na.rm=T )
	enve2<-apply(matperD, MARGIN=1, FUN=quantile, probs=lenve[2], na.rm=T )

	plot(quantobs, sec,  type='s', xlab='distance', ylab='cum. rel. freq.',... )
	lines((enve1), sec,  type='s', col='red', lty=2)
	lines(enve2, sec,  type='s', col='red', lty=2)

    return(list(quantobs=quantobs, enve1=enve1, enve2=enve2))
}
