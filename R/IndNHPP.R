IndNHPP <-
function (lambdas,dplot=TRUE, fixed.seed=NULL,...)
{

		if (is.matrix(lambdas)==F) stop('Argument lambdas must be a matrix')
		d<-dim(lambdas)[2]
		T<-dim(lambdas)[1]
		posNHs<-list(PP1=NULL)
		if (!is.null(fixed.seed))
		{
			for (i in c(1:d))	
			{
			posNHs[[i]]<-simNHPc(lambdas[,i],fixed.seed=(fixed.seed+i))$posNH
			}
		}
		else
		{
			for (i in c(1:d))	{
			posNHs[[i]]<-simNHPc(lambdas[,i])$posNH
			}
		}
	names(posNHs)<-paste('N',c(1:d), sep='')
	
	if (dplot==TRUE) PlotMargP(listpos=posNHs, T=T, ...)
	return(posNHs)
}
