IndNHPP.fun <-
function (lambdas,fixed.seed=NULL)
{

		if (is.matrix(lambdas)==F) stop('Argument lambdas must be a matrix')
		d<-dim(lambdas)[2]
		posNHs<-list(PP1=NULL)
		if (!is.null(fixed.seed))
		{
		for (i in c(1:d))	
		{
		posNHs[[i]]<-simNHPc.fun(lambdas[,i],fixed.seed=(fixed.seed+i))$posNH}
		}
		else
		{
		for (i in c(1:d))	{posNHs[[i]]<-simNHPc.fun(lambdas[,i])$posNH}
		}

	
	names(posNHs)<-paste('PP',c(1:d), sep='')
	return(posNHs)
}
