DepNHCPSP <-
function (lambdaiM, d,dplot=TRUE, pmfrow=c(2,1), fixed.seed = NULL, ...) 
{
    if (!is.matrix(lambdaiM))    stop("lambdaiM must be a matrix")
    nd<-dim(lambdaiM)[2]
    i<-c(1:d)
    ncomb<-sum(choose(d,i))
    if (nd!=ncomb) stop(" number of columns of lambdaiM must be the sum of the number of k-combinations of d")

    T<-dim(lambdaiM)[1]

    listpos<-lapply(c(1:nd), FUN=PsimNHPc, lambda=lambdaiM, fixed.seed = fixed.seed)
 
    comb<-combn(d,1, simplify=FALSE)
    for (i in c(2:d))
    {
	    combi<-combn(d,i, simplify=FALSE)
          comb<-c(comb, combi)
    }

ele<-function(j, i, comb, listpos,lambdaiM)
{
	if (is.element(i, comb[[j]]))	
	{
		auxxx<<-union(auxxx,listpos[[j]])
            lambdaM[,i]<<-lambdaM[,i]+ lambdaiM[,j]
	}
}

    N<-NULL
    lambdaM<-matrix(rep(0, d*T), ncol=d)
    for  (i in c(1:d))
    {
      m<-length(comb)
	auxxx<<-NULL
	auxxx<-NULL
      bas<-sapply(c(1:m), FUN=ele, i=i, comb=comb, listpos=listpos,lambdaiM=lambdaiM)
	N[[i]]<-sort(auxxx)
    }

	names(N)<-paste('N',c(1:d), sep='')

    if (dplot==TRUE)
	if (d==2)
	{
		par(mfrow=pmfrow)
		PlotMCPSP(N[[1]],N[[2]], T=T,...)
		PlotICPSP(listpos[[1]],listpos[[2]],listpos[[3]], T=T,...)
    	}
     else PlotMargP(listpos=N,T=T,...)

    return(list(posNH=N, NInd=listpos,lambdaM=lambdaM))
}
