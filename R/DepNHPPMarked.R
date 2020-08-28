DepNHPPMarked <-
function(lambdaTot,  MarkovM,  inival=1, dplot=TRUE, fixed.seed=NULL,...)
{

	posNHG<-simNHPc(lambda=lambdaTot,fixed.seed=fixed.seed)$posNH
	npos<-length(posNHG)
	if (dim(MarkovM)[1]!=dim(MarkovM)[2]) stop('MarkovM must be a square matrix')
	d<-dim(MarkovM)[1]

	indice<-c(1:d)
	if (!is.null(fixed.seed)) set.seed(fixed.seed)
	aux<-rmultinom(1,1,MarkovM[inival,])
	marca<-indice[aux==1]
#	marca<<-indice[aux==1]

	auxDepMarked <-function(j,  MarkovM, indice, fixed.seed=NULL)
	{
	      	if (!is.null(fixed.seed)) set.seed((fixed.seed+j))
			aux<-rmultinom(1,1,MarkovM[marca[j],])
			auxmarca<-indice[aux==1]
			marca<<-c(marca,auxmarca)
	}
	bas<-sapply(c(1:(npos-1)), FUN=auxDepMarked, MarkovM=MarkovM, indice=indice,
		fixed.seed=fixed.seed)

	posNH<-list(1)
	for (i in c(1:d))
	{
		posNH[[i]]<-posNHG[marca==i]
	}
	names(posNH)<-paste('N',c(1:d), sep='')
	if (dplot==TRUE)  PlotMargP(listpos=posNH,T=length(lambdaTot),...)

	return(list(posNH=posNH,posNHG=posNHG, mark=marca, lambdaTot=lambdaTot, MarkovM=MarkovM))
}
