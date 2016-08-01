DepMarkedNHPP.fun <-
function(lambdaTot,  MarkovM,  inival=1,fixed.seed=NULL)
{

	posNH<-simNHPc.fun(lambda=lambdaTot,fixed.seed=fixed.seed)$posNH
	npos<-length(posNH)
	if (dim(MarkovM)[1]!=dim(MarkovM)[2]) stop('MarkovM must be a square matrix')
	d<-dim(MarkovM)[1]

	indice<-c(1:d)
	if (!is.null(fixed.seed)) set.seed(fixed.seed)
	aux<-rmultinom(1,1,MarkovM[inival,])
	marca<-indice[aux==1]
	marca<<-indice[aux==1]

	auxDepMarked.fun <-function(j,  MarkovM, indice, fixed.seed=NULL)
	{
	      	if (!is.null(fixed.seed)) set.seed((fixed.seed+j))
			aux<-rmultinom(1,1,MarkovM[marca[j],])
			auxmarca<-indice[aux==1]
			marca<<-c(marca,auxmarca)
	}
	bas<-sapply(c(1:(npos-1)), FUN=auxDepMarked.fun, MarkovM=MarkovM, indice=indice,
		fixed.seed=fixed.seed)
	return(list(posNH=posNH, mark=marca, lambdaTot=lambdaTot, MarkovM=MarkovM))
}
