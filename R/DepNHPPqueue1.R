DepNHPPqueue1 <-
function(lambda, d, T=NULL, type,
	ent,Clambda=TRUE,ddist,argd, fixed.seed=NULL)
{


posNHs<-list(PP1=ent)
lambdaM<-cbind(lambda)
tt<-c(1:T)

procsal<-function(i,ent,ser)
{

sal[i]<<-(ent[i]+ser[i])*(ent[i]>sal[i-1])+
(sal[i-1]+ser[i])*(ent[i]<=sal[i-1])

}


for (i in c(2:d))
{
	ent<-posNHs[[i-1]]
	nEv<-length(ent)
	ser<-do.call(paste('r',ddist,sep=''), append(list(n=nEv),argd[i-1,]) )
	sal<-rep(0,nEv)
	sal<<-rep(0,nEv)
	sal[1]<-ent[1]+ser[1]
	sal[1]<<-ent[1]+ser[1]
	aux<-sapply(c(2:nEv),procsal,ent=ent, ser=ser)
	posNHs[[i]]<-sort(sal[sal<T])

	if ((type=='NH') &(Clambda==TRUE))
	{
	dendist<-do.call(paste('d',ddist,sep=''), append(list(x=tt),argd[i-1,]) )

	convlambda<-convolve(lambdaM[,(i-1)],rev(dendist),type='o')[1:T]
	lambdaM<-cbind(lambdaM, convlambda)
	}
}
names(posNHs)<-paste('N',c(1:d), sep='')

return(list(posNH=posNHs, lambdaM=lambdaM))
}
