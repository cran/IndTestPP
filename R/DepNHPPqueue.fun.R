DepNHPPqueue.fun <-
function(lambda, d, rate=1, type='NH', T=NULL,  nEv=NULL,fixed.seed=NULL)
{

	if (d<=1) stop('d must be an integer greater than 1')
	if (length(rate)==1)  rate<-rep(rate, (d-1))

	if ((length(lambda)>=2)&(type=='H'))
	{	
		stop('For homogenoeus processes, the length of lambda must be 1')
	}

	if ((length(lambda)==1)&(type=='NH'))
	{	
		type<-'H'
		cat('The argument lambda corresponds to a homogenous process  and type has been changed to H', fill=TRUE)
	}
	T<-length(lambda)
	if ( (length(lambda)>1)&(sum(diff(lambda))==0) ) lambda<-lambda[1]
	if (length(lambda)==1)  
	{
		entaux<-simHPc.fun(lambda, nEv=nEv,fixed.seed=fixed.seed)
		ent<-entaux$pos
		T<-ceiling(entaux$T)
	}
	else
	{
		if (is.null(T)==FALSE)	if (length(lambda)!=T) cat('T is not used since it is different from the length of lambda', fill=TRUE)
		ent<-simNHPc.fun(lambda=lambda,fixed.seed=fixed.seed)$posNH
	}

	posNHs<-list(PP1=ent)
	lambdaM<-cbind(lambda)
	tt<-c(1:T)

	procsal.fun<-function(i,ent,ser)
	{

		sal[i]<<-(ent[i]+ser[i])*(ent[i]>sal[i-1])+
			(sal[i-1]+ser[i])*(ent[i]<=sal[i-1])

	}


	for (i in c(2:d))
	{
		ent<-posNHs[[i-1]]
		nEv<-length(ent)
		ser<-rexp(nEv,rate[i-1])
		sal<-rep(0,nEv)
		sal<<-rep(0,nEv)
		sal[1]<-ent[1]+ser[1]
		sal[1]<<-ent[1]+ser[1]
		aux<-sapply(c(2:nEv),procsal.fun,ent=ent, ser=ser)
		posNHs[[i]]<-sal[sal<T]

		if (type=='NH') 
		{
			dExp<-rate[i-1]*exp(-tt*rate[i-1])
			convlambda<-convolve(lambdaM[,(i-1)],rev(dExp),type='o')[1:T]
			lambdaM<-cbind(lambdaM, convlambda)
		}
	}
	names(posNHs)<-paste('PP',c(1:d), sep='')

	return(list(posNHs=posNHs, lambdaM=lambdaM, T=T))
}
