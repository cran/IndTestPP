NHKaux3 <-
function(j, lambdaC=lambdaC, lambdaD=lambdaD, posC=posC, typeC=typeC,  posD=posD, typeD=typeD, 
		T=T,ri=ri)
{
	Kv<-0
	u<-posC[j]
	if (sum(abs(posD-u)<=ri)>0)
	{
		posWD<-posD[abs(posD-u)<=ri]
		typeWD<-typeD[abs(posD-u)<=ri]
	
#	w<-(min(T,u+ri)-max(1,u-ri)+1)*T/(2*ri+1)#edge correction
		w<-T-abs(u-posWD) #edge correction Moller and Waagepetersen (2007)
		Kv<-sum(1/(w*lambdaD[cbind(ceiling(posWD),typeWD)]))
		Kv<-Kv/lambdaC[cbind(ceiling(u),typeC[j])]
	}
	return(Kv)
}
