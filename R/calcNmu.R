calcNmu <-
function(Ipos,lambda2,pos2)
{
	I1<-ceiling(Ipos[1])
	I2<-floor(Ipos[2])
	if(I1==0) mui<-(I2>I1)*sum(lambda2[(I1+1):I2])+lambda2[(I2+1)]*(Ipos[2]-I2)
	else if (I2==length(lambda2)) mui<-(I2>I1)*sum(lambda2[(I1+1):I2])+lambda2[I1]*(I1-Ipos[1])
	     else mui<-(I2>I1)*sum(lambda2[(I1+1):I2])+
		lambda2[I1]*(I1-Ipos[1])+lambda2[(I2+1)]*(Ipos[2]-I2)-
		(I2<I1)*lambda2[I1]*(I1-I2)
# several cases have to be dealt differently:s I1+1<=I2, I1=I2 or I1>I2  and other case I1=0
#mui is the integral of lambda in the interval defined by Ipos
	Ni<-sum((pos2>=Ipos[1])&(pos2<=Ipos[2]))
	return(c(mui,Ni))
}
