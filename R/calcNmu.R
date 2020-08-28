calcNmu <-
function(Ipos,lambday,posy)
{
	I1<-ceiling(Ipos[1])
	I2<-floor(Ipos[2])
	if(I1==0) mui<-(I2>I1)*sum(lambday[(I1+1):I2])+lambday[(I2+1)]*(Ipos[2]-I2)
	else if (I2==length(lambday)) mui<-(I2>I1)*sum(lambday[(I1+1):I2])+lambday[I1]*(I1-Ipos[1])
	     else mui<-(I2>I1)*sum(lambday[(I1+1):I2])+
		lambday[I1]*(I1-Ipos[1])+lambday[(I2+1)]*(Ipos[2]-I2)-
		(I2<I1)*lambday[I1]*(I1-I2)
# several cases have to be dealt differently:s I1+1<=I2, I1=I2 or I1>I2  and other case I1=0
#mui is the integral of lambda in the interval defined by Ipos
	Ni<-sum((posy>=Ipos[1])&(posy<=Ipos[2]))
	return(c(mui,Ni))
}
