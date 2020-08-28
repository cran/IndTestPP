marca <-
function(T)
{
	if (T<15) marca<-c(1:T)
	else{
			b<-signif(T,-1)/10
			marca<-  c(1,seq(b,T, by=b))    
		}
	return(marca)
}
