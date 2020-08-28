NumI <-
function(posNH, It=c(1,100))
{
	d<-length(posNH)
	numI<-NULL
	for (j in (1:d))
	{
		indN<-((posNH[[j]]>=It[1])&(posNH[[j]]<=It[2]))
		numI[j]<-sum(indN)
	}
	return(numI)
}
