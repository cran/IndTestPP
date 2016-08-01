nMenr <-
function(ri,nearestpD, punt, T)
{
	aux<-((punt>=ri)&(punt<=(T-ri)))
	nMenri<-sum(nearestpD[aux]<=ri)
	puntr<-sum(aux)
	return(c(nMenri,puntr))
}
