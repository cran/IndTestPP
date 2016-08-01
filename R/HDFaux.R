HDFaux <-
function(punt, posD,   r=NULL, T)
{
	nearestpD<-sapply(punt, FUN = nearestD, posD=posD)
	aux<-sapply(r, FUN = nMenr,nearestpD, punt, T)
	HDFr<-aux[1,]/aux[2,]
	return(HDFr)
}
