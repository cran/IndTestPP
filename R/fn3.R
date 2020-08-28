fn3 <-
function(i,posx, lambday=NULL)
{
	posy<-simNHPc(lambda=lambday)$posNH
	aux<-nearestdist(posx, posy)
#      aux <- sort(aux, na.last = TRUE)
	return(aux)
}
