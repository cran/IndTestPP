fn3 <-
function(i,posx, lambday=NULL)
{
	posy<-simNHPc.fun(lambda=lambday)$posNH
	aux<-nearestdist.fun(posx, posy)
#      aux <- sort(aux, na.last = TRUE)
	return(aux)
}
