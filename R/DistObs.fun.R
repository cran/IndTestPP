DistObs.fun <-
function(posx,posy,posz=NULL, info=FALSE, PA=FALSE,procName=c('X','Y','Z'),...)
{
	nposx<-length(posx)

	pTri<-uniongentri.fun(posx=posx,posy=posy,posz=posz,
			info=info, PA=PA,procName=procName,...)
	DistTri<-calcdist.fun(coorx=pTri$X,coory=pTri$Y,coorz=pTri$Z, nposx=nposx)

	names(DistTri)<-round(posx,2)
	return(DistTri)
}
