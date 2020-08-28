DistShift <-
function(posx,posy,posz=NULL, T, shii1, shii2=NULL, PA = FALSE,info=FALSE,...) 
{
	NumProcess<-2+(is.null(posz)==F) 
	if ((max(shii1,shii2)>T)|(min(shii1,shii2)<0)) stop ('shii1 and shii2 values must be positive values lower than the length of lambda')
	posyN<-posy+shii1
	posyN[posyN>T]<-posyN[posyN>T]-T
	posyN<-sort(posyN)

	if (NumProcess==3)
	{
	poszN<-posz+shii2
	poszN[poszN>T]<-poszN[poszN>T]-T
	poszN<-sort(poszN)
	}
	else
	{
	poszN<-NULL
	}

	DistTri<-DistObs(posx=posx,posy=posyN,posz=poszN, info=info,
		 PA=PA,procName=c('ObsX','ShiftY','ShiftZ'),...)

	return(DistTri)
}
