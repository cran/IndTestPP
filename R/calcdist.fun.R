calcdist.fun <-
function(coorx,coory,coorz, nposx)
{
#calcdist.fun calculates the  mean of the distances of points (x,y,z)  for each x-coordinate
#nposx is the number of  events is the process X ( length of posx)


	if (!is.null(coorz)) dist<-abs(coory-coorx)+abs(coorz-coorx)
	else dist<-abs(coory-coorx)
# dist contains the (lineal) distances from x to y and z
	MDistX<-tapply(dist,INDEX=coorx, FUN=mean, na.rm=TRUE)
	if (length(MDistX)<nposx) MDistX<-c(MDistX, rep( NA,(nposx-length(MDistX)) ))
#MDistX contains the mean of the distances of points (x,y,z)  with the same x-coordinate
#a NA value is assigned to values x in posx with no close points set
	return(MDistX)
}
