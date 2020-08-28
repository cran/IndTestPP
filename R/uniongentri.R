uniongentri <-
function(posx, posy, posz=NULL,info=FALSE,  PA=FALSE,procName=c('X','Y','Z'),...)
{
#uniongentri generate the 3d  or 2d vectors  using the  3 different orders
#xyz, xzy ans yxz and join them without repetitions
#info=TRUE shows  on the screen info about the number of generated points
#in simulation functions is better to use info=F


if(info==TRUE) dev.new(record=TRUE)

#if posz=NULL , the version for only two processes is applied
if (is.null(posz))
{
	if (length(procName)>2) procName<-procName[1:2]
	if (PA==TRUE)
	{
		# 2d points are generated using the 2 possible permutations and the union of them is created
		bixy<-genbiPos(posx=posx,posy=posy, 
			info=info,PA=PA,procName=procName[c(1,2)],...)
		biyx<-genbiPos(posx=posy,posy=posx, 
			info=info,PA=PA,procName=procName[c(2,1)],...)
		
		Unoxy<-bixy[,3]*1000+bixy[,4]
		Unoyx<-biyx[,4]*1000+biyx[,3]
		aux1<-union(Unoxy, Unoyx)
			
#aux 1 contains the union of all the points in all the sets of close points 
# but in the  3 digits format

		iXunion<-floor(aux1/1000)
		iYunion<-floor(aux1-iXunion*1000)
	
#iXunion (iYunion) contains x (y) coordinates of the 2d points of the union of all sets of close points

		Xunion<-posx[iXunion]
		Yunion<-posy[iYunion]
	
	
	}
	else
	{
		bixy<-genbiPos(posx=posx,posy=posy,
			info=info,PA=PA,procName=procName[c(1,2)],...)
		Xunion<-bixy[,1]
		Yunion<-bixy[,2]
		iXunion<-bixy[,3]
		iYunion<-bixy[,4]
	}

	if (info==TRUE)	cat('Final number of 2-tuples of  points in all the sets of close points: ', length(Xunion),fill=TRUE)
	return(list(X=Xunion, Y=Yunion,Z=NULL, iX=iXunion, iY=iYunion,iZ=NULL))




}
else{
	if (PA==TRUE)
	{
		# 3d points are generated using the 6 possible permutations and the union of them is created
		bixy<-genbiPos(posx=posx,posy=posy, 
			info=info,PA=PA,procName=procName[c(1,2)],...)
		biyx<-genbiPos(posx=posy,posy=posx, 
			info=info,PA=PA,procName=procName[c(2,1)],...)
		bixz<-genbiPos(posx=posx,posy=posz, 
			info=info,PA=PA,procName=procName[c(1,3)],...)
		bizx<-genbiPos(posx=posz,posy=posx, 
			info=info,PA=PA,procName=procName[c(3,1)],...)
		biyz<-genbiPos(posx=posy,posy=posz,
			info=info,PA=PA,procName=procName[c(2,3)],...)
		bizy<-genbiPos(posx=posz,posy=posy,
			info=info,PA=PA,procName=procName[c(3,2)],...)

		Axyz<-gentriPos(dataXY=bixy, dataYZ=biyz,info=info,
			procName=procName,...)
		Unoxyz<-Axyz[,4]*1000000+Axyz[,5]*1000+Axyz[,6]
		Axzy<-gentriPos(dataXY=bixz, dataYZ=bizy,info=info,
			procName=procName[c(1,3,2)],...)
		Unoxzy<-Axzy[,4]*1000000+Axzy[,6]*1000+Axzy[,5]
		Ayxz<-gentriPos(dataXY=biyx, dataYZ=bixz, info=info,
			procName=procName[c(2,1,3)],...)
		Unoyxz<-Ayxz[,5]*1000000+Ayxz[,4]*1000+Ayxz[,6]
		Ayzx<-gentriPos(dataXY=biyz, dataYZ=bizx, info=info,
			procName=procName[c(2,3,1)],...)
		Unoyzx<-Ayzx[,6]*1000000+Ayzx[,4]*1000+Ayzx[,5]
		Azxy<-gentriPos(dataXY=bizx, dataYZ=bixy,info=info,
			procName=procName[c(3,1,2)],...)
		Unozxy<-Azxy[,5]*1000000+Azxy[,6]*1000+Azxy[,4]
		Azyx<-gentriPos(dataXY=bizy, dataYZ=biyx,info=info,
			procName=procName[c(3,2,1)],...)
		Unozyx<-Azyx[,6]*1000000+Azyx[,5]*1000+Azyx[,4]

		aux1<-union(Unoxyz, Unoxzy)
		aux2<-union(aux1, Unoyxz)
		aux3<-union(aux2, Unoyzx)	
		aux4<-union(aux3, Unozxy)
		aux5<-union(aux4, Unozyx)
	}
	else
	{
		bixy<-genbiPos(posx=posx,posy=posy,
			info=info,PA=PA,procName=procName[c(1,2)],...)
		biyx<-cbind(bixy[,2],bixy[,1],bixy[,4],bixy[,3])
		bixz<-genbiPos(posx=posx,posy=posz,
			info=info,PA=PA,procName=procName[c(1,3)],...)
		bizx<-cbind(bixz[,2],bixz[,1],bixz[,4],bixz[,3])
		biyz<-genbiPos(posx=posy,posy=posz,
			info=info,PA=PA,procName=procName[c(2,3)],...)
		bizy<-cbind(biyz[,2],biyz[,1],biyz[,4],biyz[,3])

		Axyz<-gentriPos(dataXY=bixy, dataYZ=biyz,info=info,
			procName=procName[c(1,2,3)],...)
		Unoxyz<-Axyz[,4]*1000000+Axyz[,5]*1000+Axyz[,6]
		Axzy<-gentriPos(dataXY=bixz, dataYZ=bizy, info=info,
			procName=procName[c(1,3,2)],...)
		Unoxzy<-Axzy[,4]*1000000+Axzy[,6]*1000+Axzy[,5]
		Ayxz<-gentriPos(dataXY=biyx, dataYZ=bixz,info=info,
			procName=procName[c(2,1,3)],...)
		Unoyxz<-Ayxz[,5]*1000000+Ayxz[,4]*1000+Ayxz[,6]

		aux1<-union(Unoxyz, Unoxzy)
		aux5<-union(aux1, Unoyxz)
	}
#aux 5 contains the union of all the points in all the sets of close points 
# but in the 9 digits format

	iXunion<-floor(aux5/1000000)
	iYunion<-floor((aux5-iXunion*1000000)/1000)
	iZunion<-aux5-iXunion*1000000-iYunion*1000
#iXunion (iYunion, iZunion) contains x (y, z) coordinates of the 3d points of the union of all sets of close points

	Xunion<-posx[iXunion]
	Yunion<-posy[iYunion]
	Zunion<-posz[iZunion]

	if (info==TRUE)	cat('Final number of 3-tuples of  points in all the sets of close points: ', length(Xunion),fill=TRUE)
	return(list(X=Xunion, Y=Yunion, Z=Zunion,iX=iXunion, iY=iYunion, iZ=iZunion))
}
}
