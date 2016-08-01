CPSPPOTevents.fun <-
function(X,Y,thresX,thresY, date=NULL)
{

    if (is.null(date)) 
        date <- c(1:length(X))
    date <- as.matrix(date)
    if ((dim(date)[1] != length(X))| (dim(date)[1] != length(Y))) 
        stop("X and Y and date must have the same number of observations")

n<-length(X)


casoTX<-(X>thresX)&(Y<=thresY)
casoTY<-(X<=thresX)&(Y>thresY)
casoTXY<-(X>thresX)&(Y>thresY)


pt<-c(1:n)

# to identify th start and end of each run,  the series is differentiated
casotx1<-c(0,diff(casoTX))
casoty1<-c(0,diff(casoTY))
casotxy1<-c(0,diff(casoTXY))


inrachtx<-(casotx1==1)
finrachtx<-(casotx1==-1)
inrachty<-(casoty1==1)
finrachty<-(casoty1==-1)
inrachtxy<-(casotxy1==1)
finrachtxy<-(casotxy1==-1)


# Run  start day is stored, according to its position
posicintx<-pt[inrachtx==1]
posicinty<-pt[inrachty==1]
posicintxy<-pt[inrachtxy==1]


# Run number is calcualted by the accumulated sum of inrach

numrachtx<-cumsum(inrachtx)
numerachtx<-numrachtx[casoTX==1]
numrachty<-cumsum(inrachty)
numerachty<-numrachty[casoTY==1]
numrachtxy<-cumsum(inrachtxy)
numerachtxy<-numrachtxy[casoTXY==1]

#Event positions are stored
posrachtx<-pt[casoTX==1]
posrachty<-pt[casoTY==1]
posrachtxy<-pt[casoTXY==1]


#Intensity.
# Intensity values are kept only if the series value is over the threlhold

intentx<-(X-thresX)[casoTX==1]
intenty<-(Y-thresY)[casoTY==1]
intentxyX<-(X-thresX)[casoTXY==1]
intentxyY<-(Y-thresY)[casoTXY==1]
intentxy<-intentxyX+intentxyY


# Length, maximum and mean intensities are calculated for each run event. Also the Imax position ( the first one if there are more than one)

medintx<-tapply(intentx,INDEX=numerachtx, FUN=mean)
maxintx<-tapply(intentx,INDEX=numerachtx, FUN=max)
longintx<-tapply(intentx,INDEX=numerachtx, FUN=length)

medinty<-tapply(intenty,INDEX=numerachty, FUN=mean)
maxinty<-tapply(intenty,INDEX=numerachty, FUN=max)
longinty<-tapply(intenty,INDEX=numerachty, FUN=length)

medintxy<-tapply(intentxy,INDEX=numerachtxy, FUN=mean)
maxintxy<-tapply(intentxy,INDEX=numerachtxy, FUN=max)
medintxyX<-tapply(intentxyX,INDEX=numerachtxy, FUN=mean)
maxintxyX<-tapply(intentxyX,INDEX=numerachtxy, FUN=max)
medintxyY<-tapply(intentxyY,INDEX=numerachtxy, FUN=mean)
maxintxyY<-tapply(intentxyY,INDEX=numerachtxy, FUN=max)

longintxy<-tapply(intentxy,INDEX=numerachtxy, FUN=length)


posxrachaintx<-tapply(intentx,INDEX=numerachtx, FUN=which.max)
posxintx<-posicintx+posxrachaintx-1
posxrachainty<-tapply(intenty,INDEX=numerachty, FUN=which.max)
posxinty<-posicinty+posxrachainty-1
posxrachaintxy<-tapply(intentxy,INDEX=numerachtxy, FUN=which.max)
posxintxy<-posicintxy+posxrachaintxy-1

inddatTX<-1-casoTX
inddatTX[posxintx]<-1
inddatTY<-1-casoTY
inddatTY[posxinty]<-1
inddatTXY<-1-casoTXY
inddatTXY[posxintxy]<-1


dateTX<-date[posxintx,]
dateTY<-date[posxinty,]
dateTXY<-date[posxintxy,]



event<-list(ImX=medintx,IxX=maxintx,LX=longintx,PxX=posxintx,
            PiX=posicintx,inddatX=inddatTX,
		ImY=medinty,IxY=maxinty,LY=longinty,PxY=posxinty,
            PiY=posicinty,inddatY=inddatTY, 
		ImXY=medintxy,IxXY=maxintxy,ImXYx=medintxyX,IxXYx=maxintxyX,ImXYy=medintxyY,IxXYy=maxintxyY,LXY=longintxy,PxXY=posxintxy,
		PiXY=posicintxy,inddatXY=inddatTXY, 
		X=X, Y=Y, thresX=thresX,  thresY=thresY, date=date)


cat('Number of EE in  X: ',length(medintx),fill=TRUE)
cat('Number of obs. over the threshold', thresX,':',sum(casoTX),fill=TRUE)
print(cbind(dateTX,round(event$ImX),
		event$IxX,event$LX,event$PxX,event$PiX))


cat('Number of EE in  Y: ',length(medinty),fill=TRUE)
cat('Number of obs. over the threshold', thresY,':',sum(casoTY),fill=TRUE)
print(cbind(dateTY,round(event$ImY),
		event$IxY,event$LY,event$PxY,event$PiY))


cat('Number of simultaneous  EE in   X and Y: ',length(medintxy),fill=TRUE)
cat('Number of obs. over the threshold :',sum(casoTXY),fill=TRUE)
print(cbind(dateTXY,round(event$ImXY),
		event$IxXY,event$LXY,event$PxXY,event$PiXY))


return(event)
}
