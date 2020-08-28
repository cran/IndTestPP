CPSPPOTevents <-
function(N1,N2,thres1,thres2, date=NULL, dplot=T, pmfrow=c(2,1), axispoints=NULL,...)
{

    if (is.null(date)) 
        date <- c(1:length(N1))
    date <- as.matrix(date)
    if ((dim(date)[1] != length(N1))| (dim(date)[1] != length(N2))) 
        stop("N1 and N2 and date must have the same number of observations")

n<-length(N1)


casoT1<-(N1>thres1)&(N2<=thres2)
casoT2<-(N1<=thres1)&(N2>thres2)
casoT12<-(N1>thres1)&(N2>thres2)


pt<-c(1:n)

# to identify th start and end of each run,  the series is differentiated
casot11<-c(0,diff(casoT1))
casot21<-c(0,diff(casoT2))
casot121<-c(0,diff(casoT12))


inracht1<-(casot11==1)
finracht1<-(casot11==-1)
inracht2<-(casot21==1)
finracht2<-(casot21==-1)
inracht12<-(casot121==1)
finracht12<-(casot121==-1)


# Run  start day is stored, according to its position
posicint1<-pt[inracht1==1]
posicint2<-pt[inracht2==1]
posicint12<-pt[inracht12==1]


# Run number is calcualted by the accumulated sum of inrach

numracht1<-cumsum(inracht1)
numeracht1<-numracht1[casoT1==1]
numracht2<-cumsum(inracht2)
numeracht2<-numracht2[casoT2==1]
numracht12<-cumsum(inracht12)
numeracht12<-numracht12[casoT12==1]

#Event positions are stored
posracht1<-pt[casoT1==1]
posracht2<-pt[casoT2==1]
posrachtx12<-pt[casoT12==1]


#Intensity.
# Intensity values are kept only if the series value is over the threlhold

intent1<-(N1-thres1)[casoT1==1]
intent2<-(N2-thres2)[casoT2==1]
intent121<-(N1-thres1)[casoT12==1]
intent122<-(N2-thres2)[casoT12==1]
intent12<-intent121+intent122


# Length, maximum and mean intensities are calculated for each run event. Also the Imax position ( the first one if there are more than one)

medint1<-tapply(intent1,INDEX=numeracht1, FUN=mean)
maxint1<-tapply(intent1,INDEX=numeracht1, FUN=max)
longint1<-tapply(intent1,INDEX=numeracht1, FUN=length)

medint2<-tapply(intent2,INDEX=numeracht2, FUN=mean)
maxint2<-tapply(intent2,INDEX=numeracht2, FUN=max)
longint2<-tapply(intent2,INDEX=numeracht2, FUN=length)

medint12<-tapply(intent12,INDEX=numeracht12, FUN=mean)
maxint12<-tapply(intent12,INDEX=numeracht12, FUN=max)
medint121<-tapply(intent121,INDEX=numeracht12, FUN=mean)
maxint121<-tapply(intent121,INDEX=numeracht12, FUN=max)
medint122<-tapply(intent122,INDEX=numeracht12, FUN=mean)
maxint122<-tapply(intent122,INDEX=numeracht12, FUN=max)

longint12<-tapply(intent12,INDEX=numeracht12, FUN=length)


posxrachaint1<-tapply(intent1,INDEX=numeracht1, FUN=which.max)
posxint1<-posicint1+posxrachaint1-1
posxrachaint2<-tapply(intent2,INDEX=numeracht2, FUN=which.max)
posxint2<-posicint2+posxrachaint2-1
posxrachaint12<-tapply(intent12,INDEX=numeracht12, FUN=which.max)
posxint12<-posicint12+posxrachaint12-1

inddatT1<-1-casoT1
inddatT1[posxint1]<-1
inddatT2<-1-casoT2
inddatT2[posxint2]<-1
inddatT12<-1-casoT12
inddatT12[posxint12]<-1


dateT1<-date[posxint1,]
dateT2<-date[posxint2,]
dateT12<-date[posxint12,]
dateT<-NULL
if (!is.null(date)) dateT<-date[,1]


event<-list(Im1=medint1,Ix1=maxint1,L1=longint1,Px1=posxint1,
            Pi1=posicint1,inddat1=inddatT1,
		Im2=medint2,Ix2=maxint2,L2=longint2,Px2=posxint2,
            Pi2=posicint2,inddat2=inddatT2, 
		Im12=medint12,Ix12=maxint12,Im12x=medint121,Ix121=maxint121,
		Im122=medint122,Ix122=maxint122,L12=longint12,Px12=posxint12,
		Pi12=posicint12,inddat12=inddatT12, 
		N1=N1, N2=N2, thres1=thres1,  thres2=thres2, date=date)


cat('Number of EE in  N1: ',length(medint1),fill=TRUE)
cat('Number of obs. over the threshold', thres1,':',sum(casoT1),fill=TRUE)
print(cbind(dateT1,round(event$Im1),
		event$Ix1,event$L1,event$Px1,event$Pi1))


cat('Number of EE in  N2: ',length(medint2),fill=TRUE)
cat('Number of obs. over the threshold', thres2,':',sum(casoT2),fill=TRUE)
print(cbind(dateT2,round(event$Im2),
		event$Ix2,event$L2,event$Px2,event$Pi2))


cat('Number of simultaneous  EE in   N1 and N2: ',length(medint12),fill=TRUE)
cat('Number of obs. over the threshold :',sum(casoT12),fill=TRUE)
print(cbind(dateT12,round(event$Im12),
		event$Ix12,event$L12,event$Px12,event$Pi12))


if (dplot==T)
{
	par(mfrow=pmfrow)
	pos1<-sort(union(posxint1, posxint12))
	pos2<-sort(union(posxint2, posxint12))
	posi<-0-n/10
	if(is.null(axispoints)) axispoints<-marca(n)
	PlotMCPSP(pos1,pos2, T=n, date=dateT, axispoints=axispoints)
	PlotICPSP(posxint1,posxint2,posxint12, T=n, date=dateT, axispoints=axispoints)
}

return(event)
}
