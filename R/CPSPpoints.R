CPSPpoints <-
function(N1,N2,date=NULL, dplot=T, pmfrow=c(2,1),axispoints=NULL,...)
{

n<-length(N1)

if (n!= length(N2))
        stop("N1  and N2 must have the same number of observations")

if (!is.null(date)){ 
	date <- as.matrix(date)
    	if (dim(date)[1] != n)
        stop("date  must have the same number of observations than N1 and N2")
}

if ( any( (c(N1,N2)!=0)&(c(N1,N2)!=1) ) )
        stop(" Only 0 and 1 values are allowed in N1 and N2")


it<-c(1:n)

posi12<-it[((N1==1)&(N2==1))]
posi1<-it[((N1==1)&(N2==0))]
posi2<-it[((N1==0)&(N2==1))]

pos1<-it[N1==1]
pos2<-it[N2==1]


dateTi1<-date[posi1,]
dateTi2<-date[posi2,]
dateTi12<-date[posi12,]
dateT1<-date[pos1,]
dateT2<-date[pos2,]
dateT<-NULL
if (!is.null(date)) dateT<-date[,1]



event<-list(posi1=posi1,posi2=posi2,posi12=posi12, pos1=pos1, pos2=pos2, date=date)



cat('Indicator processes  of the CPSP',fill=TRUE)
cat(fill=T)
cat('Number of points  only in N1: ',length(posi1),fill=TRUE)
cat('   Occurrence points only in N1: ', posi1,  fill=T)
if (!is.null(date)) cat('   Dates: ',dateTi1, fill=T)
cat('Number of points  only  in N2: ',length(posi2),fill=TRUE)
cat('   Occurrence points only in N2: ', posi2,  fill=T)
if (!is.null(date)) cat('   Dates:',dateTi2, fill=T)
cat('Number of  simultaneous points in  N1 and N2: ',length(posi12),fill=TRUE)
cat('   Occurrence points in N1 and N2: ', posi12, fill=T)
if (!is.null(date)) cat('   Dates: ',dateTi12, fill=T)

cat(fill=T)
cat(fill=T)
cat('Marginal processes  of the CPSP',fill=TRUE)
cat(fill=T)
cat('Number of points  in N1: ',length(pos1),fill=TRUE)
cat('   Occurrence points  in N1: ', pos1,  fill=T)
if (!is.null(date)) cat('   Dates: ',dateT1, fill=T)
cat('Number of points   in N2: ',length(pos2),fill=TRUE)
cat('   Occurrence points in N2: ', pos2,  fill=T)
if (!is.null(date)) cat('   Dates:',dateT2, fill=T)


    if (dplot==T)
    {
	par(mfrow=pmfrow)
	posi<-0-n/10
	if(is.null(axispoints)) axispoints<-marca(n)
	PlotMCPSP(pos1,pos2, T=n, date=dateT, axispoints=axispoints)
	PlotICPSP(posi1,posi2,posi12, T=n, date=dateT, axispoints=axispoints)

    }


return(event)
}
