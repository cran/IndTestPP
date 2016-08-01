CPSPpoints.fun <-
function(X,Y,date=NULL)
{

n<-length(X)

if (n!= length(Y))
        stop("X, Y and date date must have the same number of observations")

if (!is.null(date)){ 
	date <- as.matrix(date)
    	if (dim(date)[1] != n)
        stop("date date must have the same number of observations than X and Y")
}

if ( any( (c(X,Y)!=0)&(c(X,Y)!=1) ) )
        stop(" Only 0 and 1 values are allowed in X and Y")


it<-c(1:n)

posXY<-it[((X==1)&(Y==1))]
posX<-it[((X==1)&(Y==0))]
posY<-it[((X==0)&(Y==1))]


dateTX<-date[posX,]
dateTY<-date[posY,]
dateTXY<-date[posXY,]



event<-list(PxX=posX,PxY=posY,PxXY=posXY, X=X, Y=Y, date=date)


cat('Number of points  only in X: ',length(posX),fill=TRUE)
cat('   Occurrence points only in X: ', posX,  fill=T)
if (!is.null(date)) cat('   Dates: ',dateTX, fill=T)
cat('Number of points  only  in Y: ',length(posY),fill=TRUE)
cat('   Occurrence points only in Y: ', posY,  fill=T)
if (!is.null(date)) cat('   Dates:',dateTY, fill=T)
cat('Number of  simultaneous points in  X and Y: ',length(posXY),fill=TRUE)
cat('   Occurrence points in X and Y: ', posXY, fill=T)
if (!is.null(date)) cat('   Dates: ',dateTXY, fill=T)

return(event)
}
