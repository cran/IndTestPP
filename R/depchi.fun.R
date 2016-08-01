depchi.fun <-
function(X, Y, thresval=c(0:99)/100, tit='', indgraph=TRUE, 
	bothest=TRUE,xlegend='topleft')
{

	n<-length(X)


	FTx<-rank(X)/n
	FTy<-rank(Y)/n

	PTxTy<-NULL
	PTx<-NULL
	PTy<-NULL
	chix<-NULL
	chiy<-NULL
	STxTy<-NULL
	STx<-NULL
	STy<-NULL
	chiBx<-NULL
	chiBy<-NULL

	nn<-length(thresval)

	for (i in c(1:nn))
	{
		PTxTy[i]<-sum((FTx<thresval[i])&(FTy<thresval[i]))/n
		PTx[i]<-sum(FTx<thresval[i])/n
		PTy[i]<-sum(FTy<thresval[i])/n
		STxTy[i]<-sum((FTx>thresval[i])&(FTy>thresval[i]))/n
		STx[i]<-sum(FTx>thresval[i])/n
		STy[i]<-sum(FTy>thresval[i])/n
		chix[i]<-2-(1-PTxTy[i])/(1-PTx[i])
		chiy[i]<-2-(1-PTxTy[i])/(1-PTy[i]) #this definition avoids an aproximation 
		chiBx[i]<-2*log(STx[i])/log(STxTy[i])-1
		chiBy[i]<-2*log(STy[i])/log(STxTy[i])-1

	}
	print(cbind(thresval,PTxTy, PTx, PTy,chix,chiy))

	dev.new(record = TRUE)
	if (indgraph==FALSE) par(mfrow=c(2,1))
	plot(thresval,chix,  type='l', ylim=c(0,1), ylab = expression(chi(u)),
		 xlab='u (threshold)')
	lines(thresval,1-thresval, lty=2, col='grey')
	if (bothest==TRUE)
	{	lines(thresval,chiy,col='red')
		legend(xlegend, legend = c(expression(paste(hat(chi)(u), ' Y given X')), 
			expression(paste(hat(chi)(u), ' X given Y')), 
			expression(paste(chi(u), ' independence')) ),
                	col = c("black", "red", "grey"), lty = c(1, 1,2), cex = 0.8)
	}
	else
		legend(xlegend, legend = c(expression(paste(hat(chi)(u), ' Y given X')), 
			expression(paste(chi(u), ' independence')) ),
                	col = c("black", "grey"), lty = c(1,2), cex = 0.8)


	if (indgraph==TRUE) mtext(paste(tit), outer = TRUE, line = -2, cex =1)
	plot(thresval,chiBx,  type='l', ylim=c(-1,1), ylab=expression(bar(chi)(u)), xlab='u (threshold)')
	if (bothest==TRUE)
	{	lines(thresval,chiBy,col='red')
		legend(xlegend, legend = c(expression(paste(hat(bar(chi))(u), ' Y given X')), 
	 		expression(paste(hat(bar(chi))(u), ' X given Y')) ),
                  col = c("black", "red"), lty = c(1, 1), cex = 0.8)
	}
	else
		legend(xlegend, legend = c(expression(paste(hat(bar(chi))(u), ' Y given X')) ),
                  col = c("black", "red"), lty = c(1, 1), cex = 0.8)

      mtext(paste(tit), outer = TRUE, line = -2, cex = 1)

	return(list(chiX=chix,chiY=chiy,chiBX=chiBx,chiBY=chiBy,PX=PTx, PY=PTy, PXY=PTxTy, thresval))

}
