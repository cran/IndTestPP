DepNHPPqueue <-
function(lambda, d,   T=NULL,  nEv=NULL,nserv='infty',Clambda=TRUE,
	ddist='exp',argd=1,dplot=TRUE, fixed.seed=NULL, ...)
{

if (d<=1) stop('d must be an integer greater than 1')

if (!is.matrix(argd)) argd<-matrix(rep(argd, each=(d-1)), nrow=(d-1))
else
{
	if (dim(argd)[1]!=(d-1)) stop('If argd is a matrix, the number of rows must be d-1')	

}

if ((nserv!='infty')&(nserv!=1)) stop('Argument nserv must be 1 or infty')

type<-'H'

if (length(lambda)==1)
{
	if ((is.null(T))&(is.null(nEv)))stop('Argument nEv or T must be provided in homogeneous processes')
	if ((!is.null(T))&(!is.null(nEv)))
	{
		entaux<-simNHPc(rep(lambda,T),fixed.seed=fixed.seed)
		ent<-entaux$posNH
		warning('Only one of T and Ev can  be used. T has been used to generate the process')
	}
	if ((!is.null(nEv))&(is.null(T)))
	{
		entaux<-simHPc(lambda, nEv=nEv,fixed.seed=fixed.seed)
		ent<-entaux$posH
		T<-ceiling(entaux$T)
	}
}else
{
	if (!is.null(T))
	{
		if(length(lambda)!=T) cat('T is not used since it is different from the length of lambda', 
			fill=TRUE)
	}
	T<-length(lambda)
	ent<-simNHPc(lambda=lambda,fixed.seed=fixed.seed)$posNH
	if (sum(diff(lambda))!=0) type<-'NH'

}

if ( (type=='H')&(!identical(ddist,'exp')) ) 
	warning('The generated point processes are not homogeneous Poisson processes')
if (nserv=='infty') auxr<-DepNHPPqueueI(lambda=lambda, d=d,T=T,type=type, ent=ent, 
	Clambda,ddist=ddist, argd=argd,fixed.seed=fixed.seed)
else auxr<-DepNHPPqueue1(lambda=lambda, d=d,T=T,type=type, ent=ent,Clambda,ddist=ddist, 
	argd=argd,fixed.seed=fixed.seed)

if (dplot==TRUE)  PlotMargP(listpos=auxr$posNH,T=T,...)
return(auxr)
}
