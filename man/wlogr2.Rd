\name{wlogr2}
\alias{wlogr2}
\title{wlogr2}
\description{This will calculate the log-rank and Gehan statistics along with their variances}
\usage{
wlogr2(y, d, z, wty = 1)
}
\arguments{
  \item{y}{a vector of observed event times}
  \item{d}{a vector of event indicators with 1=event and 0=censored}
  \item{z}{a vector of group indicators with 1=treatment and 0=control}
  \item{wty}{a vector of weight indicators with 1=Gehan and 2=log-rank}
}
\details{More details}
\value{\item{wty}{Type of statistics, 1=Gehan, 2=log-rank}
       \item{stat}{value of the stat}
       \item{vstat}{estimated variance}
       \item{tstat}{standardized test stat}
       \item{pstat}{2-sided p-value of the standardized test stat} 
}
\references{
Gehan E.A. 1965. A generalized Wilcoxon test for comparing arbitrarily single-censored samples. Biometrika, 53, 203-223.

Peto R. and Peto J. 1972. Asymptotically Efficient Rank Invariant Test Procedures. Journal of the Royal Statistical Society, Series A, 135, 185-207.
}
\author{Xiaodong Luo}
\note{This provides Gehan test that is usually ignored}
\seealso{\code{\link{winratio}},\code{\link{wwratio}}}
\examples{
n<-300
b<-0.2
bc<-1.0
lambda0<-0.1;lambdac0<-0.09
lam<-rep(0,n);lamc<-rep(0,n)
z<-rep(0,n)
z[1:(n/2)]<-1

lam<-lambda0*exp(-b*z)
lamc<-lambdac0*exp(-bc*z)
tem<-matrix(0,ncol=2,nrow=n)

tem[,1]<--log(1-runif(n))/lam
tem[,2]<--log(1-runif(n))/lamc

y<-apply(tem,1,min)
d<-as.numeric(tem[,1]<=y)

i<-1 ##i=1,2
wtest<-wlogr2(y,d,z,wty=i)
wtest
}
\keyword{log-rank}
\keyword{Gehan}