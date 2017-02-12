\name{winratio}
\alias{winratio}
\title{Win Loss Statistics}
\description{Calculate the win loss statistics of Pocock et al. (2012) and the corresponding variances, which are based on a U-statistic method of Luo et al. (2015)}
\usage{
winratio(y1,y2,d,z,doublebad=1)
}
\arguments{
\item{y1}{ a numeric vector of event times denoting the minimum of event times \eqn{T_1}, \eqn{T_2} and censoring time \eqn{C}, where the endpoint \eqn{T_2}, corresponding to the terminal event, is considered of higher clinical importance than the endpoint \eqn{T_1}, corresponding to the non-terminal event. Note that the terminal event may censor the non-terminal event, resulting in informative censoring.}
\item{y2}{ a numeric vector of event times denoting the minimum of event time \eqn{T_2} and censoring time \eqn{C}. Clearly, y2 is not smaller than y1. }
\item{d}{ a numeric vector of event indicators with 1 denoting both the non-terminal event and the terminal event are observed; 2 dentoing only the terminal event is observed; 3 denoting only the censoring time is observed; and 4 denoting that the non-terminal event and the censoring time are observed}
\item{z}{ a numeric vector of group indicators with 1 denoting the treatment group and 0 the control group.}
\item{doublebad}{a numeric value with 1 denoting that both the non terminal event and the terminal are bad (i.e. disease progrssion and death) and 0 denoting that the non terminal event is good (i.e. response to the treatment).}
}
\details{win loss statistics}
\value{
\item{doublebad}{a numeric value with 1 denoting that both the non terminal event and the terminal are bad (i.e. disease progrssion and death) and 0 denoting that the non terminal event is good (i.e. response to the treatment).}
\item{w2}{number of winner pairs based on the terminal event}
\item{l2}{number of loser pairs based on the terminal event}
\item{w1}{number of winner pairs based on the non-terminal event}
\item{l1}{number of loser pairs based on the non-terminal event}
\item{wr}{win ratio}
\item{vr}{estimated variance of win ratio}
\item{tr}{standardized log(wr)}
\item{pr}{2-sided p-value of tr}
\item{wd}{win difference}
\item{vd}{estimated variance of win difference}
\item{td}{standardized wd}
\item{pd}{2-sided p-value of td}
\item{wp}{win product}
\item{vp}{estimated variance of win product}
\item{tp}{standardized log(wp)}
\item{pp}{2-sided p-value of tp}
}
\references{
Pocock S.J., Ariti C.A., Collier T. J. and Wang D. 2012. The win ratio: a new approach to the analysis of composite endpoints in clinical trials based on clinical priorities. European Heart Journal, 33, 176-182.

Luo X., Tian H., Mohanty S. and Tsai W.-Y. 2015. An alternative approach to confidence interval estimation for the win ratio statistic. Biometrics, 71, 139-145.
}
\author{Xiaodong Luo}
\seealso{\code{\link{wlogr2}},\code{\link{wwratio}}}
\examples{
n<-300
rho<-0.5
b2<-0.2
b1<-0.5
bc<-1.0
lambda10<-0.1;lambda20<-0.08;lambdac0<-0.09
lam1<-rep(0,n);lam2<-rep(0,n);lamc<-rep(0,n)
z<-rep(0,n)
z[1:(n/2)]<-1

lam1<-lambda10*exp(-b1*z)
lam2<-lambda20*exp(-b2*z)
lamc<-lambdac0*exp(-bc*z)
tem<-matrix(0,ncol=3,nrow=n)

y2y<-matrix(0,nrow=n,ncol=3)
y2y[,1]<-rnorm(n);y2y[,3]<-rnorm(n)
y2y[,2]<-rho*y2y[,1]+sqrt(1-rho^2)*y2y[,3]
tem[,1]<--log(1-pnorm(y2y[,1]))/lam1
tem[,2]<--log(1-pnorm(y2y[,2]))/lam2
tem[,3]<--log(1-runif(n))/lamc

y1<-apply(tem,1,min)
y2<-apply(tem[,2:3],1,min)
d1<-as.numeric(tem[,1]<=y1)
d2<-as.numeric(tem[,2]<=y2)
d<-rep(0,n)
d[d1==1&d2==1]<-1
d[d1==0&d2==1]<-2
d[d1==0&d2==0]<-3
d[d1==1&d2==0]<-4
wtest<-winratio(y1,y2,d,z,doublebad=1)
wtest
}
\keyword{win ratio}
\keyword{pairwise comparison}





