\name{wwratio}
\alias{wwratio}
\title{Weighted Win Loss Statistics}
\description{Calculate weighted win loss statistics and their corresponding variances under the global NULL hypothesis based on Luo et al. (2017) paper, which is a generalization of the win ratio of Pocock et al. (2012) and the win difference of Luo et al. (2015)}
\usage{
wwratio(y1, y2, d1, d2, z, wty1 = 1, wty2 = 1)
}
\arguments{
\item{y1}{ a numeric vector of event times denoting the minimum of event times \eqn{T_1}, \eqn{T_2} and censoring time \eqn{C}, where the endpoint \eqn{T_2}, corresponding to the terminal event, is considered of higher clinical importance than the endpoint \eqn{T_1}, corresponding to the non-terminal event. Note that the terminal event may censor the non-terminal event, resulting in informative censoring.}
\item{y2}{ a numeric vector of event times denoting the minimum of event time \eqn{T_2} and censoring time \eqn{C}. Clearly, y2 is not smaller than y1. }
\item{d1}{ a numeric vector of event indicators with 1 denoting the non-terminal event is observed and 0 else. }
\item{d2}{ a numeric vector of event indicators with 1 denoting the terminal event is observed and 0 else. }
\item{z}{ a numeric vector of group indicators with 1 denoting the treatment group and 0 the control group.}
\item{wty1}{ a numeric vector of weight indicators for the non-terminal event with values 1 to 4 corresponding to weights used in Luo et al. (2017).}
\item{wty2}{ a numeric vector of weight indicators for the terminal event with values 1 to 2 corresponding to weights used in Luo et al. (2017).}
}
\details{weighted win statistics}
\value{
\item{n1}{Number of subjects in group 1}
\item{n0}{Number of subjects in group 0}
\item{n}{Total number of subjects in both groups}
\item{wty1}{Weight for non-terminal event}
\item{wty2}{Weight for terminal event}
\item{totalw}{Total number of wins in group 1}
\item{totall}{Total number of losses in group 1}
\item{tw}{A vector of total numbers of wins in group 1 for each of the two outcomes. Note that \code{totalw}=sum(\code{tw}), and the first element is for the terminal event and the second element is for the non-terminal event.}
\item{tl}{A vector of total numbers of losses in group 1 for each of the two outcomes. Note that \code{totall}=sum(\code{tl}), and the first element is for the terminal event and the second element is for the non-terminal event.}
\item{xp}{The ratios between \code{tw} and \code{tl}}
\item{cwindex}{The win contribution index defined as the ratio between \code{tw} and \code{totalw}+\code{totall}}
\item{clindex}{The loss contribution index defined as the ratio between \code{tl} and \code{totalw}+\code{totall}}
\item{wr}{weighted win ratio}
\item{vr}{estimated variance of weighted win ratio}
\item{tr}{standardized log(wr)}
\item{pr}{2-sided p-value of tr}
\item{wd}{weighted win difference}
\item{vd}{estimated variance of weighted win difference}
\item{td}{standardized wd}
\item{pd}{2-sided p-value of td}
\item{wp}{weighted win product}
\item{vp}{estimated variance of weighted win product}
\item{tp}{standardized log(wp)}
\item{pp}{2-sided p-value of tp}
}
\references{
Pocock S.J., Ariti C.A., Collier T. J. and Wang D. 2012. The win ratio: a new approach to the analysis of composite endpoints in clinical trials based on clinical priorities. European Heart Journal, 33, 176-182.

Luo X., Tian H., Mohanty S. and Tsai W.-Y. 2015. An alternative approach to confidence interval estimation for the win ratio statistic. Biometrics, 71, 139-145.

Luo X., Qiu J., Bai S. and Tian H. 2017. Weighted win loss approach for analyzing prioritized outcomes. Statistics in Medicine, doi: 10.1002/sim.7284.
}
\author{Xiaodong Luo}
\seealso{\code{\link{wlogr2}},\code{\link{winratio}}}
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

i<-1 ##i=1,2,3,4
j<-2 ##j=1,2

wtest<-wwratio(y1,y2,d1,d2,z,wty1=i,wty2=j)
summary(wtest)
}
\keyword{weighted}
\keyword{win ratio}
\keyword{pairwise comparison}





