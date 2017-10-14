\name{genwr}
\alias{genwr}
\title{General Win Loss Statistics}
\description{Calculate the general win loss statistics and their corresponding variances under the global NULL hypothesis and under alterantive hypothesis based on Bebu and Lachin (2016) paper, which is a generalization of the win ratio of Pocock et al. (2012) and the win difference of Luo et al. (2015). This calculation needs the users to specify the win loss matrix.}
\usage{
genwr(aindex)
}
\arguments{
\item{aindex}{a numeric matrix of win loss indicators. Suppose there are group 1 and group 0 in the study with sample sizes \eqn{n_1} and \eqn{n_0} respectively. The matrix \code{aindex} is a \eqn{n_1\times n_0} matrix with elements \eqn{C_{ij}: i=1,\ldots,n_1, j=1,\ldots,n_0}. The element \eqn{C_{ij}} is equal to \eqn{1} if subject \eqn{i} in group 1 wins over subject \eqn{j} in group 0 on the most important outcome, \eqn{C_{ij}} is equal to \eqn{-1} if subject \eqn{i} in group 1 loses against subject \eqn{j} in group 0 on the most important outcome; \eqn{C_{ij}} is equal to \eqn{2} if subject \eqn{i} in group 1 wins over subject \eqn{j} in group 0 on the second most important outcome after tie on the most important outcome, \eqn{C_{ij}} is equal to \eqn{-2} if subject \eqn{i} in group 1 loses against subject \eqn{j} in group 0 on the second most important outcome after tie on the most important outcome; 
\eqn{C_{ij}} is equal to \eqn{3} if subject \eqn{i} in group 1 wins over subject \eqn{j} in group 0 on the third most important outcome after tie on the first two most important outcomes, \eqn{C_{ij}} is equal to \eqn{-3} if subject \eqn{i} in group 1 loses against subject \eqn{j} in group 0 on the third most important outcome after tie on the first two most important outcomes; and so forth until all the outcomes have been used for comparison; then \eqn{C_{ij}} is equal to \eqn{0} if an ultimate tie is resulted.}
}
\details{General win loss statistics}
\value{
\item{n1}{Number of subjects in group 1}
\item{n0}{Number of subjects in group 0}
\item{n}{Total number of subjects in both groups}
\item{totalw}{Total number of wins in group 1}
\item{totall}{Total number of losses in group 1}
\item{tw}{A vector of total numbers of wins in group 1 for each of the outcomes. Note that \code{totalw}=sum(\code{tw}), the first element is for the most important outcome, the second elemnet is for the second important outcome etc.}
\item{tl}{A vector of total numbers of losses in group 1 for each of the outcomes. Note that \code{totall}=sum(\code{tl}), the first element is for the most important outcome, the second elemnet is for the second important outcome etc.}
\item{xp}{The ratios between \code{tw} and \code{tl}}
\item{cwindex}{The win contribution index defined as the ratio between \code{tw} and \code{totalw}+\code{totall}}
\item{clindex}{The loss contribution index defined as the ratio between \code{tl} and \code{totalw}+\code{totall}}
\item{wr}{Win ratio defined as \code{totalw}/\code{totall}}
\item{vr}{Asymptotic variance of the win ratio under alterantive hypothesis}
\item{vr0}{Asymptotic variance of the win ratio under global null hypothesis}
\item{tr}{standardized log(wr) using the variance \code{vr}}
\item{pr}{2-sided p-value of \code{tr}}
\item{tr0}{standardized log(wr) using the variance \code{vr0}}
\item{pr0}{2-sided p-value of \code{tr0}}
\item{wd}{Win difference defined as \code{totalw}-\code{totall}}
\item{vd}{Asymptotic variance of the win difference under alterantive hypothesis. The first element is the variance when the group assignment is considered as fixed, and the second element is the variance when the group assignment is considered as random, so the second element is slightly larger than the first element when with unequal allocations.}
\item{vd0}{Asymptotic variance of the win difference under global null hypothesis}
\item{td}{standardized \code{wd} using the variance \code{vd}}
\item{pd}{2-sided p-values of \code{td}}
\item{td0}{standardized \code{wd} using the variance \code{vd0}}
\item{pd0}{2-sided p-value of \code{td0}}
\item{wp}{Win product defined as the product of \code{tw}/\code{tl}}
\item{vp}{Asymptotic variance of the win product under alterantive hypothesis}
\item{vp0}{Asymptotic variance of the win product under global null hypothesis}
\item{tp}{standardized log(wp) using the variance\code{vp}}
\item{pp}{2-sided p-value of \code{tp}}
\item{tp0}{standardized log(wp) using the variance\code{vp0}}
\item{pp0}{2-sided p-value of \code{tp0}}
}
\references{
Pocock S.J., Ariti C.A., Collier T. J. and Wang D. 2012. The win ratio: a new approach to the analysis of composite endpoints in clinical trials based on clinical priorities. European Heart Journal, 33, 176-182.

Luo X., Tian H., Mohanty S. and Tsai W.-Y. 2015. An alternative approach to confidence interval estimation for the win ratio statistic. Biometrics, 71, 139-145.

Bebu I. and Lachin J.M. 2016. Large sample inference for a win ratio analysis of a composite outcome based on prioritized components. Biostatistics, 17, 178-187.
}
\author{Xiaodong Luo}
\seealso{\code{\link{wlogr2}},\code{\link{winratio}},\code{\link{wwratio}}}
\examples{
##########################################################
## Example 1: survival (semi-competing risks) example 
##            with terminal event having higher priority
##########################################################
##############################
##  Step 1: data generation
##############################
n<-200
rho<-0.5
b2<-0.0
b1<-0.0
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

###un-weighted win loss
wtest<-winratio(y1,y2,d1,d2,z)
summary(wtest)

i<-1 ##i=1,2,3,4
j<-1 ##j=1,2

###weighted win loss
wwtest<-wwratio(y1,y2,d1,d2,z,wty1=i,wty2=j)
summary(wwtest)

####general win loss
###Define the win loss function
comp<-function(y,x){
  y1i<-y[1];y2i<-y[2];d1i<-y[3];d2i<-y[4]
  y1j<-x[1];y2j<-x[2];d1j<-x[3];d2j<-x[4] 
  w2<-0;w1<-0;l2<-0;l1<-0
  
  if (d2j==1 & y2i>=y2j) w2<-1
  else if (d2i==1 & y2j>=y2i) l2<-1
  
  if (w2==0 & l2==0 & d1j==1 & y1i>=y1j) w1<-1
  else if (w2==0 & l2==0 & d1i==1 & y1j>=y1i) l1<-1
  
  comp<-0
  if (w2==1) comp<-1 
  else if (l2==1) comp<-(-1)
  else if (w1==1) comp<-2
  else if (l1==1) comp<-(-2)
  
  comp
}
###Use the user-defined win loss function to calculate the win loss matrix
y<-cbind(y1,y2,d1,d2)
yy1<-y[z==1,]
yy0<-y[z==0,]
n1<-sum(z==1)
n0<-sum(z==0)
bindex<-matrix(0,nrow=n1,ncol=n0)
for (i in 1:n1)for (j in 1:n0)bindex[i,j]<-comp(yy1[i,],yy0[j,])
###Use the calculated win loss matrix to calculate the general win loss statistics
bgwr<-genwr(bindex)
summary(bgwr)

##################################################################
# Note: if n>=1000 or the win loss function is complex, 
#        one may experience long runtime. One may instead use C, C++, 
#        Fortran, Python to code the win loss function.
#        The following provides an example using Fortran 95 code to 
#        define the win loss matrix and then port it back to R 
#        using the package "inline"
##################################################################

#####################################################
#  This is to install and load package "inline" 
#  so that we can compile user-defined 
#  win loss function
#
#install.packages("inline")
library("inline")


##############################################################
#  You may also need to have rtools and gcc in the PATH
#  The following code add these 
#  for the current R session ONLY
#  Please remove the '#' in the following 6 lines.
#
#rtools <- "C:\\Rtools\\bin"
#gcc <- "C:\\Rtools\\gcc-4.6.3\\bin"
#path <- strsplit(Sys.getenv("PATH"), ";")[[1]]
#new_path <- c(rtools, gcc, path)
#new_path <- new_path[!duplicated(tolower(new_path))]
#Sys.setenv(PATH = paste(new_path, collapse = ";"))
##############################################################


###Define the win loss indicator by a user-supplied function
codex5 <- "
integer::i,j,indexij,d1i,d2i,d1j,d2j,w2,w1,l2,l1
double precision::y1i,y2i,y1j,y2j
do i=1,n1,1
   y1i=y(i,1);y2i=y(i,2);d1i=dnint(y(i,3));d2i=dnint(y(i,4))
   do j=1,n0,1
      y1j=x(j,1);y2j=x(j,2);d1j=dnint(x(j,3));d2j=dnint(x(j,4)) 
      w2=0;w1=0;l2=0;l1=0
      if (d2j==1 .and. y2i>=y2j) then
         w2=1
      else if (d2i==1 .and. y2j>=y2i) then
         l2=1
      end if

      if (w2==0 .and. l2==0 .and. d1j==1 .and. y1i>=y1j) then
         w1=1
      else if (w2==0 .and. l2==0 .and. d1i==1 .and. y1j>=y1i) then
         l1=1
      end if
      aindex(i,j)=0
      if (w2==1) then 
         aindex(i,j)=1
      else if (l2==1) then
         aindex(i,j)=-1
      else if (w1==1) then
         aindex(i,j)=2
      else if (l1==1) then
         aindex(i,j)=-2
      end if
   end do
end do
"
###End of defining the win loss indicator by a user-supplied function

###Convert the above code to Fortran 95 code and port it back to R
cubefnx5<-cfunction(sig = signature(n1="integer",n0="integer",p="integer",
           y="numeric",x="numeric", aindex="integer"), 
           implicit = "none", dim = c("", "", "", "(n1,p)","(n0,p)","(n1,n0)"), 
           codex5, language="F95")

###Use the above defined function to calculate the win loss indicators
y<-cbind(y1,y2,d1,d2)
yy1<-y[z==1,]
yy0<-y[z==0,]
n1<-sum(z==1)
n0<-sum(z==0)
options(object.size=1.0E+10)
##The following is the win loss indicator matrix
aindex<-matrix(cubefnx5(n1,n0,length(y[1,]), yy1,yy0, 
               matrix(0,nrow=n1,ncol=n0))$aindex,byrow=FALSE,ncol=n0)
###Use the win loss indicator matrix to calculate the general win loss statistics
agwr<-genwr(aindex)
summary(agwr)


##########################################################
## Example 2: Continuous outcome example 
##  suppose there are two outcomes (y1,y2) following bivariate normal dist
##  y1 is more important than y2, when comparing with (x1,x2) from another subject
##  a win of first outcome if y1>x1+1 and a loss if y1<x1-1
##  if tie, i.e. |y1-x1|<=1, then a win of second outcome if y2>x2+0.5
##  and a loss if y2<x2-0.5. The other scenarios are tie.
##########################################################
##############################
##  Step 1: data generation
##############################
n<-300
rho<-0.5
b2<-2.5
b1<-2.0
z<-rep(0,n)
z[1:(n/2)]<-1

y2y<-matrix(0,nrow=n,ncol=3)
y2y[,1]<-rnorm(n);y2y[,3]<-rnorm(n)
y2y[,2]<-rho*y2y[,1]+sqrt(1-rho^2)*y2y[,3]
y1<-b1*z+y2y[,1]
y2<-b2*z+y2y[,2]


####general win loss

###Define the win loss indicator by a user-supplied function
codex6 <- "
integer::i,j,indexij,w2,w1,l2,l1
double precision::y1i,y2i,y1j,y2j
do i=1,n1,1
   y1i=y(i,1);y2i=y(i,2)
   do j=1,n0,1
      y1j=x(j,1);y2j=x(j,2) 
      w2=0;w1=0;l2=0;l1=0
      if (y1i>(y1j+1.0)) then
         w1=1
      else if (y1i<(y1j-1.0)) then
         l1=1
      end if

      if (w1==0 .and. l1==0 .and. y2i>(y2j+0.5)) then
         w2=1
      else if (w1==0 .and. l1==0 .and. y2i<(y2j-0.5)) then
         l2=1
      end if
      aindex(i,j)=0
      if (w1==1) then 
         aindex(i,j)=1
      else if (l1==1) then
         aindex(i,j)=-1
      else if (w2==1) then
         aindex(i,j)=2
      else if (l2==1) then
         aindex(i,j)=-2
      end if
   end do
end do
"
###End of defining the win loss indicator by a user-supplied function

###Convert the above code to Fortran 95 code and port it back to R
cubefnx6<-cfunction(sig = signature(n1="integer",n0="integer",p="integer",
           y="numeric",x="numeric", aindex="integer"), 
           implicit = "none", dim = c("", "", "", "(n1,p)","(n0,p)","(n1,n0)"), 
           codex6, language="F95")

###Use the above defined function to calculate the win loss indicators
y<-cbind(y1,y2)
yy1<-y[z==1,]
yy0<-y[z==0,]
n1<-sum(z==1)
n0<-sum(z==0)
options(object.size=1.0E+10)
##The following is the win loss indicator matrix
aindex<-matrix(cubefnx6(n1,n0,length(y[1,]), yy1,yy0, 
               matrix(0,nrow=n1,ncol=n0))$aindex,byrow=FALSE,ncol=n0)

###Use the win loss indicator matrix to calculate the general win loss statistics
agwr<-genwr(aindex)
summary(agwr)
}
\keyword{general win ratio}
\keyword{win ratio}
\keyword{composite endpoint}
\keyword{pairwise comparison}





