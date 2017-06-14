"winratio"<-function(y1,y2,d,z,doublebad=1)
{
      d<-ifelse(d==1 | d==2 |d==3 |d==4, d,NA)
      if (sum(is.na(d))>0) stop("The event indicator 'd' only takes values 1,2,3,4")
      z<-ifelse(z==0 | z==1, z,NA)
      if (sum(is.na(z))>0) stop("The group indicator 'z' only takes values 0 and 1")
      if(sum(is.na(y1))>0|sum(is.na(y2))>0|sum(is.na(d))>0|sum(is.na(z))>0) stop("Please check missing data.")
      maxlen<-max(length(y1),length(y2),length(d),length(z))
      minlen<-min(length(y1),length(y2),length(d),length(z))
      if(maxlen>minlen) stop("Please check the lengths of the data.")
      n1<-sum(z==1)
      n0<-sum(z==0)
      if (n1==0|n0==0) stop("Neither group can be emepty")

      n<-length(y1)
      w2<-l2<-w1<-l1<-0.0
      wr<-1.0
      wd<-0.0
      wp<-1.0
      vr<-1.0
      vd<-1.0
      vp<-1.0
      abc1<-.Fortran("xwinratio",as.integer(n),as.double(y1),as.double(y2),as.integer(d),as.integer(z),doublebad=as.integer(doublebad),
                               wr=as.double(wr),vr=as.double(vr),wd=as.double(wd),vd=as.double(vd),wp=as.double(wp),vp=as.double(vp),
                               w2=as.double(w2),l2=as.double(l2),w1=as.double(w1),l1=as.double(l1))
      wr<-abc1$wr
      wd<-abc1$wd
      wp<-abc1$wp
      vr<-abc1$vr
      vd<-abc1$vd
      vp<-abc1$vp
      td<-n^(-3/2)*wd/sqrt(vd);pd<-2*(1-pnorm(abs(td)))
      tr<-sqrt(n)*log(wr)*wr/sqrt(vr);pr<-2*(1-pnorm(abs(tr)))
      tp<-sqrt(n)*log(wp)*wp/sqrt(vp);pp<-2*(1-pnorm(abs(tp)))
      w2<-abc1$w2;l2<-abc1$l2;w1<-abc1$w1;l1<-abc1$l1
      cwindex<-c(w2,w1)/(w2+w1+l2+l1)
      clindex<-c(l2,l1)/(w2+w1+l2+l1)
      me<-list(doublebad=doublebad,n1=n1,n0=n0,n=n,totalw=(w2+w1),totall=(l2+l1),
         tw=c(w2,w1),tl=c(l2,l1),xp=c(w2,w1)/c(l2,l1),
         cwindex=cwindex,clindex=clindex,
         wr=wr,vr=vr,tr=tr,pr=pr,wd=wd,vd=vd,td=td,pd=pd,wp=wp,vp=vp,tp=tp,pp=pp)
      class(me)<-append(class(me),"WWR")
      return(me)
}



