"winratio"<-function(y1,y2,d,z,doublebad=1)
{
      n<-length(y1)
      w2<-l2<-w1<-l1<-0.0
      wr<-1.0
      wd<-0.0
      wp<-1.0
      vr<-1.0
      vd<-1.0
      vp<-1.0
      abc1<-.Fortran("winratio",as.integer(n),as.double(y1),as.double(y2),as.integer(d),as.integer(z),doublebad=as.integer(doublebad),
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
    list(doublebad=doublebad,w2=w2,l2=l2,w1=w1,l1=l1,wr=wr,vr=vr,tr=tr,pr=pr,wd=wd,vd=vd,td=td,pd=pd,wp=wp,vp=vp,tp=tp,pp=pp)
}



