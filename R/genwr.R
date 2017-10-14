
genwr<-function(aindex){
    aindex<-as.matrix(aindex)
    n1<-nrow(aindex)
    n0<-ncol(aindex)
    n<-n1+n0
    kk<-max(abs(aindex))
    tw<-tl<-xp<-rep(0,kk)
    wr<-vr<-vr0<-wd<-vd0<-wp<-vp<-vp0<-0.0
    vd<-rep(0,2)
    abc1<-.Fortran("xgenwr",as.integer(n1),as.integer(n0),as.integer(kk), as.integer(aindex),
                           tw=as.integer(tw),tl=as.integer(tl),xp=as.double(xp),wr=as.double(wr),vr=as.double(vr),vr0=as.double(vr0),
                           wd=as.double(wd),vd=as.double(vd),vd0=as.double(vd0),
                           wp=as.double(wp),vp=as.double(vp),vp0=as.double(vp0))
    wd<-abc1$wd;wr<-abc1$wr;wp<-abc1$wp
    vd<-abc1$vd;vr<-abc1$vr;vp<-abc1$vp
    vd0<-abc1$vd0;vr0<-abc1$vr0;vp0<-abc1$vp0
    td<-n^(-3/2)*wd/sqrt(vd);pd<-2*(1-pnorm(abs(td)))
    td0<-n^(-3/2)*wd/sqrt(vd0);pd0<-2*(1-pnorm(abs(td0)))
    tr<-sqrt(n)*log(wr)*wr/sqrt(vr);pr<-2*(1-pnorm(abs(tr)))
    tr0<-sqrt(n)*log(wr)/sqrt(vr0);pr0<-2*(1-pnorm(abs(tr0)))
    tp<-sqrt(n)*log(wp)*wp/sqrt(vp);pp<-2*(1-pnorm(abs(tp)))
    tp0<-sqrt(n)*log(wp)/sqrt(vp0);pp0<-2*(1-pnorm(abs(tp0)))
    tw<-abc1$tw;tl<-abc1$tl
    cwindex<-tw/sum(tw+tl)
    clindex<-tl/sum(tw+tl)
    xp<-abc1$xp
    me<-list(n1=n1,n0=n0,n=n,totalw=sum(tw), totall=sum(tl),tw=tw,tl=tl,xp=xp,cwindex=cwindex,clindex=clindex,
         wr=wr,vr=vr,vr0=vr0,tr=tr,pr=pr,tr0=tr0,pr0=pr0,
         wd=wd,vd=vd,vd0=vd0,td=td,pd=pd,td0=td0,pd0=pd0,
         wp=wp,vp=vp,vp0=vp0,tp=tp,pp=pp,tp0=tp0,pp0=pp0)
    class(me)<-append(class(me),"WWR")
    return(me)
}



