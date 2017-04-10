"wwratio"<-function(y1,y2,d1,d2,z,wty1=1,wty2=1){
    n<-length(y1)
    w2<-l2<-w1<-l1<-0.0
    ay1<-rank(y1,ties.method="min")
    ay2<-rank(y2,ties.method="min")
    da<-cbind(ay1,ay2,d1,d2,z)
    db<-da[order(ay2,ay1,d2,d1,z),]
    stat<-vstat<-rep(1,3)
    abc1<-.Fortran("wwrnullb",as.integer(n),as.integer(db[,1]),as.integer(db[,2]),as.integer(db[,3]),as.integer(db[,4]),as.integer(db[,5]),
                              as.integer(wty1),as.integer(wty2),stat=as.double(stat),vstat=as.double(vstat),
                              w2=as.double(w2),l2=as.double(l2),w1=as.double(w1),l1=as.double(l1))

    wd<-abc1$stat[1];wr<-abc1$stat[2];wp<-abc1$stat[3]
    vd<-abc1$vstat[1];vr<-abc1$vstat[2];vp<-abc1$vstat[3]
    td<-n^(-3/2)*wd/sqrt(vd);pd<-2*(1-pnorm(abs(td)))
    tr<-sqrt(n)*log(wr)/sqrt(vr);pr<-2*(1-pnorm(abs(tr)))
    tp<-sqrt(n)*log(wp)/sqrt(vp);pp<-2*(1-pnorm(abs(tp)))
    w2<-abc1$w2;l2<-abc1$l2;w1<-abc1$w1;l1<-abc1$l1
    cwindex<-c(w2,w1)/(w2+w1+l2+l1)
    clindex<-c(l2,l1)/(w2+w1+l2+l1)
    list(wty1=wty1,wty2=wty2,totalw=(w2+w1),totall=(l2+l1),
         tw=c(w2,w1),tl=c(l2,l1),xp=c(w2,w1)/c(l2,l1),
         cwindex=cwindex,clindex=clindex,
         wr=wr,vr=vr,tr=tr,pr=pr,wd=wd,vd=vd,td=td,pd=pd,wp=wp,vp=vp,tp=tp,pp=pp)
}


