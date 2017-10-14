
wlogr2<-function(y,d,z,wty=1)
{
    n<-length(y)
    ay<-rank(y,ties.method="min")
    da<-cbind(ay,d,z)
    db<-da[order(ay,d,z),]
    stat<-0.0
    vstat<-1.0
    abc2<-.Fortran("logrank2",as.integer(n),as.integer(db[,1]),as.integer(db[,2]),as.integer(db[,3]),as.integer(wty),stat=as.double(stat),vstat=as.double(vstat))
    tstat<-abc2$stat/sqrt(n*abc2$vstat);pstat<-2*(1-pnorm(abs(tstat)))
    list(wty=wty,stat=abc2$stat,vstat=abc2$vstat,tstat=tstat,pstat=pstat)
}

