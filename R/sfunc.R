summary.WWR <- function(object,...,digits=4){
  #cat("********************************************************************************************************",\n")
  t <- object
  intera<-paste(round(c(t$wd-1.96*t$n^(3/2)*sqrt(t$vd[1]),t$wd+1.96*t$n^(3/2)*sqrt(t$vd[1])),digits=digits))
  interb<-paste(round(exp(c(log(t$wr)-1.96*sqrt(t$vr/t$wr^2/t$n),log(t$wr)+1.96*sqrt(t$vr/t$wr^2/t$n))),digits=digits))
  interc<-paste(round(exp(c(log(t$wp)-1.96*sqrt(t$vp/t$wp^2/t$n),log(t$wp)+1.96*sqrt(t$vp/t$wp^2/t$n))),digits=digits))
  cat("Total number of subjects:                           ",paste(t$n),"\n")
  cat("Number of subjects in group 1:                      ",paste(t$n1),"\n")
  cat("Number of subjects in group 0:                      ",paste(t$n0),"\n\n")
  
  cat("Win total in group 1:                               ",paste(round(t$totalw,digits=2)),"\n")
  cat("Loss total in group 1:                              ",paste(round(t$totall,digits=2)),"\n")
  cat("Win total in group 1 from most to least important:  ",paste(round(t$tw,digits=2)),"\n")
  cat("Loss total in group 1 from most to least important: ",paste(round(t$tl,digits=2)),"\n")
  cat("Win contribution indexes in group 1:                ",paste(round(t$cwindex,digits=2)),"\n")
  cat("Loss contribution indexes in group 1:               ",paste(round(t$clindex,digits=2)),"\n\n")
  
  cat("The win difference statistic:        ","Test Stat  ",paste(round(t$wd,digits=digits)),"\n")
  cat("                                     ","sd         ",paste(round(sqrt(t$vd[1]),digits=digits)),"\n")
  cat("                                     ","z          ",paste(round(t$td[1],digits=digits)),"\n")
  cat("                                     ","p-value    ",paste(round(t$pd[1],digits=digits)),"\n")
  cat("                                     ","95% CI     ",paste('(',intera[1],',',intera[2],')',sep=""),"\n\n")
  
  cat(" ","\n")
  cat("The win ratio statistic:             ","Test Stat  ",paste(round(t$wr,digits=digits)),"\n")
  cat("                                     ","sd         ",paste(round(sqrt(t$vr),digits=digits)),"\n")
  cat("                                     ","z          ",paste(round(t$tr,digits=digits)),"\n")
  cat("                                     ","p-value    ",paste(round(t$pr,digits=digits)),"\n")
  cat("                                     ","95% CI     ",paste('(',interb[1],',',interb[2],')',sep=""),"\n\n")
  
  cat(" ","\n")
  cat("The win product statistic:           ","Test Stat  ",paste(round(t$wp,digits=digits)),"\n")
  cat("                                     ","sd         ",paste(round(sqrt(t$vp),digits=digits)),"\n")
  cat("                                     ","z          ",paste(round(t$tp,digits=digits)),"\n")
  cat("                                     ","p-value    ",paste(round(t$pp,digits=digits)),"\n")
  cat("                                     ","95% CI     ",paste('(',interc[1],',',interc[2],')',sep=""),"\n")
  
}


# Now need to print the summary too:
#print.fsummary <- function(x, ...) {
#  cat("f summary!\n")
#  # Nice summary print goes here...
#}

# Plot method
#plot.f <-function(x,p=0.3,...){ cat("PLOTTING!\n") }