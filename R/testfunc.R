#' testfunc
#' 
#' A placeholder for functions while I build package.
#' 
#' @param data dataframe class object
#' @param groups A number.
#' @param per a thing
#' @param pe another thing
#' @param method other thing 
#' @param indicator of a thing
#' @param g_label thingy
#' 
#' @return A table with output
#' @examples
#' flubase(df, ?, ?)
#' 
#' @export
#' 
flubase <-
  function(dat,groups,per,pe=0,method,indicator="mortality",g_label){
    
    
    if (per==52)
    {fmw<-27
    ltmw<-26
    tb<-48
    te<-17} 
    else 
    {if (per==12)
    {fmw<-7
    ltmw<-6
    tb<-12
    te<-4} 
      else 
      {return (print("Error the per must be 12 or 52"))
      }
    }
    
    
    # name of the file with result outputs
    file_results<-"res.txt"
    
    fy<-dat$year[1]
    ny<-max(dat$year)-min(dat$year)+1
    ft<-dat$todeath[1]
    
    library(FinTS)
    library(forecast)
    
    
    # Generate label for the influenza seasons, which includes months from November through March
    for (i in 1:ny){
      dat$flu_year[(dat$year==dat$year[1]+i-1 & dat$todeath>=fmw)|
                     (dat$year==dat$year[1]+i & dat$todeath<=ltmw)]=i
    }
    
    dat$flu_year[dat$flu_year==NA]<-0
    
    if (pe==0){
      epid<-dat$epi[dat$group==1]
    } else {
      epid<-(dat$todeath[dat$group==1]>=tb | dat$todeath[dat$group==1]<=te)
    }
    
    z<-cbind(dat$year[dat$group==1],dat$todeath[dat$group==1],epid)
    
    n<-length(dat$year[dat$group==1])
    
    nev<-vector(,n)
    
    k<-0
    j<-1
    while (j<=n)
    {
      ep<-dat$epi[dat$group==1]
      if (ep[j]==1)
      {
        k<-k+1
        while(ep[j]==1)
        {
          nev[j]<-k
          j<-j+1
        }
      } 
      else 
      {
        nev[j]<-0
        j<-j+1
      }
    }
    for (i in 1:groups){dat$nevent[dat$group==i]<-nev}
    
    for (i in 1:groups){
      
      print(g_label[i])
      
      x<-switch(method,
                nrm=baseRM(dat$nod[dat$group==i],dat$todeath[dat$group==i],
                           dat$epi[dat$group==i],dat$flu_year[dat$group==i],ny,tb,te,pe,per),
                nsa=baseSA(dat$nod[dat$group==i],dat$todeath[dat$group==i],
                           dat$epi[dat$group==i],dat$flu_year[dat$group==i],ny,tb,te,pe,per),
                irm=baseIt_RM(dat$nod[dat$group==i],dat$todeath[dat$group==i],
                              dat$epi[dat$group==i],dat$flu_year[dat$group==i],ny,tb,te,pe,ni=5,train=5,per),
                isa=baseIt_SA(dat$nod[dat$group==i],dat$todeath[dat$group==i],
                              dat$epi[dat$group==i],dat$flu_year[dat$group==i],ny,tb,te,pe,ni=4,per))
      
      
      
      x1<-data.frame(x)
      base.ts<-ts(x1[1], start=c(fy, ft),frequency=per)
      base.uci.ts<-ts(x1[2], start=c(fy, ft),frequency=per)
      nodinc.ts<-ts(dat$nod[dat$group==i], start=c(fy, ft),frequency=per)
      
      x11()
      plot(base.ts, type="l", ylim=c(0,max(max(base.uci.ts),max(nodinc.ts))),
           bty="l", xlab="",  ylab=indicator, lty=1, lwd=2,axes=TRUE,cex.lab=1.5, cex.axis=1.2, font.lab=2)
      
      
      z<-cbind(z,x1$da)
      
      j<-1
      aux<-FALSE
      while (j<=n){
        if (z[j,3]==1) {
          aux<-TRUE
          a1<-z[j,1]
          m1<-z[j,2]
          
          while(z[j,3]==1 & j<=n){
            a2<-z[j,1]
            m2<-z[j,2]
            j<-j+1
          }
          
          rect(a1+(m1-1)/per,par("usr")[3],a2+(m2-1)/per,par("usr")[4],col=gray(0.7),border=FALSE)
          
        }
        if(!aux){
          j<-j+1}
        aux<-FALSE
      }
      
      
      
      j<-1
      aux<-FALSE
      while (j<=n){
        if (z[j,4+i-1]==1) {
          aux<-TRUE
          a1<-z[j,1]
          m1<-z[j,2]
          
          while(z[j,4+i-1]==1 & j<=n){
            a2<-z[j,1]
            m2<-z[j,2]
            j<-j+1
          }
          
          
          
          rect(a1+(m1-1)/per,par("usr")[3],a2+(m2-1)/per,par("usr")[4],col="yellow",border=FALSE)
          
        }
        if(!aux){
          j<-j+1}
        aux<-FALSE
      }
      
      
      
      lines(base.ts, lty=1, lwd=2, col="black")
      lines(base.uci.ts, lty=1, lwd=2, col="red")
      lines(nodinc.ts, lty=1, lwd=2, col="blue")
      title(main=paste("Observed",indicator,
                       "and model baseline for group",g_label[i],sep=' '),cex.main=1.5)
      dat$beta0[dat$group==i]<-x1$beta0
      dat$beta_up[dat$group==i]<-x1$beta_up
      dat$da[dat$group==i]<-x1$da
      dat$ex[dat$group==i]<-(dat$nod[dat$group==i]-x1$beta0)*dat$da[dat$group==i]
      
      nod<-dat$nod[dat$group==i]*x1$da
      beta0<-x1$beta0*x1$da
      ex<-dat$ex[dat$group==i]*x1$da
      
      tabela<-aggregate(cbind(round(nod),round(beta0),round(ex)),list(dat$nevent[dat$group==i]),sum)
      e<-length(tabela[,1])
      tabela<-tabela[2:e,]
      tabelax<-cbind(tabela[,1:4])
      names(tabelax)=c("Event #","Observed","Expected","Excess")
      print(tabelax)
      
    }
    
    r<-data.frame(dat)
    flubase<-r
    write.table(r,file=file_results)
  }