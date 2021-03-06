simRA <-
function(SF,LS,numberLitter,eggWeight,offspringWeight,
                             lifespan=5001+(numberLitter-1)*(round(5001/SF,digits=0)),
                             ymax=NULL,xmax=NULL,cex.axis=0.8,col="black",
                             lwd=1,margins=c(5,6,4,2),mean=TRUE,colMean="tomato",add=FALSE,
                             at=-30,boxCol="black",power=(3/2),plot=TRUE){
  
  lambda<-round(5001/SF,digits=0)
  
  seq_stage<-seq(0,50,0.01)
  
  M<-matrix(nrow = 5001+(numberLitter-1)*(lambda),ncol=numberLitter,data = NA)
  
  for(c in 1:ncol(M))
  {
    for(i in 1:5001)
    {
      M[i+(c-1)*lambda,c]<-seq_stage[i]
    }
  }
  
  
  M<-M[1:lifespan,]
  
  sub_hypothetical_function<-function(stage)
  {
    powerStage<-stage^power
    intercept<-log(eggWeight)
    beta<-(log(offspringWeight)-log(eggWeight))/(50^power)
    exp(intercept+beta*powerStage)
  }
  
  
  M2<-sub_hypothetical_function(M)
  M3<-M2*LS
  
  data<-data.frame(cbind(seq(length.out=nrow(M3),from=0,by=0.01),rowSums(M3,na.rm=T)))
  colnames(data)<-c("time","burden")
  
  if(plot==TRUE)
  {
    if(is.null(ymax))
    {
      ymax<-max(data$burden)
    }else{ymax<-ymax}
    
    if(is.null(xmax))
    {
      xmax<-max(data$time)
    }else{xmax<-xmax}
    
    if(add==FALSE)
    {
      par(mar=margins)
      plot(data$burden~data$time,type="l",cex.axis=cex.axis,axes=F,
           xlab="",ylab="Reproductive burden",col=col,xlim=c(-50,xmax),ylim=c(0,ymax),lwd=1)
      
      boxplot(data$burden,add=T,at=at,boxwex=15,border=boxCol,pch=20,axes=FALSE)
      
      if(mean==TRUE)
      {
        segments(x0 =at-3,x1=at+3,y0=mean(data$burden),y1=mean(data$burden),col=colMean,lwd=3)
        
      }
      
      axis(side = 1,labels = round(seq(0,xmax,length.out = 5),1),at=seq(0,xmax,length.out = 5),cex.axis=.8)
      axis(side = 2,labels = round(seq(0,ymax,length.out = 5),1),at=seq(0,ymax,length.out = 5),cex.axis=.8)
      mtext(text = "Time [stages]",cex = 0.8,line=-2.5,outer=T,side=1,at = .65)
      
    }
    
    if(add==TRUE)
    {
      boxplot(data$burden,add=T,at=at,boxwex=15,border=boxCol,pch=20,axes=FALSE)
      lines(data$burden~data$time,col=col,lwd=lwd)
      if(mean==TRUE)
      {
        segments(x0 =at-3,x1=at+3,y0=mean(data$burden),y1=mean(data$burden),col=colMean,lwd=3)
        
      }
    }
  }
  
  invisible(data)
}
