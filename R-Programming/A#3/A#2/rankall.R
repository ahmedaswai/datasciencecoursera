rankall<-function(outcome,num="worst")
{
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  colbaseName<-""
  if(outcome=="heart attack")
  {
    colbaseName<-paste(colbaseName,"Heart.Attack",sep="")
  } 
  else if(outcome=="heart failure")
  {
    colbaseName<-paste(colbaseName,"Heart.Failure",sep="")
  }   
  
  else if(outcome=="pneumonia")
  {
    colbaseName<-paste(colbaseName,"Pneumonia",sep="")
  }
  else
  {
    stop("invalid outcome")
  }
  states<-sort(unique(outcomedata$State))
   
  
  ma<-matrix(ncol=2,nrow=0 )
  loopCount <-1
  for(st in states)
  {
       l<-c(st,rankhospital(st,outcome,num,outcomedata))
        ma<-rbind(ma,l)
      
    }
  mframe<- data.frame(ma)
  colnames(mframe)<- c  ("state","hospital")
  rownames(mframe)<-mframe$state
  return(mframe) 
  #return(ma)
}