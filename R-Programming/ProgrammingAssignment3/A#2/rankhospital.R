rankhospital<-function(state,outcome,num="best")
{
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  colbaseName<-"Hospital.30.Day.Death..Mortality..Rates.from."
  stateDateFrame<-outcomedata[outcomedata$State==state,]
  if(length(stateDateFrame$State)==0)
  {
    stop("invalid state")
  }
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
  mvector<-suppressWarnings(as.numeric(stateDateFrame[ ,colbaseName]))
   
  bestHosiptalNames<-stateDateFrame[ order(  x=mvector   ,partial=stateDateFrame$Hospital.Name  ,na.last=NA)  , ]
   
   
  hospitalLocation<-1
  if(num=="best")
  {
    hospitalLocation<-1
  }
  else if(num=="worst")
  {
    hospitalLocation<-length(bestHosiptalNames$Hospital.Name) 
  }
  else if(num<=length(  bestHosiptalNames$Hospital.Name)) 
  {
    hospitalLocation<-num
  }
  else
  {
    return (NA)
  }
  bestHosiptalName<-bestHosiptalNames[ hospitalLocation,"Hospital.Name"]
  return(bestHosiptalName)
  
}
