best<-function( state,outcome )
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
  
  bestHosiptalNames<-stateDateFrame[suppressWarnings (as.numeric(  stateDateFrame[,colbaseName]))  ==min(suppressWarnings(as.numeric( stateDateFrame[,colbaseName]  ) ),na.rm=TRUE)
                                    ,"Hospital.Name"]
  
   
  bestHosiptalNames[5]
 
}