rankhospital <- function(state,outcome, num="best")
{
  outcomes <- read.csv("outcome-of-care-measures.csv")
  
  diseases<-c(Heart.Attack="HEART ATTACK",Heart.Failure="HEART FAILURE",Pneumonia="PNEUMONIA")
  if(length(grep(state,outcomes$State,ignore.case=TRUE))==0)
  {
    stop("invalid state")
  }
  if(length(grep(toupper(outcome),diseases,ignore.case=TRUE))==0)
  {
    
    stop("invalid outcome");
  }
  subData<-outcomes[ which(outcomes$State==state),]
  disease <- names(diseases[diseases==toupper(outcome)]) 
  colNum <-grep(paste0("^Hospital.30.Day.Death..Mortality..Rates.from.",disease),names(aloutcomes))
  
  if(num=="best")
  {
    suppressWarnings( a<-subData[order(as.numeric(as.character(subData[,colNum])),as.character(as.character(subData[,2])),na.last=TRUE), ])
    
    as.character(a[1,2])
  }
  else if(num=="worst")
  {
    #suppressWarnings( a<-subData[order(as.numeric(as.character(subData[,17])),as.character(as.character(subData[,2])),na.last=TRUE), ])
    
    suppressWarnings(a<- subset(subData,as.numeric(as.character(subData[,colNum]))==max(as.numeric(as.character(subData[,colNum])),na.rm=TRUE))[,2])
    
    
    as.character(sort(a))
  }
  else if(num>nrow(subData))
  {
    suppressWarnings( a<-subData[order(as.numeric(as.character(subData[,colNum])),as.character(as.character(subData[,2])),na.last=TRUE), ])
    as.character(a[num,2])
    
  }
  else
  {
    suppressWarnings( a<-subData[order(as.numeric(as.character(subData[,colNum])),as.character(as.character(subData[,2])),na.last=TRUE), ])
    as.character(a[num,2])
  }
  
  
}