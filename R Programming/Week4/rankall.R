rankall <- function(outcome, num = "best") 
{
  outcomes <- read.csv("outcome-of-care-measures.csv")
  
  diseases<-c(Heart.Attack="HEART ATTACK",Heart.Failure="HEART FAILURE",Pneumonia="PNEUMONIA")
  if(length(grep(toupper(outcome),diseases,ignore.case=TRUE))==0)
  {
    
    stop("invalid outcome");
  }
  #
  disease <- names(diseases[diseases==toupper(outcome)]) 
  colNum <-grep(paste0("^Hospital.30.Day.Death..Mortality..Rates.from.",disease),names(aloutcomes))
  frame <- data.frame(hospital=character(),state=character())
  i<- 1
  for(states in unique(as.character(outcomes$State)))
  {
    subData<-outcomes[ which(outcomes$State==states),]

    if(num=="best")
    {
      suppressWarnings( a<-subData[order(as.numeric(as.character(subData[,colNum])),as.character(as.character(subData[,2])),na.last=TRUE), ])
      frameRow<- data.frame(hospital=as.character(a[1,2]),state=states)
      row.names(frameRow)<-states
      frame<-rbind(frame,frameRow)
      #as.character(a[1,2])
    }
    else if(num=="worst")
    {
      suppressWarnings( a<-subData[order(as.numeric(as.character(subData[,colNum])),as.character(as.character(subData[,2])),na.last=TRUE,decreasing=TRUE), ])
      
     # suppressWarnings(a<- subset(subData,as.numeric(as.character(subData[,colNum]))==max(as.numeric(as.character(subData[,colNum])),na.rm=TRUE))[,2])
      frameRow<- data.frame(hospital=as.character(sort(as.character(a[1,2]))),state=states)
      row.names(frameRow)<-states
      frame<-rbind(frame,frameRow)
      
      #as.character(sort(a))
    }
    else if(num>nrow(subData))
    {
      suppressWarnings( a<-subData[order(as.numeric(as.character(subData[,colNum])),as.character(as.character(subData[,2])),na.last=TRUE), ])
      frameRow<- data.frame(hospital=as.character(a[num,2]),state=states)
      row.names(frameRow)<-states
      frame<-rbind(frame,frameRow)
      
      #as.character(a[num,2])
      
    }
    else
    {
      suppressWarnings( a<-subData[order(as.numeric(as.character(subData[,colNum])),as.character(as.character(subData[,2])),na.last=TRUE), ])
      frameRow<- data.frame(hospital=as.character(a[num,2]),state=states)
      row.names(frameRow)<-states
      frame<-rbind(frame,frameRow)
      #as.character(a[num,2])
    }
    
    i<-i+1
    
  }
  frame
  
}