
best <- function(state,outcome)
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
  aloutcomes<- subset(outcomes,outcomes$State==state)
  #print(aloutcomes[,11])
  disease <- names(diseases[diseases==toupper(outcome)]) 
  colNum <-grep(paste0("^Hospital.30.Day.Death..Mortality..Rates.from.",disease),names(aloutcomes))
  suppressWarnings(column <-as.numeric(as.character(aloutcomes[,colNum])))
  
  suppressWarnings( minMortalityRate <- min(column,na.rm=TRUE))
  suppressWarnings(hospitals<- subset(aloutcomes,as.numeric(as.character(aloutcomes[,colNum]))==min(as.numeric(as.character(aloutcomes[,colNum])),na.rm=TRUE))[,2])
  as.character(sort(hospitals)[1])
}