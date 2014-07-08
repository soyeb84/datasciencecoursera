add2 <- function(x,y){
  
  x+y
  
}

above10 <- function(x){
  
  use <- x>10
  x[use]
# }

above <- function(x,n=10)
{
  use <- x>n
  x[use]
  
}

columnmean <- function(y){
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(y[,i])
  }
  means
  
}

cube <- function(x, n) {
  x^3
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}



makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}



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
    
    stop("invalid state");
  }
  aloutcomes<- subset(outcomes,outcomes$State==state)
  #print(aloutcomes[,11])
  disease <- names(diseases[diseases==toupper(outcome)]) 
  colNum <-grep(paste0("^Hospital.30.Day.Death..Mortality..Rates.from.",disease),names(aloutcomes))
  suppressWarnings(column <-as.numeric(as.character(aloutcomes[,colNum])))
  
 suppressWarnings( minMortalityRate <- min(column,na.rm=TRUE))
 suppressWarnings(hospitals<- subset(aloutcomes,as.numeric(as.character(aloutcomes[,colNum]))==min(as.numeric(as.character(aloutcomes[,colNum])),na.rm=TRUE))[,2])
 as.character(hospitals)
}

