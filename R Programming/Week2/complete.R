complete <- function(directory,id=1:332)
{
	 zeros <- character()
  
  report <- data.frame(id=integer(),nobs=integer())
  for(i in id)
  {
    if(i/100 >=1)
    {
     zeros="" 
    }
    else if(i/10 >= 1)
    {
      zeros="0"
    }
    else
    {
      zeros="00"
    }
    
    a <- read.csv(paste0(directory,"\\",zeros,i,".csv"),header=TRUE)
	a<- a[complete.cases(a), ]
	nob <- nrow(a)

  newrow <- data.frame(id=i,nobs=nob)
  report <- rbind(report,newrow)
  }
  report
}