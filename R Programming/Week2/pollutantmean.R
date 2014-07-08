pollutantmean <- function(directory, pollutant, id=1:332)
{
  
  zeros <- character()
  x <- numeric()
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
    x <- c(x,a[ , pollutant])
  }
  
  mean(x,na.rm=TRUE)
  
}