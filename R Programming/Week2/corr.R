corr <- function(directory, threshold=0)
{
	zeros <- character()
	cors <- numeric()
	for(i in 1:332)
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
		data_frame <-  read.csv(paste0(directory,"\\",zeros,i,".csv"),header=TRUE)
		xl <- length(data_frame$sulfate[complete.cases(data_frame$sulfate)])
		xy <- length( data_frame$nitrate[complete.cases(data_frame$nitrate)])
		
		if(xl >=threshold & xy>=threshold)
		{
			cors <- c(cors,cor(data_frame$sulfate,data_frame$nitrate,use="pairwise.complete.obs"))
		}
		
	}
	cors
}