#The script contains a function run.analysis() that performs the following job (It expects the dataset to be already there in the working directory):

###Merges the training and the test sets to create one data set.
###Extracts only the measurements on the mean and standard deviation for each measurement. 
###Uses descriptive activity names to name the activities in the data set
###Appropriately labels the data set with descriptive variable names. 
###Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

The dataset represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

###More information can be found here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
###Data can be downloaded from this source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

##The code makes use of 	cbind() extensively to merge the training and test data.
##Logical vectors are used to subset the table, therefore only satisfying data will be extracted.
##Dataset was merged with ActivityType to get descriptive names
##Data was properly labelled
##Tiny data was generated at the end,

