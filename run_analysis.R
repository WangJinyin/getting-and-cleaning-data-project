##The purpose of this script is to collect, work with, and clean a data set
##The data collected from the accelerometers from the Samsung Galaxy S smartphone

##1.Merges the training and the test sets to create one data set.
##2.Extracts only the measurements on the mean and standard deviation for each measurement. 
##3.Uses descriptive activity names to name the activities in the data set
##4.Appropriately labels the data set with descriptive variable names. 
##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##create one R script called run_analysis
run_analysis <- function(){

##download the data
##checking for and creating directories
if(!file.exists("data")) {
dir.create("data")
}
##download the data from the website
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
setInternet2(use = TRUE) ##set the use of internet explorer for internet access
download.file(fileUrl, destfile = "./data/mydata.zip", method = "internal") ##method "internal" for windows users

##read the data
setwd("./data/UCI HAR Dataset") ##set working directory
subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")

##1.Merges the training and the test sets to create one data set.
subject <- rbind(subject_test, subject_train)
X <- rbind(X_test, X_train)
y <- rbind(y_test, y_train)
##remove the extra variables
rm(subject_test, X_test, y_test, subject_train, X_train, y_train)

##2.Extracts only the measurements on the mean and standard deviation for each measurement. 
##read the features
features <- read.table("./features.txt")
##specify the colname for X
colnames(X) <- paste(as.character(101:ncol(X) + 100), features$V2) ##unique the colname by adding the num
##extracts only the measurements on the mean and stadard deviation for each measurement
library("dplyr") ##library the package "dplyr"
X <- cbind(select(X, contains("mean()")), select(X, contains("std()")))
colnames(X) <- substr(colnames(X), start = 5, stop = 1000000L) ##back to initial colname

##3.Uses descriptive activity names to name the activities in the data set
##read the activity labels
activity_labels <- read.table("./activity_labels.txt")
##merge the activity(y) with the activity labels
y <- merge(y, activity_labels)

##4.Appropriately labels the data set with descriptive variable names. 
colnames(y) <- c("activityByInt", "activity") ##specify colname for y
colnames(subject) <- "subject" ##specify colname for subject
X <- cbind(subject, y$activity, X) ##form a new data set
colnames(X)[2] = "activity"

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjectUnique <- unique(X$subject) ##extract all the subjects
activityUnique <- unique(X$activity) ##extract all the activities
X2 <- data.frame(merge(subjectUnique, activityUnique), matrix(ncol = ncol(X) - 2)) ##create a new data set
colnames(X2) <- colnames(X) ##specific the colname
##calculate the value
for(i in 1:nrow(X2)){

	##select the observations for each activity and each subject; colculate the mean for every variable; assign it to the X2
	X2[i, 3:68] <- sapply(filter(X, subject == X2[i, 1] & activity == X2[i, 2])[, 3:68], mean)

}

##write the data
write.table(X2, file = "./result.txt", row.names = FALSE)
}

