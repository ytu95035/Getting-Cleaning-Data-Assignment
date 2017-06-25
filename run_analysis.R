# Getting and Cleaning Data Course Project
# run_analysis.R should do the following. 
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#load the necessary library for packages
library(dplyr)
library(data.table)
# setwd to the project folder
# the file (getdata-projectfiles-UCI-HAR-Dataset.zip) has been downloaded and unziped under this folder
setwd ("./R/coursera/CleanDataProject")
# CleanDataProject Folder structure
# two subfolder -- test and train
# four txt files -- README, acctivity_labels, features, features_info

# 1.Merges the training and the test sets to create one data set.
# Read features and activity labels
features <- read.table("./features.txt")
activity <- read.table("./activity_labels.txt", header = FALSE)

# read data into table first
x_train <- read.table("./train/X_train.txt", header = FALSE)
X_test <- read.table("./test/X_test.txt", header = FALSE)
y_train <- read.table("./train/y_train.txt", header = FALSE)
y_test <- read.table("./test/y_test.txt", header = FALSE)
subject_train <- read.table("./train/subject_train.txt", header = FALSE)
subject_test <- read.table("./test/subject_test.txt", header = FALSE)

# Combines both train and test data table by rows
x <- rbind(x_train, X_test)
y <- rbind(y_train, y_test)
s <- rbind(subject_train, subject_test)

#rename column name for three data sets
colnames(x) <- t(features[2])
colnames(y) <- "Activity"
colnames(s) <- "Subject"

# combines all data x, y s by columns
alldata <- cbind(x,y,s)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

# Search for matches for mean or standard deviation (std) column id

extractmeanstd <- grep("*mean*|*std*", names(alldata), ignore.case = TRUE)

# include Activity=562 and Subject=563 into the final column list
interestcolumns <- c(extractmeanstd, 562, 563)

#extract data only interestcolumns
interestdata <- alldata[, interestcolumns]

# 3. Uses descriptive activity names to name the activities in the data set:
# the column 87 is activity with number
# will replace with activity labels which read at the beginning of the script

interestdata[, 87] = activity[interestdata[, 87], 2]

#4.Appropriately labels the data set with descriptive variable names.

# change column name
names(interestdata)<-gsub("Acc", "Accelerometer", names(interestdata))
names(interestdata)<-gsub("Gyro", "Gyroscope", names(interestdata))
names(interestdata)<-gsub("BodyBody", "Body", names(interestdata))
names(interestdata)<-gsub("Mag", "Magnitude", names(interestdata))
names(interestdata)<-gsub("^t", "Time", names(interestdata))
names(interestdata)<-gsub("^f", "Frequency", names(interestdata))
names(interestdata)<-gsub("tBody", "TimeBody", names(interestdata))
names(interestdata)<-gsub("-mean()", "Mean", names(interestdata), ignore.case = TRUE)
names(interestdata)<-gsub("-std()", "STD", names(interestdata), ignore.case = TRUE)
names(interestdata)<-gsub("-freq()", "Frequency", names(interestdata), ignore.case = TRUE)
names(interestdata)<-gsub("()", "", names(interestdata), ignore.case = TRUE)


# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#First, set Subject as a factor variable.
interestdata$Subject <- as.factor(interestdata$Subject)
interestdata <- data.table(interestdata)

#Second, create tidydata as a data set with average (mean) for each activity and subject 
tidydata <- aggregate(. ~Subject + Activity, interestdata, mean)

#Then, order the enties in tidyData and write it into data file tidydata.txt

tidydatasort <- tidydata[order(tidydata$Subject,tidydata$Activity),]
write.table(tidydatasort, file = "tidydata.txt", row.names = FALSE)


