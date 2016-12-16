##------------------------##

# Howard Kaplan
# Coursera: Getting and Cleaning Data 
# Week 4
# 12-18-2016

# run_analysis.R File Description:

# 1. Merges the training and the test sets to create one data set.
##Data Set can be downloaded from:
##https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
###  average of each variable for each activity and each subject.

##------------------------##

#load data table library
library(data.table)
#Set your working directory to location of the data
setwd("/Users/howie/Documents/R/data/UCI HAR Dataset")

#Read the data to dataframes
featureNames <- read.table("./features.txt", header = FALSE) 
activityLabels <- read.table("./activity_labels.txt", header = FALSE) 

subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
xTrain <- read.table("./train/X_train.txt", header = FALSE)
yTrain <- read.table("./train/y_train.txt", header = FALSE)

subjectTest <- read.table("./test/subject_test.txt", header = FALSE)
xTest <- read.table("./test/X_test.txt", header = FALSE)
yTest <- read.table("./test/y_test.txt", header = FALSE)

#Column Names Train
colnames(activityLabels) <- c('activityId', 'activityLabel')
colnames(subjectTrain) <- "Subject"
colnames(xTrain) <- featureNames[,2]
colnames(yTrain) <- "Activity"

#Column Names Test
colnames(subjectTest) <- "Subject"
colnames(xTest) <- featureNames[,2]
colnames(yTest) <- "Activity"

#bind all train data
trainSet <- cbind(yTrain,subjectTrain,xTrain)

#bind all test data
testSet <- cbind(yTest,subjectTest,xTest)

#1) Merge the training and the test sets to create one data set
mergedDataSet <- rbind(trainSet, testSet)
#dim(mergedDataSet)

#2) Extracts only the measurements on the mean and standard deviation for each measurement
#extract mean and std columns / variables using regX grepl
extract_features <- grepl(".[mM]ean|.[sS]td", names(mergedDataSet)) #match mean std text
extractedDataSet <- mergedDataSet[extract_features==TRUE] # Extract match to new dataframe
#dim(extractedDataSet)

#3) Change Activity Number to Activity Label
#Uses descriptive activity names to name the activities in the data set
#length(activityLabels[,2])#There are 6 Activity Labels
#Read numeric as character
mergedDataSet$Activity <- as.character(mergedDataSet$Activity)
#Loop over merged data and change value to Activity Label
for (i in 1:6){
  mergedDataSet $Activity[mergedDataSet $Activity == i] <- as.character(activityLabels[i,2])
}
#4) Appropriately labels the data set with descriptive variable names
names(extractedDataSet)<-gsub("Acc", "Accelerometer", names(extractedDataSet))
names(extractedDataSet)<-gsub("Gyro", "Gyroscope", names(extractedDataSet))
names(extractedDataSet)<-gsub("BodyBody", "Body", names(extractedDataSet))
names(extractedDataSet)<-gsub("Mag", "Magnitude", names(extractedDataSet))
names(extractedDataSet)<-gsub("^t", "Time", names(extractedDataSet))
names(extractedDataSet)<-gsub("^f", "Frequency", names(extractedDataSet))
names(extractedDataSet)<-gsub("tBody", "TimeBody", names(extractedDataSet))
names(extractedDataSet)<-gsub("-mean()", "Mean", names(extractedDataSet), ignore.case = TRUE)
names(extractedDataSet)<-gsub("-std()", "STD", names(extractedDataSet), ignore.case = TRUE)
names(extractedDataSet)<-gsub("-freq()", "Frequency", names(extractedDataSet), ignore.case = TRUE)
names(extractedDataSet)<-gsub("angle", "Angle", names(extractedDataSet))
names(extractedDataSet)<-gsub("gravity", "Gravity", names(extractedDataSet))

#bind columns with activity and subject
extractedDataSet <- cbind(extractedDataSet, mergedDataSet[1], mergedDataSet[2])

#5) Create the TidyData.txt file with the average of each variable for each activity and each subject
extractedDataSet <- data.table(extractedDataSet)
tidyData <- aggregate(. ~Subject + Activity, extractedDataSet, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)