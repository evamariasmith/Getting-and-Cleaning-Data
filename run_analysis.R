rm(list=ls())
library(dplyr)

##################################################################
#1. Merges the training and the test sets to create one data set
##################################################################

#get training data
trainingvalues <- read.table("X_train.txt")
traininglabels <- read.table("y_train.txt")
trainingsub <- read.table("subject_train.txt")

#get test data
testvalues <- read.table("X_test.txt")
testlabels <- read.table("y_test.txt")
testsub <- read.table("subject_test.txt")

#get features
features <- read.table("features.txt")
features[,2] <- as.character(features[,2])

#get labels
namelabels <- read.table("activity_labels.txt")
namelabels[,2] <- as.character(namelabels[,2])

#merge training data and add labels
traindata <- cbind(trainingvalues, traininglabels, trainingsub)

#merge test data 
testdata <- cbind(testvalues, testlabels, testsub)

#merge all data
alldata <- rbind(traindata, testdata)

##################################################################
#2. Extract mean and standard deviation
##################################################################

#get mean and std from features data
featureswant <- grep(".*mean.*|.*std.*", features[,2])
featureswantnames <- features[featureswant,2]

#clean data names
featureswantnames = gsub("-mean", "Mean", featureswantnames)
featureswantnames = gsub("-std", "Std", featureswantnames)
featureswantnames <- gsub('[-()]',"",featureswantnames)

############################################################################
#3. Uses descriptive activity names to name the activities in the data set
############################################################################

#add labels to data
colnames(alldata) <- c("subject", "activity", featureswantnames)


#turn labels and subjects into factors
alldata$activity <- factor(alldata$activity, levels = namelabels[,1], labels = namelabels[,2])
alldata$subject <- as.factor(alldata$subject)

###############################################################################################
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average 
#of each variable for each activity and each subject.
##############################################################################################

library(reshape2)

alldatamelted <- melt(alldata, id = c("subject", "activity"))
alldatamean <- dcast(alldatamelted, subject + activity ~ variable, mean)

#write the table
write.table(alldatamean, "tidy.txt", row.names = FALSE, quote = FALSE)