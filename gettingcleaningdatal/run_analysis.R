## Author: Carlos Reis - 12/04/2016
## Final Project of Coursera Course Getting and Cleaning Data
## Goal:  Collect, Clean and work with data set to prepare a tidy data that can be used for later analyses
## Data: Collected from the accelerometers from the Samsung Galaxy S smartphone
## Data Information: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## Data Repository: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#######################################################################################################
## --- 1. Merges the training and the test sets to create one data set ----------------------------- ##
#######################################################################################################

# Loading the package for use with function melt()
library(reshape2)

address <- paste(getwd(), "/datasciencecoursera/gettingdata/week4project", sep = "")

# Setting my working directory 
setwd(address)

# Loading labels with their activity names
activityLabels <- read.table("./dataset/activity_labels.txt")

# Loading all the features - Metadata
features <- read.table("./dataset/features.txt")

# Loading the test set
xTest <- read.table("./dataset/test/X_test.txt") # test data 
yTest <- read.table("./dataset/test/y_test.txt") # test labels
subjectTest <- read.table("./dataset/test/subject_test.txt") # identifies the subject of the activity

# Loading the training set
xTrain <- read.table("./dataset/train/X_train.txt") # training data
yTrain <- read.table("./dataset/train/y_train.txt") # training labels
subjectTrain <- read.table("./dataset/train/subject_train.txt") # identifies the subject of the activity

# Adding column name for data sets
names(xTest) <- features$V2
names(xTrain) <- features$V2
names(subjectTest) <- "subjectID"
names(subjectTrain) <- "subjectID"
names(yTest) <- "activity"
names(yTrain) <- "activity"

# Creating a unique data set
test <- cbind(subjectTest, yTest, xTest)
train <- cbind(subjectTrain, yTrain, xTrain)

train_and_test <- rbind(train, test) # training and test

#######################################################################################################
## --- 2. Extracts only the measurements on the mean and standard deviation for each measurement --- ##
#######################################################################################################

# Define which columns contain mean or std
measurements <- grepl("mean\\(\\)", names(train_and_test)) | grepl("std\\(\\)", names(train_and_test))

# Keeping only the column subjectID and activity
measurements[1:2] <- TRUE

# Removing the other columns
train_and_test <- train_and_test[, measurements]

#######################################################################################################
## --- 3. Uses descriptive activity names to name the activities in the data set ------------------- ##
#######################################################################################################

# Done in the data set samsung_train_test_tidy.csv

#######################################################################################################
## --- 4. Appropriately labels the data set with descriptive variable names ------------------------ ##
#######################################################################################################

# Setting a name of activities
activityLabelsLower <- tolower(activityLabels$V2) # Lowering the characterr of activities
activityLabelsLower2 <- sub("_", "", activityLabelsLower) # Replacement of the "_" for "" 

train_and_test$activity <- factor(train_and_test$activity, labels = c(activityLabelsLower[1], 
                                                                      activityLabelsLower[2], 
                                                                      activityLabelsLower[3], 
                                                                      activityLabelsLower[4], 
                                                                      activityLabelsLower[5], 
                                                                      activityLabelsLower[6]))

#######################################################################################################
## --- 5. From the data set in step 4, creates a second, independent tidy data set with the average  ## 
## --- of each variable for each activity and each subject. ---------------------------------------- ##
#######################################################################################################

fusionData <- melt(train_and_test, id = c("subjectID","activity"))

# Creating a tidy data set
samsung_train_test_tidy <- dcast(fusionData, subjectID + activity ~ variable, mean)

# Writing a file
write.csv(samsung_train_test_tidy, "samsung_train_test_tidy.csv", row.names=FALSE)

# Writing a txt file
write.table(samsung_train_test_tidy, file = "./samsung_train_test_tidy.txt")
