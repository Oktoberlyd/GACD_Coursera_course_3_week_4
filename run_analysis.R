##############################################
#                                            #
# Getting and cleaning data course project   #
#                                            #
#                                            #
#                                            #
#                                            #
##############################################

# libaries are loaded
library(tidyverse)
library(lubridate)
library(data.table)

#working directory is set
setwd("C:/Users/ocari/OneDrive/Documents/Coursera")


#data i collected and unzipped
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "activity_data.zip")
unzip("activity_data.zip") #data is unzipped to /UCI HAR Dataset

#check files
list.files('UCI HAR Dataset/test')
list.files('UCI HAR Dataset/train')

#load data files
subject_test <- fread('UCI HAR Dataset/test/subject_test.txt')
subject_train <- fread('UCI HAR Dataset/train/subject_train.txt')
x_test <- fread('UCI HAR Dataset/test/X_test.txt')
y_test <- fread('UCI HAR Dataset/test/Y_test.txt')
x_train <- fread('UCI HAR Dataset/train/X_train.txt')
y_train <- fread('UCI HAR Dataset/train/Y_train.txt')

#load helper/label files
activity_labels <- fread('UCI HAR Dataset/activity_labels.txt')
features <- fread('UCI HAR Dataset/features.txt')

#inspect datasets
str(y_test)
str(x_test)
str(y_train)
str(x_train)
str(subject_test)
str(subject_train)
str(activity_labels)
str(features)


'4. Appropriately labels the data set with descriptive variable names.'
#variabelnames to test and train are inserted from features
colnames(x_test) <- features$V2
colnames(x_train) <- features$V2


'3. Uses descriptive activity names to name the activities in the data set'
#y_test and y_train af joined with activity_labels to get the right description
y_test_explained <- left_join(y_test, activity_labels, by = c("V1" = "V1"))
y_train_explained <- left_join(y_train, activity_labels, by = c("V1" = "V1"))

#rename variables
colnames(y_test_explained) <- c("activity_label", "activity_name")
colnames(y_train_explained) <- c("activity_label", "activity_name")

#subject_ variables are changed
colnames(subject_test) <- c("subject") 
colnames(subject_train) <- c("subject")

#testsets are merged using cbind (subject_test, y_test, x_test) and test_or_train column added
test_binded <- cbind(subject_test, dplyr::select(y_test_explained, activity_name), x_test)
test_binded$test_or_train <- c("test") 

#trainsets are merged using cbind (subject_train, y_train, x_train) and test_or_train column added
train_binded <- cbind(subject_train, dplyr::select(y_train_explained, activity_name), x_train)
train_binded$test_or_train <- c("train") 

'2. Extracts only the measurements on the mean and standard deviation for each measurement.' 
#only measurements on the mean and standard deviation for each measurement
test_binded_collapsed <- dplyr::select(test_binded, 1:2 | contains(c("std", "mean", "test_or_train")))
train_binded_collapsed <- dplyr::select(train_binded, 1:2 | contains(c("std", "mean", "test_or_train")))

'1. Merges the training and the test sets to create one data set.'
#train and test sets are merged

complete_data_set <- rbind(test_binded_collapsed, train_binded_collapsed)


'5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.'
#first figure out which columns to mean
#max column
ncol(dplyr::select(complete_data_set, -subject, -activity_name, -test_or_train))


tidy_data_set <- complete_data_set %>%
  group_by(subject, activity_name) %>%
  summarise(across(1:86, mean))


#writing table for upload
write.table(tidy_data_set, "C:/Users/ocari/OneDrive/Documents/Coursera/tidy_data_set.txt", row.name=FALSE)

