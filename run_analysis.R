# Script for Getting and Cleaning Data Course Project
# The purpose of this project is to demonstrate the ability to collect, 
# work with, and clean a data set. The goal is to prepare tidy data that
# can be used for later analysis.
# 
# the items to be submitted for this project are as follows:
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performingthe analysis
# 3) a code book that describes the variables, the data, and any transformations
#    or work performed to clean up the data called CodeBook.md. 
# 4) a README.md in the repo with the scripts. This repo explains how all of the scripts
#    work and how they are connected.

library("plyr")
library("dplyr")
library("tidyr")
library("data.table")

## Download file
if(!file.exists("data")) { dir.create("data") }

## Prepare download
fileProject <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileProject, destfile = "./data/courseproject.zip")

## Use download date as a reference
dateDownloaded <- date()
dateDownloaded

## Check if file was created
list.files("./data")

## Unzip file
unzip("./data/courseproject.zip")

## Check if unziped directory was created
list.files()

## Check if files were unziped
list.files("UCI HAR Dataset")

## Read files

## General files
features <- read.table("UCI HAR Dataset/features.txt")
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")

## Training files
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt")

## Test files
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt")
xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("UCI HAR Dataset/test/y_test.txt")

## Conditioning

## Modify names
### Training files
names(subjecttrain) <- c("subject")
names(ytrain) <- c("activity")

### Test files
names(subjecttest) <- c("subject")
names(ytest) <- c("activity")

## Simplify content
##General files
activitylabels[,2] <- tolower(activitylabels[,2])
activitylabels[,2] <- gsub("_", "", activitylabels[,2])
activitylabels[,2] <- gsub("stairs", "", activitylabels[,2])

##Training files
ytrain$activity <- gsub("1","walking", ytrain$activity)
ytrain$activity <- gsub("2","walkingup", ytrain$activity)
ytrain$activity <- gsub("3","walkingdown", ytrain$activity)
ytrain$activity <- gsub("4","sitting", ytrain$activity)
ytrain$activity <- gsub("5","standing", ytrain$activity)
ytrain$activity <- gsub("6","laying", ytrain$activity)

## Test files
ytest$activity <- gsub("1","walking", ytest$activity)
ytest$activity <- gsub("2","walkingup", ytest$activity)
ytest$activity <- gsub("3","walkingdown", ytest$activity)
ytest$activity <- gsub("4","sitting", ytest$activity)
ytest$activity <- gsub("5","standing", ytest$activity)
ytest$activity <- gsub("6","laying", ytest$activity)

## Merge individual files for training and testing
trainraw <- bind_cols(subjecttrain, ytrain, xtrain )
testraw <- bind_cols(subjecttest, ytest, xtest )

##Merge training ans testfiles
mdata01 = merge(trainraw, testraw, by = c("subject","activity"), all=TRUE)

## Extracts only the measurements on the mean and standard deviation for each measurement

### Step 1: Select "mean()" and "std()" from "features" object 
meanstdvect <- grep("mean\\(\\)|std\\(\\)", features$V2)

###Condition features names to populate column names
features2 <- features[meanstdvect,2]
features2 <- gsub("-", "", features2)
features2 <- gsub("\\(\\)", "", features2)
features2 <- tolower(features2)
featurestrain <- gsub("$", "_train", features2)
featurestest <- gsub("$", "_test", features2)

### Step 2
### Create 2 vectors to select columns from merged dataset
### "x" corresponds to training measurements
### "y" corresponds to test measurements
prefix <- c("V")
suffixx <- c(".x")
suffixy <- c(".y")
selvecx <- interaction(prefix, meanstdvect, suffixx, sep = "" )
selvecy <- interaction(prefix, meanstdvect, suffixy, sep = "" )

### Step 3
### Divided in 2 substeps a) & b)
#### Step 3a)
#### Select the required columns for the course project
step301 <- select(mdata01, subject, activity)
step302 <- mdata01[,grepl(paste0(selvecx, collapse="|"), colnames(mdata01))]
step303 <- mdata01[,grepl(paste0(selvecy, collapse="|"), colnames(mdata01))]

#### Condition column names before merge
names(step302) <- featurestrain
names(step303) <- featurestest

## Merge files with selected columns corresponding to mean and std measurements
mdata02 <- bind_cols(step301, step302, step303)

## Tidy dataset is created 
tidydataset <- gather(mdata02, variable, value, -subject, -activity, na.rm = TRUE)
## Improve name of column
names(tidydataset)[3] <- "measurementvariation"

## 
sumtidydataset <- ddply(tidydataset, c("subject", "activity"), summarise, mean = mean(value))

## Write dataset
### First file: tidyfinal.txt
write.table(tidydataset, "./data/tidyfinal.txt", sep = "\t")

### Second file: average of each variable for each activity and each subject  
write.table(sumtidydataset, "./data/sumtidyfinal.txt", sep = "\t")




