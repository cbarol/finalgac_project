README file corresponding to the Getting and Cleaning Data Course Project

The purpose of this project is to demonstrate our ability to collect, work with, 
and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 
We will be graded by our peers on a series of yes/no questions related to the project. 
It is required to submit: 
1) a tidy data set as described below
2) a link to a Github repository with your script for performing the analysis, and 
3) a code book that describes the variables, the data, and any transformations or work
that you performed to clean up the data called CodeBook.md. 

This README.md file is also included in the repo with my script. 
This repo explains how all of the scripts work and how they are connected.

ABOUT THE DATASET
One of the most exciting areas in all of data science right now is wearable computing
- see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing
to develop the most advanced algorithms to attract new users. The data linked to from
the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.
A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


The companion R script called run_analysis.R does the following.

1) Merges the training and the test sets to create one data set.

Input Files:
'features.txt': List of all features.
'activity_labels.txt': Links the class labels with their activity name.
subject_train.txt': Each row identifies the subject who performed the activity for each window sample. 
                    Its range is from 1 to 30. 
'train/X_train.txt': Training set.
'train/y_train.txt': Training labels.

subject_test.txt': Each row identifies the subject who performed the activity for each window sample. 
                    Its range is from 1 to 30. 
'test/X_test.txt': Test set.
test/y_test.txt': Test labels.

The companion code provides a detailed comments of the steps to merge the datasets.
 A summary of the steps is as follows:
a) Files are downloaded an unzip
b) The input files mentioned above are read in R 
c) Some data conditionungg is performed to improve the names of original columns and 
    some conversions are done for the name activities as a preparation before the merge of the
    training and test dataset.
d) The merged dataset is called "mdata01"	

2) Extracts only the measurements on the mean and standard deviation for each measurement.
a) The extraction is done in 3 steps as it is explained in the code.

3) Uses descriptive activity names to name the activities in the data set
a) The names can be seen in the final files

4) Appropriately labels the data set with descriptive variable names.
   The name of the first file is: tidyfinal.txt
5) From the data set in step 4, creates a second, independent tidy data set with the average
  of each variable for each activity and each subject.
   The name of the second file is: sumtidyfinal.txt

  
REFERENCES

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

[2] David Hood,  Getting and Cleaning the Assignment , https://thoughtfulbloke.wordpress.com/2015/09/09/getting-and-cleaning-the-assignment/
