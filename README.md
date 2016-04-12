# Coursera-Getting-and-Cleaning-Data
# Coursera Getting and Cleaning Data Week 4 Assignment

# The data is extracted and merged from a dataset by:
# Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
# and found on the following site: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

# The original dataset is downloaded from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# and described as:
# "
# ==================================================================
# Human Activity Recognition Using Smartphones Dataset
# Version 1.0
# ==================================================================
# Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
# Smartlab - Non Linear Complex Systems Laboratory
# DITEN - Università degli Studi di Genova.
# Via Opera Pia 11A, I-16145, Genoa, Italy.
# activityrecognition@smartlab.ws
# www.smartlab.ws
# ==================================================================
#
# The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
#
# The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 
# 
# For each record it is provided:
# ======================================
# 
# - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
# - Triaxial Angular velocity from the gyroscope. 
# - A 561-feature vector with time and frequency domain variables. 
# - Its activity label. 
# - An identifier of the subject who carried out the experiment.
# "
# 
# The assignment comprises the following files:
# (1) 'README.txt'
# (2) 'merged.txt' -  
# This is the result of doing the first three steps in the assignment:
#     - Merge the training and the test sets to create one data set.
#     - Extract only the measurements on the mean and standard deviation for each measurement.
#     - Use descriptive activity names to name the activities in the data set
# (3) 'variables.txt'
# This is the result of doing the fourth step in the assignment:
#    - Appropriately label the data set with descriptive variable names. 
# (4) 'summary.txt' - 
# This is the result of doing the fifth step in the assignment:
#     - From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
# (5) 'codebook.xls'

