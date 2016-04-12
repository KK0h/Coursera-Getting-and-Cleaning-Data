#  Merge the training and the test sets to create one data set:
#     Add X_train.txt to the bottom of X_test.txt
#     Assign column names according to features.txt

library(dplyr)

Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
merged <- rbind(Xtest, Xtrain)
names(merged) <- read.table("./UCI HAR Dataset/features.txt")[,2]
#  Merge the training and test subject files, subject_train.txt and subject_test.txt
Subjtest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
Subjtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
msubj <- rbind(Subjtest, Subjtrain)
msubj <- rename(msubj, Subject=V1)

#  Extract only the measurements on the mean and standard deviation for each measurement.
extract <- merged[,grep("-mean\\(\\)|-std\\(\\)", names(merged))]

#  Use descriptive activity names to name the activities in the data set
#     Merge the test and train activity labels (y_test.txt and y_train.txt)
Ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
Ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
mactivity <- rbind(Ytest, Ytrain)
#     Replace the activity number with the activity description
activitydes <- read.table("./UCI HAR Dataset/activity_labels.txt")
mactivity1 <- transmute(mactivity, Activity=activitydes[mactivity$V1,2])

#  Combine all 3 data sets - subject label, activity label, and mean and standard dev data
mergedall <- cbind(msubj, mactivity1, extract)

#  Appropriately label the data set with descriptive variable names.
label <- gsub("tBodyAcc", "Body Acceleration (Time) ", names(mergedall))
label <- gsub("tGravityAcc", "Gravity Acceleration (Time) ", label)
label <- gsub("tBodyGyro", "Angular Velocity (Time) ", label)
label <- gsub("Mag", "Magnitude ", label)
label <- gsub("fBodyAcc", "Body Acceleration (Frequency) ", label)
label <- gsub("fBodyGyro", "Angular Velocity (Frequency) ", label)
label <- gsub("fBodyBodyAcc", "Body Acceleration (Frequency) ", label)
label <- gsub("fBodyBodyGyro", "Angular Velocity (Frequency) ", label)
label <- gsub("Jerk", "Jerk ", label)
label <- gsub("-mean\\(\\)", "Mean ", label)
label <- gsub("-std\\(\\)", "Standard Deviation ", label)
label <- gsub("-X", "X Axis", label)
label <- gsub("-Y", "Y Axis", label)
label <- gsub("-Z", "Z Axis", label)
var <- cbind(names(mergedall), label)
colnames(var) <- c("Variable Name", "Variable Label")
#  Write variable labels to file
write.table(var, "variables.txt", row.names=FALSE)

#  Write merged dataset to file
write.table(mergedall, "merged.txt", row.names=FALSE)

#  From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
bysubjactiv <- mergedall %>% group_by(Subject, Activity)  %>% 
  arrange(Subject, Activity) %>%
  summarise_each(funs(mean))

#  Write summarised dataset to file
write.table(bysubjactiv, "summary.txt", row.names=FALSE)

