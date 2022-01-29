
# load up required library
library(dplyr)

# Download the required rdata from URL
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "Data.zip")
unzip("Data.zip")


# 1. Merges the training and the test sets to create one data set

# 1.1 Read all required files into R
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
column_names <- read.table("./UCI HAR Dataset/features.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# 1.2 Append X_train and X_test , y_train and y_test, subject_train and subject_test

X_data <- rbind(X_train,X_test)
y_data <- rbind(y_train,y_test)
subject_data <- rbind(subject_train,subject_test)

## 2 Extracts only the measurements on the mean and standard deviation for each measurement. 

#### 2.1 Add column names to X_data
names(X_data) <- column_names[,2]

#### 2.2 Keep only column containing mean() & std()
req_col <- grep(("-mean\\(\\)|-std\\(\\)"),column_names[,2])
X_data_f <- X_data[,req_col]
names(X_data_f) <- sub("\\(\\)","",names(X_data_f))

## 3 Uses descriptive activity names to name the activities in the data set

#### 3.1 Merge y_data and activity lables
unique(y_data$V1)
y_data_merged <- left_join(y_data , activity_labels, by=c("V1" = "V1"))
names(y_data_merged) <- c("Activity_id","Activity")


#### 3.2 Append this new column to X_data
X_data_f2 <- cbind(X_data_f,Activity = y_data_merged$Activity)

## 4 Appropriately labels the data set with descriptive variable names. 

# This has already been performed in 2.1 Add column names to X_data

#### 4.1 Just Include Subject information for completeness
names(subject_data) <- "Subject"
X_data_final <- cbind(X_data_f2 , subject_data)

## 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and subject
activity_subject_group <- group_by(X_data_final, Activity,Subject )
tidy_average_dataset <- summarise_all(activity_subject_group , .funs = mean)
write.table(tidy_average_dataset,file = "tidy_average_dataset.txt" ,row.names = FALSE)
