#load necessary package
library (dplyr)

#Download dataset

filename <- "UCI_HAR_DATASET.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#Get all data frames into R

features <- read.table("UCI HAR Dataset/features.txt", 
                       col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", 
                         col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", 
                           col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", 
                     col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt",
                     col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", 
                            col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", 
                      col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", 
                      col.names = "code")

#ANALISIS

#Create dataset with test and train variables

x<- rbind(x_test, x_train)
y<- rbind(y_test, y_train)
subject<- rbind(subject_test,subject_train)
df<- cbind(subject, y, x)

#Get the mean and standard deviation for the measures

Descriptives<- df%>%
  select(subject, code, contains("mean"), contains("std"))

#Uses descriptive activity names to name the activities in the data set

  df$code<- activities[df$code,2]
  
#Appropriately labels the data set with descriptive variable names.
  names(df)[2] = "activity"
  names(df)<-gsub("tBody", "TimeBody", names(df))
  names(df)<-gsub("BodyBody", "Body", names(df))
  
#creates a second, independent tidy data set with the average of each
  #variable for each activity and each subject.
  
  Final<- df %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
  write.table(Final, "FinalData.txt", row.name=FALSE)
  