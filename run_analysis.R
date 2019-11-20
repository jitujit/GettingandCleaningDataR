library(dplyr)
library(qdap)

#Download File

if(!file.exists('./data')){dir.create('./data')}
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile = './data/activity.zip')
unzip(zipfile = './data/activity.zip',exdir = './data')

#Read Metadata
features <- read.table('./data/UCI HAR Dataset/features.txt', col.names = c("FeatureId","FeatureType"))
activity_labels <- read.table('./data/UCI HAR Dataset/activity_labels.txt', col.names = c("ActivityId","ActivityName"))


#Read Training data
x_train <- read.table('./data/UCI HAR Dataset/train/X_train.txt', col.names = features$FeatureType)
y_train <- read.table('./data/UCI HAR Dataset/train/y_train.txt', col.names = c("ActivityId"))
subject_train <- read.table('./data/UCI HAR Dataset/train/subject_train.txt', col.names = c("SubjectId"))

#Read Test data
x_test <- read.table('./data/UCI HAR Dataset/test/X_test.txt', col.names = features$FeatureType)
y_test <- read.table('./data/UCI HAR Dataset/test/y_test.txt', col.names = c("ActivityId"))
subject_test <- read.table('./data/UCI HAR Dataset/test/subject_test.txt', col.names = c("SubjectId"))

#Setp 1 : Merges the training and the test sets to create one data set.

train <- cbind(y_train, subject_train, x_train)
test <- cbind(y_test, subject_test, x_test)
universe <- rbind(test,train)

#Step 2 : Extracts only the measurements on the mean and standard deviation for each measurement.

mean_and_std <- universe %>%
  select(ActivityId,SubjectId,matches("mean|std"))
dim(mean_and_std)

#Step 3 : Uses descriptive activity names to name the activities in the data set
activityNames_ds <- merge(mean_and_std,activity_labels)
#Step 4 : Appropriately labels the data set with descriptive variable names.

  # Replace Words starting with t,f and a with Time,Freq and A respectively
  # Replace words following . with and t,s,m,g with Time,S,M,G respectively 
  # Replace BodyBody with Body

colnames(activityNames_ds) <- mgsub(c('^t|\\.t','^f','^a','*\\.s','*\\.m','*\\.g','BodyBody','\\.*'), c('Time','Freq','A',"S","M","G",'Body',''), names(activityNames_ds),fixed = FALSE)

#Step 5 : From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_ds <- activityNames_ds %>%
            group_by(ActivityId,ActivityName,SubjectId) %>%
            summarise_all(funs(mean))

write.table(tidy_ds, "TidtyData.txt", row.name=FALSE)
