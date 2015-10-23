##Merges the training and the test sets to create one data set.

library(RCurl)

if (!file.info('UCI HAR Dataset')$isdir) {
  dataFile <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  dir.create('UCI HAR Dataset')
  download.file(dataFile, 'UCI-HAR-dataset.zip', method='curl')
  unzip('./UCI-HAR-dataset.zip')
}

#Read tables.
X_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
Y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')
subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
X_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
Y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')
subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')

#merging rows
allsubjectdata <- rbind(subject_train,subject_test)
names(allsubjectdata) <- "subject"
allYdata <- rbind(Y_train, Y_test)
names(allYdata) <- "activitynum"
allXdata <- rbind(X_train,X_test)

#merging columns
data <- cbind(allsubjectdata,allYdata)
data <- cbind(data, allXdata)

##Extracts only the measurements on the mean and standard deviation for each measurement. 

#Loading in features
features <- fread("features.txt")
names(features) <- c("activitynum","activityname")

#Subsetting
features$activitycode <- features[, paste0("V", activitynum)]
features_meansd <- features[grep("mean\\(\\)|std\\(\\)",activityname)]
activitycode <- features_meansd$activitycode
datameansd <- data[,activitycode]

##Uses descriptive activity names to name the activities in the data set
activitynames <- fread("activity_labels.txt")
setnames(activitynames, names(activitynames), c("activitynum", "activityname"))

##Appropriately labels the data set with descriptive variable names. 
data <- merge(data, activitynames, by = "activitynum", all.x=TRUE)
names(data) <- c(names(data[c(1,2)]),features$activityname,names(data[564]))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
averages <- aggregate(x=data, by=list(activities=data$activityname, subj=data$subject), FUN=mean)
averages <- averages[, !(colnames(averages) %in% c("subject", "activityname"))]
write.table(averages,'averages.txt',row.name=FALSE)