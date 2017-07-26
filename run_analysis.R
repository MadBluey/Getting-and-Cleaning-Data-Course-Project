## Getting and Cleaning data Course project 2
# KKH

# Importing needed libraries for the project.
library(reshape2)

## Merging the training and the test sets to create one data set.

# Downloading and unzipping the project file.

if (sum(dir() %in% "Dataset.zip") != 1){
        download.file(
                "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                destfile = "Dataset.zip")
        unzip("Dataset.zip")}

# Importing the labels and features.

features <- read.table("./UCI HAR Dataset/features.txt")
activity.labels <-
        read.table("./UCI HAR Dataset/activity_labels.txt")

# Importing the test set.

x.test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y.test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject.test <-
        read.table("./UCI HAR Dataset/test/subject_test.txt")

# Importing the training set.

x.train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y.train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject.train <-
        read.table("./UCI HAR Dataset/train/subject_train.txt")


# Naming the columns.

names(x.test) <- features[, 2]
names(x.train) <- features[, 2]

colnames(y.test) <- "activity.id"
colnames(y.train) <- "activity.id"

colnames(subject.test) <- "subject.id"
colnames(subject.train) <- "subject.id"

colnames(activity.labels) <- c("activity.id", "activity.label")

# Merging the sets using column binding.

test.set <- cbind(subject.test, y.test, x.test)
train.set <- cbind(subject.train, y.train, x.train)

# Merging the sets in to one dataset.

mrg.set <- rbind(test.set, train.set)

## Extracting only the measurements on the mean and standard deviation for each measurement.

extr.index <-
        which(grepl("subject.id|activity.id|-mean()|std()", names(mrg.set)))
data <- mrg.set[, extr.index]

## Use descriptive activity names to name the activities in the data set

activity.id <- data$activity.id
factor.activity.id <- factor(activity.id)
levels(factor.activity.id) <- activity.labels[, 2]
data$activity.id <- factor.activity.id

## Appropriately labeling the data set with descriptive variable names.

# I'll keep the functions used to get the data, couse otherwise the variable names would be even longer and 
# info about the functions used can be found in /UCI HAR Dataset/features_info.txt or R help. 

names(data) <- tolower(names(data))
names(data) <- gsub("^t", "time.", names(data)) 
names(data) <- gsub("^f","frequency.",names(data))
names(data) <- gsub("acc", " .accelerometer", names(data))
names(data) <- gsub("-",".",names(data))
names(data) <- gsub("jerk",".jerk",names(data))
names(data) <- gsub("gyro",".gyroscope",names(data))
names(data) <- gsub("mag",".magnitude",names(data))
names(data) <- gsub("activity.id","activity", names(data))

## Create a second, independent tidy data set with the average of each variable for each activity and each subject.

data.melt <- melt(data,id=c("subject.id","activity"))
tidy.data <- dcast(data.melt,subject.id+activity ~ variable)       

# Export the file 
write.table(x = tidy.data,file = "tidy.data.txt",row.names = FALSE)