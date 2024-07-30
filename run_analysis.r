library(dplyr)
library(purrr)

# Download and unzip the dataset
if(!file.exists("./getcleandata")) {
  dir.create("./getcleandata")
}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./getcleandata/projectfiles.zip")
unzip(zipfile = "./getcleandata/projectfiles.zip", exdir = "./getcleandata")

# Function to read data
read_data <- function(type, dataset) {
  read.table(paste0("./getcleandata/UCI HAR Dataset/", type, "/", dataset, "_", type, ".txt"))
}

# Read train and test sets
x_train <- read_data("train", "X")
y_train <- read_data("train", "y")
subject_train <- read_data("train", "subject")
x_test <- read_data("test", "X")
y_test <- read_data("test", "y")
subject_test <- read_data("test", "subject")

# Read feature vector and activity labels
features <- read.table("./getcleandata/UCI HAR Dataset/features.txt")
activity_labels <- read.table("./getcleandata/UCI HAR Dataset/activity_labels.txt", col.names = c("activityID", "activityType"))

# Assign column names
colnames(x_train) <- features[, 2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"
colnames(x_test) <- features[, 2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

# Merge all sets into one
train_set <- cbind(y_train, subject_train, x_train)
test_set <- cbind(y_test, subject_test, x_test)
combined_data <- rbind(train_set, test_set)

# Extract only the measurements on the mean and standard deviation for each measurement
mean_and_std <- grepl("activityID|subjectID|mean\\(\\)|std\\(\\)", colnames(combined_data))
mean_and_std_data <- combined_data[, mean_and_std]

# Use descriptive activity names to name the activities in the data set
data_with_act_names <- merge(mean_and_std_data, activity_labels, by = "activityID", all.x = TRUE)

# Label the data set with descriptive variable names
colnames(data_with_act_names) <- tolower(colnames(data_with_act_names))
colnames(data_with_act_names) <- gsub("^t", "time", colnames(data_with_act_names))
colnames(data_with_act_names) <- gsub("^f", "frequency", colnames(data_with_act_names))
colnames(data_with_act_names) <- gsub("acc", "accelerometer", colnames(data_with_act_names))
colnames(data_with_act_names) <- gsub("gyro", "gyroscope", colnames(data_with_act_names))
colnames(data_with_act_names) <- gsub("mag", "magnitude", colnames(data_with_act_names))
colnames(data_with_act_names) <- gsub("bodybody", "body", colnames(data_with_act_names))
colnames(data_with_act_names) <- gsub("[-()]", "", colnames(data_with_act_names))

# Create tidy dataset with the average of each variable for each activity and each subject
tidy_set <- data_with_act_names %>%
  group_by(subjectid, activityid, activitytype) %>%
  summarise(across(everything(), mean))

# Save data to a file
write.table(tidy_set, "tidy_set.txt", row.names = FALSE)
