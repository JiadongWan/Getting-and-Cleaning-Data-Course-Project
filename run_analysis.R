setwd("C:/Users/Administrator/Downloads/getdata_projectfiles_UCI HAR Dataset")
library(tidyverse)

# Getting test data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# Getting train data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# Getting activity labels data
act_Labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)

# Getting features data
features <- read.table('./UCI HAR Dataset/features.txt', header = FALSE)

# Creates column names
colnames(x_test) = features[,2]
colnames(y_test) = "activityId"
colnames(subject_test) = "subjectId"

colnames(x_train) = features[,2]
colnames(y_train) = "activityId"
colnames(subject_train) = "subjectId"

colnames(act_Labels) <- c('activityId','activityType')

# Merges the training and the test sets to create one data set.
test_data <- cbind(y_test, subject_test, x_test)
train_data <- cbind(y_train, subject_train, x_train)
train_test <- rbind(train_data, test_data)

# Extracts only the measurements on the mean and standard deviation for each measurement.
colNames = colnames(train_test)

mean_and_std = (grepl("activityId", colNames) | 
                    grepl("subjectId", colNames) | 
                    grepl("mean..", colNames) | 
                    grepl("std..", colNames))

subset_mean_and_std <- train_test[ , mean_and_std == TRUE]

# Merge activity labels
train_test <- left_join(subset_mean_and_std, act_Labels, by = "activityId")

# Creates a second, independent tidy data set with the average of each variable 
# for each activity and each subject
tidy_data <- train_test %>%
    group_by(subjectId, activityType) %>%
    summarise(across(everything(), mean), .groups = "drop")

# Save the tidy data
write.table(tidy_data, "tidy_data.txt", row.name = FALSE, quote = FALSE)
