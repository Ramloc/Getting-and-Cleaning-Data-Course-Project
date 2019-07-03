# first i need to download the file using R
# in order to do that I need the URL
library(dplyr)
URL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

# now i need to get the file
# in this case its a zip file

zipfile <-"UCI HAR Dataset.zip"

# now i check if the file exists
if (!file.exists(zipfile)) {
  download.file(URL, zipfile, mode = "wb")
}

# the Above code is checking if it doesnt not exist, and if it doesnt it should download it.
# now we check for the unziped file, if it does not exist then it will unzip
# we do this by checking if the path for the folder exists
#first we must assign a path

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipfile)
}

#The above downloaded and unzipped the file


#Data prep -read the data into R

activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activity-ID", "activity-Desc")
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

tr_sub <- read.table(file.path(dataPath, "train", "subject_train.txt"))
tr_val <- read.table(file.path(dataPath, "train", "X_train.txt"))
tr_act <- read.table(file.path(dataPath, "train", "y_train.txt"))
tr_df <- cbind(tr_sub, tr_val, tr_act)

tst_sub <-read.table(file.path(dataPath, "test", "subject_test.txt"))
tst_val <-read.table(file.path(dataPath, "test", "X_test.txt"))
tst_act <-read.table(file.path(dataPath, "test", "y_test.txt"))
tst_df <- cbind(tst_sub, tst_val, tst_act)

#combine the training and test data (Merges the training and the test sets to create one data set.)

tnt <- rbind(tr_df, tst_df)
rm(tr_sub, tr_val, tr_act, tst_act, tst_val, tst_sub, tr_df, tst_df)


#extract wanted columns and add activities (Extracts only the measurements on the mean and standard deviation for each measurement. )

colnames(tnt) <- c("subject", features[, 2], "activity")
selected_columns <- grepl("subject|activity|mean|std", colnames(tnt))
tnt <- tnt[, selected_columns]

#descriptive activity titles
tnt$activity <- factor(tnt$activity, levels = activities[, 1], labels = activities[, 2])


#rename the columns (Appropriately labels the data set with descriptive variable names. )
#Uses descriptive activity names to name the activities in the data set

tnt_col_names <- colnames(tnt)
tnt_col_names <- gsub("[\\(\\)-]", "", tnt_col_names)
tnt_col_names <- gsub("^f", "frequencyDomain", tnt_col_names)
tnt_col_names <- gsub("^t", "timeDomain", tnt_col_names)
tnt_col_names <- gsub("Acc", "Accelerometer", tnt_col_names)
tnt_col_names <- gsub("Gyro", "Gyroscope", tnt_col_names)
tnt_col_names <- gsub("Mag", "Magnitude", tnt_col_names)
tnt_col_names <- gsub("Freq", "Frequency", tnt_col_names)
tnt_col_names <- gsub("mean", "Mean", tnt_col_names)
tnt_col_names <- gsub("std", "StandardDeviation", tnt_col_names)
tnt_col_names <- gsub("BodyBody", "Body", tnt_col_names)

colnames(tnt) <- tnt_col_names


#calculate the new dataset and give output
avg_train_test <- tnt %>%group_by(subject, activity) %>%summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(avg_train_test, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
