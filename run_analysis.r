# Purpose: run_analysis.r reads in the UCI HAR files and tidies them into a ready-to-analyze dataset
# per the 'Getting and Cleaning Data' course project specification.
#
# Author: Sara Thiebaud, June 2017


### Step 0: Read in the assorted input files.
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt", col.names="label")
test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names="subject")
test_data <- read.table("UCI HAR Dataset/test/X_test.txt")
train_labels <- read.table("UCI HAR Dataset/train/y_train.txt", col.names="label")
train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names="subject")
train_data <- read.table("UCI HAR Dataset/train/X_train.txt")


### Step 1: Merge the training and the test sets to create one data set.
data <- rbind(cbind(test_subjects, test_labels, test_data),
              cbind(train_subjects, train_labels, train_data))


### Step 2: Extract only the measurements on the mean and standard deviation for each measurement.

# Grab the relevant metadata
features <- read.table("UCI HAR Dataset/features.txt", strip.white=TRUE, stringsAsFactors=FALSE)
# Which features are we looking for?
features_of_interest <- features[grep("mean\\(\\)|std\\(\\)", features$V2), ]
# Pull out the means and standard deviations from the dataset
data_filtered <- data[, c(1, 2, features_of_interest$V1+2)]


### Step 3: Use descriptive activity names to name the activities in the data set

# First, grab the labels from the metadata file.
labels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)
# ...and then stick them into the dataset as labels on the appropriate columns.
data_filtered$label <- labels[data_filtered$label, 2]


### Step 4: Appropriately label the data set with descriptive variable names.

# Begin with a mapping of the current column names and feature names.
input_colnames <- c("subject", "label", features_of_interest$V2)
# Tidy that list by removing non-alpha characters and casting everything to lower case.
tidy_colnames <- tolower(gsub("[^[:alpha:]]", "", input_colnames))
# Then apply the list back to the dataset
colnames(data_filtered) <- tidy_colnames

### Step 5: From the data set in step 4, create a second, independent tidy data set 
###           with the average of each variable for each activity and each subject.

# Calculate the mean for each subject-label pair.
data_tidy <- aggregate(
                        data_filtered[, 3:ncol(data_filtered)],
                        by=list(subject = data_filtered$subject, 
                        label = data_filtered$label),
                        mean
                    )

# Write out the completed dataset to a descriptively-named file.
write.table(format(data_tidy, scientific=T), "uci_har_data_tidy.txt", row.names=F, col.names=F, quote=2)