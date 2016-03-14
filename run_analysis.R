#########################################

# R Script for Getting and CLeaning data 

#########################################

# Load data from downloaded text files
filepath <- "/Users/lucky1eva/Downloads/UCI HAR Dataset/train/X_train.txt"
rawdata_train <- read.table(filepath)
filepath <- "/Users/lucky1eva/Downloads/UCI HAR Dataset/train/y_train.txt"
rawdata_train_y <- read.table(filepath)
filepath <- "/Users/lucky1eva/Downloads/UCI HAR Dataset/test/X_test.txt"
rawdata_test <- read.table(filepath)
filepath <- "/Users/lucky1eva/Downloads/UCI HAR Dataset/test/y_test.txt"
rawdata_test_y <- read.table(filepath)
filepath <- "/Users/lucky1eva/Downloads/UCI HAR Dataset/train/subject_train.txt"
subject_train <- read.table(filepath)
filepath <- "/Users/lucky1eva/Downloads/UCI HAR Dataset/test/subject_test.txt"
subject_test <- read.table(filepath)
filepath = "/Users/lucky1eva/Downloads/UCI HAR Dataset/features.txt"
labels <- read.table(filepath)

# Merges the training and the test sets to create one data set
# Uses descriptive activity names to name the activities in the data set; 
# Appropriately labels the data set with descriptive variable names.

## Add feature lables to all measurements from main dataframes
colnames(rawdata_test) <- lables[, 2]
colnames(rawdata_train) <- lables[, 2]

## Create activity names to the y_test and y_train activity ID table
attach(rawdata_test_y)
rawdata_test_y$V2 <- ifelse(V1 == "1", "WALKING", ifelse(V1 == "2", "WALKING_UPSTAIRS", 
                                                         ifelse(V1 == "3", "WALKING_DOWNSTAIRS", 
                                                                ifelse(V1 == "4", "SITTING", ifelse(V1 == "5", "STANDING", "LAYING")))) )
detach(rawdata_test_y)
attach(rawdata_train_y)
rawdata_train_y$V2 <- ifelse(V1 == "1", "WALKING", ifelse(V1 == "2", "WALKING_UPSTAIRS", 
                                                         ifelse(V1 == "3", "WALKING_DOWNSTAIRS", 
                                                                ifelse(V1 == "4", "SITTING", ifelse(V1 == "5", "STANDING", "LAYING")))) )
detach(rawdata_train_y)

## Create new variables with lables
newnames <- c("User_ID", "Activity_ID", "Activity_Names")

test_id <- cbind(subject_test, rawdata_test_y)
train_id <- cbind(subject_train, rawdata_train_y)
fullset_id <- rbind( train_id, test_id)

names(fullset_id) <- newnames

## Merge datasets into one table
fullset <- rbind(rawdata_train, rawdata_test)
fullset <- cbind(fullset_id, fullset)

# Extracts only the measurements on the mean and standard deviation for each measurement.

sub_id <- grep (("mean|std") , names(fullset)) # select variable with either "mean" or "std" in the names

fullset_sub <- fullset[, sub_id]

fullset2 <- cbind(fullset_id, fullset_sub)

# STEP 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Use tapply() and for loop to create the seperate clean data set by Subject or Activity
x <- NULL
for (i in 4:length(fullset2)) {
        avg <- tapply(fullset2[, i], fullset2$User_ID, mean)
        x <- cbind(x, as.vector(avg))
}

bySubject <- data.frame(x)
colnames(bySubject) <- names(fullset_sub)
ID <- data.frame( "ID" = as.factor(c(1:30)))
Table_subject <- cbind(ID, bySubject)


y <- NULL
for (i in 4:length(fullset2)) {
        avg <- tapply(fullset2[, i], fullset2$Activity_ID, mean)
        y <- cbind(y, as.vector(avg))
}

byActivity <- data.frame(y)
colnames(byActivity) <- names(fullset_sub)
ID <- data.frame( "ID" = c("WALKING", "WALKING_UPSTAIRS",
                                     "WALKING_DOWNSTAIRS",
                                     "SITTING",
                                     "STANDING",
                                     "LAYING"))

## Combine the two new dataset into one tidy data set

Table_activity <- cbind(ID, byActivity)

Tidyset <- rbind(Table_activity,Table_subject)

## Write the table into a text file 
setwd("/Users/lucky1eva/Downloads/")
write.table(Tidyset, file = "tidyset.txt", row.names = FALSE)



