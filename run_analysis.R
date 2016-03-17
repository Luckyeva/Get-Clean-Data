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

## Get the variable names from the lables 
colnames(rawdata_test) <- lables[, 2]
colnames(rawdata_train) <- lables[, 2]

## Create 2 columns for subject and activity ID numbers with descrptive names
newnames <- c("User_ID", "Activity_ID")

test_id <- cbind(subject_test, rawdata_test_y)
train_id <- cbind(subject_train, rawdata_train_y)
fullset_id <- rbind(train_id, test_id)

names(fullset_id) <- newnames

## Merge datasets into one table
fullset <- rbind(rawdata_train, rawdata_test)
fullset <- cbind(fullset_id, fullset)

## Extracts only the measurements on the mean and standard deviation for each measurement.
sub_id <- grep (("mean|std") , names(fullset)) # select variable with either "mean" or "std" in the names

fullset_sub <- fullset[, sub_id]

## bind ID table with data table
fullset2 <- cbind(fullset_id, fullset_sub)

## To calculate mean for each subject by each activity, use aggregate()
## function and a for_loop to compute the mean value for each variable.

tb <- aggregate(fullset2[, 3] ~ User_ID + Activity_ID, data = fullset2, FUN = "mean" )
names(tb)[3] <- colnames(fullset2)[3]

for (i in 4:length(fullset2)) {
        tb.x <- aggregate(fullset2[, i] ~ User_ID + Activity_ID, data = fullset2, FUN = "mean" )
        tb <- cbind(tb, tb.x[, 3])
        names(tb)[length(tb)] <- colnames(fullset2)[i]
}

## Create new variable to store Activity_Name based on the Activity_ID
attach(tb)
tb$Activity_Name <- ifelse(Activity_ID == "1", "WALKING", 
                           ifelse(Activity_ID == "2", "WALKING_UPSTAIRS", 
                                ifelse(Activity_ID == "3", "WALKING_DOWNSTAIRS", 
                                        ifelse(Activity_ID == "4", "SITTING", 
                                                ifelse(Activity_ID == "5", "STANDING", "LAYING")))) )
detach(tb)

## Rearrange the order for Activity_Name column
tb <- tb[, c(1,2, length(tb), 3:(length(tb)-1) )]

## Write the table into a text file 
setwd("/Users/lucky1eva/Downloads/")
write.table(tb, file = "tidydata.txt", row.names = FALSE)



