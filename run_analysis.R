rm(list=ls()) 
setwd("C:/Users/ChitraR/Desktop/Rwork/GetCleanData")
dir()

# For our analysis we need the files: activity_labels,features,subject_test,subject_train,X_test,X_train,Y_test,Y_train
# STEP1: Merge training and test data sets and create a nice final file 

subject_test <- read.table ("subject_test.txt", header = FALSE)
subject_train <- read.table ("subject_train.txt", header = FALSE)
X_test <- read.table ("X_test.txt", header = FALSE)
X_train <- read.table ("X_train.txt", header = FALSE)
Y_test <- read.table ("Y_test.txt", header = FALSE)
Y_train <- read.table ("Y_train.txt", header = FALSE)
features <- read.table ("features.txt", header = FALSE)
activity_labels <- read.table ("activity_labels.txt", header = FALSE)

colnames(X_test) <- features[,2]
colnames(X_train) <- features[,2]
colnames(Y_test) <- "Activity_ID"
colnames(Y_train) <- "Activity_ID"
colnames(subject_test) <- "Subject_ID"
colnames(subject_train) <- "Subject_ID"

# Creating the final data set
test.data <- cbind(X_test,Y_test,subject_test)
train.data <- cbind(X_train,Y_train,subject_train)
CleanData <- rbind(test.data,train.data)

# Bringing the acivity and suubject ID columns to the front for making the data more readable.
col_idx <- grep("Activity_ID", names(CleanData))
CleanData <- CleanData[, c(col_idx, (1:ncol(CleanData))[-col_idx])]
col_idx <- grep("Subject_ID", names(CleanData))
CleanData <- CleanData[, c(col_idx, (1:ncol(CleanData))[-col_idx])]
CleanData <- CleanData[order(CleanData$Subject_ID, CleanData$Activity_ID),] 

# We have the final data set now called CleanData 
# Extract mean and Standard deviation:
col <- colnames(CleanData)
test2 <- grep("mean",col)
test3 <- grep("std",col)
CleanData.mean.std  <- CleanData [,c(1,2, test2,test3)]

# Another way to extract mean and STD
colNames <- colnames(CleanData)
mean_and_std <- (grepl("Activity_ID" , colNames) | 
                   grepl("Subject_ID" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)
CleanData.mean.std <- CleanData[ , mean_and_std == TRUE]

# Adding Activity Name and shuffling columns to make data more readable. Our file now is CleanData.mean.std
colnames(activity_labels) <- c("Activity_ID","Activity_Name")
CleanData.mean.std <- merge(CleanData.mean.std, activity_labels, by = "Activity_ID")
col_idx1 <- grep("Activity_Name", names(CleanData.mean.std))
CleanData.mean.std <- CleanData.mean.std[, c(col_idx1, (1:ncol(CleanData.mean.std))[-col_idx1])]

# Now let's create another tidy data set with the average of each variable for each activity and each subject.
summary <- aggregate(CleanData.mean.std[,4:82],by = list (CleanData.mean.std$Activity_Name, CleanData.mean.std$Subject_ID), mean) 
names(summary)[names(summary)=="Group.1"] <- "Subject_ID"
names(summary)[names(summary)=="Group.2"] <- "Activity_Name"
colnames (summary)

# We are done. Our final data set is "summary". 
# Create a .txt file. This will create a file called summary in the working directory which is our final output file. 
write.table(summary, "summary.txt", row.name=FALSE)
