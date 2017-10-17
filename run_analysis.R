## Course Project: Getting and Cleaning Data

library(dplyr)

## Getting Started: mapping labels and features to train set and data set

trainset <- read.table("X_train.txt")
trainlabel <- read.table("y_train.txt")
features <- read.table("features.txt")
colnames(trainset) <- features$V2
subtrain <- read.table("subject_train.txt")
trainset <- cbind(subtrain$V1,trainlabel$V1,trainset)
colnames(trainset)[1] <- "subject"
colnames(trainset)[2] <- "activity"

testset <- read.table("X_test.txt")
testlabel <- read.table("y_test.txt")
colnames(testset) <- features$V2
subtest <- read.table("subject_test.txt")
testset <- cbind(subtest$V1,testlabel$V1,testset)
colnames(testset)[1] <- "subject"
colnames(testset)[2] <- "activity"


## 1.Merges the training and the test sets to create one data set

allset <- rbind(trainset,testset)


## 2.Extracts only the measurements on the mean and standard deviation for each measurement.

valid_column_names <- make.names(names=names(allset), unique=TRUE, allow_ = TRUE)
names(allset) <- valid_column_names
allset2 <- cbind(allset[,1:2],select(allset,contains("mean"),contains("std")))

## 3.Uses descriptive activity names to name the activities in the data set

allset2$activity[allset2$activity==1] <- "WALKING"
allset2$activity[allset2$activity==2] <- "WALKING_UPSTAIRS"
allset2$activity[allset2$activity==3] <- "WALKING_DOWNSTAIRS"
allset2$activity[allset2$activity==4] <- "SITTING"
allset2$activity[allset2$activity==5] <- "STANDING"
allset2$activity[allset2$activity==6] <- "LAYING"
table(allset2$activity)

## 4.Appropriately labels the data set with descriptive variable names.

names(allset2)
cleannames <- colnames(allset2)
cleannames <- gsub("\\."," ",cleannames)
cleannames <- gsub("\\   "," ",cleannames)
cleannames <- gsub("^\\s+|\\s+$","",cleannames)
cleannames <- gsub("BodyBody","Body",cleannames)
colnames(allset2) <- cleannames

## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

actlist <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
cleanset <- data.frame(matrix(ncol=88,nrow=180))
a <- 1

for (i in 1:30){
    for (j in 1:6){
        cleanset[a,1] <- i
        cleanset[a,2] <- actlist[j]
        cleanset[a,3:88] <- colMeans(filter(allset2,subject==i&activity==actlist[j])[,3:88])
        a <- a+1
    }
}

colnames(cleanset) <- colnames(allset2)
write.table(cleanset,file = "cleanset.txt",row.names = FALSE)

