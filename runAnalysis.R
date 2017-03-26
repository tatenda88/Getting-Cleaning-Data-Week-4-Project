# JHU Getting and Cleaning Data Course Project
# Ruben Jongstra
# March 25, 2017

# runAnalysis.r will:

# Input UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

###### 1. Merge the training and the test sets to create one data set.
setwd('C:/Users/rjongstr/Desktop/Data Course/Getting & Cleaning Data/Week 4/project/UCI HAR Dataset');

# Import training data from files & Name columns 
features <- read.table('./features.txt',header=FALSE);
activityType <- read.table('./activity_labels.txt',header=FALSE); 
        colnames(activityType) <- c('activityId','activityType');
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); 
        colnames(subjectTrain) <- "subjectId";
xTrain <- read.table('./train/x_train.txt',header=FALSE); colnames(xTrain) <- 
                features[,2];
yTrain <- read.table('./train/y_train.txt',header=FALSE); colnames(yTrain) <- 
                "activityId";

# Merge Data into complete training set
trainingSet = cbind(yTrain,subjectTrain,xTrain);

# Import test data from files & Name columns
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); 
        colnames(subjectTest) <- "subjectId";
xTest <- read.table('./test/x_test.txt',header=FALSE); colnames(xTest) <- 
        features[,2];
yTest <- read.table('./test/y_test.txt',header=FALSE); colnames(yTest) <- 
        "activityId";

# Merge Data into complete test set
testSet = cbind(yTest,subjectTest,xTest);

# Combine Training Data Set and Test Data Set into one Merged Data Set
MergedDataSet = rbind(trainingSet,testSet);

# Create columns vector to prepare data for subsetting
columns <- colnames(MergedDataSet);

###### 2. Extract only the measurements on the mean and standard deviation for each measurement

# Create a vector that indentifies the ID, mean & stddev columns as TRUE
vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) &
                  !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
                        grepl("-std..",columns) & !grepl("-std()..-",columns));

# Update MergedDataSet based on previously identified columns
MergedDataSet <- MergedDataSet[vector==TRUE];

###### 3. Use descriptive activity names to name the activities in the data set

# Add in descriptive activity names to MergedDataSet & update columns vector
MergedDataSet <- merge(MergedDataSet,activityType,by='activityId',all.x=TRUE);
        columns <- colnames(MergedDataSet);

###### 4. Appropriately label the data set with descriptive activity names.

# Tidy column names
for (i in 1:length(columns)) 
        {
                columns[i] <- gsub("\\()","",columns[i])
                columns[i] <- gsub("-std$","StdDev",columns[i])
                columns[i] <- gsub("-mean","Mean",columns[i])
                columns[i] <- gsub("^(t)","time",columns[i])
                columns[i] <- gsub("^(f)","freq",columns[i])
                columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
                columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
                columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
                columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
                columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
                columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
                columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
        };
        
# Update MergedDataSet with new descriptive column names
colnames(MergedDataSet) <- columns;

###### 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Creates a new table removing the activityType column
MergedDataActivityRemoved <- MergedDataSet[,names(MergedDataSet) != 'activityType'];

# Averaging of each variable for each activity and each subject as Tidy Data
tidyData <- aggregate(MergedDataActivityRemoved[,names(MergedDataActivityRemoved) 
                != c('activityId','subjectId')],by=list
                        (activityId=MergedDataActivityRemoved$activityId,
                                subjectId = MergedDataActivityRemoved$subjectId),mean);

# Including descriptive names in tidyData
tidyData <- merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')