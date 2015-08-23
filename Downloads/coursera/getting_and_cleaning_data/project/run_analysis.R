tidyData <- function () {
  ##read the activity labels into a table for activity_id to activity_label referencing 
  label <- read.table("./UCI HAR Dataset/activity_labels.txt")
  names(label)[1] <- "activity_id"
  names(label)[2] <- "activity_label"
  
  ##cleaning test data set
  test <- read.table("./UCI HAR Dataset/test/X_test.txt")
  ##subset mean and std col only (excludes mean freq cols)
  test_filter <- test[,c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,345,346,347,348,349,350,424,425,426,427,428,429,503,504,516,517,529,530,542,543)]
  test_activity <- read.table("./UCI HAR Dataset/test/y_test.txt")
  names(test_activity)[1] <- "activity_id"
  ##column bind activity ids to test data set
  test_filter_activity <- cbind(test_filter,test_activity)
  test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  names(test_subject)[1] <- "subject_id"
  ##column bind subject ids to test data set
  test_filter_activity_subject <- cbind(test_filter_activity,test_subject)
  ##reference activity_id to activity_label using label table
  mergedTestData <- merge(test_filter_activity_subject,label,by="activity_id")
  
  ##cleaning train data set
  train <- read.table("./UCI HAR Dataset/train/X_train.txt")
  ##subset mean and std col only (excludes mean freq cols)
  train_filter <- train[,c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,345,346,347,348,349,350,424,425,426,427,428,429,503,504,516,517,529,530,542,543)]
  train_activity <- read.table("./UCI HAR Dataset/train/y_train.txt")
  names(train_activity)[1] <- "activity_id"
  ##column bind activity ids to train data set
  train_filter_activity <- cbind(train_filter,train_activity)
  train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  names(train_subject)[1] <- "subject_id"
  ##column bind subject ids to train data set
  train_filter_activity_subject <- cbind(train_filter_activity,train_subject)
  ##reference activity_id to activity_label using label table
  mergedTrainData <- merge(train_filter_activity_subject,label,by="activity_id")
  
  ##row bind train data set to test data set
  mergedData <- rbind(mergedTestData,mergedTrainData)
  
  ##calculate average of each variable for each activity and each subject
  s1 <- group_by(mergedData,subject_id,activity_label)
  s2 <- summarize(s1,
                  tBodyAccMeanX=mean(V1),tBodyAccMeanY=mean(V2),tBodyAccMeanZ=mean(V3),tBodyAccStdX=mean(V4),tBodyAccStdY=mean(V5),tBodyAccStdZ=mean(V6),
                  tGravityAccMeanX=mean(V41),tGravityAccMeanY=mean(V42),tGravityAccMeanZ=mean(V43),tGravityAccStdX=mean(V44),tGravityAccStdY=mean(V45),tGravityAccStdZ=mean(V46),
                  tBodyAccJerkMeanX=mean(V81),tBodyAccJerkMeanY=mean(V82),tBodyAccJerkMeanZ=mean(V83),tBodyAccJerkStdX=mean(V84),tBodyAccJerkStdY=mean(V85),tBodyAccJerkStdZ=mean(V86),
                  tBodyGyroMeanX=mean(V121),tBodyGyroMeanY=mean(V122),tBodyGyroMeanZ=mean(V123),tBodyGyroStdX=mean(V124),tBodyGyroStdY=mean(V125),tBodyGyroStdZ=mean(V126),
                  tBodyGyroJerkMeanX=mean(V161),tBodyGyroJerkMeanY=mean(V162),tBodyGyroJerkMeanZ=mean(V163),tBodyGyroJerkStdX=mean(V164),tBodyGyroJerkStdY=mean(V165),tBodyGyroJerkStdZ=mean(V166),
                  tBodyAccMagMean=mean(V201),tBodyAccMagStd=mean(V202),
                  tGravityAccMagMean=mean(V214),tGravityAccMagStd=mean(V215),
                  tBodyAccJerkMagMean=mean(V227),tBodyAccJerkMagStd=mean(V228),
                  tBodyGyroMagMean=mean(V240),tBodyGyroMagStd=mean(V241),
                  tBodyGyroJerkMagMean=mean(V253),tBodyGyroJerkMagStd=mean(V254),
                  fBodyAccMeanX=mean(V266),fBodyAccMeanY=mean(V267),fBodyAccMeanZ=mean(V268),fBodyAccStdX=mean(V269),fBodyAccStdY=mean(V270),fBodyAccStdZ=mean(V271),
                  fBodyAccJerkMeanX=mean(V345),fBodyAccJerkMeanY=mean(V346),fBodyAccJerkMeanZ=mean(V347),fBodyAccJerkStdX=mean(V348),fBodyAccJerkStdY=mean(V349),fBodyAccJerkStdZ=mean(V350),
                  fBodyGyroMeanX=mean(V424),fBodyGyroMeanY=mean(V425),fBodyGyroMeanZ=mean(V426),fBodyGyroStdX=mean(V427),fBodyGyroStdY=mean(V428),fBodyGyroStdZ=mean(V429),
                  fBodyAccMagMean=mean(V503),fBodyAccMagStd=mean(V504),
                  fBodyAccJerkMagMean=mean(V516),fBodyAccJerkMagStd=mean(V517),
                  fBodyGyroMagMean=mean(V529),fBodyGyroMagStd=mean(V530),
                  fBodyGyroJerkMagMean=mean(V542),fBodyGyroJerkMagStd=mean(V543))
  
  df <- data.frame(s2)
  ##write data frame to file
  write.table(df,file="Clean_UCI HAR Dataset.txt",row.names=FALSE)
}