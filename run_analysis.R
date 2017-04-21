##############################
##OS:Windows7
##R Versin:i386 3.3.3
##Data Path: UCI_Dataset
make_tiny_dataset <- function(){
    
    ##1.Merges the training and the test sets to create one data set
    
    #Read the features name
    features <- read.table("UCI_Dataset/features.txt",col.names = c("ID","Features_Name"),
                           colClasses = c("integer", "character"))
    
    #Read train data/lables/Subject 
    trainData <- read.table("UCI_Dataset/train/X_train.txt",col.names = features$Features_Name,
                            colClasses = "numeric")
    trainLables <-read.table("UCI_Dataset/train/y_train.txt",col.names = "LableID",
                             colClasses = "integer")
    trainSubject <- read.table("UCI_Dataset/train/subject_train.txt",col.names = "SubejctID",
                               colClasses = "integer")
    
     #Read test data/lables/Subject
    testData <-  read.table("UCI_Dataset/test/X_test.txt",col.names = features$Features_Name, 
                            colClasses = "numeric")
    testLables <-read.table("UCI_Dataset/test/y_test.txt",col.names = "LableID",
                             colClasses = "integer")
    testSubject <-read.table("UCI_Dataset/test/subject_test.txt",col.names = "SubejctID",
                            colClasses = "integer")
    
    #Merges train dataset
    trainData <- cbind(trainSubject,trainLables,trainData)
    #Merges test dataset
    testData <- cbind(testSubject,testLables,testData)
    #Merges test and train dataset
    allSet <- rbind(trainData,testData)
    
    ##2.Extracts only the measurements on the mean and standard deviation for each measurement.
    measureSet <- allSet[,c("SubejctID","LableID",grep("(mean|std)",names(allSet),value = T))]
    names(measureSet)
    
    ##3.Uses descriptive activity names to name the activities in the data set
    
    #Read active labes file
    activelabels <- read.table("UCI_Dataset/activity_labels.txt", col.names=c("id", "name"), 
                               colClasses=c("integer", "character"))
    
    # Assign activity labels as level    
    measureSet$LableID <- as.factor(measureSet$LableID)
    levels(measureSet$LableID) <- activelabels$name
   
    
    ##4.Appropriately labels the data set with descriptive variable names
    names(measureSet) <- gsub("^t", "time", names(measureSet))
    names(measureSet) <- gsub("^f", "freq", names(measureSet))
    names(measureSet)
    
   
    #  creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    re <- aggregate(measureSet[,-(1:2)], FUN=mean,
              by=list(SubejctID=measureSet$SubejctID, activityLabel=measureSet$LableID))
    
    re
}