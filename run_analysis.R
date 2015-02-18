run_analysis <- function(){
  #Read data from txt files
  y_test <- read.table("test/y_test.txt")
  y_train <- read.table("train/y_train.txt")
  
  x_test <- read.table("test/X_test.txt")
  x_train <- read.table("train/X_train.txt")
  
  subject_test <- read.table("test/subject_test.txt")
  subject_train <- read.table("train/subject_train.txt")
  #Bind data by rows
  data_y <- rbind(y_train, y_test)
  data_x <- rbind(x_train, x_test)
  subject <- rbind(subject_train, subject_test)
  #Set column names 
  names(data_y) <- c("Y")
  featureNames <- read.table("features.txt")
  names(data_x) <-  featureNames$V2
  names(subject) <- c("subject")
  #Bind data
  data <- cbind(subject, data_y)
  data <- cbind(data_x, data)
  
  #2.Extract feature names have mean() and std()
  select_featureNames <- featureNames$V2[grep("mean\\(\\)|std\\(\\)", featureNames$V2)]
  #Get seleted Data by feature names
  selectedData <- subset(data, select=c(as.character(select_featureNames), "subject", "Y"))
  
  #3.Use descriptive activity names to name the activities
  y_labels <-read.table("activity_labels.txt")
  selectedData$Y <- y_labels[selectedData$Y, 2]
  
  #4. Appropriately labels the data set with descriptive variable names.
  names(selectedData) <- gsub("^t", "time", names(selectedData)) 
  names(selectedData) <- gsub("Acc", "Accelerometer", names(selectedData))
  names(selectedData) <- gsub("Mag", "Magnitude", names(selectedData))
  names(selectedData) <- gsub("^f", "frequency", names(selectedData))
  names(selectedData) <- gsub("Gyro", "Gyroscope", names(selectedData))
  
  #5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
  result <- aggregate(.~subject+Y, selectedData, mean)
  write.table(result, "result.txt", sep="\t", row.name=FALSE)
  
}