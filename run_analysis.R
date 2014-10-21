#initialisation
#clear workspace
rm(list = ls())

#set working directory to be root folder containing data set
oldwd <- getwd()
newwd <- "C:/Users/David/Downloads/Coursera/Data Discovery"
directory <- "UCI HAR Dataset"
newwd <- paste(newwd, directory, sep="/")
setwd(newwd)


#1. Merges the training and the test sets to create one data set.

#i) Import data from input files
features <- read.table("./features.txt",header=FALSE)
activity_labels <- read.table("./activity_labels.txt",header=FALSE)

x_train <- read.table("./train/x_train.txt",header=FALSE)
y_train <- read.table("./train/y_train.txt",header=FALSE)
subject_train <- read.table("./train/subject_train.txt",header=FALSE)

x_test <- read.table("./test/x_test.txt",header=FALSE)
y_test <- read.table("./test/y_test.txt",header=FALSE)
subject_test <- read.table("./test/subject_test.txt",header=FALSE)


#ii) Assign column names to imported data
colnames(features) <- c("actID", "actLabel")
colnames(activity_labels) <- c("actID", "actType")

colnames(x_train) <- features[, 2] 
colnames(y_train) <- "actID"
colnames(subject_train) <- "subjectID"

colnames(x_test) <- features[,2]
colnames(y_test) <- "actID"
colnames(subject_test) <- "subjectID"


#iii) Merge separate parts into training & test data sets
training <- cbind(x_train, y_train, subject_train)
test <- cbind(x_test, y_test, subject_test)

#iv) combine training & test data sets into one
combined <- rbind(training, test)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#i) Get mean/SD column references
labels <- colnames(combined)
#col_ref <- grepl("mean|std|subjectID|actID", labels)
col_ref = (grepl("actID",labels) | grepl("subjectID",labels) | grepl("-mean..",labels) & !grepl("-meanFreq..",labels) & !grepl("mean..-",labels) | grepl("-std..",labels) & !grepl("-std()..-",labels))

mean_sd <- combined[, col_ref]

#3. Uses descriptive activity names to name the activities in the data set
#i) combine mean/SD data with activity labels
merged <- merge(mean_sd, activity_labels, by = "actID", all.x = TRUE)

#4. Appropriately labels the data set with descriptive variable names. 
#i) get existing column names
columns <- colnames(merged)
#ii) determine more descriptive labels
for (i in 1:length(columns)) 
{
  columns[i] = gsub("\\()","", columns[i])
  columns[i] = gsub("-std","StdDev", columns[i])
  columns[i] = gsub("-mean","Mean", columns[i])
  columns[i] = gsub("^(t)","time", columns[i])
  columns[i] = gsub("^(f)","freq", columns[i])
  columns[i] = gsub("([Gg]ravity)","Gravity", columns[i])
  columns[i] = gsub("[Gg]yro","Gyro", columns[i])
  columns[i] = gsub("[Mm]ag","Magnitude", columns[i])
}
#iii) update column names with new labels
colnames(merged) <- columns

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#i) remove activity type column
tidy_1 <- merged[, names(merged) != "actType"]
#ii) summarise data by subject ID + activity ID
tidy <- aggregate(tidy_1[,names(tidy_1) != c('actID','subjectID')],by=list(actID=tidy_1$actID,subjectID = tidy_1$subjectID), mean)
#iii) add activity type back
tidy <- merge(tidy, activity_labels, by = "actID", all.x = TRUE)
#iv) remove duplicate columns created
tidy <- tidy[, !duplicated(colnames(tidy))]

#6. Export data set as txt file created with write.table() using row.name=FALSE
write.table(tidy, "./tidy.txt", sep = "\t", row.name = FALSE)

