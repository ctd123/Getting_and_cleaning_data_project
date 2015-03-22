## This script takes data from files as specified in the working directory,
##merges it, renames the columns, renames the activity variables to text,
## then creates a new data set called both_mean which includes the mean for each
## feature variable groupedby subject and activity
#######################
##Starting conditions: 
##Assumes files are in working directory. 
##Assumes dplyr package is installed.

##load packages
library(dplyr)

##read in the data from the local directory
trainX.data<-read.table("./UCI HAR Dataset/train/X_train.txt",sep = "", header=FALSE)
testX.data<-read.table("./UCI HAR Dataset/test/X_test.txt",sep = "", header=FALSE)
trainY.data<-read.table("./UCI HAR Dataset/train/Y_train.txt",sep = "", header=FALSE)
testY.data<-read.table("./UCI HAR Dataset/test/Y_test.txt",sep = "", header=FALSE)
activitylabels.data<-read.table("./UCI HAR Dataset/activity_labels.txt",sep = "", header=FALSE)
featurenames.data<-read.table("./UCI HAR Dataset/features.txt",sep = "", header=FALSE)
subject_test.data<-read.table("./UCI HAR Dataset/test/subject_test.txt",sep = "", header=FALSE)
subject_train.data<-read.table("./UCI HAR Dataset/train/subject_train.txt",sep = "", header=FALSE)

##1.Merges the training and the test sets to create one data set.
##First combines the data set with the suject information for both test and train.
testX1.data<-cbind(subject_test.data,testX.data)
trainX1.data<-cbind(subject_train.data,trainX.data)

##Combines the test and train data
newdataX<-rbind(testX1.data,trainX1.data)
newdataY<-rbind(testY.data,trainY.data)

## names columns of X data with subject and feature names from feature file
names(newdataX)<-c("subject",as.character(featurenames.data[,2])) 

##rename column header for Y data
names(newdataY)<-"activity_number"

## combine the two data sets together so activity number is with each row
##make it a datatable 
## newdata is 10299 x 563 in size
newdata<-tbl_df(cbind(newdataX$subject,newdataY,newdataX[,2:562]))

##rename column 1 to subject
names(newdata)[1]<-"subject"

##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##use select from dplyr package with string matching
##result is 68 columns
stat<-select(newdata, matches("(subject)+|(activity_number)+|(mean\\(\\))+|(std\\(\\))+", ignore.case = TRUE))

##3. Uses descriptive activity names to name the activities in the data set
##Renames each activity number to the associated character text
actnumber <- stat[,2] == 1
stat[which(actnumber),2] <- "walking"
actnumber <- stat[,2] == 2
stat[which(actnumber),2] <- "walking_upstairs"
actnumber <- stat[,2] == 3
stat[which(actnumber),2] <- "walking_downstairs"
actnumber <- stat[,2] == 4
stat[which(actnumber),2] <- "sitting"
actnumber <- stat[,2] == 5
stat[which(actnumber),2] <- "standing"
actnumber <- stat[,2] == 6
stat[which(actnumber),2] <- "laying"

##4. Appropriately labels the data set with descriptive variable names.
##Rename the activity number column to activity name
names(stat)[2]<-"activity_name"

##Rename the features by removing capitals (except X,Y,Z), adding 
##underscores between words, removing dashes and (), removing 
##duplicated "body" in some labels, using full words so descriptive (except std)
newnames<-sub("^t","time_",names(stat[,3:68]))
newnames<-sub("^f","frequency_",newnames)  
newnames<-gsub("-","_",newnames)  
newnames<-sub("\\(\\)","",newnames)  
newnames<-gsub("Body","body_",newnames)  
newnames<-gsub("body_body_","body_",newnames)  
newnames<-gsub("Acc","acceleration_",newnames)  
newnames<-gsub("Gravitya","gravity_a",newnames)
newnames<-gsub("Gyro","gyroscope_",newnames) 
newnames<-gsub("J","j",newnames)  
newnames<-gsub("jerkM","jerk_m",newnames)  
newnames<-gsub("M","m",newnames)  
newnames<-gsub("mag","magnitude",newnames)
newnames<-gsub("__","_",newnames)  
  
##apply descriptive names to features
names(stat)[3:68]<-newnames

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and 
##  each subject

##group by activity name and subject using dplyr
bothgroup<-group_by(stat,activity_name,subject)

##calculate the mean of each feature column using the groups of activities 
## and subjects. Result is 180 x 68 in size.
both_mean<-summarise_each(bothgroup, funs(mean))

## write out table to file. It will be put into the working directory

write.table(both_mean,file="TidyMeans.txt",row.names=FALSE)

##to read it back in use (assumes you are in that directory)
##correctoutput<-read.table("TidyMeans.txt",sep=" ",header=TRUE)