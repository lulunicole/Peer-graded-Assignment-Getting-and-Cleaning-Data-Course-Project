# JHU Data Science Specialization
## Course 3, Peer Graded Project
#-------------------------------------------------------------------------------------------------------#

## Objective
## Create one R script called run_analysis.R that does the following.

###  1. Merges the training and the test sets to create one data set.
###  2. Extracts only the measurements on the mean and standard deviation for each measurement.
###  3. Uses descriptive activity names to name the activities in the data set
###  4. Appropriately labels the data set with descriptive variable names.
###  5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Resources
### Datapack: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#-------------------------------------------------------------------------------------------------------#    

# Step 0: loading data

setwd("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset")
xtest<-read.table("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset/test/X_test.txt",stringsAsFactors = FALSE)
ytest<-read.table("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset/test/y_test.txt",stringsAsFactors = FALSE)
stest<-read.table("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset/test/subject_test.txt",stringsAsFactors = FALSE)
xtrain<-read.table("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset/train/X_train.txt",stringsAsFactors = FALSE)
ytrain<-read.table("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset/train/y_train.txt",stringsAsFactors = FALSE)
strain<-read.table("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset/train/subject_train.txt",stringsAsFactors = FALSE)
activitylabels <-read.table("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset/activity_labels.txt",stringsAsFactors = FALSE)
features <-read.table("C:/Users/jingting.lu/Desktop/JHU_Data science/3_Data Cleaning/UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)



# Step 1: Merge the training and the test sets to create one data set.
## First, combine the X,y, and subject columns of the test set into one data frame
test <-cbind(xtest,ytest,stest)
## Then combine the X,y, and subject columns of the training set into one data frame
train<-cbind(xtrain,ytrain,strain)
## Combine the rows from the test set and training set
data <- rbind(test,train)


# Step 2:Appropriately labels the data set with descriptive variable names.
## features[["V2"]] column contains column header information for X data
## Also name the y column "Activity" and the subject column "Subject"
## Replace the acronyms in the column titles with descriptive names
names(data)<- c(features[["V2"]],"Activity","Subject")
names(data)<-gsub("^t", "time", names(data))
names(data)<-gsub("^f", "frequency", names(data))
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))

# Step 3: Extract only the measurements on the mean and standard deviation for each measurement.
## Store names from all columns
allcols<-names(data)
## Extract column names that contain mean(), std() as well as  Activity and Subject
extract_mean_std<-(grepl("Activity" , allcols) | grepl("Subject" , allcols) | grepl("mean()" , allcols) | grepl("std()" , allcols))
## subset the extracted columns
subdata<- data[,extract_mean_std]

 
# Step 4: Uses descriptive activity names to name the activities in the data set
## To prepare for merging, name the colomns in the activity labels data frame
names(activitylabels)<-c("Activity","Activity description") 
## Merge data and activitylabel by the Activity column
merged <- merge(activitylabels,subdata,by = "Activity")
## rearrange the columns to retain original order and insert the activity description column after activity
merged <- merged[,c(3:(ncol(merged)-1),1:2,ncol(merged))]


# Step 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  Export tidydata.txt to the repo folder
## Aggregate calculate mean of the measurement portion of the merged data frame by activity and subject
## Recall that the measurement portion of the merged data frame is merged[,1:(ncol(merged)-3)]
secondset<- aggregate(merged[,1:(ncol(merged)-3)],by = list(Activity=merged$Activity,Subject=merged$Subject),mean)
## Map back in the Activity Description column 
secondset<- merge(activitylabels,secondset,by = "Activity")
## Arrange the secondset dataframe for easier viewing
secondset<- arrange(secondset,Subject)
## Drop Activity column
secondset$Activity<- NULL
## Write secondset as tidyData.txt
write.table(secondset, './repo/tidyData.txt',row.names=FALSE ,sep='\t')

