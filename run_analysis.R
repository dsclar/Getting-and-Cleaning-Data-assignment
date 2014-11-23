#Getting and cleaning Data - Course Project
setwd("C:/Users/Anne-Catherine/Documents/Cleaning Data")

#1. Merge training and test sets
library(dplyr)
library(tidyr)

e=read.table("X_test.txt") # read test data
d=read.table("X_train.txt") # read train data
a=read.table("y_train.txt", colClasses= "character") # training labels
k=read.table("y_test.txt", colClasses= "character") # test labels
b=read.table("subject_train.txt", colClasses= "character") #subject labels
g=read.table("subject_test.txt", colClasses= "character") # subject labels
l=read.table("activity_labels.txt", colClasses= "character")
c=read.table("features.txt", colClasses= "character") # read features data
n=c[,2] # extract features names
dt=rbind_list(d, e) # merge training and test data
colnames(dt)= c(n) # Assign features names to merged dataset "dt"

#2. Extract only the measurements on the mean and standard deviation for each measurement
u=make.unique(n, sep=".") #make variables unique
colnames(dt)= c(u) #apply unique variables to merged dataset "dt"
m=select(dt, contains("mean")) #select variables from "dt" that have the word "mean"
s=select(dt, contains("std")) #select variables from "dt" that have the word "std"
ms=cbind(m, s) #merge the two datasets above

#3. Uses descriptive activity names to name the activities in the data set
v=make.names(names(ms), unique=T)# make valid names of column names of dataset "ms"
colnames(ms)=c(v)#Assign the valid varaible names from vector "v" to dataset "ms"

Body=select(ms, contains("BodyBody"))# select from dataset "ms" the variables with unappropriate names (e.g., with "BodyBody")
ap=sub("Body", "", names(Body),) #Correct the names of the variable subset 
colnames(Body)=c(ap)# Assign the appropriate names to the data subset "Body"
ms=cbind(ms, Body) # merge the main data set "ms" with data subset "Body". The merged dataset has 97 variables.
ms=select(ms, -contains("BodyBody")) #remove variables with inappropriate names from subset "ms"

#4. Appropriately labels the data set with descriptive variable names.
act=rbind(a, k) #merge train and test activity numbers datasets
dfact=cbind(act, ms) #merge activity numbers to main clean dataset "ms"

#Replace activity numbers with labels
dfact$V1[dfact$V1=="1"]="WALKING"
dfact$V1[dfact$V1=="2"]="WALKING_UPSTAIRS"
dfact$V1[dfact$V1=="3"]="WALKING_DOWNSTAIRS"
dfact$V1[dfact$V1=="4"]="SITTING"
dfact$V1[dfact$V1=="5"]="STANDING"
dfact$V1[dfact$V1=="6"]="LAYING"

actn=sub("V1", "Activity", names(dfact),) #replace "V1" by column name="Activity"
colnames(dfact)=c(actn)



#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.    
#Insert subject labels ("ID") 
library(data.table)

#Add subject labels to main dataset
subj=rbind_list(b, g)#Merge subjects' labels for both train and test sets
df=cbind(subj, dfact)#Add column with subjects' labels to main dataset "dfact" 
subjn=sub("V1", "ID", names(df),)#replace "V1" by column name="ID"
colnames(df)=c(subjn)# Assign new column name to main dataset "df"

#Create factors for "ID" and "Activity" variables
ID=factor(df$ID)
attributes(ID)
Activity=factor(df$Activity)
attributes(Activity)

#Final step: Summarize main dataset into averages by "ID" and "Activity"
df_by=group_by(df, ID, Activity)#Group data by "ID" & "Activity"
dt_final= data.table(df_by)#Convert grouped dataset "df_by" into a data table
dt_mean=dt_final[,lapply(dt_final, mean), by=list(ID, Activity)]# Compute the mean by subject "ID" & "Activity"
dt_mean=select(dt_mean, -(3:4))#Remove from final dataset "dt_mean" irrelevant numeric "ID" & "Activity" = "NA" resulting in a file with 180 observations and 88 varoables.

#Output final dataset "dt_mean"
write.table(dt_mean, file="dt_mean.txt", quote=FALSE, row.names=F) # Write output file "dt_mean" as text file.

















