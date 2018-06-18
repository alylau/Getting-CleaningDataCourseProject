setwd("~/UCIHAR/UCIHARDataset")
library(tidyr)
library(dplyr)
library(reshape2)

features <- read.table("features.txt", sep = " ")

# adding labels and feature names to train dataset
training_set <- read.table("train/X_train.txt")
training_label <- read.table("train/y_train.txt")
training_subject <- read.table("train/subject_train.txt")
names(training_set) <- features$V2
names(training_label) <- "label"
names(training_subject) <- "subject"
training_label <- mutate(training_label, section = "train")
training_set <- cbind(training_subject, training_label,training_set)


# adding labels and feature names to test dataset
testing_set <- read.table("test/X_test.txt")
testing_label <- read.table("test/y_test.txt")
testing_subject <- read.table("test/subject_test.txt")
names(testing_set) <- features$V2
names(testing_label) <- "label"
names(testing_subject) <- "subject"
testing_label <- mutate(testing_label, section = "test")
testing_set <- cbind(testing_subject, testing_label,testing_set)

#Merges the training and the test sets to create one data set
wholeset <- rbind(testing_set, training_set)
write.csv(wholeset,"wholeset.csv")

#Extracts only the measurements on the mean and standard deviation for each measurement.
uniquecol <- unique(names(wholeset))
extract_1 <- grep("mean()|std()", uniquecol)
extracctset <- wholeset[, uniquecol[extract_1]]
extracctset <- cbind(wholeset[,1:3],extracctset)

#Uses descriptive activity names to name the activities in the data set
activitylabel <- read.csv("activity_labels.txt", sep = " ", header = FALSE)
names(activitylabel) <- c("label", "activity")
extracctset[,2] <- sapply(extracctset[,2] ,function(x)activitylabel$activity[x])
names(extracctset)[2] <- "activity"
write.csv(extracctset, "extract.csv")

#Appropriately labels the data set with descriptive variable names.
colName <- names(extracctset)
reshape_set <- melt(extracctset, id=colName[1:3])
c <- dim(reshape_set)[1]
Type = character()
Object = character()
Measurement_1 = character()
Measurement_2 = character()
Calculation = character()
Axis = character()
for(i in 1:c){
  x <- reshape_set$variable[i]
  y <- gsub('([[:upper:]])|-',' \\1', x)
  z<- unlist(strsplit(y," "))
  Type[i] <- case_when(
    "t" %in% z ~ c("denoteTime"),
    "f" %in% z ~ c("frequencyDomainSignals"))
  Object[i] <- case_when(
    "Body" %in% z ~ c("body"),
    "Gravity"%in% z ~ c("gravity"))
  Measurement_1[i] <- case_when(
    "Acc" %in% z ~ "acceleration",
    "Gyro" %in% z  ~ "angularVelocity")
  Measurement_2[i] <- case_when(
    "Jerk" %in% z  ~ "Jerk",
    "Mag" %in% z  ~ "Magnitude",
    "Jerk&Mag" %in% z  ~ "Magnitude of Jerk")
  Calculation[i] <- case_when(
    "mean()" %in% z   ~ "MeanValue",
    "std()" %in% z  ~ "StandardDeviation",
    "Freq()" %in% z ~ "MeanFrequencyValue" )
  Axis[i] <- case_when(
    "X" %in% z  ~ "X",
    "Y" %in% z  ~ "Y",
    "Z" %in% z  ~ "Z" )
}
reshape_Set <- mutate(reshape_set, Type = Type, Object = Object, Measurement_1 = Measurement_1, Measurement_2 = Measurement_2, Calculation = Calculation, Axis = Axis)


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
var <- unique(reshape_Set$variable)
for(i in 1){ 
  for(j in activitylabel$activity){
   newsubset <- subset(reshape_Set, subject == i)
   newsubset <- subset(newsubset, activity == j)
   for(c in var){VarMeanSet <- subset(newsubset, variable == c)
                VarMean <- mean(VarMeanSet$value)
                newrow <- cbind(VarMeanSet[1,c(1:4,6:11)],VarMean)
                if(!(exists("newset"))){
                newset <- newrow
                }else{
                newset <- rbind(newset, newrow)}
                }
  }
}
write.csv(newset,"variableMeans.csv")



