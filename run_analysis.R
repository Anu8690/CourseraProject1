setwd("./UCI HAR Dataset")
library(tidyverse)

################################################################################
## First creating the test dataframe
XBA =
  strsplit(str_squish(readLines("./test/Inertial Signals/body_acc_x_test.txt")),split=" ")%>%
  lapply(as.numeric)
YBA =
  strsplit(str_squish(readLines("./test/Inertial Signals/body_acc_y_test.txt")),split=" ")%>%
  lapply(as.numeric)
ZBA =
  strsplit(str_squish(readLines("./test/Inertial Signals/body_acc_z_test.txt")),split=" ")%>%
  lapply(as.numeric)
XBG =
  strsplit(str_squish(readLines("./test/Inertial Signals/body_gyro_x_test.txt")),split=" ")%>%
  lapply(as.numeric)
YBG =
  strsplit(str_squish(readLines("./test/Inertial Signals/body_gyro_y_test.txt")),split=" ")%>%
  lapply(as.numeric)
ZBG =
  strsplit(str_squish(readLines("./test/Inertial Signals/body_gyro_z_test.txt")),split=" ")%>%
  lapply(as.numeric)
XTA =
  strsplit(str_squish(readLines("./test/Inertial Signals/total_acc_x_test.txt")),split=" ")%>%
  lapply(as.numeric)
YTA =
  strsplit(str_squish(readLines("./test/Inertial Signals/total_acc_y_test.txt")),split=" ")%>%
  lapply(as.numeric)
ZTA =
  strsplit(str_squish(readLines("./test/Inertial Signals/total_acc_z_test.txt")),split=" ")%>%
  lapply(as.numeric)

test<- data.frame(XBA_Mean=sapply(XBA,mean),XBA_SD=sapply(XBA,sd),
                  YBA_Mean=sapply(YBA,mean),YBA_SD=sapply(YBA,sd),
                  ZBA_Mean=sapply(ZBA,mean),ZBA_SD=sapply(ZBA,sd),
                  XBG_Mean=sapply(XBG,mean),XBG_SD=sapply(XBG,sd),
                  YBG_Mean=sapply(YBG,mean),YBG_SD=sapply(YBG,sd),
                  ZBG_Mean=sapply(ZBG,mean),ZBG_SD=sapply(ZBG,sd),
                  XTA_Mean=sapply(XTA,mean),XTA_SD=sapply(XTA,sd),
                  YTA_Mean=sapply(YTA,mean),YTA_SD=sapply(YTA,sd),
                  ZTA_Mean=sapply(ZTA,mean),ZTA_SD=sapply(ZTA,sd))
test$Subject <- as.factor(scan("./test/subject_test.txt"))
test$Activity <- factor(scan("./test/Y_test.txt"),
                        levels=c(1,2,3,4,5,6),
                        labels= c("Walking","Walking Upstairs",
                                  "Walking Downstairs","Sitting",
                                  "Standing","Laying"))
other_vars <- read.table("./test/X_test.txt",col.names = sapply(readLines("./features.txt"),as.character)%>%
                           sapply(function(x){strsplit(x,split=" ")[[1]][2]}))
test <- cbind(test,other_vars)
###############################################################################

## Now creating the train dataframe
XBA =
  strsplit(str_squish(readLines("./train/Inertial Signals/body_acc_x_train.txt")),split=" ")%>%
  lapply(as.numeric)
YBA =
  strsplit(str_squish(readLines("./train/Inertial Signals/body_acc_y_train.txt")),split=" ")%>%
  lapply(as.numeric)
ZBA =
  strsplit(str_squish(readLines("./train/Inertial Signals/body_acc_z_train.txt")),split=" ")%>%
  lapply(as.numeric)
XBG =
  strsplit(str_squish(readLines("./train/Inertial Signals/body_gyro_x_train.txt")),split=" ")%>%
  lapply(as.numeric)
YBG =
  strsplit(str_squish(readLines("./train/Inertial Signals/body_gyro_y_train.txt")),split=" ")%>%
  lapply(as.numeric)
ZBG =
  strsplit(str_squish(readLines("./train/Inertial Signals/body_gyro_z_train.txt")),split=" ")%>%
  lapply(as.numeric)
XTA =
  strsplit(str_squish(readLines("./train/Inertial Signals/total_acc_x_train.txt")),split=" ")%>%
  lapply(as.numeric)
YTA =
  strsplit(str_squish(readLines("./train/Inertial Signals/total_acc_y_train.txt")),split=" ")%>%
  lapply(as.numeric)
ZTA =
  strsplit(str_squish(readLines("./train/Inertial Signals/total_acc_z_train.txt")),split=" ")%>%
  lapply(as.numeric)

train<- data.frame(XBA_Mean=sapply(XBA,mean),XBA_SD=sapply(XBA,sd),
                  YBA_Mean=sapply(YBA,mean),YBA_SD=sapply(YBA,sd),
                  ZBA_Mean=sapply(ZBA,mean),ZBA_SD=sapply(ZBA,sd),
                  XBG_Mean=sapply(XBG,mean),XBG_SD=sapply(XBG,sd),
                  YBG_Mean=sapply(YBG,mean),YBG_SD=sapply(YBG,sd),
                  ZBG_Mean=sapply(ZBG,mean),ZBG_SD=sapply(ZBG,sd),
                  XTA_Mean=sapply(XTA,mean),XTA_SD=sapply(XTA,sd),
                  YTA_Mean=sapply(YTA,mean),YTA_SD=sapply(YTA,sd),
                  ZTA_Mean=sapply(ZTA,mean),ZTA_SD=sapply(ZTA,sd))

train$Subject <- as.factor(scan("./train/subject_train.txt"))
train$Activity <- factor(scan("./train/Y_train.txt"),
                        levels=c(1,2,3,4,5,6),
                        labels= c("Walking","Walking Upstairs",
                                  "Walking Downstairs","Sitting",
                                  "Standing","Laying"))
other_vars <- read.table("./train/X_train.txt",col.names = sapply(readLines("./features.txt"),as.character)%>%
                           sapply(function(x){strsplit(x,split=" ")[[1]][2]}))
train <- cbind(train,other_vars)

################################################################################

## Now Merging the two data frames
data<- rbind(test,train)
levels(data$Subject) <- 1:30

################################################################################

## Creating the summary data frame

data2<- data[-(19:20)]%>% split(list(data$Subject,data$Activity)) %>%
  lapply(function(x){lapply(x,function(t){mean(t)})})
data2 <- data.frame(t(sapply(data2,c)))

###############################################################################
setwd("../")
library("data.table")
fwrite(data2,"./summary.txt",row.names = T,sep = " ")

