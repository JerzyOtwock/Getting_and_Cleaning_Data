library(dplyr)
library(tidyverse)
library(data.table)

# read input files and rename columns

featuresMap<-read.table("features.txt", header = FALSE)
featuresMap$V2_unique <- make.names(featuresMap$V2,unique = TRUE)
# featuresMap[featuresMap$V2 == "fBodyAcc-bandsEnergy()-1,8",] # label occurs many times
activitiesMap<-read.table("activity_labels.txt", header = FALSE)
colnames(activitiesMap) <- c("ActivityId" , "Activity")

dfTrainX<-read.table("./train/X_train.txt", header = FALSE)
dfTrainY<-read.table("./train/Y_train.txt", header=FALSE)
dfTrainX2<-read.table("./train/subject_train.txt", header=FALSE)
dfTestX<-read.table("./test/X_test.txt", header = FALSE)
dfTestY<-read.table("./test/Y_test.txt", header=FALSE)
dfTestX2<-read.table("./test/subject_test.txt", header=FALSE)

colnames(dfTrainX)<-featuresMap$V2_unique
colnames(dfTestX)<-featuresMap$V2_unique
colnames(dfTrainY)<-"ActivityId"
colnames(dfTestY)<-"ActivityId"
colnames(dfTrainX2)<-"Subject"
colnames(dfTestX2)<-"Subject"

#### question 1

df1 <- cbind(dfTrainX, dfTrainY, dfTrainX2 )
df2 <- cbind(dfTestX, dfTestY,  dfTestX2)
df <- rbind(df1, df2)

#### question 2

df_filterd <- df %>% 
    select(contains("mean"), contains("std"), contains("Subject"), contains("Activity"))

#### question 3

df_rename <- df_filterd
df_rename <- merge(df_rename , activitiesMap, by = "ActivityId" )
df_rename$ActivityId <- NULL
#### question 5

df_5 <- df_rename %>% arrange(Subject , Activity)
df_5 <- data.table(df_5)

df_6a <- df_5[,lapply(.SD, mean, na.rm=TRUE), by = .(Subject, Activity) ]

df_6b <- df_5 %>% group_by(Subject, Activity) %>% summarise_all(mean, na.rm = TRUE)

df_6 <-  aggregate(x = df_5 ,               
          by = list(df_5$Subject   , df_5$Activity),           
          FUN = mean)    
df_6 <-  df_6 %>% select( -Group.1, )
rename()
# df_6$Group.2 <- NULL
setdiff(df_6b , df_6)

write.table(df_6, 'output.txt', row.names = F)