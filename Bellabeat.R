install.packages('tidyverse')

install.packages('tidyr')
install.packages("dplyr")
install.packages("readr")
install.packages('ggplot2')
install.packages('ggpubr')
library(dplyr)
library(readr)
library(tidyr)
library('tidyverse')
library(ggplot2)
library(ggpubr)

#We will now need to import CSV files. We will be using the read.csv function to open and read the 3 files, which we will be using in this exercise.
daily_activity <- read_csv("dailyActivity_merged.csv")
sleepDay <- read_csv("sleepDay_merged.csv")
weightlog <-read_csv("weightLogInfo_merged.csv")

#load the datasets
data("daily_activity")
data("sleepDay")
data("weightlog")

#to view the datsets
View(daily_activity)
View(sleepDay)
View(weightlog)

#The head() function displays first n rows in the input data frame.
head(daily_activity)
head(sleep)
head(weightlog)

#to check column names
colnames(daily_activity)
colnames(sleepDay)
colnames(weightlog)
#checking basic statisitcs of all there data frames
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()

sleepDay %>%
  select(TotalTimeInBed, 
         TotalMinutesAsleep) %>%
  summary()

weightlog %>%
  select(WeightKg,
         BMI)%>%
  summary()
#splitting date and time
sleepDay_new <- sleepDay %>%
  separate(SleepDay,c("Date", "Time"), " ")

weightlog_new <- weightlog %>%
  separate(Date, into = c("date","time"),sep = " ")

#new data frames
View(sleepDay_new)
View(weightlog_new)

#merging 3 tables into 1 using the merge fucntion by 'ID'

combine_1 <- merge(daily_activity,sleepDay_new, by = "Id")
combined_all <- merge(combine_1,weightlog_new,by="Id")
head(combined_all)
#number of rows
nrow(combined_all)

#final table for analysis

final_table1 <- combined_all %>%
  select(Id,
         TotalSteps,
         LightActiveDistance,
         FairlyActiveMinutes,
         TotalMinutesAsleep,
         TotalDistance,
         VeryActiveDistance,
         SedentaryActiveDistance,
         TotalTimeInBed,
         WeightKg,
         ModeratelyActiveDistance,
         VeryActiveMinutes,
         SedentaryMinutes,
         Calories)

View(final_table1)

# #total enteries for different ID 
# final_table1 %>%
#   group_by(Id) %>%
#   summarise(total = n())

#check the if there are duplicate rows
duplicated(final_table1)
#to check the number of duplicate rows

sum(duplicated(final_table1))

#to remove duplicates

final_table <- unique(final_table1)
View(final_table)

#basic stats in the data frame

final_table %>%
  summary()

#SHARE
#relationship b/w total steps and calories, positive correlation
ggplot(data = final_table)+geom_point(mapping = aes(x=TotalSteps, y = Calories,color= "orange"))+geom_smooth(mapping = aes(x=TotalSteps, y = Calories),color = "black")+ labs(title = "Total steps vs Calories")

#relationship b/w total steps and weight, negative correlation
ggplot(data=final_table) + geom_point(mapping=aes(x=TotalSteps, y=WeightKg), color="black") +geom_smooth(mapping=aes(x=TotalSteps, y=WeightKg)) + labs(title="The Relationship Between Total Steps and WeightKg")

#The Relationship Between Very Active Minutes and Burnt calories
p1 <- ggplot(data=final_table) + geom_point(mapping=aes(x=VeryActiveMinutes, y=Calories),color="black") +geom_smooth(mapping=aes(x=VeryActiveMinutes, y=Calories)) + labs(title="The Relationship Between Very Active Minutes and Burnt calories")
p1
#The Relationship Between Sedentary Minutes and Burnt calories
p2 <- ggplot(data=final_table) + geom_point(mapping=aes(x=SedentaryMinutes, y=Calories),color="green") +geom_smooth(mapping=aes(x=SedentaryMinutes, y=Calories)) + labs(title="The Relationship Between Sedentary Minutes and Burnt calories")
p2

p3<-ggplot(data=final_table) + geom_point(mapping=aes(x=FairlyActiveMinutes, y=Calories),color="black") +geom_smooth(mapping=aes(x=FairlyActiveMinutes, y=Calories)) + labs(title="The Relationship Between Fairly active Minutes and Burnt calories")
p3
ggarrange(v1,v2,v3)
         
ggplot(data = final_table) + geom_point(mapping = aes(x = TotalMinutesAsleep,y=TotalTimeInBed )) +  geom_smooth(mapping = aes(x = TotalMinutesAsleep,y=TotalTimeInBed)) + labs(title="The Relationship Between Total minutes Asleep and Total time in bed")


mean_activity_min <- c(991,192,13,21)

intensity_min <- data.frame(mean_activity_min, activity_intensity)

#the average intensity 
result_mean <- c(24.12,707.8,18.5, 192)
activity_intensity <- c("Very Active","Sedentary","Fairly", "lightly")
intensity_min <- data.frame(result_mean, activity_intensity)
ggplot(data = intensity_min ) + 
  geom_col(aes(x = activity_intensity, y = result_mean),fill = 'orange') +
  labs(title = 'Average Minutes by Intensity', x = 'Activity Intensity', y = 'Average Activity Minutes') 

