rm(list = ls())
graphics.off()
library("RSQLite")
library("googledrive")
library("Hmisc")
library("plyr")
library("dplyr")
library("ggplot2")
library("zoo")
library("tidyverse")
library("lubridate")
setwd("C:/Users/aharo/Google Drive/mylabDATA/subjects_schedules/schedules_ended") #directory where the schedule file is located

db_files <- list.files(  path='C:/Users/aharo/Google Drive/mylabDATA/subjects_schedules/schedules_ended/',  # Specify path
                         pattern = "\\_schedule.db$",
                         recursive=TRUE,no.. = FALSE)

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

getdate_ymd = function(unix_time){
  
  x = unix_time/1000
  x = anydate(x)
  return(x)
  
  
}

trialFiles<- list()
stimuliFiles <- list()
subject_names <-c()
for(i in 1:length(db_files)){
  
  if(grepl("schedule",db_files[[i]])){
    data_1 = dbConnect(SQLite(),as.character(db_files[i]))
    stimuliFiles[[i]] = dbGetQuery(data_1, "SELECT * FROM stimuli")
    trialFiles[[i]] = dbGetQuery(data_1, "SELECT * FROM trials")
    subject_names[i] <- as.numeric(numextract(db_files[i]))
    
  }
  
}

trialFiles <- trialFiles[lengths(trialFiles) != 0]
subject_names <- subject_names[ !is.na(subject_names)]

# combine in to one large data set 
source('bind_with_id.R')
Fulldata <- bind_with_id(trialFiles,subject_names,stimuliFiles)


# remove unplayed 
if (length(which(is.na(Fulldata$choice)))!=0){
  Fulldata=Fulldata[-c(which(is.na(Fulldata$choice))),]}

# remove practice and fMRI blocks and testing day block
Fulldata <- Fulldata %>% filter(block>12,block<55,stim1<64,stim2<64)
# add conditions 
Fulldata <- Fulldata %>% mutate(condition_left=stimuliFiles[[1]]$condition[Fulldata$stim1+1],condition_right=stimuliFiles[[1]]$condition[Fulldata$stim2+1])
# add rank 
Fulldata <- Fulldata %>% mutate(rank_left=stimuliFiles[[1]]$rank[Fulldata$stim1+1],rank_right=stimuliFiles[[1]]$rank[Fulldata$stim2+1])

Fulldata <- Fulldata %>% mutate(Easy=abs(relative_stim1-relative_stim2))
##accuracy analysis according to designated probabilities##
#add objective expected value (EV) for each stimulus and objective accuracy for each trial

for (i in 1:length(Fulldata$block)){
  Fulldata$EVStim1[i]=-stimuliFiles[[1]]$punishment[Fulldata$stim1[i]+1]
  Fulldata$EVStim2[i]=-stimuliFiles[[1]]$punishment[Fulldata$stim2[i]+1]
  Fulldata$accuracy[i]=((Fulldata$EVStim1[i]>Fulldata$EVStim2[i])&&(Fulldata$choice[i]==0)||(Fulldata$EVStim1[i]<Fulldata$EVStim2[i])&&(Fulldata$choice[i]==1))
  if (Fulldata$EVStim1[i]==Fulldata$EVStim2[i])
    Fulldata$accuracy[i]=NA
}

Fulldata = Fulldata %>% 
  mutate(group = ifelse(subject<200,1,2))
Fulldata= Fulldata %>% mutate(group = factor(group, levels = c(1,2),labels = c("Money","Noise")))

Fulldata = Fulldata %>% mutate(which_day=as.POSIXct(choice_time/1000, origin="1970-01-01")) 

Feedback <- Fulldata %>% filter(feedback==1)
NoFeedback = Fulldata %>% filter(feedback==0)




# Find last block of stimuli in feedback trials
block_learned = ddply(Feedback,.(subject,group,stim1),plyr::summarize, maxblock=max(block, na.rm = T))
# mean accuracy in NF trials for each stimuli
mean_accuracy <- ddply(NoFeedback,.(subject,group,stim1),plyr::summarize, mean=mean(accuracy, na.rm = T))
#mean_relative_accuracy = ddply(NoFeedback,.(subject,group,stim1),plyr::summarize, mean=mean(relatively_correct, na.rm = T))

block_learned$mean_accuracy = mean_accuracy$mean
block_learned$subject = as.factor(block_learned$subject)
block_learned$maxblock = as.factor(block_learned$maxblock)
block_learned$group = as.factor(block_learned$group)
avg_subs_accuracy = ddply(block_learned,.(subject,group,maxblock),plyr::summarize, mean=mean(mean_accuracy, na.rm = T))
avg_groups_accuracy = ddply(block_learned,.(group,maxblock),plyr::summarize, mean=mean(mean_accuracy, na.rm = T))

avg_groups_accuracy = avg_groups_accuracy %>% 
  arrange(group,maxblock)

#avg_block_accuracy = ddply(block_learned,.(maxblock),plyr::summarize, mean=mean(mean_accuracy, na.rm = T))
#avg_block_accuracy$maxblock = as.factor(avg_block_accuracy$maxblock)
avg_groups_accuracy$maxblock = as.numeric(avg_groups_accuracy$maxblock)
#first block for money group
x = 15
#first block for noise group
y = 15
#counters
c1 = 1
c2 = 1
# vector that stores the day values
day = c()
#for loop to cut every 4 blocks and create a day vector
for (i in 1:nrow(avg_groups_accuracy)) {
 if(avg_groups_accuracy$group[i]=="Money"){
   if(avg_groups_accuracy$maxblock[i] <= x + 3){
    day[i] = c1
  }else{
    day[i] = c1 + 1
    x = avg_groups_accuracy$maxblock[i]
    c1 = c1 + 1
  }
   }else{
     if(avg_groups_accuracy$maxblock[i] <= y + 3){
       day[i] = c2
     }else{
       day[i] = c2 + 1
       y = avg_groups_accuracy$maxblock[i]
       c2 = c2 + 1
     }
  }
}


#days = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8)
#avg_block_accuracy$days = days
#avg_learn_day_accuracy = ddply(avg_block_accuracy,.(days),plyr::summarize, mean=mean(mean, na.rm = T))

#avg_groups_accuracy$maxblock = as.factor(avg_groups_accuracy$maxblock)
avg_groups_accuracy$day = day
avg_groups_accuracy$day = as.factor(avg_groups_accuracy$day)
avg_groups_accuracy$group = as.factor(avg_groups_accuracy$group)
avg_groups = ddply(avg_groups_accuracy,.(group,day),plyr::summarize, mean=mean(mean, na.rm = T))
ggplot(avg_groups_accuracy, aes(x = day, y = mean,fill = group)) +
  geom_dotplot(binaxis = "y", binwidth = .012, stackdir = "center") +
  geom_point(data = avg_groups,aes(col = group), stat = "identity",size=8,alpha=2,shape = 9,stroke = 2)+
  labs(title = "day learned blocks - accuracy by group")+
  labs(y = "% accuracy")

avg_learn_day_accuracy = ddply(avg_stim_accuracy,.(group,day),plyr::summarize, mean=mean(mean, na.rm = T))
ggplot(avg_learn_day_accuracy, aes(x = day, y = mean,fill = group)) +
  geom_dotplot(binaxis = "y", binwidth = .008, stackdir = "center") +
  labs(title = "day learned - accuracy by group")+
  labs(y = "% accuracy")
