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

# remove practice and fMRI blocks 
Fulldata <- Fulldata %>% filter(block>12,block<99,stim1<64,stim2<64)
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
# add group levele col
Fulldata = Fulldata %>% 
  mutate(group = ifelse(subject<200,"Money","Noise"))
Fulldata$group = as.factor(Fulldata$group)

Fulldata = Fulldata %>% mutate(which_day=as.POSIXct(choice_time/1000, origin="1970-01-01")) 

# filter feedback/nofeedback
Feedback <- Fulldata %>% filter(feedback==1)
# create day col
Feedback = Feedback %>% mutate(day = day(which_day))
learned.stim.day.sub = Feedback %>% group_by(stim1,subject) %>% summarise(maxday = max(day),accuracy = mean(accuracy,na.rm = TRUE))
learned.stim.day.sub = learned.stim.day.sub %>% arrange(subject,maxday)


#rename each day of the month as 1,2,3...
subjects = unique(Fulldata$subject)
abs_days = c()
i = 1  
  
  for (sub in subjects) {
    start = FALSE
    c = 1
    for (day in learned.stim.day.sub$maxday[which(learned.stim.day.sub$subject == sub)]) {
      
      if(start == FALSE){
        abs_days[i] = c
        var_day = min(learned.stim.day.sub$maxday[which(learned.stim.day.sub$subject == sub)])
        i = i+1
        start = TRUE
      }else{
        if(day > var_day){
          abs_days[i] = c + 1
          var_day = day
          i = i + 1
          c = c + 1
        }else{
          abs_days[i] = c
          i = i + 1
        }
      }
      
    }
  }
  
learned.stim.day.sub$day = abs_days  
learned.stim.day.sub = learned.stim.day.sub %>% mutate(group = ifelse(subject<200,"money","noise"))
learned.stim.day.sub$day = as.factor(learned.stim.day.sub$day)   
learned.stim.day.sub$group = as.factor(learned.stim.day.sub$group)   

#learned.stim.day.sub$day = as.numeric(learned.stim.day.sub$day)   
avg_days = learned.stim.day.sub %>% group_by(day) %>% summarise(mean_day = mean(accuracy))
avg_days_group = learned.stim.day.sub %>% group_by(group,day) %>% summarise(mean_day = mean(accuracy))
avg_group = learned.stim.day.sub %>% group_by(group) %>% summarise(mean_day = mean(accuracy))

fig1 = ggplot(avg_days,aes(x = day,y = mean_day,col = day))+
 geom_point()

ggplot(learned.stim.day.sub,aes(x = day,y = accuracy,col = day))+
  geom_point()+
  stat_summary(aes(group = day,size = 0.01),fun=mean, geom="point")

learned.stim.day.sub$subject = as.numeric(learned.stim.day.sub$subject)
#learned.stim.day.sub$subject = as.factor(learned.stim.day.sub$subject)   

ggplot(avg_days_group,aes(x = day,y = mean_day,group = group, col = group))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=avg_group$mean_day[2], linetype="dashed", color = "cyan4")+
  geom_hline(yintercept=avg_group$mean_day[1], linetype="dashed", color = "firebrick2")
 
