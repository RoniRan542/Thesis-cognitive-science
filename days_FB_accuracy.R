##############################################################################################
##################################  ACCURACY BY DAYS  ########################################
##############################################################################################


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

Fulldata <- Fulldata %>% mutate(Easy=abs(EVStim1-EVStim2))
Fulldata <- Fulldata %>% mutate(Easy=ifelse(Easy==0.34,0.33,Easy))
##accuracy analysis according to designated probabilities##
#add objective expected value (EV) for each stimulus and objective accuracy for each trial

for (i in 1:length(Fulldata$block)){
  index = which(subject_names == Fulldata$subject[i])
  
  if(subject_names[index]>1000){
    Fulldata$EVStim1[i]=stimuliFiles[[index]]$reward[Fulldata$stim1[i]+1]
    Fulldata$EVStim2[i]=stimuliFiles[[index]]$reward[Fulldata$stim2[i]+1]
  }else{
    Fulldata$EVStim1[i]=-stimuliFiles[[index]]$punishment[Fulldata$stim1[i]+1]
    Fulldata$EVStim2[i]=-stimuliFiles[[index]]$punishment[Fulldata$stim2[i]+1]
  }
  Fulldata$accuracy[i]=((Fulldata$EVStim1[i]>Fulldata$EVStim2[i])&&(Fulldata$choice[i]==0)||(Fulldata$EVStim1[i]<Fulldata$EVStim2[i])&&(Fulldata$choice[i]==1))
  if (Fulldata$EVStim1[i]==Fulldata$EVStim2[i])
    Fulldata$accuracy[i]=NA
}
# add group levele col
Fulldata = Fulldata %>% 
  mutate(group = ifelse(subject>1000,"Reward",ifelse(subject<200,"Money loss","White noise")))
Fulldata$group = as.factor(Fulldata$group)

Fulldata = Fulldata %>% mutate(which_day=as.POSIXct(choice_time/1000, origin="1970-01-01")) 

# create day col
Fulldata = Fulldata %>% mutate(day = day(which_day))



#rename each day of the month as 1,2,3...
subjects = unique(Fulldata$subject)
abs_days = c()
i = 1  

for (sub in subjects) {
  start = FALSE
  c = 1
  for (day in Fulldata$day[which(Fulldata$subject == sub)]) {
    
    if(start == FALSE){
      abs_days[i] = c
      var_day = Fulldata$day[which(Fulldata$subject == sub)][1]
      i = i+1
      start = TRUE
    }else{
      if((day > var_day)||((var_day==30||var_day==31||var_day==28||var_day==29)&day==1)){
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



Fulldata$abs_days = abs_days
# filter feedback/nofeedback
Feedback <- Fulldata %>% filter(feedback==1)
NoFeedback <- Fulldata %>% filter(feedback==0)

Feedback = Feedback[which(Feedback$abs_days<12),]
day_avg = Feedback %>% group_by(abs_days,subject,group) %>% summarise(accuracy = mean(accuracy,na.rm = TRUE))
averages_days_group = day_avg %>% group_by(group,abs_days) %>% summarise(se_accuracy = sd(accuracy)/sqrt(length(accuracy)),accuracy = mean(accuracy))
averages_days_group$group = factor(averages_days_group$group,levels = c("Reward","Money loss","White noise"))
day_avg$group = factor(day_avg$group,levels = c("Reward","Money loss","White noise"))
day_avg = day_avg[which(day_avg$group!="Reward"),]

source("Performance_5subjects.R")
summarySE(day_avg,measurevar = "accuracy",groupvars = c("abs_days","group"),na.rm=TRUE,
          conf.interval=.95, .drop=TRUE)



fig1 = ggplot(day_avg,aes(x = abs_days,y = accuracy,col=group))+
 geom_point(size = 4,alpha = 0.5)+
  geom_point(data = averages_days_group,aes(abs_days,accuracy,col=group),size = 8,shape = 15)+
  geom_errorbar(data = averages_days_group[which(averages_days_group$group=="Reward"),],
                aes(ymin=(accuracy-se_accuracy),ymax=(accuracy+se_accuracy)), width=.4)+
  scale_color_manual(values = c("cyan4","darkorange3","red4"))+
  scale_fill_manual(values = c("Reward","White noise","Money loss"))+
  scale_x_continuous(breaks = seq(1,11,1))+
labs(title = "Day stimulus learned - accuracy in learning trials")+
  labs(y = "accuracy",x = "days")+
  theme(
    plot.title = element_text(size = 17, face = "bold.italic"),
    axis.title.x = element_text(size = 15,face = "bold"),
    axis.title.y = element_text(size = 15,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
    panel.background = element_rect(fill = "grey85")
  )




NoFeedback = NoFeedback[which(NoFeedback$abs_days<12),]
day_avg = NoFeedback %>% group_by(abs_days,subject,group) %>% summarise(accuracy = mean(accuracy,na.rm = TRUE))
averages_days_group = day_avg %>% group_by(group,abs_days) %>% summarise(se_accuracy = sd(accuracy)/sqrt(length(accuracy)),accuracy = mean(accuracy))
averages_days_group$group = factor(averages_days_group$group,levels = c("Reward","Money loss","White noise"))
day_avg$group = factor(day_avg$group,levels = c("Reward","Money loss","White noise"))
day_avg = day_avg[which(day_avg$group!="Reward"),]


ggplot(day_avg,aes(x = abs_days,y = accuracy,col=group))+
  geom_point(size = 4,alpha = 0.5)+
  geom_point(data = averages_days_group,aes(abs_days,accuracy,col=group),size = 8,shape = 15)+
  geom_errorbar(data = averages_days_group[which(averages_days_group$group=="Reward"),],
                aes(ymin=(accuracy-se_accuracy),ymax=(accuracy+se_accuracy)), width=.4)+
  scale_color_manual(values = c("cyan4","darkorange3","red4"))+
  scale_fill_manual(values = c("Reward","White noise","Money loss"))+
  scale_x_continuous(breaks = seq(1,11,1))+
  labs(title = "day learned - accuracy in No feedback trials")+
  labs(y = "accuracy",x = "days")+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic"),
    axis.title.x = element_text(size = 10,face = "bold"),
    axis.title.y = element_text(size = 10,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
    panel.background = element_rect(fill = "grey85")
  )
