####################### CONDITIONS 3:2 3:4 6:2 6:4 #####################
########################################################################

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
Feedback = Feedback %>% mutate(day = day(which_day))

NoFeedback = Fulldata %>% filter(feedback==0)

