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

#only last day trials
lastDayData = Fulldata %>% filter(block>52,block<57,stim1<64,stim2<64)

# filter feedback/nofeedback
Feedback <- FBDayData %>% filter(feedback==1)
NoFeedback = lastDayData %>% filter(feedback==0)
# create day col
Feedback = Feedback %>% mutate(day = day(which_day))
learned.stim.day.sub = Feedback %>% group_by(stim1,subject) %>% summarise(maxday = max(day),accuracy = mean(accuracy))
learned.stim.day.sub = learned.stim.day.sub %>% arrange(subject,maxday)

sub1 = Fulldata %>% filter(subject == 101)
NoFeedback.1 = lastDayData %>% filter(subject==101)
Feedback.1 = sub1 %>% filter(feedback==1)
NoFeedback.1 = NoFeedback.1 %>% filter(feedback==0)
lastDayTest1= ddply(NoFeedback,.(subject,stim1),plyr::summarize, accuracy=mean(accuracy, na.rm = FALSE))
lastDayTest2= ddply(NoFeedback,.(subject,stim2),plyr::summarize, accuracy=mean(accuracy, na.rm = FALSE))

learned.stim.block = ddply(Feedback.1,.(stim1),plyr::summarize, maxblock=max(block, na.rm = T))
lastDayTest1= ddply(NoFeedback.1,.(stim1),plyr::summarize, accuracy=mean(accuracy, na.rm = T))
lastDayTest2= ddply(NoFeedback.1,.(stim2),plyr::summarize, accuracy=mean(accuracy, na.rm = T))

compair_unmatch_vectors = function(vec1,vec2){
  
  vecIndexes = c() 
  r = length(vec1)
  c=1
  for(i in 1:r){
      if(vec1[i] != vec2[i]){
        
          vecIndexes[c] = i
          c=c+1
      }
  }
  r2 = length(vec2)-length(vec1)-length(vecIndexes)-1
  c2 = length(vecIndexes)+1
  for (j in r2:0) {
    vecIndexes[c2] = length(vec2)-j
    c2 = c2 +1
    }
  return(vecIndexes)
  
  
}  
  
stims_Indexes = compair_unmatch_vectors(lastDayTest1$stim1,learned.stim.block$stim1)  
  
learned.stim.block = learned.stim.block[-stims_Indexes,]  
  
lastDayTest1$accuracy2 = lastDayTest2$accuracy  
prox = lastDayTest1 %>% select(accuracy,accuracy2)
lastDayTest1$meanAcc = apply(prox,1,mean)
                             
learned.stim.block = ddply(Feedback,.(subject,group,stim1,block),plyr::summarize, accuracy=mean(accuracy, na.rm = T))
lastDayTest1= ddply(NoFeedback,.(subject,group,stim1),plyr::summarize, accuracy=mean(accuracy, na.rm = T))
lastDayTest2= ddply(NoFeedback,.(subject,group,stim2),plyr::summarize, accuracy=mean(accuracy, na.rm = T))
#remove NaNs
lastDayTest1= na.omit(lastDayTest1)
lastDayTest2= na.omit(lastDayTest2)
