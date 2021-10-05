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
    data_1 = dbConnect(SQLite(),as.character(db_files[[i]]))
    stimuliFiles[[i]] = dbGetQuery(data_1, "SELECT * FROM stimuli")
    trialFiles[[i]] = dbGetQuery(data_1, "SELECT * FROM trials")
    subject_names[i] <- as.numeric(numextract(db_files[[i]]))
    
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

# remove practice and fMRI blocks and last tow blocks of mixed conditiones 
Fulldata <- Fulldata %>% filter(block>12,block<55,stim1<64,stim2<64)
# add conditions 
Fulldata <- Fulldata %>% mutate(condition_left=stimuliFiles[[1]]$condition[Fulldata$stim1+1],condition_right=stimuliFiles[[1]]$condition[Fulldata$stim2+1])
# add rank 
Fulldata <- Fulldata %>% mutate(rank_left=stimuliFiles[[1]]$rank[Fulldata$stim1+1],rank_right=stimuliFiles[[1]]$rank[Fulldata$stim2+1])
Fulldata <- Fulldata %>% mutate(Easy=abs(EVStim1-EVStim2))
Fulldata <- Fulldata %>% mutate(Easy=ifelse(Easy==0.34,0.33,Easy))
##accuracy analysis according to designated probabilities##
#add objective expected value (EV) for each stimulus and objective accuracy for each trial
# add group levele col
Fulldata = Fulldata %>% 
  mutate(group = ifelse(subject>1000,"Reward",ifelse(subject<200,"Money loss","White noise")))
Fulldata$group = as.factor(Fulldata$group)


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

# filter feedback/nofeedback
Feedback <- Fulldata %>% filter(feedback==1)
NoFeedback = Fulldata %>% filter(feedback==0)
condition_accuracy = ddply(NoFeedback,.(condition_right),plyr::summarize,mean=mean(accuracy,na.rm=TRUE))
F_condition_desigAccuracy = ddply(Feedback,.(group,condition_right),plyr::summarize,mean=mean(accuracy,na.rm=TRUE))
NoF_condition_desigAccuracy = ddply(NoFeedback,.(group,condition_right),plyr::summarize,mean=mean(accuracy,na.rm=TRUE))

ggplot(F_condition_desigAccuracy,aes(x = condition_right,y = mean,fill = group))+
  geom_bar(position = "dodge", stat = "identity", width = 0.7) + 
  labs(title = "condition VS designated accuracy - with feedback")

ggplot(NoF_condition_desigAccuracy,aes(x = condition_right,y = mean,fill=group))+
  geom_bar(position = "dodge", stat = "identity", width = 0.7) + 
  theme_minimal()+
  labs(title = "condition VS Designated accuracy - No feedback")

condition_accuracy$condition_right = as.factor(condition_accuracy$condition_right)
ggplot(condition_accuracy,aes(x = condition_right,y = mean,fill=condition_right))+
  geom_bar(position = "dodge", stat = "identity", width = 0.7) + 
  theme_minimal()+
  labs(title = "condition VS Designated accuracy - No feedback")

# Designated Accuracy by block and group
Fulldata_accuracy = ddply(Fulldata,.(group,block,feedback),plyr::summarize, mean=mean(accuracy, na.rm = T))
F_block_accuracy<- ddply(Feedback,.(group,block),plyr::summarize, mean=mean(accuracy, na.rm = T))
NoF_block_accuracy<- ddply(NoFeedback,.(group,block),plyr::summarize, mean=mean(accuracy, na.rm = T))

Fulldata_accuracy$feedback = as.factor(Fulldata_accuracy$feedback)

#Plot Accuracy by block and punish group
ggplot(Fulldata_accuracy, aes(x = block, y = mean,fill = group,col = group)) + 
  geom_point() + 
  geom_smooth(method = lm)+
  labs(title = "accuracy over blocks - F and noF")+
  facet_wrap(vars(feedback))

ggplot(F_block_accuracy, aes(x = block, y = mean,fill = group,col = group)) + 
  geom_point(size = 3) + 
  geom_smooth(method = lm)+
  labs(title = "F+ accuracy over blocks")

ggplot(NoF_block_accuracy, aes(x = block, y = mean,fill = group,col = group)) + 
  geom_point(size = 3) + 
  geom_smooth(method = lm)+
  labs(title = "F- accuracy over blocks")


####### From Alon Script:  ###########################
#By block
objective_accuracy_by_block=ddply(Fulldata, .(block, feedback), plyr::summarize, mean=mean(accuracy, na.rm = T))
#Overall
overall_with_feedback_mean=mean(objective_accuracy_by_block$mean[objective_accuracy_by_block$feedback==1], na.rm = T)*100
overall_no_feedback_mean=mean(objective_accuracy_by_block$mean[objective_accuracy_by_block$feedback==0], na.rm = T)*100
objective_accuracy_by_block$feedback=as.factor(objective_accuracy_by_block$feedback)
figure_1 <- ggplot(data=objective_accuracy_by_block, aes(x=block, y=mean*100, group=feedback, colour=feedback)) +
  geom_line() +
  geom_point(aes(shape = feedback), size=2)+
  geom_hline(yintercept=overall_with_feedback_mean, linetype="dashed", color = "cyan4")+
  geom_hline(yintercept=overall_no_feedback_mean, linetype="dashed", color = "firebrick2")+
  ylab("% correct choice")+
  ggtitle("Accuracy according to designated probabilities")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0, max(objective_accuracy_by_block$block),by = 1))+
  geom_text(aes(0,overall_with_feedback_mean,label =round(overall_with_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="cyan4")+
  geom_text(aes(0,overall_no_feedback_mean,label =round(overall_no_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="firebrick2")

###########################################################################################################
################################## MONEY GROUP ANALYSIS ################################################
#################################################################################################

Money_Subs = Fulldata %>% 
  filter(subject < 200)

Feedback = Money_Subs %>% filter(feedback==1)
NoFeedback = Money_Subs %>% filter(feedback==0)

F_condition_desigAccuracy = ddply(Feedback,.(condition_right),plyr::summarize,mean=mean(accuracy,na.rm=TRUE))
F_condition_relatAccuracy = ddply(Feedback,.(condition_right),plyr::summarize,mean=mean(relatively_correct,na.rm=TRUE))
NoF_condition_desigAccuracy = ddply(NoFeedback,.(condition_right),plyr::summarize,mean=mean(accuracy,na.rm=TRUE))
NoF_condition_relatAccuracy = ddply(NoFeedback,.(condition_right),plyr::summarize,mean=mean(relatively_correct,na.rm=TRUE))


ggplot(F_condition_desigAccuracy,aes(x = condition_right,y = mean))+
  geom_bar(stat = "identity", width = 0.6,fill = "#FF6666") + 
  geom_text(aes(label=mean), vjust=-0.3, size=3.5)+
  theme_minimal()+
  labs(title = "condition VS designated accuracy - with feedback")

ggplot(F_condition_relatAccuracy,aes(x = condition_right,y = mean))+
  geom_bar(stat = "identity", width = 0.6,fill = "#FF6666") + 
  geom_text(aes(label=mean), vjust=-0.4, size=4)+
  theme_minimal()+
  labs(title = "condition VS Relative accuracy - with feedback")

ggplot(NoF_condition_relatAccuracy,aes(x = condition_right,y = mean))+
  geom_bar(stat = "identity", width = 0.6,fill = "#FF6666") + 
  geom_text(aes(label=mean), vjust=-0.4, size=4)+
  theme_minimal()+
  labs(title = "condition VS Relative accuracy- No feedback")

ggplot(NoF_condition_desigAccuracy,aes(x = condition_right,y = mean))+
  geom_bar(stat = "identity", width = 0.6,fill = "#FF6666") + 
  geom_text(aes(label=mean), vjust=-0.4, size=4)+
  theme_minimal()+
  labs(title = "condition VS Designated accuracy - No feedback")



# Designated Accuracy by block
Fulldata_accuracy = ddply(Money_Subs,.(block),plyr::summarize, mean=mean(accuracy, na.rm = T))
F_block_accuracy<- ddply(Feedback,.(block),plyr::summarize, mean=mean(accuracy, na.rm = T))
NoF_block_accuracy<- ddply(NoFeedback,.(block),plyr::summarize, mean=mean(accuracy, na.rm = T))

#Plot Accuracy by block
ggplot(Fulldata_accuracy, aes(x = block, y = mean)) + 
  geom_point(size = 4, color = "steelblue") + 
  geom_smooth(method = lm, color = "red")+
  labs(title = "accuracy over blocks")

ggplot(F_block_accuracy, aes(x = block, y = mean)) + 
  geom_point(size = 4) + 
  geom_smooth(method = lm, color = "steelblue")+
  labs(title = "F+ accuracy over blocks")

ggplot(NoF_block_accuracy, aes(x = block, y = mean)) + 
  geom_point(size = 4) + 
  geom_smooth(method = lm, color = "steelblue")+
  labs(title = "F- accuracy over blocks")
