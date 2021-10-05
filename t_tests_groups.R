####################  MEANS AND SDS OF GROUPS #####################

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
library("ggpubr")
#######################
## Load subjects ##
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
# add Reaction time column
Fulldata$RT=as.numeric(((Fulldata$choice_time)-(Fulldata$stim_time)))




#means of groups
means = Fulldata %>% 
  group_by(subject,group) %>% 
  summarise(accuracy = mean(accuracy,na.rm = TRUE),RT = mean(RT,na.rm = TRUE))
means$group = factor(means$group,levels = c("Reward","Money loss","White noise"))
acc = ggplot(means,aes(group,accuracy,col = group))+
  geom_point(size = 4)+
  stat_summary(geom = "point",fun = "mean",col = "black",size = 6, fill = "blue",shape = 24)+
  scale_color_manual(values = c("darkorange3","cyan4","red4"))+
  labs(title = "Group overall accuracy")+
  ylab("Accuracy")+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic",hjust = 0.5),
    axis.title.x = element_text(size = 11,face = "bold"),
    axis.title.y = element_text(size = 11,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
    panel.background = element_rect(fill = "grey85")
  )

rt = ggplot(means,aes(group,RT,col = group))+
  geom_point(size = 4)+
  stat_summary(geom = "point",fun = "mean",col = "black",size = 6, fill = "blue",shape = 24)+
  scale_color_manual(values = c("darkorange3","cyan4","red4"))+
  labs(title = "Group overall RT")+
  ylab("Miliseconds")+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic",hjust = 0.5),
    axis.title.x = element_text(size = 11,face = "bold"),
    axis.title.y = element_text(size = 11,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
    panel.background = element_rect(fill = "grey85")
  )




means_sds = means %>% 
  group_by(group) %>% 
  summarise(mean_accuracy = mean(accuracy,na.rm = TRUE),SD_accuracy = sd(accuracy,na.rm = TRUE),
            mean_RT = mean(RT,na.rm = TRUE),SD_RT = sd(RT,na.rm = TRUE))

# function that finds sd, se and ci 
source("Performance_5subjects.R")
summarySE(means,measurevar = "accuracy",groupvars = c("group"),na.rm=TRUE,
          conf.interval=.95, .drop=TRUE)

means_sds$group = factor(means_sds$group,levels = c("Reward","Money loss","White noise"))
levels(means_sds$group)
means_sds_bar = ggplot(means_sds,aes(group,mean_accuracy*100,fill=group))

means_sds_bar + geom_bar(stat="identity",color="black",alpha = 0.8,
                         position=position_dodge()) +
  scale_fill_manual(values = c("darkorange3","cyan4","red4"))+
  geom_errorbar(aes(ymin=(mean_accuracy-SD_accuracy)*100, ymax=(mean_accuracy+SD_accuracy)*100), width=.2,
                position=position_dodge(.9))+ 
  labs(title = "Group mean accuracy")+
    ylab("Accuracy")+
    theme(
      plot.title = element_text(size = 14, face = "bold.italic",hjust = 0.5),
      axis.title.x = element_text(size = 11,face = "bold"),
      axis.title.y = element_text(size = 11,face = "bold"),
      panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
      panel.background = element_rect(fill = "grey85")
    )


RT_bar = ggplot(means_sds,aes(group,mean_RT,fill=group))

RT_bar + geom_bar(stat="identity",color="black",alpha = 0.8,
                         position=position_dodge()) +
  scale_fill_manual(values = c("darkorange3","cyan4","red4"))+
  geom_errorbar(aes(ymin=(mean_RT-SD_RT), ymax=(mean_RT+SD_RT)), width=.2,
                position=position_dodge(.9))+ 
  labs(title = "Group mean RT")+
  ylab("Miliseconds")+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic",hjust = 0.5),
    axis.title.x = element_text(size = 11,face = "bold"),
    axis.title.y = element_text(size = 11,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
    panel.background = element_rect(fill = "grey85")
  )
#means sds of groups and feedback columns
means.feedback = Fulldata %>% 
  group_by(group,feedback,subject) %>% 
  summarise(accuracy = mean(accuracy,na.rm = TRUE),RT = mean(RT,na.rm = TRUE))
means_sds.feedback = means.feedback %>% 
  group_by(group,feedback,subject) %>% 
  summarise(mean_accuracy = mean(accuracy,na.rm = TRUE),SD_accuracy = sd(accuracy,na.rm = TRUE),
            mean_RT = mean(RT,na.rm = TRUE),SD_RT = sd(RT,na.rm = TRUE))
means.feedback$feedback[means.feedback$feedback==1] = "Learning"
means.feedback$feedback[means.feedback$feedback==0] = "Testing"

means_sds.feedback$feedback = as.factor(means_sds.feedback$feedback)
means_sds_bar = ggplot(means_sds.feedback,aes(group,mean_accuracy,fill=feedback))

means_sds_bar + geom_bar(stat="identity",color="black",
                         position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_accuracy-SD_accuracy, ymax=mean_accuracy+SD_accuracy), width=.2,
                position=position_dodge(.9)) 

group_averages = means.feedback %>% 
  group_by(group,feedback) %>% summarise(accuracy = mean(accuracy),RT = mean(RT))
means.feedback$feedback = as.factor(means.feedback$feedback)
means.feedback$feedback = factor(means.feedback$feedback,levels = c("Learning","Testing"))
group_averages$feedback = as.factor(group_averages$feedback)
group_averages$feedback = factor(group_averages$feedback,levels = c("Learning","Testing"))
means.feedback$group = factor(means.feedback$group,levels = c("Reward","Money loss","White noise"))
levels(means.feedback$group)

accFBvsNoF = ggplot(data = means.feedback,aes(x=feedback,y=accuracy,group=feedback,col=group))+
  geom_point(position = position_dodge(0.8),size = 5,alpha = 0.5)+
  geom_line(aes(feedback,accuracy,group=subject))+
  geom_point(data = group_averages,aes(x=feedback,y=accuracy,col=group),
             size=8,shape=19,stroke=2,
             position = position_dodge(1),
             alpha = 0.9)+
  geom_line(data = group_averages,aes(feedback,accuracy,group=group),size=3.5,alpha = 0.9)+
  labs(title = "Accuracy in Learning VS Testing")+
  ylab("Accuracy")+
  xlab("")+
  scale_color_manual(values = c("darkorange3","cyan4","red4"))+
  facet_wrap(vars(group),1)+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic",hjust = 0.5),
    axis.title.x = element_text(size = 11,face = "bold"),
    axis.title.y = element_text(size = 11,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
    panel.background = element_rect(fill = "grey85")
  )

rtFBvsNoF = ggplot(data = means.feedback,aes(x=feedback,y=RT,group=feedback,col=group))+
  geom_point(position = position_dodge(0.8),size = 5,alpha = 0.5)+
  geom_line(aes(feedback,RT,group=subject))+
  geom_point(data = group_averages,aes(x=feedback,y=RT,col=group),
             size=8,shape=19,stroke=2,
             position = position_dodge(1),
             alpha = 0.9)+
  geom_line(data = group_averages,aes(feedback,RT,group=group),size=3.5,alpha = 0.9)+
  labs(title = "RT in Learning VS Testing")+
  ylab("Miliseconds")+
  xlab("")+
  scale_color_manual(values = c("darkorange3","cyan4","red4"))+
  facet_wrap(vars(group),1)+
  theme(
    plot.title = element_text(size = 14, face = "bold.italic",hjust = 0.5),
    axis.title.x = element_text(size = 11,face = "bold"),
    axis.title.y = element_text(size = 11,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
    panel.background = element_rect(fill = "grey85")
  )

# arrange all plots in one figure

ggarrange(acc,accFBvsNoF, rt,rtFBvsNoF,labels = c("A", "B","C","D"),
          ncol = 2, nrow = 2)

# OVERALL
#means of groups
Fulldata2 = Fulldata[Fulldata$group!="Reward",]
overall_means = Fulldata2 %>% 
  group_by(feedback) %>% 
  summarise(accuracy = mean(accuracy,na.rm = TRUE),RT = mean(RT,na.rm = TRUE))

subs = c(101,103,105,106,109,110,201,202,203,204,206,207)



means = c()

for(k in 1:length(subs)){
  count = c()
  sub = Fulldata[Fulldata$subject==subs[k],]
  stims = unique(sub$stim1)
  for(i  in 1:length(stims)){
    count[i] = nrow(sub[((sub$stim1==stims[i])|(sub$stim2==stims[i]))&(sub$feedback==1),])
  }
  means[k] = mean(count)
}
