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
  mutate(group = ifelse(subject>1000,"Reward",ifelse(subject<200,"Money loss","White Noise")))
Fulldata$group = as.factor(Fulldata$group)


Feedback = Fulldata %>% filter(feedback==1)
MaxLearned = Feedback %>% group_by(subject,group,stim1) %>% summarise(maxtime = max(feedback_time),accuracy = mean(accuracy,na.rm = TRUE))
MaxLearned$maxtime = round(MaxLearned$maxtime/1000)
NoFeedback = Fulldata %>% filter(feedback == 0)

stim.time.NF = NoFeedback %>% select(stim1,stim2,subject,group,choice_time,accuracy,Easy)
stim.time.NF$choice_time = round(stim.time.NF$choice_time/1000)


# subtract last feedback trial time from all No FB times for each stim and sub  
subjects = unique(Fulldata$subject)
stims = unique(stim.time.NF$stim1)
stims = sort(stims)
diff = c()
stim_num = c()
accuracy = c()
subject = c()



i = 1

for(sub in subjects){
  for(stim in stims){
    for(j in 1:length(stim.time.NF$subject[which(stim.time.NF$subject == sub)])){
      
      if(stim.time.NF$stim1[which(stim.time.NF$subject == sub)][j]==stim|stim.time.NF$stim2[which(stim.time.NF$subject == sub)][j]==stim){
        choice_time = stim.time.NF$choice_time[which(stim.time.NF$subject == sub)][j]
        maxFBtime = MaxLearned$maxtime[which(MaxLearned$subject==sub&MaxLearned$stim1==stim)]
        diff[i] = choice_time-maxFBtime
        stim_num[i] = stim
        accuracy[i] = stim.time.NF$accuracy[which(stim.time.NF$subject == sub)][j]
        subject[i] = sub
        i = i + 1
      }
      
      
    }
  }
 
}

diff = round(diff/3600)

df = data.frame(stim = stim_num, difference = diff, accuracy = accuracy, subject = subject)
df = df %>% mutate(group = ifelse(subject>1000,"Reward",ifelse(subject<200,"Money loss","White Noise")))
df = na.omit(df)
few_hours_remove = function(df,min_amount){
  
  hours = unique(df$difference)
  hours = sort(hours)
  hours_to_remove = c()
  i = 1
  for(hour in hours){
    sum = sum(df$difference==hour)
    if(sum<min_amount){
      hours_to_remove[i] = hour
      i = i + 1
    }
  
  }
  
  inx = c()
  for(hour in hours_to_remove){
    
    inx = which(df$difference==hour)
    df = df[-inx,]
    
  }
  return(df)

}


df = few_hours_remove(df,20)
df = df[df$difference<=75,]

df2 = df %>% group_by(difference,subject,group) %>% summarise(mean_accuracy = mean(accuracy,na.rm = TRUE))
df2 = few_hours_remove(df2,10)
df2 = df2[df2$group!="Reward",]
df2$subject = as.factor(df2$subject)
df2$group = as.factor(df2$group)
df2 = na.omit(df2)


fig1 = ggplot(data = df2, aes(x=difference, y=mean_accuracy*100,col=group)) +
  geom_point(stat = "identity",alpha = 0.3)+
  geom_smooth(method = "lm",aes(group = subject),se = FALSE,linetype = "twodash")+
  geom_smooth(method = "lm",aes(group = group),size = 2)+
  scale_color_manual(values = c("cyan4","red4"))+
  scale_y_continuous(breaks = seq(0,100,by = 10))+
  geom_hline(yintercept=50, linetype="dashed", color = "firebrick2")+
  labs(title = "Accuracy in Testing as a function of hours since last Learning trial")+
  ylab("% acuracy")+
  xlab("hours since training phase")+
  theme(
    plot.title = element_text(size = 22, face = "bold.italic",hjust = 0.5),
    axis.title.x = element_text(size = 16,face = "bold"),
    axis.title.y = element_text(size = 16,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2)
    )+
  facet_wrap(vars(group,subject),2)

####################################################################################
###################### ALL, GROUP, CONDITIONS PLOTS ################################
# overall recall
df2 = df %>% group_by(difference) %>% summarise(mean_accuracy = mean(accuracy,na.rm = TRUE))
df2 = na.omit(df2)


fig2 = ggplot(data = df2, aes(x=difference, y=mean_accuracy*100)) +
  geom_point(stat = "identity",alpha = 0.4,col = "steelblue",size = 4)+
  geom_smooth(data = df2, aes(x=difference, y=mean_accuracy*100),method = "loess",inherit.aes = FALSE,color = "purple")+
  geom_hline(yintercept=50, linetype="dashed", color = "firebrick2")+
  labs(title = "accuracy in NF over hours since last FB trial")+
  ylab("% acuracy")+
  xlab("hours")

# overall recall with condition and group   
df2 = df %>% group_by(difference,condition,group) %>% summarise(mean_accuracy = mean(accuracy,na.rm = TRUE))
df2 = na.omit(df2)
df2 = few_hours_remove(df2,10)
df2$condition = as.factor(df2$condition)
df2$group = as.factor(df2$group)
fig4 = ggplot(data = df2, aes(x=difference, y=mean_accuracy*100,col=group)) +
  geom_point(stat = "identity",alpha = 0.3)+
  geom_smooth(data = df2, aes(x=difference, y=mean_accuracy*100,group = condition),method = "lm",inherit.aes = FALSE,color = "skyblue")+
  geom_hline(yintercept=50, linetype="dashed", color = "firebrick2")+
  labs(title = "accuracy in NF over hours since last FB trial - condition")+
  ylab("% acuracy")+
  xlab("hours")+
  facet_wrap(vars(group,condition),3)

#recall by how easy
df2 = df %>% group_by(difference,Easy,group,subject) %>% summarise(mean_accuracy = mean(accuracy,na.rm = TRUE))
df2 = na.omit(df2)
df2 = few_hours_remove(df2,5)
df2 = df2[df2$difference<100,]
df2$Easy = as.factor(df2$Easy)
df2$group = as.factor(df2$group)

fig6 = ggplot(data = df2, aes(x=difference, y=mean_accuracy*100,col=group)) +
  geom_point(stat = "identity",alpha = 0.5,size = 3)+
  geom_smooth(data = df2, aes(x=difference, y=mean_accuracy*100,group = Easy),method = "lm",inherit.aes = FALSE,size = 2,color = "slateblue")+
  geom_hline(yintercept=50, linetype="dashed", color = "firebrick2")+
  labs(title = "How Easy - accuracy in NF over hours since last FB trial")+
  ylab("% acuracy")+
  xlab("hours")+
  facet_wrap(vars(group,Easy),2)+
  theme(panel.border = element_rect(colour = "black",fill = NA,size = 0.2))

#############################################################################################
#rename each day of the month as 0,1,2,3...
subjects = unique(Fulldata$subject)
abs_days = c()
i = 1  

for (sub in subjects) {
  start = FALSE
  c = 0
  for (day in stim.day.hour$day[which(stim.day.hour$subject == sub)]) {
    
    if(start == FALSE){
      abs_days[i] = c
      var_day = stim.day.hour$day[which(stim.day.hour$subject == sub)][1]
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


abs_hours = c()

for(i in 1:length(stim.day.hour$abs_days)){
  abs_hours[i] =  stim.day.hour$hour[i] + (stim.day.hour$abs_days[i]*24)
}
