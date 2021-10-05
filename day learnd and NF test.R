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
library("ggpubr")

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
# add time stamp
Fulldata = Fulldata %>% mutate(which_day=as.POSIXct(choice_time/1000, origin="1970-01-01")) 

Feedback <- Fulldata %>% filter(feedback==1)

Feedback = Feedback %>% mutate(day = day(which_day)) # day of the month

NoFeedback = Fulldata %>% filter(feedback==0)

#rename each day of the month as 1,2,3...
subjects = unique(Fulldata$subject)
abs_days = c()
i = 1  

for (sub in subjects) {
  start = FALSE
  c = 1
  for (day in Feedback$day[which(Feedback$subject == sub)]) {
    
    if(start == FALSE){
      abs_days[i] = c
      var_day = Feedback$day[which(Feedback$subject == sub)][1]
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

Feedback$abs_days = abs_days

# Find last day of stimuli in feedback trials
day_stim_learned = ddply(Feedback,.(subject,group,stim1),plyr::summarize, maxday=max(abs_days))


# mean accuracy in NF trials for each stimuli
mean_accuracy <- ddply(NoFeedback,.(subject,group,stim1),plyr::summarize, mean=mean(accuracy,na.rm = TRUE))

# find rows in df1 that are missing in df2. Another way is to filter them out using semi_join()
a1 =day_stim_learned %>% select(subject,group,stim1)
a2 =mean_accuracy %>% select(subject,group,stim1)
res = anti_join(a1,a2)

# filter those rows out
for (i in 1:length(res$subject)){
  day_stim_learned = day_stim_learned[-which((day_stim_learned$subject==res$subject[i])&(day_stim_learned$stim1==res$stim1[i])),]
}
# add mean accuracy to the day_stim_learned df
day_stim_learned = day_stim_learned %>% mutate(accuracy = mean_accuracy$mean[which(mean_accuracy$subject==subject&mean_accuracy$stim1==stim1)])
day_stim_learned = day_stim_learned[which(day_stim_learned$maxday<12),]
# get confidence intervals and mean day for each group 
CI_accuracy <- ddply(day_stim_learned,.(group,maxday),plyr::summarize,
                     ci_day = 1.96 * sqrt(mean(accuracy, na.rm = TRUE) * (1 - mean(accuracy, na.rm = TRUE)) / sum(!is.na(accuracy))),
                     mean_day = mean(accuracy, na.rm = TRUE))

avg_day = ddply(day_stim_learned,.(maxday),plyr::summarize,mean_day = mean(accuracy))
group_day = ddply(day_stim_learned,.(maxday,group),plyr::summarize,mean_day = mean(accuracy),se_day = sd(accuracy)/sqrt(length(accuracy)))
subject_day = ddply(day_stim_learned,.(subject,maxday,group),plyr::summarize,mean_day = mean(accuracy))
# plot only points from the punishment groups and add mean and CI for the reward group
subject_day = subject_day[which(subject_day$group!="Reward"),] 
fig2 = ggplot(subject_day, aes(x = maxday, y = mean_day,col = group)) +
  geom_point(size=3,alpha=0.5)+
  geom_point(data = group_day,aes(col = group), stat = "identity",size=8,shape = 15)+
  geom_errorbar(data = group_day[which(group_day$group=="Reward"),],
                aes(ymin=(mean_day-se_day),ymax=(mean_day+se_day)), width=.4)+
  labs(title = "Day stimulus learned - accuracy in testing trials",y = "accuracy",x = "days")+
  scale_x_continuous(breaks = seq(1,11,by=1))+
  scale_color_manual(values = c("cyan4","darkorange3","red4"))+
  theme(
    plot.title = element_text(size = 17, face = "bold.italic"),
    axis.title.x = element_text(size = 15,face = "bold"),
    axis.title.y = element_text(size = 15,face = "bold"),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.2),
    panel.background = element_rect(fill = "grey85")
  )

ggarrange(fig1,fig2,labels = c("A", "B"),
          ncol = 2, nrow = 1)
