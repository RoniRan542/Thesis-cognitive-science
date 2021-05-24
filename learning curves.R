library(runner)


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





#######################
## Parameters to set ##

smoothing_window <- 10

#######################

# TODO: create base figure
IDs <- c(101,103,105,106,109,201,202,203,204,206)

for (n in IDs){
  
  # TODO: Load in the dataframe:
  trials_1 <- Fulldata %>% filter(subject ==n)
  
  # Filter feedback trials and non-practice blocks
  trials_f <- trials_1 %>% filter(feedback > .5) %>% filter(block > 12)
  
  # Add a counter for the feedback trials:
  num_f <- 0
  b <- 0
  trials_f$num_f <- -1
  t <- 1
  while (t <= nrow(trials_f)){
    if (trials_f$block[t] > b){
      b <- b+1
      num_f <- 0
    }
    else {
      trials_f$num_f[t] <- num_f
      num_f <- num_f + 1
      t <- t + 1
    }
  }
  
  # Create groups of trials based on the feedback counter, and compute the average accuracy over those:
  acc_per_trial_number <- trials_f %>% group_by(num_f) %>% summarise(mean = mean(accuracy,na.rm = TRUE))
  
  # Apply smoothing with sliding windows, of size 'wmoothing_window', and compute the mean:
  learning_curve <- rollapply(acc_per_trial_number$mean, smoothing_window, mean)
  index <- seq(1,40)
  
  # Add to DataFrame:
  if (n==IDs[1]){
    df<-data.frame(learning_curve = learning_curve, index = index, subject = n)
  } else {
    df <- rbind(df, data.frame(learning_curve = learning_curve, index = index, subject = n))
  }
  
}
df = df %>% mutate(group = ifelse(subject<200,"Money","Noise"))
df$subject <- as.factor(df$subject)
df$group <- as.factor(df$group)
dfgroups = df %>% group_by(group,index) %>% summarise(group_curve = mean(learning_curve,na.rm = TRUE))
meanGroups = df %>% group_by(group) %>% summarise(group_curve = mean(learning_curve,na.rm = TRUE))
money_avg = meanGroups$group_curve[1]
noise_avg = meanGroups$group_curve[2]

fig1 <- ggplot(data = df, aes(x=index, y=learning_curve*100,fill = subject)) +
  geom_line(aes(color = group),stat = "identity") +
  stat_summary(aes(group = group,col = group,size = 0.01),fun=mean, geom="line")+
  geom_hline(yintercept=noise_avg*100, linetype="dashed", color = "cyan4")+
  geom_hline(yintercept=money_avg*100, linetype="dashed", color = "firebrick2")+
  ylab("% correct choice")+
  xlab("window")+
  ggtitle("Learning curves")
fig1

fig2 = ggplot(data = dfgroups, aes(x=index, y=group_curve*100)) +
  geom_line(aes(color = group,size = 0.01, alpha = group_curve)) +
  ylab("% correct choice")+
  xlab("window")+
  ggtitle("Learning curves")
fig2

df <- Fulldata %>% filter(subject == 203)
counts <- df %>% group_by(block) %>% summarise(counts = sum(feedback))
