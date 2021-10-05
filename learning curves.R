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
  mutate(group = ifelse(subject>1000,"Money-reward",ifelse(subject<200,"Money-punisher","Noise-punisher")))
Fulldata$group = as.factor(Fulldata$group)




#######################
## Parameters to set ##

smoothing_window <- 10

#######################

# TODO: create base figure
IDs <- unique(Fulldata$subject)

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

df = df %>% mutate(group = ifelse(subject>1000,"Money-reward",ifelse(subject<200,"Money-punisher","Noise-punisher")))
df$subject <- as.factor(df$subject)
df$group <- as.factor(df$group)

fig1 <- ggplot(data = df, aes(x=index, y=learning_curve*100,fill = subject)) +
  geom_line(data = subset(df,group!="Money-reward"),aes(color = group),stat = "identity",size = 1,alpha = 0.8) +
  stat_summary(aes(group = group,col = group),fun=mean, geom="line",size = 4,alpha = 0.7)+
  stat_summary(data = subset(df,group=="Money-reward"),
               aes(group = group,col = group),
               fun.data = mean_se, 
               geom="errorbar",
               size = 1,
               alpha = 0.5
               )+
  ylab("% correct choice")+
  xlab("window")+
  ggtitle("Learning curves")+
  scale_color_manual(values = c("cyan4","tan3","firebrick"))+
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "grey87"),
        plot.title = element_text(size = 14, face = "bold.italic",hjust = 0.5),
        axis.title.x = element_text(size = 11,face = "bold"),
        axis.title.y = element_text(size = 11,face = "bold"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))
fig1
