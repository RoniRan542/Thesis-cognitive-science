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
answers <- list()
subject_names <-c()
for(i in 1:length(db_files)){
  
  if(grepl("schedule",db_files[[i]])){
    data_1 = dbConnect(SQLite(),as.character(db_files[i]))
    answers[[i]] = dbGetQuery(data_1, "SELECT * FROM answers")
    subject_names[i] <- as.numeric(numextract(db_files[i]))
    answers[[i]]$subject = subject_names[i]
    
  }
  
}

end_quest = data.frame()
counter = 1
for(i in 1:length(answers)){
  if(sum(answers[[i]]$questionnaire_type==20)>0){
    if(counter == 1){
      end_quest = subset(answers[[i]],questionnaire_type==20)
      counter = counter + 1
    }else{
      end_quest= rbind(end_quest,subset(answers[[i]],questionnaire_type==20))
    }
  }
}

end_quest = end_quest %>% mutate(group = ifelse(subject<200,"Money loss","White Noise"))
end_quest$answer = as.numeric(end_quest$answer) 

quest_1 = subset(end_quest, question==0)
quest_2 = subset(end_quest, question==1)
quest_3 = subset(end_quest, question==2)
quest_4 = subset(end_quest, question==3)
quest_5 = subset(end_quest, question==4)
quest_6 = subset(end_quest, question==5)
quest_7 = subset(end_quest, question==6)

mean_bygroup_quest_3 = quest_3 %>% group_by(group) %>% summarise(mean = mean(answer,na.rm = TRUE))
mean_bygroup_quest_4 = quest_4 %>% group_by(group) %>% summarise(mean = mean(answer,na.rm = TRUE))
mean_bygroup_quest_5 = quest_5 %>% group_by(group) %>% summarise(mean = mean(answer,na.rm = TRUE))
mean_bygroup_quest_6 = quest_6 %>% group_by(group) %>% summarise(mean = mean(answer,na.rm = TRUE))
mean_bygroup_quest_7 = quest_7 %>% group_by(group) %>% summarise(mean = mean(answer,na.rm = TRUE))
