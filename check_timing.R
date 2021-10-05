check_timing <- function(Fulldata){
  times_e <- list()    
  datas <- c()
      stim_distance <- c()
      learning_distance <-c()
      subject_list <- unique(Fulldata$subject)
      # after how many blocks do we test
   for(j in 1:length(unique(Fulldata$subject))){
for(i in 1:63){
  subject_data=Fulldata %>% filter(subject==subject_list[j])    
  check_testing <- subject_data %>%
      filter(feedback==0 & (stim1==i | stim2==i)) %>%
      group_by(block,trial) %>% select(block,trial,feedback,stim1,stim2,stim_time)
  
  testing_block <- min(unique(check_testing$block))
  testing_time  <- min(as.double(check_testing$stim_time))
  
  check_learning <- subject_data %>%
     filter(feedback==1 & (stim1==i | stim2==i)) %>%
     group_by(block,trial) %>% select(block,trial,feedback,stim1,stim2,stim_time)
  
 learning_block <- max(unique(check_learning$block))
 learning_time <- max(as.double(check_learning$stim_time))
 if(i <31){
   
   
   
 }else{
   
 }
      # it will be in the form of a list
stim_distance[i] <- testing_block-learning_block
 learning_distance[i] <- (testing_time-learning_time)/(1000*3600)
}
     datas$stim_distance=stim_distance
     datas$learning_distance=learning_distance
     datas$subject =rep(j,length(datas$stim_distance))
     datas$stim =1:63
     datas <- as.data.frame(datas)
     datas <- datas %>% mutate(need_filter = ifelse(learning_distance<3,1,0))
times_e[[j]] <- datas
}
return(times_e)}