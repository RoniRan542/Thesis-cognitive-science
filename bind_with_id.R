bind_with_id <- function(datalist,subject_names,stimuli){
      require(dplyr) 
   # z score everything per subject 
      for(j in 1:length(datalist)){
        #  datalist[[i]] <- datalist[[i]] %>% filter(block>12,feedback==0)
          #  Feedback= datalist[[i]] %>% filter(feedback==1)
            #  timing <- check_timing(datalist[[i]])
         datalist[[j]]$relative_stim1=rep(NaN, nrow(datalist[[j]]))
         datalist[[j]]$relative_stim2=rep(NaN, nrow(datalist[[j]]))
         for (i in 2:nrow(datalist[[j]])){
           
            datalist[[j]]$relative_stim1[i]=mean(datalist[[j]]$outcome[which(datalist[[j]]$stim1==datalist[[j]]$stim1[i]&datalist[[j]]$choice==0&datalist[[j]]$feedback==1&((datalist[[j]]$trial<datalist[[j]]$trial[i]&datalist[[j]]$block==datalist[[j]]$block[i])|datalist[[j]]$block<datalist[[j]]$block[i])|datalist[[j]]$stim2==datalist[[j]]$stim1[i]&datalist[[j]]$choice==1&datalist[[j]]$feedback==1&((datalist[[j]]$trial<datalist[[j]]$trial[i]&datalist[[j]]$block==datalist[[j]]$block[i])|datalist[[j]]$block<datalist[[j]]$block[i]))]) 
            datalist[[j]]$relative_stim2[i]=mean(datalist[[j]]$outcome[which(datalist[[j]]$stim1==datalist[[j]]$stim2[i]&datalist[[j]]$choice==0&datalist[[j]]$feedback==1&((datalist[[j]]$trial<datalist[[j]]$trial[i]&datalist[[j]]$block==datalist[[j]]$block[i])|datalist[[j]]$block<datalist[[j]]$block[i])|datalist[[j]]$stim2==datalist[[j]]$stim2[i]&datalist[[j]]$choice==1&datalist[[j]]$feedback==1&((datalist[[j]]$trial<datalist[[j]]$trial[i]&datalist[[j]]$block==datalist[[j]]$block[i])|datalist[[j]]$block<datalist[[j]]$block[i]))])
         }
         datalist[[j]]$relatively_correct=NA
         index=which(!is.nan(datalist[[j]]$relative_stim1)&!is.nan(datalist[[j]]$relative_stim2))
         
         #accuracy according to experienced probabilities (differences of less than 10% between probabilties are omitted)
         for (i in index){
            datalist[[j]]$relatively_correct[i]=((datalist[[j]]$relative_stim1[i]>datalist[[j]]$relative_stim2[i]+0.1)&&(datalist[[j]]$choice[i]==0)||(datalist[[j]]$relative_stim1[i]+0.1<datalist[[j]]$relative_stim2[i])&&(datalist[[j]]$choice[i]==1))
            if (((abs(datalist[[j]]$relative_stim1[i]-datalist[[j]]$relative_stim2[i])<0.1)&&(abs(datalist[[j]]$relative_stim2[i]-datalist[[j]]$relative_stim1[i]))<0.1))
               datalist[[j]]$relatively_correct[i]=NA
         }
         
         for (i in 1:length(datalist[[j]]$block)){
            if(datalist[[j]]$stim1[i] >0 & datalist[[j]]$stim2[i] >0){
               datalist[[j]]$EVStim1[i]=stimuli[[j]]$reward[datalist[[j]]$stim1[i]+1]
               datalist[[j]]$EVStim2[i]=stimuli[[j]]$reward[datalist[[j]]$stim2[i]+1]
               datalist[[j]]$accuracy[i]=((datalist[[j]]$EVStim1[i]>datalist[[j]]$EVStim2[i])&&(datalist[[j]]$choice[i]==0)||(datalist[[j]]$EVStim1[i]<datalist[[j]]$EVStim2[i])&&(datalist[[j]]$choice[i]==1))
               if (datalist[[j]]$EVStim1[i]==datalist[[j]]$EVStim2[i])
                  datalist[[j]]$accuracy[i]=NA
            }
      
    
         }
         
         
         
             datalist[[j]] <- datalist[[j]] %>% mutate(subject=subject_names[j]);
            # datalist[[i]] <- datalist[[i]][-c((datalist[[i]]$stim1 %in% stim_to_filter$stim | datalist[[i]]$stim2 %in% stim_to_filter$stim))]

      }
     Fulldata <- dplyr::bind_rows(datalist)
   #  datalist[[i[datalist[[i$subject==7,]$subject=6;
    # datalist[[i[datalist[[i$subject==9,]$subject=7;
     #datalist[[i[datalist[[i$subject==10,]$subject=8;
     
      return(Fulldata); 
}
