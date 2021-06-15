function [HRoutcome, stats, HR, punish,nopunish] = getHRperOutcome(name, first_block, last_block, resetFlag)   
    if nargin<4 || isempty(resetFlag); resetFlag = 0; end

    %% get HR data
    HR = readHR(name, first_block, last_block, resetFlag);
    
    %% read trial data
    filename = fullfile(pwd,'Data_Raw',['subject_' name],[name '_schedule.db']);
    db = sqlite(filename);   
    temp = cell2mat(fetch(db, 'SELECT feedback_time, feedback, block, outcome FROM trials WHERE choice_time IS NOT NULL ORDER BY choice_time ASC'));
    Trial.feedback = temp(find((first_block<=temp(:,3)&(last_block>=temp(:,3)))),2);
    Trial.feedbackTimes = temp(find((first_block<=temp(:,3)&(last_block>=temp(:,3)))),1);
    Trial.outcome = temp(find((first_block<=temp(:,3)&(last_block>=temp(:,3)))),4);
    db.close;
    
    %% get HR for each outcome
    epoch_data = Utilities.epoch(HR.times, HR.rate, Trial.feedbackTimes, 1000, 10000, 100);
    
    %% filter trials by outcome and feedback type
    feedbacksHR = epoch_data(find(Trial.feedback == 1),:,:);
    nofeedbackHR = epoch_data(find(Trial.feedback == 0),:,:);
    punish = epoch_data(find(Trial.outcome == -1 & Trial.feedback == 1),:,:);
    nopunish = epoch_data(find(Trial.outcome == 0 & Trial.feedback == 1),:,:);
    
    %% determine how many are missing and noisy
    stats.Ntrials = size(epoch_data,1);
    stats.Ntrials_missing = sum(sum(isnan(epoch_data),3) > 0);
    stats.Ntrials_noisy = sum(nanstd(epoch_data,[],3) > 5*nanmedian(nanstd(epoch_data,[],3))) ;
    
    %% filter missing and noisy all trials
    inc = sum(isnan(epoch_data),3)==0 & nanstd(epoch_data,[],3) < 5*nanmedian(nanstd(epoch_data,[],3)) ;
    HRoutcome = epoch_data(inc,:,:);    
    figure
    plot(squeeze(mean(HRoutcome)))
     %% filter missing and noisy all trials
    inc = sum(isnan(nofeedbackHR),3)==0 & nanstd(nofeedbackHR,[],3) < 5*nanmedian(nanstd(nofeedbackHR,[],3)) ;
    HRoutcome0 = nofeedbackHR(inc,:,:);    
    figure
    plot(squeeze(mean(HRoutcome0)))
     %% filter missing and noisy all trials
    inc = sum(isnan(feedbacksHR),3)==0 & nanstd(feedbacksHR,[],3) < 5*nanmedian(nanstd(feedbacksHR,[],3)) ;
    HRoutcome1 = feedbacksHR(inc,:,:);    
    figure
    plot(squeeze(mean(HRoutcome1)))
    %% filter missing and noisy punishment trials
    inc = sum(isnan(punish),3)==0 & nanstd(punish,[],3) < 5*nanmedian(nanstd(punish,[],3)) ;
    HRoutcome2 = punish(inc,:,:);    
    figure
    plot(squeeze(mean(HRoutcome2)))
%% filter missing and noisy no punishment trials
    inc = sum(isnan(nopunish),3)==0 & nanstd(nopunish,[],3) < 5*nanmedian(nanstd(nopunish,[],3)) ;
    HRoutcome3 = nopunish(inc,:,:);    
    figure
    plot(squeeze(mean(HRoutcome3)))
end
