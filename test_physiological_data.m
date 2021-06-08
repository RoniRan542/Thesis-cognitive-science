%This script runs a few functions to help you test the quality of your EEG and HR data
%the path for the raw data is assumed to be: fullfile(pwd,'Data_Raw',['subject_' name])
clear; clc
name='110'; %insert subject ID as a string
%defining which range of blocks to test:
first_block=13;
last_block=22;
mkdir(fullfile(pwd, 'Data_Processed', ['subject_' name])) %prepearing output folder
delete_duplicates_and_merge_physio(name); %merge all physio to one file
[Ntotal, Ngood, epoch_data]=EEGanalysis_test(name, first_block, last_block); %test EEG data
[HRoutcome, stats, HR] = getHRperOutcome(name, first_block, last_block, 1);%test HR data
% calculate the percentage of trials with good data (seperate for EEG and
% HR). Should be above 70%.
EEG_percen=(Ngood/Ntotal)*100;
HR_percen=((stats.Ntrials-stats.Ntrials_missing-stats.Ntrials_noisy)/stats.Ntrials)*100;