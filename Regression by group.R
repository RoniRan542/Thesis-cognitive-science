################ REGRESSION F score ~ NoF score by group #################

rm(list = ls())
graphics.off()
library("RSQLite")
library("plyr")
library("dplyr")
library("ggplot2")
library("zoo")
library("tidyverse")
library("anytime")
library("writexl")
library("readxl")
library("readr")
locale("he")

setwd("C:/Users/aharo/Google Drive/mylabDATA") #directory where the schedule file is located


####### NOISE #######

noise_table = read_excel("subjects_noise.xlsx")

Fscore_noise = noise_table$Objective_accuracy_mean_1
NoFscore_noise = noise_table$Objective_accuracy_mean_0
F_ReTime_noise = noise_table$RT_mean_1
NoF_ReTime_noise = noise_table$RT_mean_0

# plot regression line for feedback vs no feedback scores
plot(Fscore_noise, NoFscore_noise, pch = 16, cex = 1.3, col = "blue", main = "F score ~ NoF score - Noise", xlab = "F score", ylab = "Nof score")
abline(lm(NoFscore_noise ~ Fscore_noise))

# plot regression line for feedback RT vs feedback scores
plot(F_ReTime_noise, Fscore_noise, pch = 16, cex = 1.3, col = "yellow", main = "ReacTime ~ F score - Noise", xlab = "ReacTime", ylab = "F score")
abline(lm(Fscore_noise~F_ReTime_noise))

# plot regression line for no feedback RT vs no feedback scores
plot(NoF_ReTime_noise, NoFscore_noise, pch = 16, cex = 1.3, col = "green", main = "ReacTime ~ NoF score - Noise", xlab = "ReacTime", ylab = "Nof score")
abline(lm(NoFscore_noise ~ NoF_ReTime_noise))

#### MONEY ######

money_table = read_excel("subjects_money.xlsx")

Fscore_money = money_table$Objective_accuracy_mean_1
NoFscore_money = money_table$Objective_accuracy_mean_0
F_ReTime_money = money_table$RT_mean_1
NoF_ReTime_money = money_table$RT_mean_0

plot(Fscore_money, NoFscore_money, pch = 16, cex = 1.3, col = "red", main = "F score ~ NoF score - Money", xlab = "F score", ylab = "Nof score")
abline(lm(NoFscore_money ~ Fscore_money))
plot(F_ReTime_money, Fscore_money, pch = 16, cex = 1.3, col = "yellow", main = "ReacTime ~ F score - Noise", xlab = "ReacTime", ylab = "F score")
abline(lm(Fscore_money~F_ReTime_money))
plot(NoF_ReTime_money, NoFscore_money, pch = 16, cex = 1.3, col = "green", main = "ReacTime ~ NoF score - Noise", xlab = "ReacTime", ylab = "Nof score")
abline(lm(NoFscore_money ~ NoF_ReTime_money))

N_and_M = data.frame(G = c(1,1,1,1,0,0,0,0),fscore = c(Fscore_noise,Fscore_money),nofscore = c(NoFscore_noise,NoFscore_money))





#Loading data.table package and converting data frame
library(data.table)
N_and_M = data.table(N_and_M)

#Creating linear regression model groups defined in column G
N_and_M[,as.list(coef(lm(nofscore ~ fscore))), by=G]
