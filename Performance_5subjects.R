## The following are general performance levels from subjects 006, 102, 103, 105, 106
subj1 = c(0.76,0.81,0.71,0.86,0.78,0.7,0.66,0.81,0.68,0.77,0.72,0.77,0.68,0.69)
subj2 = c(0.7,0.78,0.82,0.68,0.75,0.76,0.79,0.87,0.78,0.78,0.85,0.84,0.87,0.77)
subj3 = c(0.61,0.78,0.68,0.83,0.83,0.79,0.76,0.73,0.79,0.73,0.83,0.85,0.66,0.77)
subj4 = c(0.77,0.71,0.8,0.67,0.7,0.61,0.68,0.82,0.78,0.69,0.72,0.78,0.74,0.59)
subj5 = c(0.73,0.74,0.92,0.7,0.77,0.74,0.82,0.79,0.78,0.72,0.82,0.71,0.71,0.76)

data = data.frame(performance = c(subj1,subj2,subj3,subj4,subj5), subject = as.factor(c(rep(1,14),rep(2,14),rep(3,14),rep(4,14),rep(5,14))), day = c(1:14,1:14,1:14,1:14,1:14) )

library(ggplot2)

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

dataSE <- summarySE(data, measurevar="performance", groupvars=c("day"))

# Standard error of the mean
ggplot() +
  geom_point(data = data, aes(x=day, y=performance, color = subject), size = 3) +
  geom_errorbar(data = dataSE, aes(x=day, y=performance, ymin=performance-se, ymax=performance+se), width=.1) +
  geom_line(data = dataSE, aes(x=day, y=performance)) +
  geom_point(data = dataSE, aes(x=day, y=performance)) +
  geom_line(data = data.frame(x = 1:14, y = rep(0.5, 14)), aes(x=x, y=y), linetype = 2) +
  ylab("Correct choices") + xlab("Day") +
  scale_x_continuous(breaks = seq(1, 14, by = 1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25), labels = scales::percent) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = "none", axis.title=element_text(size=18), text=element_text(size=15))

#ggsave('Performance.pdf', width = 5, height = 3, units = "in")