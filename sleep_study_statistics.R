# Statistics for Reward, Sleep & Memory experiment

###set up working directory, read file
wd="C:/Users/Renerts/Documents/Sleep_study/sleep_study_statistics"

setwd(wd)
Data=read.csv2("Relevant_data_09_05_14.csv")

###check data
nrow(Data)
ncol(Data)
tail(Data)

###Statistics

#### Means

mean.sure_low_vs_sure_high= t.test(Data$d.sure_low,Data$d.sure_high,paired=T) 