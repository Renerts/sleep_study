# Statistics for Reward, Sleep & Memory experiment

###set up working directory, read file
wd="C:/Users/Renerts/Documents/Sleep_study/sleep_study_statistics.git"

setwd(wd)
Data=read.csv2("Relevant_data_09_05_14.csv")

Data$Group=factor(Data$Group,labels=c("sleep","wake"),ordered=FALSE)
Data$Gender=factor(Data$Gender,labels=c("female","male"),ordered=FALSE)

reward_group_subset=data.frame(Data$Code, Data$Group,Data$d.sure_low,Data$d.sure_high)
reward_group_subset=reshape(reward_group_subset,direction="long",varying=list(3:4),timevar="reward",times=c("low","high"))
colnames(reward_group_subset)=c("code","group","reward","dprime","id")
reward_group_subset$reward=as.factor(reward_group_subset$reward)

###check data
nrow(Data)
ncol(Data)
head(Data)

###Statistics
options(contrasts=c("contr.sum","contr.poly"))
#### Means

mean.sure_low_vs_sure_high= t.test(Data$d.sure_low,Data$d.sure_high,paired=T)

pairwise_T=pairwise.t.test(reward_group_subset$dprime,reward_group_subset$reward,p.adjust.method="bonferroni",paired=TRUE)
