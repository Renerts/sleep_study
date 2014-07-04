# Statistics for Reward, Sleep & Memory experiment

###set up working directory, read file
wd<-"C:/Users/Renerts/Documents/Sleep_study/sleep_study_statistics.git"
setwd(wd)

a.study.all <- read.csv2("A_study_all.csv")
b.study.RT <- read.csv2("B_study_RT.csv")

str(a.study.all)

Data$Group=factor(Data$Group,labels=c("sleep","wake"),ordered=FALSE)
Data$Gender=factor(Data$Gender,labels=c("female","male"),ordered=FALSE)

reward_group_subset=data.frame(Data$Code, Data$Group,Data$d.sure_low,Data$d.sure_high)
reward_group_subset=reshape(reward_group_subset,direction="long",varying=list(3:4),timevar="reward",times=c("low","high"))
colnames(reward_group_subset)=c("code","group","reward","dprime","id")
reward_group_subset$reward=as.factor(reward_group_subset$reward)

sleep.group=subset(Data,Group=="sleep")
wake.group=subset(Data,Group=="wake")

###check data
nrow(Data)
ncol(Data)
head(Data)

###Statistics
options(contrasts=c("contr.sum","contr.poly"))
#### Means

mean.sure_low_vs_sure_high= t.test(Data$d_sure_low,Data$d_sure_high,paired=T)
Rcorr_all= t.test(Data$Rcorr_low,Data$Rcorr_high,paired=T)
Rcorr_sleep= t.test(sleep.group$Rcorr_low,sleep.group$Rcorr_high,paired=T)
Rcorr_wake= t.test(wake.group$Rcorr_low,wake.group$Rcorr_high,paired=T)

pairwise_T=pairwise.t.test(reward_group_subset$dprime,reward_group_subset$reward,p.adjust.method="bonferroni",paired=TRUE)

# mean_rcorr_low=mean(Data$Rcorr_low)
# mean_rcorr_high=mean(Data$Rcorr_high)
# 
# mean_rcorr=as.numeric(c(mean_rcorr_low,mean_rcorr_high))
# sd.mean_rcorr_low=sd(Data$Rcorr_low)
# sd.mean_rcorr_high=sd(Data$Rcorr_high)
# sd.rcorr_up=as.numeric(c(sd.mean_rcorr_low,sd.mean_rcorr_high))+mean_rcorr
# sd.rcorr_dn=as.numeric(c(sd.mean_rcorr_low,sd.mean_rcorr_high))-mean_rcorr

rcorr.low.sleep.wake=t.test(sleep.group$Rcorr_low,wake.group$Rcorr_low,paired=F)
rcorr.high.sleep.wake=t.test(sleep.group$Rcorr_high,wake.group$Rcorr_high,paired=F)