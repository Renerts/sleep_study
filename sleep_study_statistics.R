# Statistics for Reward, Sleep & Memory experiment

###set up working directory, read file
wd <-"C:/Users/Renerts/Documents/Sleep_study/sleep_study_statistics.git"
setwd(wd)

a.study.all <- read.csv2("A_study_all.csv")
b.study.RT <- read.csv2("B_study_RT.csv")
### Check data
# nrow(Data)
# ncol(Data)
# head(Data)
# str(a.study.all)
# str(b.study.RT)
names(a.study.all)

### Function definitions

StErr <- function(x) {  # Takes array as input, gives st.error as output
  sqrt(var(x,na.rm=TRUE)/sum(!is.na(x)))
  }

## Function ready, integrate into code!!!!!

# table.plot <- function(x) {  #Takes array as input, returns data.frame ready for plotting
#   
#   means <- colMeans(x, na.rm=TRUE)
#   names.x <- names(x)
#   stErr <- apply(x, 2, StErr)
#   stErr.UP <- means + stErr
#   stErr.DN <- means - stErr
#   sub.x <- data.frame(means,stErr, stErr.UP, stErr.DN)
#   return(sub.x)
# }

### Create subsets of data

#### subsets for RTs

names.RT.a <- names(a.study.all[c(26:35)])
means.RT.a <- colMeans(a.study.all[c(26:35)], na.rm=TRUE)
StErr.RT.a <- apply(a.study.all[c(26:35)], 2, StErr)
StErr.RT.a.UP <- means.RT.a + StErr.RT.a
StErr.RT.a.DN <- means.RT.a - StErr.RT.a
sub.RT.a <- data.frame(means.RT.a, StErr.RT.a.UP, StErr.RT.a.DN)

#### subsets for ACCs

names.ACC.a <- names(a.study.all[c(36:49)])
means.ACC.a <- colMeans(a.study.all[c(36:49)], na.rm=TRUE)
StErr.ACC.a <- apply(a.study.all[c(36:49)], 2, StErr)
StErr.ACC.a.UP <- means.ACC.a + StErr.ACC.a
StErr.ACC.a.DN <- means.ACC.a - StErr.ACC.a
sub.ACC.a <- data.frame(means.ACC.a, StErr.ACC.a.UP, StErr.ACC.a.DN)



### Plot that shit


x.RT <- 1:10*2-1
plot(sub.RT.a$means.RT.a[c(1:10)]~x.RT,  # Reaction times
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(500,850), 
     xlab='Condition', 
     ylab='Reaction Time, ms', 
     main='RTs across conditions', 
     col=c(rep('black',4),rep('blue',4),rep('red',2)), 
     pch=16, 
     bty='l')
axis(1, at=x.RT, labels=FALSE)
text(x = x.RT, par("usr")[3]-5, labels = names.RT.a, srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(500, 850, by=50), labels = FALSE)
text(y = seq(500, 850, by=50),labels = seq(500, 850, by=50), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x,StErr.RT.a.UP,x,StErr.RT.a.DN,code=3,length=0.2,angle=90,col=c(rep('black',4),rep('blue',4),rep('red',2)))
legend("bottomright",paste(names.RT.a,": mean",round(sub.RT.a$means.RT.a, digits=2)),ncol=2,text.width=5)

x.ACC.sc <- 1:8*2-1
plot(sub.ACC.a$means.ACC.a[c(1:8)]~x.ACC,  # Accuracy, reward cue and single-correct word stim
                      cex=1.5, 
                      xaxt='n', 
                      yaxt='n',
                      ylim=c(0.7,1), 
                      xlab='Condition', 
                      ylab='Accuracy', 
                      main='Response accuracy to reward cue stimulus', 
                      col=c(rep('black',4),rep('blue',4)), 
                      pch=16, 
                      bty='l')
axis(1, at=x.ACC, labels=FALSE)
text(x = x.ACC, par("usr")[3]-0.01, labels = names.ACC.a[c(1:8)], srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(0.7, 1, by=0.1), labels = FALSE)
text(y = seq(0.7, 1, by=0.1),labels = seq(0.7, 1, by=0.1), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x,StErr.ACC.a.UP[c(1:8)],x,StErr.ACC.a.DN[c(1:8)],code=3,length=0.2,angle=90,col=c(rep('black',4),rep('blue',4)))
legend("bottomright",paste(names.ACC.a[c(1:8)],": mean",round(sub.ACC.a$means.ACC.a[c(1:8)], digits=2)),ncol=2,text.width=4.2)

x.ACC.dc <- 1:6*2-1
plot(sub.ACC.a$means.ACC.a[c(9:14)]~x.ACC.dc,  #Accuracy, double-correct word stim and living/non-living
                      cex=1.5, 
                      xaxt='n', 
                      yaxt='n',
                      ylim=c(0.7,1), 
                      xlab='Condition', 
                      ylab='Accuracy', 
                      main='Response accuracy to word stimulus', 
                      col=c(rep('black',4),rep('blue',2)), 
                      pch=16, 
                      bty='l')
axis(1, at=x.ACC, labels=FALSE)
text(x = x.ACC, par("usr")[3]-0.01, labels = names.ACC.a[c(9:14)], srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(0.7, 1, by=0.1), labels = FALSE)
text(y = seq(0.7, 1, by=0.1),labels = seq(0.7, 1, by=0.1), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x,StErr.ACC.a.UP[c(9:14)],x,StErr.ACC.a.DN[c(9:14)],code=3,length=0.2,angle=90,col=c(rep('black',4),rep('blue',2)))
legend("bottomright",paste(names.ACC.a[c(9:14)],": mean",round(sub.ACC.a$means.ACC.a[c(9:14)], digits=2)),ncol=2,text.width=3)



# Data$Group=factor(Data$Group,labels=c("sleep","wake"),ordered=FALSE)
# Data$Gender=factor(Data$Gender,labels=c("female","male"),ordered=FALSE)
# 
# reward_group_subset=data.frame(Data$Code, Data$Group,Data$d.sure_low,Data$d.sure_high)
# reward_group_subset=reshape(reward_group_subset,direction="long",varying=list(3:4),timevar="reward",times=c("low","high"))
# colnames(reward_group_subset)=c("code","group","reward","dprime","id")
# reward_group_subset$reward=as.factor(reward_group_subset$reward)
# 
# sleep.group=subset(Data,Group=="sleep")
# wake.group=subset(Data,Group=="wake")



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