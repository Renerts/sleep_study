# Statistics for Reward, Sleep & Memory experiment

###set up working directory, read file
wd <- "C:/Users/Renerts/Documents/Sleep_study/sleep_study_statistics.git"
setwd(wd)

plot.dir <- "C:/Users/Renerts/Documents/Sleep_study/Paper/tables_graphs"

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

table.plot <- function(x) {  #Takes array as input, returns data.frame ready for plotting
  
  StErr <- function(x) {  # Standart error
    sqrt(var(x,na.rm=TRUE)/sum(!is.na(x)))
  }
  means <- colMeans(x, na.rm=TRUE)
  stErr <- apply(x, 2, StErr)
  stErr.UP <- means + stErr
  stErr.DN <- means - stErr
  sub.x <- data.frame(means,stErr, stErr.UP, stErr.DN)
  return(sub.x)
}

### Create subsets of data

#### subsets for RTs

sub.RT.a <- table.plot(a.study.all[c(26:35)])  # Reaction times, a.study
sub.RT.b.all <- table.plot(b.study.RT[c(5:16)])
sub.RT.b.sleep <- table.plot(b.study.RT[b.study.RT$Group=='sleep',c(5:16)])
sub.RT.b.wake <- table.plot(b.study.RT[b.study.RT$Group=='wake',c(5:16)])

#### subsets for ACCs

sub.ACC.sc <- table.plot(a.study.all[c(36:43)])  # Accuracy, a.study, single-correct answers
sub.ACC.dc <- table.plot(a.study.all[c(44:49)])  # Accuracy, a.study, double-correct answers (&liv/nonliv)

#### subsets for memory performance and statistical analysis

sub.answers.piled=data.frame(R1=(a.study.all$r1.low+a.study.all$r1.high), R2=(a.study.all$r2.low+a.study.all$r2.high), F3=(a.study.all$f3.low+a.study.all$f3.high), F4=(a.study.all$f4.low+a.study.all$f4.high), Dist1=a.study.all$dist1, Dist2=a.study.all$dist2, Dist3=a.study.all$dist3, Dist4=a.study.all$dist4)

inter.performance.across <- table.plot(a.study.all[c('rCorr.low','rCorr.high')])
rownames(inter.performance.across)=c('all.low','all.high')
inter.performance.sleep <- table.plot(a.study.all[a.study.all$Group=='sleep', c('rCorr.low','rCorr.high')])
rownames(inter.performance.sleep)=c('sleep.low','sleep.high')
inter.performance.wake <- table.plot(a.study.all[a.study.all$Group=='wake',c('rCorr.low','rCorr.high')])
rownames(inter.performance.wake)=c('wake.low','wake.high')

sub.performance.combined <- rbind(inter.performance.across,inter.performance.sleep,inter.performance.wake)

sub.sleep.rcorr <- a.study.all[a.study.all$Group=="sleep",c('Gender','rCorr.low','rCorr.high')]
sub.wake.rcorr <- a.study.all[a.study.all$Group=="wake",c('Gender','rCorr.low','rCorr.high')]

##### for dprime plotting

inter.dsure.across <- table.plot(a.study.all[c('d.low.sure','d.high.sure')])
rownames(inter.dsure.across)=c('all.low','all.high')
inter.dsure.sleep <- table.plot(a.study.all[a.study.all$Group=='sleep', c('d.low.sure','d.high.sure')])
rownames(inter.dsure.sleep)=c('sleep.low','sleep.high')
inter.dsure.wake <- table.plot(a.study.all[a.study.all$Group=='wake',c('d.low.sure','d.high.sure')])
rownames(inter.dsure.wake)=c('wake.low','wake.high')

sub.dsure.combined <- rbind(inter.dsure.across,inter.dsure.sleep,inter.dsure.wake)

#### reshaping data in long format

sub.all.long <- reshape(a.study.all[c('Group','Gender','rCorr.low','rCorr.high')],direction='long',varying=c('rCorr.low','rCorr.high'),timevar='reward',times=c('low','high'))
sub.all.long$reward <- as.factor(sub.all.long$reward)
sub.all.long$id <- as.factor(sub.all.long$id)
                                 
sub.dprime.long <- reshape(a.study.all[c('Code','Group','Gender','d.low.all','d.high.all')],direction='long',varying=c('d.low.all','d.high.all'),v.names='d.prime', timevar='reward',times=c('low','high'))
sub.dprime.long$reward <- as.factor(sub.dprime.long$reward)
sub.dprime.long$id <- as.factor(sub.dprime.long$id)
                                    
sub.dprime.sure.long <- reshape(a.study.all[c('Code','Group','Gender','d.low.sure','d.high.sure')],direction='long',varying=c('d.low.sure','d.high.sure'),v.names='d.prime.sure', timevar='reward',times=c('low','high'))
sub.dprime.sure.long$reward <- as.factor(sub.dprime.long$reward)
sub.dprime.sure.long$id <- as.factor(sub.dprime.long$id)

### Plot that sh**

png(filename=file.path(plot.dir,'rt_a_study.png'),width=850, height=500, pointsize = 16)
x.RT <- 1:nrow(sub.RT.a)*2-1
plot(sub.RT.a$means~x.RT,  # Reaction times
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(500,850), 
     xlab='Condition', 
     ylab='Reaction Time, ms', 
     main='RTs across conditions, session I', 
     col=c(rep('black',4),rep('blue',4),rep('red',2)), 
     pch=16, 
     bty='l')
axis(1, at=x.RT, labels=FALSE)
text(x = x.RT, par("usr")[3]-5, labels = rownames(sub.RT.a), srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(500, 850, by=50), labels = FALSE)
text(y = seq(500, 850, by=50),labels = seq(500, 850, by=50), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.RT,sub.RT.a$stErr.UP,x.RT,sub.RT.a$stErr.DN,code=3,length=0.2,angle=90,col=c(rep('black',4),rep('blue',4),rep('red',2)))
legend("bottomright",paste(rownames(sub.RT.a),": mean",round(sub.RT.a$means, digits=2)),ncol=2,text.width=5)
dev.off()

png(filename=file.path(plot.dir,'acc_sc_a_study.png'),width=850, height=500, pointsize = 16)
x.ACC.sc <- 1:nrow(sub.ACC.sc)*2-1
plot(sub.ACC.sc$means~x.ACC.sc,  # Accuracy, reward cue and single-correct word stim
                      cex=1.5, 
                      xaxt='n', 
                      yaxt='n',
                      ylim=c(0.7,1), 
                      xlab='Condition', 
                      ylab='Accuracy', 
                      main='Response accuracy to reward cue \nand word stimuli, session I', 
                      col=c(rep('black',4),rep('blue',4)), 
                      pch=16, 
                      bty='l')
axis(1, at=x.ACC.sc, labels=FALSE)
text(x = x.ACC.sc, par("usr")[3]-0.01, labels = rownames(sub.ACC.sc), srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(0.7, 1, by=0.1), labels = FALSE)
text(y = seq(0.7, 1, by=0.1),labels = seq(0.7, 1, by=0.1), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.ACC.sc, sub.ACC.sc$stErr.UP, x.ACC.sc,sub.ACC.sc$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('black',4), rep('blue',4)))
legend("bottomright", paste(rownames(sub.ACC.sc), ": mean", round(sub.ACC.sc$means, digits=2)),ncol=2,text.width=4.2)
dev.off()

png(filename=file.path(plot.dir,'acc_dc_a_study.png'),width=850, height=500, pointsize = 16)
x.ACC.dc <- 1:nrow(sub.ACC.dc)*2-1
plot(sub.ACC.dc$means~x.ACC.dc,  # Accuracy, word stimulus plus living/non-living stimuli
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(0.7,1), 
     xlab='Condition', 
     ylab='Accuracy', 
     main='Response accuracy for reward & word stimuli \nand living vs nonliving stimuli, session I', 
     col=c(rep('black',4),rep('blue',4)), 
     pch=16, 
     bty='l')
axis(1, at=x.ACC.dc, labels=FALSE)
text(x = x.ACC.dc, par("usr")[3]-0.01, labels = rownames(sub.ACC.dc), srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(0.7, 1, by=0.1), labels = FALSE)
text(y = seq(0.7, 1, by=0.1),labels = seq(0.7, 1, by=0.1), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.ACC.dc, sub.ACC.dc$stErr.UP, x.ACC.dc,sub.ACC.dc$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('black',4), rep('blue',4)))
legend("bottomright", paste(rownames(sub.ACC.dc), ": mean", round(sub.ACC.dc$means, digits=2)),ncol=2,text.width=3)
dev.off()

png(filename=file.path(plot.dir,'distribution.png'),width=850, height=500, pointsize = 18)
barplot(colSums(sub.answers.piled), main="Distribution of answers", xlab='Response', ylab='Answers summed', col=c('darkblue',rep('gray',6),'darkblue'))  # Plot to see where most of the answers land
dev.off()

png(filename=file.path(plot.dir,'RT.b.all.png'),width=850, height=600, pointsize = 16)
x.RT.b.all <- 1:nrow(sub.RT.b.all)*2-1
plot(sub.RT.b.all$means~x.RT.b.all,  
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(700,1400), 
     xlab='Condition', 
     ylab='Reaction time in ms', 
     main='Reaction times across groups, session II', 
     col=c(rep('black',4),rep('darkblue',4),rep('darkgray',4)), 
     pch=16, 
     bty='l')
axis(1, at=x.RT.b.all, labels=FALSE)
text(x = x.RT.b.all, par("usr")[3]-0.01, labels = rownames(sub.RT.b.all), srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(700, 1400, by=50), labels = FALSE)
text(y = seq(700, 1400, by=50),labels = seq(700, 1400, by=50), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.RT.b.all, sub.RT.b.all$stErr.UP, x.RT.b.all,sub.RT.b.all$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('black',4),rep('darkblue',4),rep('darkgray',4)))
legend("bottomright", paste(rownames(sub.RT.b.all), ": mean", round(sub.RT.b.all$means, digits=2)),ncol=3,text.width=5)
dev.off()

png(filename=file.path(plot.dir,'RT.b.sleep.png'),width=850, height=600, pointsize = 16)
x.RT.b.sleep <- 1:nrow(sub.RT.b.sleep)*2-1
plot(sub.RT.b.sleep$means~x.RT.b.sleep,
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(700,1400), 
     xlab='Condition', 
     ylab='Reaction time in ms', 
     main='Reaction times in sleep group, session II', 
     col=c(rep('black',4),rep('darkblue',4),rep('darkgray',4)), 
     pch=16, 
     bty='l')
axis(1, at=x.RT.b.sleep, labels=FALSE)
text(x = x.RT.b.sleep, par("usr")[3]-0.01, labels = rownames(sub.RT.b.sleep), srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(700, 1400, by=50), labels = FALSE)
text(y = seq(700, 1400, by=50),labels = seq(700, 1400, by=50), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.RT.b.sleep, sub.RT.b.sleep$stErr.UP, x.RT.b.sleep,sub.RT.b.sleep$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('black',4),rep('darkblue',4),rep('darkgray',4)))
legend("bottomright", paste(rownames(sub.RT.b.sleep), ": mean", round(sub.RT.b.sleep$means, digits=2)),ncol=3,text.width=5)
dev.off()

png(filename=file.path(plot.dir,'RT.b.wake.png'),width=850, height=600, pointsize = 16)
x.RT.b.wake <- 1:nrow(sub.RT.b.wake)*2-1
plot(sub.RT.b.wake$means~x.RT.b.wake,  
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(700,1400), 
     xlab='Condition', 
     ylab='Reaction time in ms', 
     main='Reaction times in wake group, session II', 
     col=c(rep('black',4),rep('darkblue',4),rep('darkgray',4)), 
     pch=16, 
     bty='l')
axis(1, at=x.RT.b.wake, labels=FALSE)
text(x = x.RT.b.wake, par("usr")[3]-0.01, labels = rownames(sub.RT.b.wake), srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(700, 1400, by=50), labels = FALSE)
text(y = seq(700, 1400, by=50),labels = seq(700, 1400, by=50), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.RT.b.wake, sub.RT.b.wake$stErr.UP, x.RT.b.wake,sub.RT.b.wake$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('black',4),rep('darkblue',4),rep('darkgray',4)))
legend("bottomright", paste(rownames(sub.RT.b.wake), ": mean", round(sub.RT.b.wake$means, digits=2)),ncol=3,text.width=5)
dev.off()

# sub.performance.combined
png(filename=file.path(plot.dir,'Rcorr_all.png'),width=850, height=450, pointsize = 16)
x.performance.combined <- 1:nrow(sub.performance.combined)
plot(sub.performance.combined$means~x.performance.combined,  # Memory performance as Rcorr values
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(0.2,0.5), 
     xlab='Condition', 
     ylab='Rcorr value', 
     main='Memory performance for "sure" responses', 
     col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)), 
     pch=16, 
     bty='l')
axis(1, at=x.performance.combined, labels=FALSE)
text(x = x.performance.combined, par("usr")[3]-0.01, labels = rownames(sub.performance.combined), srt = 0, pos = 1, xpd = TRUE)
axis(2, at=seq(0.2, 0.5, by=0.1), labels = FALSE)
text(y = seq(0.2, 0.5, by=0.1),labels = seq(0.2, 0.5, by=0.1), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.performance.combined, sub.performance.combined$stErr.UP, x.performance.combined,sub.performance.combined$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)))
legend("bottomright", paste(rownames(sub.performance.combined), ": mean", round(sub.performance.combined$means, digits=2), '±',round(sub.performance.combined$stErr, digits=3)),ncol=3,text.width=1.5)
dev.off()

# sub.dsure.combined scatter
png(filename=file.path(plot.dir,'Dprime_sure.png'),width=850, height=450, pointsize = 16)
x.dsure.combined <- 1:nrow(sub.dsure.combined)
plot(sub.dsure.combined$means~x.dsure.combined,  # Memory performance as Dprime values
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(0.4,1), 
     xlab='Condition', 
     ylab='D-prime value', 
     main='Memory performance for "sure" responses', 
     col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)), 
     pch=16, 
     bty='l')
axis(1, at=x.dsure.combined, labels=FALSE)
text(x = x.dsure.combined, par("usr")[3]-0.01, labels = rownames(sub.dsure.combined), srt = 0, pos = 1, xpd = TRUE)
axis(2, at=seq(0.4, 1, by=0.1), labels = FALSE)
text(y = seq(0.4, 1, by=0.1),labels = seq(0.4, 1, by=0.1), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.dsure.combined, sub.dsure.combined$stErr.UP, x.dsure.combined,sub.dsure.combined$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)))
legend("bottomright", paste(rownames(sub.dsure.combined), ": mean", round(sub.dsure.combined$means, digits=2), '±',round(sub.dsure.combined$stErr, digits=3)),ncol=3,text.width=1.5)
dev.off()

# sub.dsure.combined barplot
png(filename=file.path(plot.dir,'Dprime_sure_bar.png'),width=850, height=450, pointsize = 16)
sub.barplot.dsure <- sub.dsure.combined
dsure.combined <- barplot(sub.dsure.combined[, 'means'],  # Memory performance as Dprime values
#      cex=1.5, 
#      xaxt='n', 
#      yaxt='n',
        axes=FALSE,
        width=rep(0.8, 6),
     ylim=c(0,1.2),
        xlim=c(0,6),
        space=rep(c(0.2, 0),3),
     xlab='Condition', 
     ylab='D-prime value', 
     main='Memory performance for "sure" responses', 
     col=c(rep('lightgray',2), rep('lightblue',2)), 
     pch=16,
        names.arg=rownames(sub.dsure.combined)
)
axis(2, at=seq(0, 1.2, by=0.2), labels = FALSE)
text(y = seq(0, 1.2, by=0.2),labels = seq(0, 1.2, by=0.2), par("usr")[1], srt = 0, pos = 2, xpd = TRUE)
arrows(dsure.combined, sub.dsure.combined$stErr.UP, dsure.combined,sub.dsure.combined$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('red',4)))
text(dsure.combined, 0, paste(round(sub.dsure.combined$means, 2), '±', round(sub.dsure.combined$stErr, 2)),cex=1,pos=3) 
text(paste('p=', round(dprime.sure.wake[['p.value']],3)), x=mean(dsure.combined[c(5,6), ]), y=max(sub.dsure.combined$stErr.UP+0.15 )) 
lines(x=dsure.combined[c(5,6), ], y=rep(max(sub.dsure.combined$stErr.UP+0.1),2))
dev.off()

# Data$Group=factor(Data$Group,labels=c("sleep","wake"),ordered=FALSE)
# Data$Gender=factor(Data$Gender,labels=c("female","male"),ordered=FALSE)
# 
# reward_group_subset=data.frame(Data$Code, Data$Group,Data$d.sure_low,Data$d.sure_high)
# reward_group_subset=reshape(reward_group_subset,direction="long",varying=list(3:4),timevar="reward",times=c("low","high"))
# colnames(reward_group_subset)=c("code","group","reward","dprime","id")
# reward_group_subset$reward=as.factor(reward_group_subset$reward)
# 




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

rcorr.low.across <- t.test(sub.sleep.rcorr$rCorr.low,sub.wake.rcorr$rCorr.low,paired=F)
rcorr.high.across <- t.test(sub.sleep.rcorr$rCorr.high,sub.wake.rcorr$rCorr.high,paired=F)

rcorr.sleep <- t.test(sub.sleep.rcorr$rCorr.low,sub.sleep.rcorr$rCorr.high,paired=T)
rcorr.wake <- t.test(sub.wake.rcorr$rCorr.low,sub.wake.rcorr$rCorr.high,paired=T)

rcorr.across <- t.test(a.study.all$rCorr.low,a.study.all$rCorr.high,paired=T)
r.corr.group <- t.test(rCorr ~ reward,data=sub.all.long,paired=T)

r.corr.group <- t.test(rCorr ~ Group,data=sub.all.long,paired=F, var.equal=T)

dprime.sure.wake <- t.test(d.prime.sure ~ reward, data=sub.dprime.sure.long[sub.dprime.sure.long$Group=='wake', ], paired=T)
dprime.sure.wake

### ezANOVA
library(ez)

ez.rCorr <- ezANOVA(data=sub.all.long,dv=rCorr, wid=id, within=reward, between=Group, type=2)
stargazer(ez.rCorr, type='text', summary=F)

ez.dprime <- ezANOVA(data=sub.dprime.long,dv=d.prime, wid=Code, within=reward, between=Group, type=2)
stargazer(ez.dprime, type='text', summary=F)

ez.dprime.sure <- ezANOVA(data=sub.dprime.sure.long,dv=d.prime.sure, wid=Code, within=reward, between=Group, type=2)
ez.dprime.sure
stargazer(ez.dprime.sure, type='text', summary=F)
capture.output(stargazer(ez.dprime.sure, type='text', summary=F), file=file.path(plot.dir,"dprime.sure.doc"))

### ANOVA
aov.goup.reward.dprime <- aov(d.prime.sure ~ Group*reward + Error(id/reward), data=sub.dprime.sure.long)
summary(aov.goup.reward.dprime)
capture.output(summary(aov.goup.reward.dprime), file=file.path(plot.dir,"aov.dprime.sure.doc"))

aov.reward.dprime <- aov(d.prime.sure ~ reward + Error(id), data=sub.dprime.sure.long)
summary(aov.reward.dprime)

