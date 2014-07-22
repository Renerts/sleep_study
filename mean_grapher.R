inter.performance.across <- table.plot(a.study.all[c('d.low.sure','d.high.sure')])
rownames(inter.performance.across)=c('all.low','all.high')
inter.performance.sleep <- table.plot(a.study.all[a.study.all$Group=='sleep', c('d.low.sure','d.high.sure')])
rownames(inter.performance.sleep)=c('sleep.low','sleep.high')
inter.performance.wake <- table.plot(a.study.all[a.study.all$Group=='wake',c('d.low.sure','d.high.sure')])
rownames(inter.performance.wake)=c('wake.low','wake.high')

sub.d.performance.combined <- rbind(inter.performance.across,inter.performance.sleep,inter.performance.wake)

# sub.performance.combined
png(filename=file.path(plot.dir,'d_prime_all.png'),width=850, height=600, pointsize = 16)
x.d.performance.combined <- 1:nrow(sub.d.performance.combined)
plot(sub.d.performance.combined$means~x.d.performance.combined,  # Memory performance as Rcorr values
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(0.4,1), 
     xlab='Condition', 
     ylab='d prime value', 
     main='Memory performance for "sure" responses', 
     col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)), 
     pch=16, 
     bty='l')
axis(1, at=x.d.performance.combined, labels=FALSE)
text(x = x.d.performance.combined, par("usr")[3]-0.01, labels = rownames(sub.d.performance.combined), srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(0.4, 0.9, by=0.1), labels = FALSE)
text(y = seq(0.4, 0.9, by=0.1),labels = seq(0.4, 0.9, by=0.1), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.d.performance.combined, sub.d.performance.combined$stErr.UP, x.d.performance.combined,sub.d.performance.combined$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)))
legend("bottomright", paste(rownames(sub.d.performance.combined), ": mean", round(sub.d.performance.combined$means, digits=2), '±',round(sub.d.performance.combined$stErr, digits=3)),ncol=3,text.width=1.5)
dev.off()


inter.performance.across <- table.plot(a.study.all[c('d.low.all','d.high.all')])
rownames(inter.performance.across)=c('all.low','all.high')
inter.performance.sleep <- table.plot(a.study.all[a.study.all$Group=='sleep', c('d.low.all','d.high.all')])
rownames(inter.performance.sleep)=c('sleep.low','sleep.high')
inter.performance.wake <- table.plot(a.study.all[a.study.all$Group=='wake',c('d.low.all','d.high.all')])
rownames(inter.performance.wake)=c('wake.low','wake.high')

sub.d.performance.combined <- rbind(inter.performance.across,inter.performance.sleep,inter.performance.wake)

# sub.performance.combined
png(filename=file.path(plot.dir,'d_prime_all.png'),width=850, height=600, pointsize = 16)
x.d.performance.combined <- 1:nrow(sub.d.performance.combined)
plot(sub.d.performance.combined$means~x.d.performance.combined,  # Memory performance as Rcorr values
     cex=1.5, 
     xaxt='n', 
     yaxt='n',
     ylim=c(0.7,1.7), 
     xlab='Condition', 
     ylab='d prime value', 
     main='Memory performance', 
     col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)), 
     pch=16, 
     bty='l')
axis(1, at=x.d.performance.combined, labels=FALSE)
text(x = x.d.performance.combined, par("usr")[3]-0.01, labels = rownames(sub.d.performance.combined), srt = 15, pos = 1, xpd = TRUE)
axis(2, at=seq(0.7, 1.7, by=0.1), labels = FALSE)
text(y = seq(0.7, 1.7, by=0.1),labels = seq(0.7, 1.7, by=0.1), par("usr")[1]-0.1, srt = 0, pos = 2, xpd = TRUE)
arrows(x.d.performance.combined, sub.d.performance.combined$stErr.UP, x.d.performance.combined,sub.d.performance.combined$stErr.DN, code=3, length=0.2, angle=90,col=c(rep('darkgray',2), rep('darkblue',2), rep('black',2)))
legend("bottomright", paste(rownames(sub.d.performance.combined), ": mean", round(sub.d.performance.combined$means, digits=2), '±',round(sub.d.performance.combined$stErr, digits=3)),ncol=3,text.width=1.5)
dev.off()





# names=data$method
# x = 1:13*2-1
# CI.up = as.numeric(data$mean)+as.numeric(data$ci)
# CI.dn = as.numeric(data$mean)-as.numeric(data$ci)
# plot(data$mean~x, cex=1.5,xaxt='n',ylim=c(0.3,0.40), xlab='',ylab='lalala!', main='blahblahblah',col='blue',pch=16)
# axis(1, at=x, labels=names)
# arrows(x,CI.dn,x,CI.up,code=3,length=0.2,angle=90,col='red')
# legend("bottomleft",paste(names,": S.E=",data$se),ncol=6,text.width=1)