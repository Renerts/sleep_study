b.study=read.csv2("B_study_RT.csv")
means=colMeans(Data[c(3:14)],na.rm=TRUE)

plot(means,ylim=c(0,1400),xlab='conditions',pch=16) 











# names=data$method
# x = 1:13*2-1
# CI.up = as.numeric(data$mean)+as.numeric(data$ci)
# CI.dn = as.numeric(data$mean)-as.numeric(data$ci)
# plot(data$mean~x, cex=1.5,xaxt='n',ylim=c(0.3,0.40), xlab='',ylab='lalala!', main='blahblahblah',col='blue',pch=16)
# axis(1, at=x, labels=names)
# arrows(x,CI.dn,x,CI.up,code=3,length=0.2,angle=90,col='red')
# legend("bottomleft",paste(names,": S.E=",data$se),ncol=6,text.width=1)