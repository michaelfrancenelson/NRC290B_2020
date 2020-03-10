{
  require(here)
}

{ # Site difference boxplot ----
  pdf(here("slides", "slide_images", "boxXY.pdf"), height = 3, width = 3)
  par(oma = c(0,0,0,0),mar = c(4,4,1,1))
  set.seed(123)
  Abundance <- rnorm(60,c(50,80),5)
  Site <- rep(c("A","B"),times = 30)
  boxplot(Abundance ~ Site, col = adjustcolor(c(3,4),0.5),las = 1)
  dev.off()
}



pdf(here("slides", "slide_images", "varbox.pdf"), height = 3, width = 6)
par(mfrow = c(1,2), oma = c(0,0,0,0),mar = c(4,4,2,2))
y2 <- rnorm(60,rep(c(50,100,70),each = 20),10)
y3 <- rnorm(60,c(70),10)

y2df <- data.frame(Total = y2,
                   A = c(y2[1:20],rep(NA,40)),
                   B = c(y2[21:40],rep(NA,40)),
                   C = c(y2[41:60],rep(NA,40)))
y3df <- data.frame(Total = y3,
                   A = c(y3[1:20],rep(NA,40)),
                   B = c(y3[21:40],rep(NA,40)),
                   C = c(y3[41:60],rep(NA,40)))

boxplot(y2df,las = 1,ylim = c(20,130),col = adjustcolor(c(1,2,3,4),0.4), pch = "", whisklty = 1)
abline(v = 1.5, lty = 2)
boxplot(y3df,las = 1,ylim = c(20,130),col = adjustcolor(c(1,2,3,4),0.4), pch = "", whisklty = 1)
abline(v = 1.5, lty = 2)
dev.off()

pdf(here("slides", "slide_images", "sst.pdf"), height = 3, width = 5)
par(mfrow = c(1,1), oma = c(0,0,0,0),mar = c(4,4,2,2))
plot(y2,type = "n",bty = "l",xlab = "", ylab = "Response",axes = F,ylim = c(20,130))
axis(2,las = 1)
box(bty = "l")
arrows(1:length(y2),mean(y2),1:length(y2),y2,length = 0,col = "grey")
abline(h = mean(y2),lwd = 2)
points(y2,pch = 16,col = rep(adjustcolor(c(2,3,4),0.5),each = 20),cex = 1.5)
dev.off()

pdf(here("slides", "slide_images", "ssw.pdf"), height = 3, width = 5)
par(mfrow = c(1,1), oma = c(0,0,0,0),mar = c(4,4,2,2))
plot(y2,type = "n",bty = "l",xlab = "", ylab = "Response",axes = F,ylim = c(20,130))
axis(2,las = 1)
box(bty = "l")
arrows(1:20,mean(y2[1:20]), 1:20,y2[1:20], length = 0,col = "grey")
arrows(21:40,mean(y2[21:40]),21:40,y2[21:40],length = 0,col = "grey")
arrows(41:60,mean(y2[41:60]),41:60,y2[41:60],length = 0,col = "grey")
arrows(c(1,21,41),c(mean(y2[1:20]),mean(y2[21:40]),mean(y2[41:60])),
       c(20,40,60),c(mean(y2[1:20]),mean(y2[21:40]),mean(y2[41:60])),
       lwd = 2,col = c(2,3,4),length = 0)
abline(h = mean(y2),lwd = 1)
points(y2,pch = 16,col = rep(adjustcolor(c(2,3,4),0.5),each = 20),cex = 1.5)
dev.off()

pdf(here("slides", "slide_images", "ssb.pdf"), height = 3, width = 5)
par(mfrow = c(1,1), oma = c(0,0,0,0),mar = c(4,4,2,2))
plot(y2,type = "n",bty = "l",xlab = "", ylab = "Response",axes = F,ylim = c(20,130))
axis(2,las = 1)
box(bty = "l")
arrows(1:20,mean(y2[1:20]), 1:20,mean(y2), length = 0,col = "grey")
arrows(21:40,mean(y2[21:40]),21:40,mean(y2),length = 0,col = "grey")
arrows(41:60,mean(y2[41:60]),41:60,mean(y2),length = 0,col = "grey")
arrows(c(1,21,41),c(mean(y2[1:20]),mean(y2[21:40]),mean(y2[41:60])),
       c(20,40,60),c(mean(y2[1:20]),mean(y2[21:40]),mean(y2[41:60])),
       lwd = 2,col = c(2,3,4),length = 0)
abline(h = mean(y2),lwd = 1)
points(y2,pch = 16,col = rep(adjustcolor(c(2,3,4),0.5),each = 20),cex = 1.5)
dev.off()


pdf(here("slides", "slide_images", "sstA.pdf"), height = 3, width = 5)
par(mfrow = c(1,1), oma = c(0,0,0,0),mar = c(4,4,2,2))
plot(y3,type = "n",bty = "l",xlab = "", ylab = "Response",axes = F,ylim = c(20,130))
axis(2,las = 1)
box(bty = "l")
arrows(1:length(y3),mean(y3),1:length(y3),y3,length = 0,col = "grey")
abline(h = mean(y3),lwd = 2)
points(y2,pch = 16,col = rep(adjustcolor(c(2,3,4),0.5),each = 20),cex = 1.5)
dev.off()

pdf(here("slides", "slide_images", "sswA.pdf"), height = 3, width = 5)
par(mfrow = c(1,1), oma = c(0,0,0,0),mar = c(4,4,2,2))
plot(y3,type = "n",bty = "l",xlab = "", ylab = "Response",axes = F,ylim = c(20,130))
axis(2,las = 1)
box(bty = "l")
arrows(1:20,mean(y3[1:20]), 1:20,y3[1:20], length = 0,col = "grey")
arrows(21:40,mean(y3[21:40]),21:40,y3[21:40],length = 0,col = "grey")
arrows(41:60,mean(y3[41:60]),41:60,y3[41:60],length = 0,col = "grey")
arrows(c(1,21,41),c(mean(y3[1:20]),mean(y3[21:40]),mean(y3[41:60])),
       c(20,40,60),c(mean(y3[1:20]),mean(y3[21:40]),mean(y3[41:60])),
       lwd = 2,col = c(2,3,4),length = 0)
abline(h = mean(y3),lwd = 1)
points(y3,pch = 16,col = rep(adjustcolor(c(2,3,4),0.5),each = 20),cex = 1.5)
dev.off()

pdf(here("slides", "slide_images", "ssbA.pdf"), height = 3, width = 5)
par(mfrow = c(1,1), oma = c(0,0,0,0),mar = c(4,4,2,2))
plot(y3,type = "n",bty = "l",xlab = "", ylab = "Response",axes = F,ylim = c(20,130))
axis(2,las = 1)
box(bty = "l")
arrows(1:20,mean(y3[1:20]), 1:20,mean(y3), length = 0,col = "grey")
arrows(21:40,mean(y3[21:40]),21:40,mean(y3),length = 0,col = "grey")
arrows(41:60,mean(y3[41:60]),41:60,mean(y3),length = 0,col = "grey")
arrows(c(1,21,41),c(mean(y3[1:20]),mean(y3[21:40]),mean(y3[41:60])),
       c(20,40,60),c(mean(y3[1:20]),mean(y3[21:40]),mean(y3[41:60])),
       lwd = 2,col = c(2,3,4),length = 0)
abline(h = mean(y3),lwd = 1)
points(y3,pch = 16,col = rep(adjustcolor(c(2,3,4),0.5),each = 20),cex = 1.5)
dev.off()


pdf(here("slides", "slide_images", "anovaNULL.pdf"), height=3,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(y2,type="n",bty="l",xlab="", ylab="Response",axes=F,ylim=c(20,130), main = "NULL")
axis(2,las=1)
box(bty="l")
arrows(1:length(y2),mean(y2),1:length(y2),y2,length = 0,col="grey")
abline(h=mean(y2),lwd=2)
points(y2,pch=16,col=rep(adjustcolor(1,0.5),each=20),cex=1.5)
dev.off()

pdf(here("slides", "slide_images", "anovaALT.pdf"), height=3,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(y2,type="n",bty="l",xlab="", ylab="Response",axes=F,ylim=c(20,130), main = "Alternative")
axis(2,las=1)
box(bty="l")
arrows(1:20,mean(y2[1:20]), 1:20,y2[1:20], length = 0,col="grey")
arrows(21:40,mean(y2[21:40]),21:40,y2[21:40],length = 0,col="grey")
arrows(41:60,mean(y2[41:60]),41:60,y2[41:60],length = 0,col="grey")
arrows(c(1,21,41),c(mean(y2[1:20]),mean(y2[21:40]),mean(y2[41:60])),
       c(20,40,60),c(mean(y2[1:20]),mean(y2[21:40]),mean(y2[41:60])),
       lwd=2,col=c(2,3,4),length=0)
points(y2,pch=16,col=rep(adjustcolor(c(2,3,4),0.5),each=20),cex=1.5)
dev.off()



pdf(here("slides", "slide_images", "sswC.pdf"), height=3,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(y2,type="n",bty="l",xlab="", ylab="Response",axes=F,ylim=c(20,130), main="Significant")
axis(2,las=1)
box(bty="l")
arrows(1:20,mean(y2[1:20]), 1:20,y2[1:20], length = 0,col="grey")
arrows(21:40,mean(y2[21:40]),21:40,y2[21:40],length = 0,col="grey")
arrows(41:60,mean(y2[41:60]),41:60,y2[41:60],length = 0,col="grey")
arrows(c(1,21,41),c(mean(y2[1:20]),mean(y2[21:40]),mean(y2[41:60])),
       c(20,40,60),c(mean(y2[1:20]),mean(y2[21:40]),mean(y2[41:60])),
       lwd=2,col=c(2,3,4),length=0)
abline(h=mean(y2),lwd=1)
points(y2,pch=16,col=rep(adjustcolor(c(2,3,4),0.5),each=20),cex=1.5)
dev.off()

pdf(here("slides", "slide_images", "sswD.pdf"), height=3,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(y3,type="n",bty="l",xlab="", ylab="Response",axes=F,ylim=c(20,130), main="Not significant")
axis(2,las=1)
box(bty="l")
arrows(1:20,mean(y3[1:20]), 1:20,y3[1:20], length = 0,col="grey")
arrows(21:40,mean(y3[21:40]),21:40,y3[21:40],length = 0,col="grey")
arrows(41:60,mean(y3[41:60]),41:60,y3[41:60],length = 0,col="grey")
arrows(c(1,21,41),c(mean(y3[1:20]),mean(y3[21:40]),mean(y3[41:60])),
       c(20,40,60),c(mean(y3[1:20]),mean(y3[21:40]),mean(y3[41:60])),
       lwd=2,col=c(2,3,4),length=0)
abline(h=mean(y3),lwd=1)
points(y3,pch=16,col=rep(adjustcolor(c(2,3,4),0.5),each=20),cex=1.5)
dev.off()

graze <- data.frame(graze= factor(rep(rep(c("Lo","Mid","Hi"),each=3),2),
                                  levels=c("Lo","Mid","Hi")),
                    Site = factor(rep(c("Top","Lower"),each=9), 
                                  levels=c("Lower","Top")),
                    Abundance = c(9,11,6,14,17,19,28,31,32,
                                  7,6,5,14,17,15,44,38,37))

pdf(here("slides", "slide_images", "twoway.pdf"), height=4.5,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
boxplot(graze$Abundance~graze$Site+graze$graze, axes=F, col=adjustcolor(c(2,4),0.3),ylab="Abundance")
legend("topleft",levels(graze$Site), pch=22,pt.bg=adjustcolor(c(2,4),0.3),bty="n", cex=1.2)
axis(1,at=c(1.5,3.5,5.5),labels=levels(graze$graze))
axis(2,las=1)
box(bty="l")
abline(v=c(2.5,4.5),col="grey",lty=2)
dev.off()

pdf(here("slides", "slide_images", "twowaypt.pdf"), height=4.5,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
xxx <- (rep(c(1,2,3,4,5,6),each=3) + c(-0.2,0,0.2))
plot(graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)] ~ xxx, 
     axes=F, col=adjustcolor(rep(rep(c(2,4),each=3),3),0.3),pch=16,cex=2,xlim=c(0.5,6.5),ylab="Abundance",xlab="")
legend("topleft",levels(graze$Site), pch=16,col=adjustcolor(c(2,4),0.3),bty="n", cex=1.2)
axis(1,at=c(1.5,3.5,5.5),labels=levels(graze$graze))
axis(2,las=1)
box(bty="l")
abline(v=c(2.5,4.5),col="grey",lty=2)
dev.off()

pdf(here("slides", "slide_images", "twowayptA.pdf"), height=4.5,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)] ~ xxx, 
     axes=F, col=adjustcolor(rep(c(2,3,4),each=6),0.3),pch=16,cex=2,xlim=c(0.5,6.5),ylab="Abundance",xlab="")
#legend("topleft",levels(graze$Site), pch=16,col=adjustcolor(c(2,3,4),0.3),bty="n", cex=1.2)
axis(1,at=c(1.5,3.5,5.5),labels=levels(graze$graze))
axis(2,las=1)
box(bty="l")
arrows(xxx[1:6],graze$Abundance[c(10:12,1:3)],
       xxx[1:6], mean(graze$Abundance[c(10:12,1:3)]),length=0,col=2)
arrows(xxx[7:12],graze$Abundance[c(13:15,4:6)],
       xxx[7:12], mean(graze$Abundance[c(13:15,4:6)]),length=0,col=3)
arrows(xxx[13:18],graze$Abundance[c(16:18,7:9)],
       xxx[13:18], mean(graze$Abundance[c(16:18,7:9)]),length=0,col=4)
arrows(c(xxx[c(1,7,13)]), 
       c(mean(graze$Abundance[c(10:12,1:3)]),
         mean(graze$Abundance[c(13:15,4:6)]),
         mean(graze$Abundance[c(16:18,7:9)])),
       c(xxx[c(6,12,18)]),
       c(mean(graze$Abundance[c(10:12,1:3)]),
         mean(graze$Abundance[c(13:15,4:6)]),
         mean(graze$Abundance[c(16:18,7:9)])),
       length=0,col=c(2,3,4))
dev.off()



pdf(here("slides", "slide_images", "twowayptB.pdf"), height=4.5,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)] ~ xxx, 
     axes=F, col=adjustcolor(rep(rep(c(2,4),each=3),3),0.3),pch=16,cex=2,xlim=c(0.5,6.5),ylab="Abundance",xlab="")
#legend("topleft",levels(graze$Site), pch=16,col=adjustcolor(c(2,3,4),0.3),bty="n", cex=1.2)
axis(1,at=c(1.5,3.5,5.5),labels=levels(graze$graze))
axis(2,las=1)
box(bty="l")
abline(h=c(mean(graze$Abundance[c(1:9)]),mean(graze$Abundance[c(10:18)])),col="grey90")

arrows(xxx[c(1:3,7:9,13:15)],   graze$Abundance[10:18],
       xxx[c(1:3,7:9,13:15)],   mean(graze$Abundance[10:18]),length=0,col=2)
arrows(xxx[c(4:6,10:12,16:18)], graze$Abundance[1:9],
       xxx[c(4:6,10:12,16:18)], mean(graze$Abundance[1:9]),length=0,col=4)


arrows(c(xxx[c(1,4,7,10,13,16)]), 
       c(mean(graze$Abundance[c(10:18)]),
         mean(graze$Abundance[c(1:9)]),
         mean(graze$Abundance[c(10:18)]),
         mean(graze$Abundance[c(1:9)]),
         mean(graze$Abundance[c(10:18)]),
         mean(graze$Abundance[c(1:9)])),
       c(xxx[c(3,6,9,12,15,18)]),
       c(mean(graze$Abundance[c(10:18)]),
         mean(graze$Abundance[c(1:9)]),
         mean(graze$Abundance[c(10:18)]),
         mean(graze$Abundance[c(1:9)]),
         mean(graze$Abundance[c(10:18)]),
         mean(graze$Abundance[c(1:9)])),
       length=0,col=c(2,4,2,4,2,4),lwd=2)
dev.off()

pdf(here("slides", "slide_images", "twowayptC.pdf"), height=4.5,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)] ~ xxx, 
     axes=F, col=adjustcolor(rep(rep(c(2,4),each=3),3),0.3),pch=16,cex=2,xlim=c(0.5,6.5),ylab="Abundance",xlab="")
#legend("topleft",levels(graze$Site), pch=16,col=adjustcolor(c(2,3,4),0.3),bty="n", cex=1.2)
axis(1,at=c(1.5,3.5,5.5),labels=levels(graze$graze))
axis(2,las=1)
box(bty="l")

arrows(xxx,   
       graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)],
       xxx,   
       rep(c(mean(graze$Abundance[10:12]),
             mean(graze$Abundance[1:3]),
             mean(graze$Abundance[13:15]),
             mean(graze$Abundance[4:6]),
             mean(graze$Abundance[16:18]),
             mean(graze$Abundance[7:9])),each=3),
       length=0,col=rep(c(2,4),each=3))

arrows(c(xxx[c(1,4,7,10,13,16)]), 
       c(mean(graze$Abundance[10:12]),
         mean(graze$Abundance[1:3]),
         mean(graze$Abundance[13:15]),
         mean(graze$Abundance[4:6]),
         mean(graze$Abundance[16:18]),
         mean(graze$Abundance[7:9])),
       c(xxx[c(3,6,9,12,15,18)]),
       c(mean(graze$Abundance[10:12]),
         mean(graze$Abundance[1:3]),
         mean(graze$Abundance[13:15]),
         mean(graze$Abundance[4:6]),
         mean(graze$Abundance[16:18]),
         mean(graze$Abundance[7:9])),
       length=0,col=c(2,4,2,4,2,4),lwd=2)
dev.off()


pdf(here("slides", "slide_images", "twowayptD.pdf"), height=4.5,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)] ~ xxx, 
     axes=F, col=adjustcolor(1,0.3),pch=16,cex=2,xlim=c(0.5,6.5),ylab="Abundance",xlab="")
#legend("topleft",levels(graze$Site), pch=16,col=adjustcolor(c(2,4),0.3),bty="n", cex=1.2)
axis(1,at=c(1.5,3.5,5.5),labels=levels(graze$graze))
axis(2,las=1)
box(bty="l")
arrows(xxx,graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)],
       xxx, mean(graze$Abundance),length=0,col="grey")
abline(h=mean(graze$Abundance))
dev.off()

pdf(here("slides", "slide_images", "twowayptD.pdf"), height=4.5,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
plot(graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)] ~ xxx, 
     axes=F, col=adjustcolor(1,0.3),pch=16,cex=2,xlim=c(0.5,6.5),ylab="Abundance",xlab="")
#legend("topleft",levels(graze$Site), pch=16,col=adjustcolor(c(2,4),0.3),bty="n", cex=1.2)
axis(1,at=c(1.5,3.5,5.5),labels=levels(graze$graze))
axis(2,las=1)
box(bty="l")
arrows(xxx,graze$Abundance[c(10:12,1:3,13:15,4:6,16:18,7:9)],
       xxx, mean(graze$Abundance),length=0,col="grey")
abline(h=mean(graze$Abundance))
dev.off()


pdf(here("slides", "slide_images", "FFF.pdf"), height=3,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
ff <- seq(0,10,0.1)
mat <- matrix(NA,nrow=length(ff),ncol=5)
mat[,1] <- df(ff,2,8)
mat[,2] <- df(ff,2,29)
mat[,3] <- df(ff,2,89)
mat[,4] <- df(ff,2,299)
mat[,5] <- df(ff,2,749)

plot(mat[,3]~ff, main="F-distribution", xlab="F statistic",
     ylab="Probability",lwd=2,col=4,bty="l",las=1,type="l")
abline(h=0.05,lty=2)
dev.off()


pdf(here("slides", "slide_images", "F.pdf"), height=3,width=4)
par(mfrow=c(1,1), oma=c(0,0,0,0),mar=c(4,4,2,2))
ff <- seq(0,10,0.1)
mat <- matrix(NA,nrow=length(ff),ncol=5)
mat[,1] <- df(ff,2,8)
mat[,2] <- df(ff,2,29)
mat[,3] <- df(ff,2,89)
mat[,4] <- df(ff,2,299)
mat[,5] <- df(ff,2,749)

plot(mat[,3]~ff, main="F-distribution (df = 2,89)", xlab="F statistic",
     ylab="Probability",lwd=2,col=4,bty="l",las=1,type="l")
abline(h=0.05,lty=2)
dev.off()