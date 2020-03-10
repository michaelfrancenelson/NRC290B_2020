source("data/environment_vars.R")

classData <- read.csv(here(chris_dat_dir, "classData16.csv"), h=T)
mander <- read.csv(here(chris_dat_dir, "mander.csv"), h=T)
bp <- read.table(here(chris_dat_dir, "bearpoop.txt"), h=T)

i = 1;
set.seed(12345)
nrow = 10
dat = data.frame(length = runif(n = nrow), width = rexp(nrow), mass = rpois(nrow, 34))

# Random Histograms ----
{
  {
    pdf(here(slide_img_dir, "randhist.pdf"), height=3, width=7)
    par(mfrow=c(1,2), oma=c(0,0,0,0), mar=c(4,4,1,1))
    set.seed(123)
    hist(rnorm(300,50,10), col="orange", xlab="Values", main="", las=1, ylim=c(0,70),xlim=c(10,90))
    box(bty="l")
    plot(0,0,type="n",axes=F,xlab="",ylab="")
    dev.off()
  }
  
  {
    pdf(here(slide_img_dir, "randhist2.pdf"), height=3, width=7)
    par(mfrow=c(1,2), oma=c(0,0,0,0), mar=c(4,4,1,1))
    set.seed(123)
    hist(rnorm(300,50,10), col="orange", xlab="Values", main="", las=1, ylim=c(0,70),xlim=c(10,90))
    box(bty="l")
    set.seed(123)
    hist(rnorm(300,50,10), col="orange", xlab="Values", main="", las=1, ylim=c(0,70),xlim=c(10,90),breaks=seq(10,90,2))
    box(bty="l")
    dev.off()
  }
  
  {
    pdf(here(slide_img_dir, "randhist3.pdf"), height=3, width=5)
    par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(4,4,1,1))
    set.seed(123)
    rnd <- rnorm(300,50,10)
    hist(rnd, col="orange", xlab="Values", main="", las=1, ylim=c(0,0.05), xlim=c(10,90), probability = TRUE)
    box(bty="l")
    lines(density(rnd),lwd=2,col=4)
    dev.off()
  }
}

# Unknown ----
{
  pdf(here(slide_img_dir, "bw.pdf"),height=3.5, width=3)
  par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,2,2))
  boxplot(rnd,las=1, pch="")
  dev.off()
  
  pdf(here(slide_img_dir, "four.pdf"),height=3.5, width=3)
  par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,2,2))
  boxplot(rnd,las=1, axes=FALSE, pch="")
  dev.off()
}

# Running average ----
{
  pdf(here(slide_img_dir, "runavg.pdf"), height=4, width=5)
  plot(bp$Poop, lwd=2,col=4,bty="l",las=1,ylab="Running Average", xlab="no. of samples", ylim=c(0,2), type="l")
  lines(bp$Bear, lwd=2,col=2,bty="l",las=1,ylab="Running Average", xlab="no. of samples")
  abline(h=mean(bp$Bear), lty=2, col=2)
  abline(h=mean(bp$Poop), lty=2, col=4)
  legend("bottomright", lwd=2,col=c(4,2),legend=c("Poop","bear"), bty="n")
  dev.off()
}


# Salamander plots ----
{
  
  # boxplot ---- 
  {
    pdf(here(slide_img_dir, "salbox.pdf"), height=4, width=5)
    boxplot(
      mander$SVL ~ mander$Sex, 
      col = adjustcolor(c(2, 3, 4), 0.2),
      xlab = "Sex", ylab = "SV length (mm)", 
      las = 1)
    dev.off()
  }
  
  # bar plot ---- 
  {
    pdf(here(slide_img_dir, "salbar.pdf"), height = 4, width = 5)
    bars <- tapply(mander$SVL,mander$Sex,mean)
    barplot(
      bars, 
      col = adjustcolor(c(2, 3, 4), 0.2), 
      xlab = "Sex", ylab = "SV length (mm)", 
      las = 1, ylim=c(0,50)
    )
    dev.off()
  }
  
  # scatterplot ----
  {
    pdf(here(slide_img_dir, "salscatter.pdf"), height=4, width=7)
    par(mfrow=c(1,2))
    plot(
      mander$SVL ~ mander$Total_length, 
      bg = adjustcolor(3,0.3), 
      ylab="SV length (mm)", 
      xlab="Total length (mm)", 
      las = 1, ylim = c(10,55), 
      bty = "l", pch = 21, cex = 1.2)
    plot(
      mander$SVL ~ jitter(as.numeric(mander$Date)+5,3), 
      bg = adjustcolor(4,0.3), 
      ylab = "SV length (mm)",
      xlab = "Temperature (C)",
      las = 1, ylim = c(10,55),
      bty = "l", pch = 21, cex = 1.2)
    dev.off()
  }
  
  # bar with SE ----
  {
    tmp <- c(0.7,1.9,3.1)
    pdf(here(slide_img_dir, "salbarse.pdf"), height=4, width=5)
    bars <- tapply(mander$SVL,mander$Sex,mean)
    ss <- c(tapply(mander$SVL,mander$Sex,sd))/c(sqrt(table(mander$Sex)))
    barplot(
      bars, 
      col=adjustcolor(c(2,3,4),0.2),
      ylab = "SV length (mm)", 
      las=1, ylim=c(0,50))
    arrows(c(tmp),bars-4.96*ss,c(tmp),bars+4.96*ss,lwd=2,length=0)
    dev.off()
  }
}

# Class plots ----
{
  pdf(here(slide_img_dir, "classpie.pdf"), height=4, width=4)
  par(mar=c(1,1,1,1),oma=c(0,0,0,0))
  pietab <- table(classData$Eyes)
  pie(pietab, labels = paste(names(pietab),"\n (",pietab,")",sep=""), col = adjustcolor(c(4,"turquoise","brown",3),0.6), main="Eye color")
  dev.off()
  
  pdf(here(slide_img_dir, "classbar.pdf"), height=4, width=7)
  bartab <- table(classData$Gender,classData$Eyes)
  barplot(bartab, col = adjustcolor(c(2,4),0.4), main="Eye color by Gender", las=1, ylab="Count", beside=TRUE, ylim=c(0,10),legend=rownames(bartab))
  dev.off()
}
