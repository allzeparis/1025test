f <- function(x){
  len <- length(x)
  mean <- mean(x)
  median <- median(x)
  sd <- sd(x)
  min <- min(x)
  max <- max(x)
  library(moments)
  skew <- skewness(x)
  kurt <- kurtosis(x)
  JB <- (len/6)*(skew^2+(kurt-3)^2/4)
  JB_pro <- 1-pchisq(JB,2)
  
  round(c(len=len,mean=mean,sd=sd,min=min,max=max,skew=skew,kurtosis=kurt,
          JB=JB,JB_pro=JB_pro),3)
}

setwd("D:\\Desktop\\R.demo\\Homework\\data_2")

library(readxl)
SCI <- read_excel("深证成份指数历史数据.xlsx")

SCI <- SCI[,c(1,2)]
names(SCI) <- c("date","close")
close.ptd.SCI <- SCI$close
close.rtd.SCI <- diff(log(SCI$close))*100

f(close.rtd.SCI)




  # 5)  Q-Q图、核密度图、
qqnorm(close.rtd.SCI,main="(a) 深证成指收益率Q-Q图",cex.main=0.95,
       xlab='理论分位数',ylab='样本分位数')            
qqline(close.rtd.SCI)                                 

D <-density(close.rtd.SCI)                          
plot(D, main="(c) 深证成指核密度曲线图 ",xlab="收益", ylab='密度',
     xlim = c(-7,7), ylim=c(0,0.5),cex.main=0.95)       
polygon(D, col="gray", border="black")                 
curve(dnorm,lty = 2, add = TRUE,col="blue")                        

x2 <- c(-7:7)
lines(x2,dnorm(x2,mean=0,sd=1),col="darkorange")      
abline(v=0,lty = 3)                                     
legend("topright", legend=c("核密度","正态密度"),lty=c(1,2),cex=0.5)



##CPI
CPI <- read_excel("近10年月度CPI数据.xlsx")
names(CPI) <- c("date","data")
f(CPI$data)

##绘制QQ图和核密度图
par(mfrow=c(2,1))
qqnorm(CPI$data-100,main="(a) CPI Q-Q图",xlab="理论分位数",ylab="样本分位数",
       cex.main=0.95)
qqline(CPI$data-100)

D1 <-density(CPI$data-100)                          
plot(D1, main="(b) CPI核密度曲线图 ",xlab="CPI", ylab='密度',
     xlim = c(-4,6), ylim=c(0,0.5),cex.main=0.95)       
polygon(D1, col="yellow", border="black")                 
curve(dnorm(x,mean = 1),-2,6,lty = 2, col="black",add = TRUE)                        

x2 <- c(-4:6)
lines(x2,dnorm(x2,mean=1,sd=sd(CPI$data)),col="blue")      
abline(v=1,lty = 3,col="blue")                                     
legend("topright", legend=c("核密度","正态密度"),lty=c(2,2),cex=0.5)




  ##第二题
library(readxl)
setwd("D:\\Desktop\\R.demo\\Homework\\data_2")

rm(list = ls())
SSCI <- read_excel(path="上&深&创业.xlsx",sheet="上综指")
SCI <- read_excel(path="上&深&创业.xlsx",sheet="深成指")
GEM <- read_excel(path="上&深&创业.xlsx",sheet="创业板指")

SSCI$Date <- as.Date(SSCI$Date)
SCI$Date <- as.Date(SCI$Date)
GEM$Date <- as.Date(GEM$Date)

head(SSCI)


close.SSCI <- diff(log(SSCI$Close))*100
close.SCI <- diff(log(SCI$Close))*100
close.GEM <- diff(log(GEM$Close))*100

close.SSCI <- data.frame(Date = SSCI$Date[-1],
                         earn = close.SSCI)
close.SCI <- data.frame(Date = SCI$Date[-1],
                        earn  = close.SCI)
close.GEM <- data.frame(Date = GEM$Date[-1],
                        earn = close.GEM)

close.SSCI.1 <- close.SSCI[which(weekdays(close.SSCI$Date) == "星期一"),2]
close.SSCI.4 <- close.SSCI[which(weekdays(close.SSCI$Date) == "星期四"),2]
close.SSCI.5 <- close.SSCI[which(weekdays(close.SSCI$Date) == "星期五"),2]
close.SCI.1 <- close.SCI[which(weekdays(close.SCI$Date) == "星期一"),2]
close.SCI.4 <- close.SCI[which(weekdays(close.SCI$Date) == "星期四"),2]
close.SCI.5 <- close.SCI[which(weekdays(close.SCI$Date) == "星期五"),2]
close.GEM.1 <- close.GEM[which(weekdays(close.GEM$Date) == "星期一"),2]
close.GEM.4 <- close.GEM[which(weekdays(close.GEM$Date) == "星期四"),2]
close.GEM.5 <- close.GEM[which(weekdays(close.GEM$Date) == "星期五"),2]

ave.SSCI.1 <- mean(na.omit(close.SSCI.1))
ave.SSCI.4 <- mean(na.omit(close.SSCI.4))
ave.SSCI.5 <- mean(na.omit(close.SSCI.5))
ave.SCI.1 <- mean(na.omit(close.SCI.1))
ave.SCI.4 <- mean(na.omit(close.SCI.4))
ave.SCI.5 <- mean(na.omit(close.SCI.5))
ave.GEM.1 <- mean(na.omit(close.GEM.1))
ave.GEM.4 <- mean(na.omit(close.GEM.4))
ave.GEM.5 <- mean(na.omit(close.GEM.5))

cat("上综指周一、四、五的期望收益率：",ave.SSCI.1,ave.SSCI.4,ave.SSCI.5,sep=" ")
cat("深成指周一、四、五的期望收益率：",ave.SCI.1,ave.SCI.4,ave.SCI.5,sep=" ")
cat("创业板指周一、四、五的期望收益率：",ave.GEM.1,ave.GEM.4,ave.GEM.5,sep=" ")
f(na.omit(close.SSCI.1))
f(na.omit(close.SSCI.4))
f(na.omit(close.SSCI.5))
f(na.omit(close.SCI.1))
f(na.omit(close.SCI.4))
f(na.omit(close.SCI.5))
f(na.omit(close.GEM.1))
f(na.omit(close.GEM.4))
f(na.omit(close.GEM.5))


earn <- close.SCI$earn
cat.mar <- NULL
bea.mar <- NULL
move.mar <- NULL

for (i in  1:583){
   if (sum(sign(earn[(5*i-4):(5*i)])==1) >= 4)
   {cat.mar <- c(cat.mar,i)}
    else if (sum(sign(earn[(5*i-4):(5*i)])==-1) >= 4)
    {bea.mar <- c(bea.mar,i)}
    else{
      move.mar <- c(move.mar,i)
    }
}
bear <- NULL
cattle <-NULL
for (i in cat.mar){
  temp <- close.SCI[(5*i-4):(5*i),]
  cattle <- rbind(cattle,temp)
}

for (i in bea.mar){
  temp <- close.SCI[(5*i-4):(5*i),]
  bear <- rbind(bear,temp)
}

move <- NULL
for (i in move.mar){
  temp <- close.SCI[(5*i-4):(5*i),]
  move <- rbind(move,temp)
}

cattle.1 <- cattle[weekdays(cattle$Date) == "星期一",]
cattle.4 <- cattle[weekdays(cattle$Date) == "星期四",]
cattle.5 <- cattle[weekdays(cattle$Date) == "星期五",]
f(cattle.1$earn)
f(cattle.4$earn)
f(cattle.5$earn)


bear.1 <- bear[weekdays(bear$Date) == "星期一",]
bear.4 <- bear[weekdays(bear$Date) == "星期四",]
bear.5 <- bear[weekdays(bear$Date) == "星期五",]
f(na.omit(bear.1$earn))
f(bear.4$earn)
f(bear.5$earn)



move.1 <- move[weekdays(move$Date) == "星期一",]
move.4 <- move[weekdays(move$Date) == "星期四",]
move.5 <- move[weekdays(move$Date) == "星期五",]
f(na.omit(move.1$earn))
f(na.omit(move.4$earn))
f(na.omit(move.5$earn))
