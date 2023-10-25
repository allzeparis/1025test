n <- 10
x <- c(1:n)
x
y <- factor(1:n,1:10)  
y

set.seed(12345)
a <- rnorm(5000)
b <- rchisq(5000,3)
A <- matrix(a,c(1000,5))
B <- matrix(b,c(1000,5))

C <- rbind(A,B)

par(mfrow = c(2,2))
hist(A[,1],xlab="A_1",ylab="密度",col ="yellow",main="A_1直方图")
hist(B[,1],xlab="B_1",ylab="密度",col="yellow",main="B_1直方图")

hist(A[,2],xlab="A_2",ylab="密度",col ="yellow",main="A_2直方图")
hist(B[,2],xlab="B_2",ylab="密度",col="yellow",main="B_2直方图")

hist(A[,3],xlab="A_3",ylab="密度",col ="yellow",main="A_3直方图")
hist(B[,3],xlab="B_3",ylab="密度",col="yellow",main="B_3直方图")

hist(A[,4],xlab="A_4",ylab="密度",col ="yellow",main="A_4直方图")
hist(B[,4],xlab="B_4",ylab="密度",col="yellow",main="B_4直方图")

hist(A[,5],xlab="A_5",ylab="密度",col ="yellow",main="A_5直方图")
hist(B[,5],xlab="B_5",ylab="密度",col="yellow",main="B_5直方图")

hist(C[,1],xlab="C_1",ylab="密度",col="orange",main="C_1直方图")
hist(C[,2],xlab="C_2",ylab="密度",col="orange",main="C_2直方图")
hist(C[,3],xlab="C_3",ylab="密度",col="orange",main="C_3直方图")
hist(C[,4],xlab="C_4",ylab="密度",col="orange",main="C_4直方图")
hist(C[,5],xlab="C_5",ylab="密度",col="orange",main="C_5直方图")
dev.off()


sigma <- (30000-20000)/qnorm(0.5+0.95/2)
num <- c(15000,18000,24000,28000)
p <- 1-pnorm((num-20000)/sigma)
p
money <- NULL
fun <- function(x){
  for (i in 1:4){
  if (x[i]<20000){
    S <- (24-15)*x[i]
  }else if (x[i] >= 20000){
    S <- (24-15)*20000 + (5-15)*(x[i]-20000)
  } 
  money<-c(money,S)}
  money}

fun(num)



##最佳方案是期望收益率的最大值,设进货量为y，需求量为x，求出利润函数的表达式，对y求导即可
##求出期望收益率的最大值，求出满足条件的分布函数F（y)值为8/19
y <- qnorm(8/19)*sigma + 20000
y1 <- round(y)

fx1 <- function(x) (19*x-11*y1)*(1/(sqrt(2*pi)*sigma))*exp(-(x-20000)^2/(2*sigma^2))
fx2 <- function(x) (1/(sqrt(2*pi)*sigma))*exp(-(x-20000)^2/(2*sigma^2))*8*y1
S1 <- integrate(fx1,-Inf,y1)
S2 <- integrate(fx2,y1,Inf)
S1
S2
y1
S <- S1$value+S2$value
S

