x <- runif(1000,min = -5, max = 5)

##?????????
e1 <- rnorm(1000)

##t???
e2 <- rt(1000,3)

##???????
e3 <- rchisq(1000,3)

y <-  2 + 10 * x + abs(x) * e1
lm.model <- lm(y~x)
summary(lm.model)
par(mfrow=c(2,2))
plot(lm.model)

y <- 2 + 10 * x + abs(x) * e2
lm.model <- lm(y~x)
summary(lm.model)
plot(lm.model)


y <- 2 + 10 * x + abs(x) * e3
lm.model <- lm(y~x)
summary(lm.model)

plot(lm.model)

rm(list = ls())
pextreme <- function(x) 1-exp(-exp(x))      
height <- pextreme(0)
height
cat("???????????????(0,",height,")",sep="")

pextreme1 <- function(x) 1-exp(-exp(x))-0.5
root <- uniroot(pextreme1,c(-2,0),tol=0.1)$root
root
cat("????????????????(",root,",0)",sep="")
