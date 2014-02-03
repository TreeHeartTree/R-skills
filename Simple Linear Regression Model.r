#########################################
#########   Question 2.1      ###########
#########################################
#### data input
library("alr3")
ht <- htwt$Ht
wt <- htwt$Wt

#### 2.1.1 draw a scatterplot of ht and wt
plot(ht, wt)
fit <- lm(wt~ht)
abline(fit, col="blue")

#### 2.1.2 
xbar <- mean(ht)
ybar <- mean(wt)
sxx <- sum((ht-xbar)^2)
syy <- sum((wt-ybar)^2)
sxy <- sum((ht-xbar)*(wt-ybar))
c(xbar, ybar, sxx, syy, sxy)
## estimate the slope(b1) and intercept(b0)
b1 <- sxy/sxx
b0 <- ybar-b1*xbar
c(b0, b1)
abline(b0, b1, col="red")

#### 2.1.3 estimate RSS and theta
##
n <- length(ht)
rss <- syy - b1^2*sxx
theta <- rss/(n-2)
##
se.b1 <- sqrt(theta/sxx)
se.b0 <- sqrt(theta*(1/n+xbar^2/sxx))
cov.b0b1 <- -theta*xbar/sxx
c(theta, se.b0, se.b1, cov.b0b1)
## t-test
t1 <- b1/se.b1
t0 <- b0/se.b0
## p.t <- 2*(1-pt(c(t0, t1), 8))
c(t0, t1)

#### 2.1.4 do f-test
##
ssreg <- sxy^2/sxx
f <- ssreg/theta
p.f <- 1-pf(f, 1, 8)
c(ssreg, f, p.f)
## show f=sqr(t)
ts <- t1^2
c(f, ts)
##
anova(fit)

########################################
##########   Question 2.5    ###########
########################################
#####   2.5.1
head("wblake")
fish.len <- wblake$Length
fish.age <- wblake$Age
fit.wblake <- lm(fish.len~fish.age)
predict(fit.wblake, data.frame(fish.age=c(2, 4, 6)), interval="confidence",level=0.95)

#####	2.5.2
predict(fit.wblake, data.frame(fish.age=c(9)), interval="confidence",level=0.95)

#####	2.5.3
plot(fish.age, fish.len)
abline(fit.wblake, col="green")
lines(lowess(wblake), col="red")

########################################
##########		Question 2.13 	########
########################################
#####	2.13.1
cspd <- wm1$CSpd
rspd <- wm1$RSpd
plot(rspd, cspd)
fit.wm <- lm(cspd~rspd)
abline(fit.wm, col="red")

####	2.13.2
summary(fit.wm)

####	2.13.3
predict(fit.wm, data.frame(rspd=7.4285), interval="confidence", level=0.95)

####	2.13.5
se <- function(m, n, xstar){
	cspd <- wm1$CSpd
	rspd <- wm1$RSpd
	cspd.bar <- mean(cspd)
	rspd.bar <- mean(rspd)
	sxx <- sum((cspd-cspd.bar)^2)
	syy <- sum((rspd-rspd.bar)^2)
	sxy <- sum((cspd-cspd)*(rspd-rspd))
	rss <- syy-sxy^2/sxx
	theta <- rss/(n-2)
	var.m <- theta/m+theta*(1/n+(xstar-rspd.bar)^2/sxx)
	return (var.m)
}
se(62039, 1116, 7.4285)

