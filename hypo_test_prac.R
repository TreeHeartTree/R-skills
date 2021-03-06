library(MASS)
set.seed(1)
y <- sample(60, 100, replace = TRUE)
y2 <- y
y2[50] <- 1000
par(mfrow=(c(2, 2)))
plot(y)
boxplot(y)
hist(y, main = "")
plot(y2)
# summary data
summary(y)
fivenum(y)
fivenumber <- function(X){
  x <- sort(X)
  n <- length(X)
  five <- c(1, 0.5*floor(0.5*(n + 3)), 0.5*(n + 1),
            n + 1 - 0.5*floor(0.5 * (n + 3)), n)
  return (x[five])
}
fivenumber(y)

# plots for testing normality
qqnorm(y)
qqline(y, lty = 2)
qqnorm(y2)
qqline(y2, lty = 2)

# test for normality - shapiro test
x <- exp(rnorm(40))
shapiro.test(x)

library(datasets)
attach(morley)
names(morley)
hist(Speed)
summary(Speed)

shapiro.test(Speed) # non-normality with p-value > 0.05
wilcox.test(Speed, mu = 880) # deal with non-normality

k <- rep(0, 10000) # bootstrap method
for (i in 1:10000) {
  k[i] <- mean(sample(Speed, replace = TRUE))
}
hist(k) 
shapiro.test(sample(k, 4999)) # it is normal dist now

# skewness
skew <- function(x) {
  m3 <- sum((x - mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  return (m3/s3)
}
skew(y)
# kurtosis
kurtosis <- function(x) {
  m4 <- sum((x - mean(x))^4)/length(x)
  s4 <- sqrt(var(x))^4
  return (m4/s4 - 3)
}
kurtosis(y)

# two samples test - variance test
fisher.ftest <- function(x, y) {
  df.x <- length(x) - 1
  df.y <- length(y) - 1
  var.x <- var(x)
  var.y <- var(y)
  f.ratio <- max(var.x, var.y)/min(var.x, var.y)
  return (round(2 * (1 - pf(f.ratio, max(df.x, df.y), min(df.x, df.y))), 4))
}
set.seed(2)
x1 <- sample(1000, 50, replace = T)
x2 <- sample(1000, 50, replace = F)
X <- c(x1, x2)
f <- factor(rep(c("a", "b"), c(50, 50)))

var.test(x1, x2) # fisher f test (var1 = var2?)
bartlett.test(X ~ f) # bartlett test (var1 = var2 = var3 ...?)
fligner.test(X ~ f) # fligner test (var1 = var2 = var3 ...?)
