
# bootstrap
#library(boot)
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return ((var(Y)-cov(X, Y))/(var(X) + var(Y) - 2*cov(X, Y)))
}
alpha.fn(Portfolio, 1:100)
set.seed(3)
alpha.fn(Portfolio, sample(100, 100, replace = TRUE))
boot(Portfolio, alpha.fn, R = 1000)

boot.func <- function(data, index){
  lm.fit <- lm(mpg ~ horsepower, data = data, subset = index)
  return (coef(lm.fit))
}
boot(Auto, boot.func, R = 1000)
plot(boot(Auto, boot.func, R = 1000))
boot.ci(boot(Auto, boot.func, R = 1000), type = "bca", index = 2)

boot.func2 <- function(data, index){
  lm.fit <- lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index)
  return (coefficients(lm.fit))
}
bsp <- boot(Auto, boot.func2, R = 1000)
boot.ci(bsp, type = "bca", index = 1)
boot.ci(bsp, type = "bca", index = 2)
boot.ci(bsp, type = "bca", index = 3)
plot(bsp, index = 2)
plot(bsp, index = 3)
