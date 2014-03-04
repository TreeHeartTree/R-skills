##########################
# Linear regression in R #
##########################


# Create some data
x <- c(1,3,4,5,4,3,6,4,8,9,10,11,4,6,2,4,9,12,4)
n = length(x)
y <- 3 + 1.6 * x + + 0.5 * x^2 + rnorm(n,mean=0,sd=4)
plot(x,y)      #scatterplot

# Fit a linear regression model
fit1 <- lm(y~x)       # intercept is included automatically
summary(fit1)

plot(fit1)      # Some standard diagnostics for checking model assumptions

# Alternatively can do this by hand
fitted1 <- fit1$fitted      # fitted values y_hat
rstd1 <- rstandard(fit1)    # standardized residuals

# Residuals against fitted. Always look at this plot. Goal: See no pattern whatsoever
plot(fitted1,rstd1)                 # See the U-shaped pattern?
                                    # Suggests including a quadratic term

# Normality
qqnorm(rstd1)
qqline(rstd1)               # Normality looks OK




# Try a new model
x.sq = x^2
fit2 <- lm(y ~ x + x.sq)
summary(fit2)

fitted2 <- fit2$fitted      # fitted values y_hat
rstd2 <- rstandard(fit2)    # standardized residuals

plot(fitted2,rstd2)         # Now it looks OK. => Use model fit2, not fit1.
          
qqnorm(rstd2)
qqline(rstd2)               # OK




?lm

fit2$model                  # It doesn't show the first column that is constant all 1's. But it is used nevertheless (intercept).

fit2$coefficients           # The OLS estimate beta_hat
vcov(fit2)                  # The variance/covariance matrix of beta_hat





# Illustrate Near-collinearity
x3 <- 2*x + 5*x.sq + rnorm(n,mean=0,sd=0.01)

fit3 <- lm(y ~ x + x.sq + x3)
summary(fit3)               # The standard errors became huge now. Hard to identify the beta's.
plot(fit3)

fitted3 <- fit3$fitted      # fitted values y_hat
rstd3 <- rstandard(fit3)    # standardized residuals

plot(fitted3,rstd3)         #

qqnorm(rstd3)
qqline(rstd3)               # 




# How to detect near (multi-)collinearity?
# Check whether one of the Variance Inflation Factors (VIF) is > 10.  If yes => potential problem.

# install.packages("car")
library(car)                # need to install package 'car' first

vif(fit2)                   # Not great, but not tooo bad either
vif(fit3)                   # Catastrophic. Clearly, one of the variables has to go.




# Check numerical stability of calculations by hand


# Create model matrix X for model fit3
fit3$model                  # Doesn't include constant column with all ones. Have to add it by hand
const <- rep(1,n)
X3_ <- cbind(const,fit3$model[,2:4])
X3_                         # Can't do matrix opertions on this array. Have to convert to matrix.
X3 <- as.matrix(X3_)    
X3                          # That's it

# Easier alternative: X3 <- model.matrix(fit3)

beta_hat3_naive <- solve(t(X3) %*% X3) %*% t(X3) %*% y
t(beta_hat3_naive)
fit3$coef                   # Here the naive method doesn't create problems yet.

summary(fit3)
names(summary(fit3))
sigma3 <- summary(fit3)$sigma

V3_naive <- sigma3^2 * solve(t(X3) %*% X3)      
V3_naive
vcov(fit3)                  # Comparison shows that the naive method still works






# Let's make it worse
x4 <- 2*x + 5*x.sq + rnorm(n,mean=0,sd=0.00005)

fit4 <- lm(y ~ x + x.sq + x4)
summary(fit4)
vif(fit4)           # HUGE

X4 <- model.matrix(fit4)

beta_hat4_naive <- solve(t(X4) %*% X4) %*% t(X4) %*% y
t(beta_hat4_naive)
fit4$coef                   # Here the naive method gives a slightly different result. Numerical instability (rounding error).

sigma4 <- summary(fit4)$sigma
V4_naive <- sigma4^2 * solve(t(X4) %*% X4)      
V4_naive
vcov(fit4)                  # Comparison shows that the naive method start's getting into trouble







# Even worse
x5 <- 2*x + 5*x.sq + rnorm(n,mean=0,sd=0.000005)

fit5 <- lm(y ~ x + x.sq + x5)
summary(fit5)                 # Even R's QR decomposition can't deal with it any more.




