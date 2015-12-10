Enter file contents herelibrary(ISLR)
set.seed(1)

attach(Auto)
names(Auto)
auto.size <- dim(Auto)
train <- sample(auto.size[1], 0.7*auto.size[1])

# linear approach
lm.fit1 <- lm(mpg ~ horsepower, data = Auto, subset = train)
lm.pred <- predict(lm.fit1, Auto)
mean(((mpg - lm.pred)[-train])^2)
# quadratic approach
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
lm.pred2 <- predict(lm.fit2, Auto)
mean(((mpg - lm.pred2)[-train])^2)
# cubic approach
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
lm.pred3 <- predict(lm.fit3, Auto)
mean(((mpg - lm.pred3)[-train])^2)

# LOOCV
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta # showing the test error is approximately 24.23
# repeat the analysis for different poly
cv.err <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.err[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.err
barplot(cv.err)

# k-fold CV
set.seed(2)
attach(Auto)
kcv.err <- rep(0, 10)
for (i in 1:10){
  kcv.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  kcv.err[i] <- cv.glm(Auto, kcv.fit, K = 10)$delta[1]
}
bplot <- barplot(kcv.err, axisnames = TRUE)
text(bplot, kcv.err, labels = round(kcv.err, 2))
