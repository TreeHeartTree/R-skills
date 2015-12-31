#### Subset selection method:

library(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
library(leaps)
regfit.full <- regsubsets(Salary ~ ., data = Hitters)
reg.sum <- summary(regfit.full) # asterisk indicates inclusion of variables
regfit.full2 <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
summary(regfit.full2)
names(summary(regfit.full))
summary(regfit.full)$rsq 
par(mfrow = c(2,2))
plot(reg.sum$rss, type = "l")
plot(reg.sum$adjr2, type = "l")
max.point <- which.max(reg.sum$adjr2)
points(max.point, reg.sum$adjr2[max.point], col = "red", cex = 2, pch = 20)
plot(reg.sum$cp, type = "l")
min.point <- which.min(reg.sum$cp)
points(min.point, reg.sum$cp[min.point], col = "red", cex = 2, pch = 20)
plot(reg.sum$bic, type = "l")
points(which.min(reg.sum$bic), reg.sum$bic[which.min(reg.sum$bic)], col = "red", 
       cex = 2, pch = 20)
#?plot.regsubsets
#plot(regfit.full, scale = c("bic", "Cp", "adjr2", "r2"))

#### forward/backward stepwise selection
regfit.full3 <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
regfit.full4 <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.full3)
smr4 <- summary(regfit.full4)
smr4$bic
plot(smr4$bic, type = "l")
best.pred <- which.min(smr4$bic)
points(best.pred, smr4$bic[best.pred], col = "red", cex = 2, pch = 20)
text(best.pred, smr4$bic[best.pred] - 2, "best point")


