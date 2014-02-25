#####################################################################
######  						 Applied Linear Model  	                  ###### 
#####################################################################
library("alr3")
###############
#A data frame with 32 observations on 11 variables in mtcars
#[, 1]	 mpg	 Miles/(US) gallon
#[, 2]	 cyl	 Number of cylinders
#[, 3]	 disp	 Displacement (cu.in.)
#[, 4]	 hp	 Gross horsepower
#[, 5]	 drat	 Rear axle ratio
#[, 6]	 wt	 Weight (lb/1000)
#[, 7]	 qsec	 1/4 mile time
#[, 8]	 vs	 V/S
#[, 9]	 am	 Transmission (0 = automatic, 1 = manual)
#[,10]	 gear	 Number of forward gears
#[,11]	 carb	 Number of carburetors
######
###### Problem2 R code
scatterplotMatrix(mtcars[,c(1, 2, 3, 4, 5, 6, 7)], smooth=FALSE)
y <- mtcars$mpg
wt <- mtcars$wt
hp <- mtcars$hp
qsec <- mtcars$qsec
disp <- mtcars$disp
cyl <- mtcars$cyl
dc <- disp/cyl
m3 <- lm(y~wt+hp+qsec+dc, mtcars)
summary(m3)
anova(m3)
######
m4 <- lm(y~wt+hp+qsec, mtcars)
summary(m4)
anova(m4)
######
m5 <- lm(y~wt+hp, mtcars)
summary(m5)
anova(m5)

###### Problem 3.1 R code
library("alr3")
scatterplotMatrix(BGSgirls[, c(2, 3, 4, 5, 6, 7, 12)], smooth=FALSE)
print(cor(BGSgirls[, c(2, 3, 4, 5, 6, 7, 12)]), 3)

######
par(mfrow=c(2,2))

wt9 <- BGSall$WT9
lg9 <- BGSall$LG9
soma <-BGSall$Soma 

plot(wt9, soma)
fit1 <- lm(soma~wt9)
abline(fit1)

plot(lg9, soma)
fit2 <- lm(soma~lg9)
abline(fit2)

plot(wt9, lg9)
fit12 <- lm(lg9~wt9)
abline(fit12)

e12 <- fit12$residuals
e1 <- fit1$residuals
plot(e12, e1)
abline(h=0, lty="dashed")
abline(v=0, lty="dashed")
abline(lm(e1~e12))

######
m1 <- lm(Soma~HT2+WT2+HT9+WT9+ST9, BGSgirls)
summary(m1)

######
anova(m1)

######
m2 <- lm(Soma~ST9+WT9+HT9+WT2+HT2, BGSgirls)
anova(m2)
