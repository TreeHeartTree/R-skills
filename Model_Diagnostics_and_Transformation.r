## 7.1
bl <- baeskel
y <- bl$Tension
x <- bl$Sulfur
plot(x, y)
abline(lm(y ~ x, data=bl))
lam <- c(-1, 0, 1)
new <- with(bl, seq(min(x), max(x), length=12))
with(bl, plot(x, y))
with(bl, 
     for (j in 1:3){
         m1 <- lm(y ~ bcPower(x, lam[j]))
         lines(new, predict(m1, data.frame(x=new)), lty=j, col=j, lwd=2)
     }
)
legend("topleft", inset=0.02, legend=as.character(lam), lty=1:3, col=1:3, xjust=1, yjust=1)

## 7.2.1
stopping
plot(Distance ~ Speed, stopping)
m1 <- lm(Distance ~ Speed, data=stopping)
abline(m1)
m2 <- lm(Distance ~ Speed + I(Speed^2), data=stopping)
lines(stopping$Speed[order(stopping$Speed)], predict(m2)[order(stopping$Speed)], col="red", lty="dashed" )
pureErrorAnova(m1)
anova(m2)

## 7.2.2
invResPlot(m1)

## 7.8
UN3
mc <- UN3$ModernC
cg <- UN3$Change
pp <- UN3$PPgdp
fr <- UN3$Frate
pop <- UN3$Pop
ft <- UN3$Fertility
pb <- UN3$Purban
scatterplotMatrix(UN3[, c(1:7)], smooth=FALSE)
summary(bc <- powerTransform(cbind(pp, fr, pop, ft, pb) ~ 1, data=UN3))
logpp <- log(pp)
logpop <- log(pop)
logft <- log(ft)
newUN <- cbind(mc, logpp, fr, logpop, logft, pb)
scatterplotMatrix(newUN[, c(1:6)], smooth=FALSE)

## 7.8.2
newUN <- data.frame(newUN)
m1 <- lm(ModernC ~ logb(PPgdp,2) + Frate + logb(Pop, 2) + logb(Fertility,2) + Purban, data=UN3)
invResPlot(m1)
boxCox(m1, xlab=expression(lambda[y]))

## 8.3
head(pipeline)
scatterplot(Lab ~ Field | Batch, data=pipeline, smoother=FALSE, reg.line=FALSE, legend.plot=FALSE)
abline(lm(Lab ~ Field, pipeline))
abline(0,1, lty="dashed")

## 8.3.2
m1 <- lm(Lab ~ Field, pipeline)
ncvTest(m1)
e <- m1$residuals
#Field <- pipeline$Field
#Batch <- pipeline$Batch
#pipeline2 <- cbind(residule, Field, Batch)
plot(pipeline$Field, e)
abline(h=0, lty="dashed")

## residualPlots(m1, quadratic=FALSE)

## 8.4
stopping
m1 <- lm(Distance ~ Speed + I(Speed^2), data=stopping)
ncvTest(m1)
ncvTest(m1, ~Speed)
ncvTest(m1, ~Speed + I(Speed^2))

## 8.10
logBSAAM <- log(water$BSAAM)
logAPMAM <- log(water$APMAM)
logAPSAB <- log(water$APSAB)
logAPSLAKE <- log(water$APSLAKE)
logOPBPC <- log(water$OPBPC)
logOPRC <- log(water$OPRC)
logOPSLAKE <- log(water$OPSLAKE)
new <- data.frame(cbind(logBSAAM, logAPMAM, logAPSAB, logAPSLAKE, logOPBPC, logOPRC, logOPSLAKE))
m1 <- lm(logBSAAM ~ logAPMAM + logAPSAB + logAPSLAKE + logOPBPC + logOPRC + logOPSLAKE, data=new)
e <- m1$residuals
par(mfrow=c(1,1))
plot(e ~ m1$fitted.values); abline(h=0, lty="dashed")
par(mfrow=c(2,3))
for ( i in 1:6){
    plot(e ~ new[, i], data=new)
    abline(h=0, lty="dashed")
}
mmps(m1, layout=c(2,2))

