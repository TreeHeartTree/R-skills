library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[, -9])
attach(Smarket)
plot(Volume)
head(Smarket)
# Logistic regression model
lr.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = binomial)
summary(lr.fit)
coef(lr.fit)
summary(lr.fit)$coef[, 4] # p-values for the coef
glm.probs <- predict(lr.fit, type = "response") #predict the class using predict()
glm.probs[1:10]
contrasts(Direction)
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)
# split into train and test sets
train = (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
glm.fit2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 +Lag4 + Lag5 + Volume,
                data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit2, Smarket.2005, type = "response")
glm.pred <- rep("Down", dim(Smarket.2005)[1])
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) # 48% accurate
mean(glm.pred != Direction.2005) # 52% test error rate

# cutting back the predictors to only Lag 1 & 2
glm.fit3 <- glm(Direction ~ Lag1 + Lag2, data = Smarket,
                family = binomial, subset = train)
glm.probs3 <- predict(glm.fit3, Smarket.2005, type = "response") 
glm.pred3 <- rep("Down", dim(Smarket.2005)[1])
glm.pred3[glm.probs3 > 0.5] = "Up"
table(glm.pred3, Direction.2005)
mean(glm.pred3 == Direction.2005)
mean(glm.pred3 != Direction.2005)
# predict additional data points
predict(glm.fit3, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

# LDA / Naive Bayes 
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
summary(lda.fit)
'''
Prior probabilities of groups:
    Down       Up 
0.491984 0.508016 
'''
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005) # 56% accurate prediction

# QDA model
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket,
               family = binomial, subset = train)
qda.fit
'''
Prior probabilities of groups:
    Down       Up 
0.491984 0.508016 

Group means:
            Lag1        Lag2
Down  0.04279022  0.03389409
Up   -0.03954635 -0.03132544
'''
qda.pred <- predict(qda.fit, Smarket.2005)
qda.class <- qda.pred$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) #60% accurate prediction

# KNN classifier
library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.class <- Direction[train]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.class, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# Caravan insuarance application
attach(Caravan)
dim(Caravan)
summary(Purchase)
standardized.X <- scale(Caravan[, -86])
test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
mean(knn.pred == test.Y)
mean(knn.pred != test.Y)
table(knn.pred, test.Y)
'''
        test.Y
knn.pred  No Yes
     No  873  50
    Yes  68   9
'''

# Logistic regression on Caravan
glm.fit <- glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fit, Caravan[test, ], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)

glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, test.Y) # 33.3% accurate for "yes"
mean(glm.pred == test.Y) # 93% overall accuracy

############# Hypothesis testing by Coutney Brown
library(gmodels)
democracy <- read.table("democracy.txt", header = TRUE)
panel80 <- read.table("panel80.txt", header = TRUE)
usparty <- read.table("usparty.txt", header = TRUE)

names(panel80)
mynewdata <- panel80[which(panel80$VOTE <= 2), ]
mynewdata$incomecategories <- ifelse(mynewdata$INC < mean(mynewdata$INC, na.rm = TRUE), 1, 2)
CrossTable(mynewdata$SEX, mynewdata$VOTE, chisq = TRUE, expected = TRUE, format = "SAS")
CrossTable(mynewdata$incomecategories, mynewdata$VOTE, chisq = TRUE, expected = TRUE)

##
attach(mtcars)
library(class)
library(lattice)
with(mtcars, xyplot(wt ~ mpg, groups = cyl, auto.key = T, pch = 20, cex = 3))
train <- cbind(mpg, wt)
test <- c(26, 2.2)

knn(train, test, cl = cyl, k = 2, prob = T)





