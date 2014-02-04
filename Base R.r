#####################################################
#######			          	Practise			    	#########
#####################################################
## basic attributes in R
help(rank)
help.search("correlation")
library(alr3)
heights
head(heights)
dim(heights)
names(heights)
summary(heights)
heights$Mheight
## Graphical capabilities
hist(heights$Mheight, main="Histogram of Mother's height", xlab="Mother's height")
##
plot(heights$Dheight, heights$Mheight, main="Scatterplot of Mother/Daughter Height", ylab="Mother's Height", xlab="Daughter's height")
## subset the data by requesting certain rows or columns
## 10th row
heights[10, ]
## 2nd column
heights[,2]
## 2nd and 4th rows
heights[c(2,4), ]
## first 10 rows
heights[1:10, ]
## rows where Daughter's height is greater than 63
heights[heights$Dheight>63, ]


########################################################
#######			           		Questions		      		########
########################################################

### install and load the packages "alr3"
install.packages("alr3")
library("alr3")

### 1, how many observations and variables in the heights data
dim(heights)

### 2, the mean of the mother's height and maximum of daughter's height
summary(heights)

### 3, the correlation between mother's height and daughter's height
x <- heights$Mheight
y <- heights$Dheight
cor(x, y) ## in packages "stats"

### 4, the mean mothers's height for observations where daughter's height
###   greater than 63
yy <- heights[heights$Dheight>63, ]
yyM <- yy$Mheight
MyyM <- mean(yyM)
MyyM  ### the mean of the target vector
