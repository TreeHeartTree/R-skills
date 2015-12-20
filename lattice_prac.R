library(lattice)
attach(iris)
histogram(~Sepal.Length | Species, xlab = "", data = iris, layout = c(3, 1), 
          type = "density", main = "Lattice Histogram", sub = "Iris dataset")


qqmath(~Sepal.Length | Species, data = iris)
qqmath(~Sepal.Length | Species, data = iris, distribution = qunif)

bwplot(Species ~ Sepal.Length, data = iris)
bwplot(Species ~ Sepal.Length, data = iris,
       panel = panel.violin)


# text mining in r sample - sentiment analysis

