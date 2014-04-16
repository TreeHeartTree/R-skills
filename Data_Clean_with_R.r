
library('rattle')
ds <- get("weather")
vars <- names(ds)
target <- "rain_tomorrow"
risk <- "risk_mm"
id <- ("date", "location")
ignore <- union(id, if(exists("risk")) risk)
(ids <- which(sapply(ds, function(x) length(unique(x)))==nrow(ds)))
ignore <- union(ignore, ids)
## ignore variables with missing values
mvc <- sapply(ds[vars], function(x) sum(is.na(x)))
mvn <- names(which(mvc == nrow(ds)))
ignore <- union(ignore, mvn)
mvn <- names(which(mvc >= 0.7*nrow(ds)))
ignore <- union(ignore, mvn)
## ignore variables with too many levels
factors <- which(sapply(ds[vars], is.factor))
lvls <- sapply(factors, function(x) length(levels(ds[[x]])))
many <- names(which(lvls > 20))
ignore <- union(ignore, many)
## ignore variables with constant values
(constants <- names(which(sapply(ds[vars], function(x) all(x==x[1L])))))
ignore <- union(ignore, constants)
## remove the variables
length(vars)
vars <- setdiff(vars, ignore)
length(vars)
## remove missing target
dim(ds)
sum(is.na(ds[target]))
ds <- ds[!is.na(ds[target]), ]
sum(is.na(ds[target]))
dim(ds)
## deal with missing values
ods <- ds
dim(ds[vars])
sum(is.na(ds[vars]))
ds[vars] <- na.roughfix(ds[vars])
sum(is.na(ds[vars]))
dim(ds[vars])
ds <-ods
## omitting observations
ods <- ds
omit <- NULL
dim(ds[vars])
sum(is.na(ds[vars]))
mo <- attr(na.omit(ds[vars]), "na.action")
omit <- union(omit, mo)
if(length(omit)) ds <- ds[-omit,]
sum(is.na(ds[vars]))
dim(ds[vars])
ds <- ods
## normalise factors
factors <- which(sapply(ds[vars], is.factor))
for (f in factors) levels(ds[[f]]) <- normVarNames(levels(ds[[f]]))
## ensure target is categoric
ds[target] <- as.factor(ds[[target]])
table(ds[target])
p <- ggplot(ds, aes_string(x=target))
p <- p+geom_bar(width=0.2)
print(p)
