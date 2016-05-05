
brand.ratings <- read.csv("http://goo.gl/IQl8nc")

# Let's scale the attribute ratings so that data is more 
# comparable across individuals and samples. 
# For each attribute x, function scale does (x - mean(x))/sd(x)

attr_cols <- 1:9


brand.sc <- brand.ratings
brand.sc[ ,attr_cols] <- scale(brand.sc[,attr_cols])
summary(brand.sc)

# Analysis of a correlation plot reveals three likely clusters:
# {fun, latest, trendy}, {rebuy, bargain, value}, and 
# {perform, leader, serious}

library(corrplot)
corrplot(cor(brand.sc[,attr_cols]), order = "hclust")

# Simplest analysis - for each brand, what is the mean value 
# on each attribute? 

brand.mean <- aggregate(fo = . ~ brand, data = brand.sc, FUN = mean)


# This will allow us to answer which brand
# is the leader in each attribute category.

attributeLeader <- function(col, df) {
  index <- which(max(col) == col)
  attr_leader <- df$brand[index]
  return(attr_leader)
}

lapply(X = brand.mean[,2:10], 
       FUN = attributeLeader, 
       brand.mean)

# House cleaning - create brand row names
row.names(brand.mean) <- brand.mean$brand
brand.mean <- brand.mean[,-1]


library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean),
          col=brewer.pal(9,"GnBu"), trace = "none", key = FALSE, 
          dend = "none", main = "\n\n\n\n\nBrand Attribute")


# PCA example
# Create df w/ 3 partially correlated variables
#
set.seed(98286)
xvar <- sample(1:10, 100, replace = TRUE)
yvar <- xvar
yvar[sample(1:length(yvar), 50)] <- sample(1:10, 50, replace = TRUE)
zvar <- yvar
zvar[sample(1:length(zvar), 50)] <- sample(1:10, 50, replace = TRUE)
my.vars <- cbind(xvar, yvar, zvar)

# xvar is correlated w yvar
# zvar is correlated w yvar
# zvar is slightly correlated w xvar
plot(xvar ~ yvar, data = jitter(my.vars))
plot(zvar ~ yvar, data = jitter(my.vars))
plot(zvar ~ xvar, data = jitter(my.vars))
cor(my.vars)

# What components might we expect with this data?
# (1) There is shared variance across all 3 variables bc
# they are all positively correlated. Expect one component
# to pick up association across all three variables.
# (2) Expect component that shows xvar & zvar similar to yvar,
# but less similarity between xvar and zvar.
# (3) 

my.pca <- prcomp(my.vars)
summary(my.pca)
my.pca

# PCA components have no correlation
cor(my.pca$x)

# PCA can be visualized with `biplot`
# Every data point is plotted with arrows superimposed that
# show the best fit of each of the variables to the
# principle components (x-axis and y-axis). Useful to inspect
# the direction and angle of the arrows. Closer angle indicates
# higher positive association, while direction indicates pos/neg
# association of the variables.
biplot(my.pca)

# Let's examine the components of the brand data
brand.pc <- prcomp(brand.sc[,1:9])
summary(brand.pc)

# show the successive proportion of additional variance 
# that each PCA component adds
plot(brand.pc, type = "l")
biplot(brand.pc)

# Better solution is to perform PCA using aggregated
# ratings by brand

brand.mu.pc <- prcomp(brand.mean, scale = TRUE)
summary(brand.mu.pc)

biplot(brand.mu.pc, main = "Brand positioning")

# Now you can do brand comparisons
brand.mean["c",] - brand.mean["e",]


