### machine learning text ###
### https://bradleyboehmke.github.io/HOML/intro.html

library(dplyr)

library(datasets)
data(iris)
summary(iris)

#######################
#4.2 k-means clustering
#######################


i <- grep("Length", names(iris))
x <- iris[, i]
cl <- kmeans(x, 3, nstart = 10)
plot(x, col = cl$cluster)


dt2 <- iris %>%
  select(Sepal.Length, Petal.Length) #this is just how i would have built the data table


#4.2.1

#randomly assign a class membership
set.seed(12)
init <- sample(3, nrow(x), replace = TRUE)
plot(x, col = init)

par(mfrow = c(1, 2))
plot(x, col = init)
centres <- sapply(1:3, function(i) colMeans(x[init == i, ], ))
centres <- t(centres)
points(centres[, 1], centres[, 2], pch = 19, col = 1:3)

tmp <- dist(rbind(centres, x))
tmp <- as.matrix(tmp)[, 1:3]

ki <- apply(tmp, 1, which.min)
ki <- ki[-(1:3)]

plot(x, col = ki)
points(centres[, 1], centres[, 2], pch = 19, col = 1:3)

#######################
#4.2.3 
#######################

cl1 <- kmeans(x, 1, nstart = 10)
ss1 <- cl1$tot.withinss

cl2 <- kmeans(x, 2, nstart = 10)
ss2 <- cl2$tot.withinss

cl3 <- kmeans(x, 3, nstart = 10)
ss3 <- cl3$tot.withinss

cl4 <- kmeans(x, 4, nstart = 10)
ss4 <- cl4$tot.withinss

cl5 <- kmeans(x, 5, nstart = 10)
ss5 <- cl5$tot.withinss

ks <- 1:5
tot_within_ss <- sapply(ks, function(k) {
  cl <- kmeans(x, k, nstart = 10)
  cl$tot.withinss
})
plot(ks, tot_within_ss, type = "b")











#######################
#4.3 
#######################
