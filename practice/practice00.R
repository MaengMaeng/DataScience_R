data("iris")
newiris<-iris
newiris$Species <- NULL
kc <- kmeans(newiris, 3)

str(kc)
kc$centers
kc$size
kc$cluster

table(iris$Species, kc$cluster)

plot(newiris[, c("Sepal.Length", "Sepal.Width")], col = kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)
