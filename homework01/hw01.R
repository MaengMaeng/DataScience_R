#data set 만드는 방법 1
#DAT1 <- rnorm(200, 2, 1)
#DAT2 <- rnorm(200, 1, 2)
#myDAT = data.frame(DAT1, DAT2)

#data 만드는 방법 2
n = 200
m = 200
DAT1 <- matrix(rnorm(400,2,1), n, m, dimnames = list(paste("x", 1 : n, sep=""),paste("DAT", 1 : m, sep="")))
DAT2 <- matrix(rnorm(400,-2,4), n, m, dimnames = list(paste("x", 1 : n, sep=""),paste("DAT", 1 : m, sep="")))
myDAT <- rbind(DAT1,DAT2)

k <- 5
myKC <- kmeans(myDAT, k)

plot(myDAT[, c("DAT1", "DAT200")], col = myKC$cluster)
points(myKC$centers[,c("DAT1", "DAT200")], col = 1:k, pch = 4, cex = 2, lwd = 3)

y <- matrix(rnorm(50), 10, 5, dimnames = list(paste("g", 1:10, sep=""), paste("t", 1:5, sep = "")))

c <- cor(t(y), method = "spearman")
d <- as.dist(1-c) 

hr <- hclust(d, method = "complete", members = NULL)

plot(as.dendrogram(hr), edgePar = list(col = 1, lwd = 1), horiz = F)