## Question 2

randomSample = beerdata[sample(nrow(beerdata), 2000), ]
beerCluster <- kmeans(randomSample[, c(5,6,9,10)], 5)

d = randomSample[, c(5,6,9,10)]
plotcluster(d, beerCluster$cluster)
with(beerdata, pairs(d, col=c(5,6,9,10)[beerCluster$cluster])) 