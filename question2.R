## Question 2

library(cluster)
library(fpc)
library(lattice)
library(mclust)

beerdata = data.table::fread("beer_reviews.csv")

#random sample of 2000 items
randomSample = beerdata[sample(nrow(beerdata), 2000), ]

d = randomSample[, c(5,6,9,10)]

#jitter
d.jitter = data.frame(lapply(d, jitter))

#k-means with 5 clusters
beerCluster <- kmeans(d, 5)

#show the centers
beerCluster$centers

#show the clusters
beerCluster$cluster

plotcluster(d.jitter, beerCluster$cluster)

with(beerdata, pairs(d.jitter, col=c(5,6,9,10)[beerCluster$cluster])) 

splom(d.jitter, col=c(5,6,9,10)[beerCluster$cluster])

#K-means clustering : 2 ways of doing it

#First way : evaluate the inertie
d.cr <- scale(d,center=T,scale=T)

inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(d.cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss }
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#Second way : usage of fpc package
sol.kmeans <- kmeansruns(d.cr,krange=2:10,criterion="ch")
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

#These two experiments both show that the optimal number of cluster is 2

#Stat calcul function
stat.comp <- function(x,y){
  #number of group
  K <- length(unique(y))
  #number of observations
  n <- length(x)
  #global mean
  m <- mean(x)
  #total variablility
  TSS <- sum((x-m)^2)
  #conditional effective
  nk <- table(y)
  #conditional mean
  mk <- tapply(x,y,mean)
  #explained variability
  BSS <- sum(nk * (mk - m)^2)
  #means plus explained variance
  result <- c(mk,100.0*BSS/TSS)
  #we name the elements of the vector
  names(result) <- c(paste("G",1:K),"% epl.")
  #we return the vector
  return(result) }

#we apply stat.comp to the variable d and not to the standardardized normal distribution
print(sapply(d,stat.comp,y=groupes.cah))

#Groups are mostly dominated by review_aroma