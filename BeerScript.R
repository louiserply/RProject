library(data.table)
library(fpc)
library(cluster)
library(mclust)
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(lattice)

beerdata = data.table::fread("beer_reviews.csv")

## Question 1

### Top10 for Aroma
aggregAroma = aggregate(beerdata$review_aroma, by=list(beerdata$beer_name), FUN=mean)
top10Aroma = head(aggregAroma[ order(-aggregAroma[,2], aggregAroma[,1]), ], 10)
top10Aroma

### Top 10 for Palate:
aggregPalate = aggregate(beerdata$review_palate, by=list(beerdata$beer_name), FUN=mean)
top10Palate = head(aggregPalate[ order(-aggregPalate[,2], aggregPalate[,1]), ], 10)
top10Palate

### Top 10 for Appearance:
aggregAppearance = aggregate(beerdata$review_appearance, by=list(beerdata$beer_name), FUN=mean)
top10Appearance = head(aggregAppearance[ order(-aggregAppearance[,2], aggregAppearance[,1]), ], 10)
top10Appearance

### Top 10 for Taste:
aggregTaste = aggregate(beerdata$review_taste, by=list(beerdata$beer_name), FUN=mean)
top10Taste = head(aggregTaste[ order(-aggregTaste[,2], aggregTaste[,1]), ], 10) 
top10Taste

## Question 2

#random sample of 2000 items
randomSample = beerdata[sample(nrow(beerdata), 2000), ]

d = randomSample[, c(5,6,9,10)]

#jitter
d.jitter = data.frame(lapply(d, jitter))

#k-means with 3 clusters
beerCluster <- kmeans(d, 3)

#show the centers
beerCluster$centers

#show the clusters
beerCluster$cluster

plotcluster(d.jitter, beerCluster$cluster)
clusplot(d.jitter, beerCluster$cluster, color=TRUE, lines=0)

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

#These two experiments both show that the optimal number of class is 2


#dendrogram

d.cr <- scale(d,center=T,scale=T)
dendro.d <- dist(d.cr)
cah.ward <- hclust(dendro.d,method="ward.D2")
plot(cah.ward)
groupes.cah <- cutree(cah.ward,k=3)

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

#Groups are mostly dominated by review_aroma, review_palate and review_appearance.


## Question 3

library(recommenderlab)
library(data.table)

beerdata = data.table::fread("beer_reviews.csv")

# Loading to pre-computed affinity data	
affinity.data<-beerdata[,c(4,11)]
affinity.matrix<- as(affinity.data,"realRatingMatrix")

# Creation of the model - U(ser) B(ased) C(ollaborative) F(iltering)
Rec.model<-Recommender(affinity.matrix, method = "UBCF")

recommended.items.1 <- predict(Rec.model, affinity.matrix["1",], n=10)

as(recommended.items.1, "list")





