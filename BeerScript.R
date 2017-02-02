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

### Top 10 for Palate:
aggregPalate = aggregate(beerdata$review_palate, by=list(beerdata$beer_name), FUN=mean)
top10Palate = head(aggregPalate[ order(-aggregPalate[,2], aggregPalate[,1]), ], 10)

### Top 10 for Appearance:
aggregAppearance = aggregate(beerdata$review_appearance, by=list(beerdata$beer_name), FUN=mean)
top10Appearance = head(aggregAppearance[ order(-aggregAppearance[,2], aggregAppearance[,1]), ], 10)

### Top 10 for Taste:
aggregTaste = aggregate(beerdata$review_taste, by=list(beerdata$beer_name), FUN=mean)
top10Taste = head(aggregTaste[ order(-aggregTaste[,2], aggregTaste[,1]), ], 10) 


## Question 2

#sous-échantillon aléatoire de 2000 observations
randomSample = beerdata[sample(nrow(beerdata), 2000), ]

d = randomSample[, c(5,6,9,10)]

#jitter
d.jitter = data.frame(lapply(d, jitter))

#k-means avec 5 clusters
beerCluster <- kmeans(d, 2)

#show the centers
beerCluster$centers

#show the clusters
beerCluster$cluster

plotcluster(d.jitter, beerCluster$cluster)
clusplot(d.jitter, beerCluster$cluster, color=TRUE, lines=0)

with(beerdata, pairs(d.jitter, col=c(5,6,9,10)[beerCluster$cluster])) 

splom(d.jitter, col=c(5,6,9,10)[beerCluster$cluster])



#methode des centres mobiles : 2 methodes

#1ere methode : évaluer la proportion d'inertie expliquée
d.cr <- scale(d,center=T,scale=T)

inertie.expl <- rep(0,times=10)
for (k in 2:10){
    clus <- kmeans(d.cr,centers=k,nstart=5)
    inertie.expl[k] <- clus$betweenss/clus$totss }
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#2eme methode : largeur moyenne de silhouette - utilisation du package fpc
sol.kmeans <- kmeansruns(d.cr,krange=2:10,criterion="ch")
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

#Les deux methodes indiquent que le nombre optimal de clusters est 2


#dendrogramme

#d.cr <- scale(d,center=T,scale=T)
#dendro.d <- dist(d.cr)
#cah.ward <- hclust(dendro.d,method="ward.D2")
#plot(cah.ward)

#rect.hclust(cah.ward,k=3)
#groupes.cah <- cutree(cah.ward,k=3)

#fonction de calcul des stats
stat.comp <- function(x,y){
  #nombre de groupes
  K <- length(unique(y))
  #nb. d'observations
  n <- length(x)
  #moyenne globale
  m <- mean(x)
  #variabilité totale
  TSS <- sum((x-m)^2)
  #effectifs conditionnels
  nk <- table(y)
  #moyennes conditionnelles
  mk <- tapply(x,y,mean)
  #variabilité expliqué
  BSS <- sum(nk * (mk - m)^2)
  #moyennes + prop. variance expliquée
  result <- c(mk,100.0*BSS/TSS)
  #nommer les élements du vecteur
  names(result) <- c(paste("G",1:K),"% epl.")
  #renvoyer le vecteur résultat
  return(result) }

#appliquer stat.comp aux variables de la base originelle d
#et non pas aux variables centrées et réduites 
print(sapply(d,stat.comp,y=groupes.cah))

#La définition des groupes est avant tout dominée par review_aroma

## question 3

d.matrix<- as(d,"realRatingMatrix")


