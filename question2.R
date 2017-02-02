## Question 2

#sous-échantillon aléatoire de 2000 observations
randomSample = beerdata[sample(nrow(beerdata), 2000), ]

d = randomSample[, c(5,6,9,10)]

#k-means avec 5 clusters
beerCluster <- kmeans(randomSample[, c(5,6,9,10)], 5)

#show the centers
beerCluster$centers

#show the clusters
beerCluster$cluster

plotcluster(d, beerCluster$cluster)
#show the centers
#points(beerCluster$centers, pch=2,col='red')

with(beerdata, pairs(d, col=c(5,6,9,10)[beerCluster$cluster])) 

#http://eric.univ-lyon2.fr/~ricco/cours/didacticiels/R/cah_kmeans_avec_r.pdf

#methode des centres mobiles : 2 methodes

#1ere methode : évaluer la proportion d'inertie expliquée
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(d.cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss }
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#2eme methode : largeur moyenne de silhouette - utilisation du package fpc
sol.kmeans <- kmeansruns(d.cr,krange=2:10,criterion="ch")
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

#Les deux methodes indiquent que le nombre optimal de clusters est 2