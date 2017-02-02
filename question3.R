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

