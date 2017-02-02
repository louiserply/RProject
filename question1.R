## Question 1

library(data.table)

beerdata = data.table::fread("beer_reviews.csv")

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