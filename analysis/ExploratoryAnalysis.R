setwd("C:/Users/adalton/Documents/Week6LiveSession/data/")
library(plyr)
bronx = read.csv("rollingsales_bronx.csv", skip=4)
## Check the data
head(bronx)
summary(bronx)
str(bronx) # Very handy function!

## clean/format the data with regular expressions
## More on these later. For now, know that the
## pattern "[^[:digit:]]" refers to members of the variable name that
## start with digits. We use the gsub command to replace them with a blank space.
# We create a new variable that is a "clean' version of sale.price.
# And sale.price.n is numeric, not a factor.
bronx$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", bronx$SALE.PRICE))

count(is.na(bronx$SALE.PRICE.N))

names(bronx) <- tolower(names(bronx)) # make all variable names lower case
## Get rid of leading digits
bronx
bronx$gross.square.feet
bronx$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bronx$gross.square.feet))
bronx$land.sqft <- as.numeric(gsub("[^[:digit:]]","", bronx$land.square.feet))
bronx$year.built <- as.numeric(as.character(bronx$year.built))

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bronx)
hist(sale.price.n) 
detach(bronx)

## keep only the actual sales

bronx.sale <- bronx[bronx$sale.price.n!=0,]
plot(bronx.sale$gross.sqft,bronx.sale$sale.price.n)
plot(log10(bronx.sale$gross.sqft),log10(bronx.sale$sale.price.n))

## for now, let's look at 1-, 2-, and 3-family homes
bronx.homes <- bronx.sale[which(grepl("FAMILY",bronx.sale$building.class.category)),]
dim(bronx.homes)
plot(log10(bronx.homes$gross.sqft),log10(bronx.homes$sale.price.n))
summary(bronx.homes[which(bronx.homes$sale.price.n<100000),])

## remove outliers that seem like they weren't actual sales
bronx.homes$outliers <- (log10(bronx.homes$sale.price.n) <=5) + 0
bronx.homes <- bronx.homes[which(bronx.homes$outliers==0),]
plot(log10(bronx.homes$gross.sqft),log10(bronx.homes$sale.price.n))
bronx.homes
summary(bronx)
fit <- aov(sale.price.n ~ neighborhood, data=bronx.homes)
head(bronx.homes, n=10)
summary(fit)

ddply(bronx.homes,~neighborhood,summarise,mean=mean(sale.price.n),sd=sd(sale.price.n))


attach(bronx.homes)
plot(land.sqft, sale.price.n, main="Correlation between square footage and sale price")