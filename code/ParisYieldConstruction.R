# Code to estimate house price and rent price index for Paris

# change working directory to the directory of the script
try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

library(dplyr)
library(raster)
library(data.table)

options(scipen=999)

# load data
data <- read.csv("../data/DataParisAnalysis.csv")
data$X <- NULL

# add ERPI for each observation to the data
tseries1 <- read.csv("../data/TimeSeriesParisAnalysis.csv")
tseries <- tseries1[,c("Year", "RPItot","HPI")] 
tseries$RPI <- tseries$RPItot
tseries$YieldPrior <- tseries$RPI/tseries$HPI # this is to be tweaked
tseries$YieldPrior <- tseries$YieldPrior/tseries$YieldPrior[tseries$Year==1884]*0.07
data$RPI <- NULL
data$HPI <- NULL
data$YieldPrior <- NULL
data <- merge(data, tseries, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
data <- data[order(data$PropID, data$Year, data$Month, data$Day),]
# data <- data[,c(2:18,1,19,20,21,22,23,24)]

data <- subset(data, data$Price > 0 & data$Year > 1805 & data$Year < 1944)
data$TransID2 <- 1

data$Day[is.na(data$Day)] <- 15
data$Month[is.na(data$Month)] <- 7

# Create Trans IDs
for (i in 2:nrow(data)) {
  if (data$PropID[i-1] == data$PropID[i]) {
      data$TransID2[i] <- data$TransID2[i-1] + 1
  }
  else data$TransID2[i] <- 1
}


### STEP 1: IDENTIFY CAPITALIZED RENT OBSERVATIONS 
data$Count <- NA

for (i in (nrow(data)-1):1) {
  if (data$PropID[i+1] != data$PropID[i]) {
    data$Count[i] <- data$TransID2[i]
  }
  else data$Count[i] <- data$Count[i+1]
}

# construct matrices to store, for each succession the estimated yields
YieldMatrix <- matrix(NA, nrow=nrow(data), ncol=max(data$TransID2))

# find all rents and prices on the same property, and compute the yield for rent observations
for (i in 1:nrow(data)) {
  if (data$Count[i] > 1 & (data$SaleType[i] == "succession" | data$SaleType[i] == "donation")) {
    for (j in 1:data$Count[i]) {
      if ((data$SaleType[i-data$TransID2[i]+j] == "vente" | data$SaleType[i-data$TransID2[i]+j] == "adjudication" | data$SaleType[i-data$TransID2[i]+j] == "licitation" | data$SaleType[i-data$TransID2[i]+j] == "liquidation") & data$Year[i] - data$Year[i-data$TransID2[i]+j] > -30 & data$Year[i] - data$Year[i-data$TransID2[i]+j] < 30 ) {
        YieldMatrix[i,j] <- log(data$Price[i]/(data$Price[i-data$TransID2[i]+j]/data$HPI[i-data$TransID2[i]+j]*data$HPI[i])) - log(data$YieldPrior[i]) # this still needs to be deflated with the prior yield for each observation
      }
    }
  }
}

# Bring values to the front of matrix
YieldMatrixT <- as.data.frame(t(apply(YieldMatrix,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
YieldMatrixT <- YieldMatrixT[,1:21]
YieldCount <- rowSums(!is.na(YieldMatrixT)) # number of comparison yields
YieldMean <- rowMeans(YieldMatrixT, na.rm=TRUE) # mean implied yield per observations
YieldMean[is.na(YieldMean)] <- 0
       
# construct matrices to store, for each succession the estimated log rent differences   
RentMatrix <- matrix(NA, nrow=nrow(data), ncol=50)

# find all rents on the same property and compute the log price differences
for (i in 1:nrow(data)) {
  if (data$Count[i] > 1 & (data$SaleType[i] == "succession" | data$SaleType[i] == "donation")) {
    for (j in 1:data$Count[i]) {
      if (data$SaleType[i-data$TransID2[i]+j] == "bail" & data$Year[i] - data$Year[i-data$TransID2[i]+j] > -30 & data$Year[i] - data$Year[i-data$TransID2[i]+j] < 30) {
        RentMatrix[i,j] <- log(data$Price[i]) - log(data$Price[i-data$TransID2[i]+j]/data$RPI[i-data$TransID2[i]+j]*data$RPI[i])
      }
    }
  }
}

# YieldMinT <- apply(YieldmatrixT, 1, function(x) max[x[x<=0]])
RentMatrixT <- as.data.frame(t(apply(RentMatrix,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
RentMatrixT <- RentMatrixT[,1:13]
RentMatrixCount <- rowSums(!is.na(RentMatrixT))
RentMean <- rowMeans(RentMatrixT, na.rm=TRUE)
RentMean[is.na(RentMean)] <- 0
Total <- RentMatrixCount + YieldCount

# computed per succession a score:
Score <- (RentMean*RentMatrixCount + YieldMean*YieldCount)/(Total+0.00001)
Score <- data.frame(Score, Total)
hist(Score$Score[Score$Total != 0], breaks=80) # histogram of scores

# add score to data frame
data$Score <- Score$Score
data$ScoreCount <- Score$Total
data$Score[data$ScoreCount == 0] <- NA

# histogram of scores by time period
hist(data$Score[data$Year > 1814 & data$Year < 1820], breaks = 80)
hist(data$Score[data$Year > 1819 & data$Year < 1825], breaks = 80)
hist(data$Score[data$Year > 1824 & data$Year < 1830], breaks = 80)
hist(data$Score[data$Year > 1829 & data$Year < 1835], breaks = 80)
hist(data$Score[data$Year > 1834 & data$Year < 1840], breaks = 50, xlab = "Score", main=NA) # plot in main paper
hist(data$Score[data$Year > 1839 & data$Year < 1845], breaks = 80)
hist(data$Score[data$Year > 1844 & data$Year < 1850], breaks = 80)
hist(data$Score[data$Year > 1849 & data$Year < 1855], breaks = 80)
hist(data$Score[data$Year > 1854 & data$Year < 1860], breaks = 80)
hist(data$Score[data$Year > 1859 & data$Year < 1870], breaks = 80)
hist(data$Score[data$Year > 1869 & data$Year < 1880], breaks = 80)
hist(data$Score[data$Year > 1879 & data$Year < 1890], breaks = 80)
hist(data$Score[data$Year > 1889 & data$Year < 1900], breaks = 80)
hist(data$Score[data$Year > 1899 & data$Year < 1905], breaks = 80)
hist(data$Score[data$Year > 1904 & data$Year < 1910], breaks = 80)
hist(data$Score[data$Year > 1909 & data$Year < 1914], breaks = 80)
hist(data$Score[data$Year > 1913 & data$Year < 1916], breaks = 80)
hist(data$Score[data$Year > 1915 & data$Year < 1919], breaks = 80)
hist(data$Score[data$Year > 1918 & data$Year < 1930], breaks = 80)
hist(data$Score[data$Year > 1929 & data$Year < 1944], breaks = 80)

#### Apply adjustments to capitalized rents, based on info from histograms
data$PriceMod <- data$Price

# apply -1, 1 before 1830 as limit, and divide range 1.25-4.75 by 20 
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year < 1830 & !is.na(data$Score) & (data$Score > 4.75 | data$Score < -1 | (data$Score > 1 & data$Score <= 1.25))] <- NA
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year < 1830 & !is.na(data$Score) & data$Score > 1.25 & data$Score < 4.75] <- data$Price[(data$SaleType == "succession" | data$SaleType == "donation") & !is.na(data$Score) & data$Year < 1830 & data$Score > 1.25 & data$Score < 4.75]/20

# apply -1.25, 1.25 for 1830-1834 as limit, and divide range 1.75-4.25 by 20 
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year >= 1830 & data$Year <= 1834 & !is.na(data$Score) & (data$Score > 4.25 | data$Score < -1.25 | (data$Score > 1.25 & data$Score <= 1.75))] <- NA
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year >= 1830 & data$Year <= 1834 & !is.na(data$Score) & data$Score > 1.75 & data$Score < 4.25] <- data$Price[(data$SaleType == "succession" | data$SaleType == "donation") & !is.na(data$Score) & data$Year >= 1830 & data$Year <= 1834 & data$Score > 1.75 & data$Score < 4.25]/20

# apply -1.75, 1.75 for 1835-1859 as limit, and divide range 2-4 by 20 
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year >= 1835 & data$Year <= 1917 & !is.na(data$Score) & (data$Score > 4 | data$Score < -1.75 | (data$Score > 1.75 & data$Score <= 2))] <- NA
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year >= 1835 & data$Year <= 1917 & !is.na(data$Score) & data$Score > 2 & data$Score < 4] <- data$Price[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year >= 1835 & data$Year <= 1917 & !is.na(data$Score) & data$Score > 2 & data$Score < 4]/20

# apply -1.75, 1.75 for 1918 as limit, and divide range 2-4 by 20 
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year == 1918 & data$Month <= 5 & !is.na(data$Score) & (data$Score > 4 | data$Score < -1.75 | (data$Score > 1.75 & data$Score <= 2))] <- NA
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year == 1918 & data$Month <= 5 & !is.na(data$Score) & data$Score > 2 & data$Score < 4] <- data$Price[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year == 1918 & data$Month <= 5 & !is.na(data$Score) & data$Score > 2 & data$Score < 4]/20

# no limit from 1918
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year == 1918 & data$Month > 5 & !is.na(data$Score) & (data$Score > 4 | data$Score < -1.75 | (data$Score > 1.75 & data$Score <= 2))] <- NA
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year == 1918 & data$Month > 5 & !is.na(data$Score) & data$Score > 2 & data$Score < 4] <- data$Price[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year == 1918 & data$Month > 5 & !is.na(data$Score) & data$Score > 2 & data$Score < 4]/12
data$PriceMod[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year > 1918] <- data$Price[(data$SaleType == "succession" | data$SaleType == "donation") & data$Year > 1918]/12

# rewrite file with updated rent prices
write.csv(data, file="../data/DataParisAnalysis.csv")

# count removed observations
table(!is.na(data$PriceMod))

# next, use the adjusted succession rates to compute for each sales price the implied yield
# to do so, we take the two nearest rent prices on these observations on the left and the right side
# take a weighted average of them; this is the rent to be used in the yield series.

# load data for taxes (only to compute tax rate for 1855 - 1926)
# data <- read.csv("../data/ParisTax18551926.csv")

# step 1: find for each sale the nearest rent observations
#rents.all <- subset(rents, SaleType=="Bail")
nearest.rent <- function(x, rents.data, x.out, max.distance.years=NA ){
  id <- as.numeric( x["PropID"])
  year <- as.numeric( x["Year"] )

  rents <- subset(rents.data, PropID == id)
  
  # drop observations that are from distant years,
  # threshold defined in max.distance.years (default: take all)
  if(!is.na(max.distance.years)){
    rents <- subset(rents, (Year - year)^2 <= (max.distance.years^2))
  }
  
  # rebase rents, using market wide index
  rent.index.base <- as.numeric( x["RPI"] )
  rents$PriceMod.rebased <- (rents$PriceMod/rents$RPI)*rent.index.base
  
  # in case there are multiple rent observations per year, take annual average
  rents.av <- tapply(rents$PriceMod.rebased, rents$Year, mean, na.rm=TRUE)
  
  if(length(rents.av) > 1){
    # In case there are two or more rent observations, interpolate linearly.
    # In cases where interpolation is not possible, don't extrapolate but simply pick nearest value
    rent.interpolated <- as.data.frame( approx(as.numeric(names(rents.av)), rents.av, xout=x.out ) )
    colnames(rent.interpolated) <- c("year","rent.interpolated")
    rent.interpolated <- subset(rent.interpolated, !is.na(rent.interpolated))
    rent.interpolated$distance <- (rent.interpolated$year - year)^2
    rent.interpolated <- rent.interpolated[order(rent.interpolated$distance),]
    return(c(id, year, rent.interpolated[1,"rent.interpolated"]))
  } else if( length(rents.av) == 1){
    # In case there is only one rent observation, return just that one
    return(c(id, year, rents.av))
  }
  else {
    return(c(id, year, NA))
  }
}
# Subsample of rents and range of relevant years; for "bail" robustness check remove succession and donation observations
rents.data <- subset(data, !is.na(PriceMod) & ( SaleType == "bail" | SaleType == "succession" | SaleType == "donation" ))
x.out <- min(data$Year):max(data$Year)
# rent, interpolated with no max on year difference
rent.interpolated <- as.data.frame( t( apply( unique( subset(data, select=c("Year","PropID","RPI"))), 1, nearest.rent, rents.data = rents.data, x.out=x.out)))
colnames(rent.interpolated) <- c("PropID","Year","rent.interpolated")
data$rent.interpolated <- NULL
data <- merge(data, rent.interpolated, by=c("PropID","Year"), all.x=TRUE)
# using rental information within 10 years only
rent.interpolated.10 <- as.data.frame( t( apply( unique( subset(data, select=c("Year","PropID","RPI"))), 1, nearest.rent, rents.data = rents.data, x.out=x.out, max.distance.years=10 )))
colnames(rent.interpolated.10) <- c("PropID","Year","rent.interpolated.10")
data$rent.interpolated.10 <- NULL
data <- merge(data, rent.interpolated.10, by=c("PropID","Year"), all.x=TRUE)
# using rental information within 30 years only
rent.interpolated.30 <- as.data.frame( t( apply( unique( subset(data, select=c("Year","PropID","RPI"))), 1, nearest.rent, rents.data = rents.data, x.out=x.out, max.distance.years=30 )))
colnames(rent.interpolated.30) <- c("PropID","Year","rent.interpolated.30")
data$rent.interpolated.30 <- NULL
data <- merge(data, rent.interpolated.30, by=c("PropID","Year"), all.x=TRUE)

# Construct Tax Rate Series (requires loading the Tax Data in line 163, and not use the regular data)
 datat <- subset(data, SaleType == "taxes" & !is.na(rent.interpolated.30) & PriceMod > 0 & Year != 1864 & Year != 1876 & Year != 1897)
 datat$rate <- datat$PriceMod/datat$rent.interpolated.30
 mediantax <- aggregate(datat$rate, by=list(datat$Year), median)
 mediantax$count <- table(datat$Year)
# write.csv(mediantax, file="medtaxrate.csv")

# Construct Yield series; modify for robustness checks 
# datas <- subset(data, (SaleType == "vente" | SaleType == "adjudication" | SaleType == "licitation" | SaleType == "liquidation"))
datay <- subset(data, (SaleType == "vente" | SaleType == "adjudication" | SaleType == "licitation" | SaleType == "liquidation") & !is.na(rent.interpolated.10) & Year > 1805 & Year < 1944)
datay$yield <- datay$rent.interpolated.30 / datay$Price

# export yields, for maps of Paris
# write.csv(datay, file="../data/yields.paris.csv", row.names=FALSE)

# Compute Initial Yield Series
median <- aggregate(datay$yield, by=list(datay$Year), median)
medianprice <- aggregate(datay$PriceMod, by=list(datay$Year), median)
colnames(median) <- c("Year", "MedianYieldInit")
colnames(medianprice) <- c("Year", "MedianPriceInit")
datay <- merge(datay, median, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
datay <- merge(datay, medianprice, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")

datay$yieldev <- log(datay$yield) - log(datay$MedianYieldInit)

# Compute deviation from mean value for each observation
datay$avg <- log(datay$PriceMod) - log(datay$MedianPriceInit)

# Study distribution of yields
hist(datay$yieldev[datay$Year > 1805 & datay$Year < 1918],  breaks=80)
hist(datay$yieldev[datay$Year > 1830 & datay$Year < 1850],  breaks=80)
hist(datay$yieldev[datay$Year > 1808 & datay$Year < 1820],  breaks=80)
hist(datay$yieldev[datay$Year > 1849 & datay$Year < 1880],  breaks=80)
hist(datay$yieldev[datay$Year > 1880 & datay$Year < 1900],  breaks=80)
hist(datay$yieldev[datay$Year > 1900 & datay$Year < 1919],  breaks=80)
hist(datay$yieldev[datay$Year > 1920 & datay$Year < 1940],  breaks=80)

# Remove extreme observations; modify for robustness checks
dataya <- subset(datay, yieldev > -1.386 & yieldev < 1.386 & avg < 2.198)

# Compute median yield and portfolio yield after adjusting for extreme observations
medianyield <- aggregate(dataya$yield, by=list(dataya$Year), median)
colnames(medianyield) <- c("Year", "MedianYield")
sumrent <- aggregate(dataya$rent.interpolated.30, by=list(dataya$Year), sum)
sumprice <- aggregate(dataya$PriceMod, by=list(dataya$Year), sum)
sumprice$yield <- sumrent$x/sumprice$x
sumprice$count <- table(datay$Year)

### Other taxes
tax <- read.csv("../data/ParisTax18101860.csv")
tax1 <- subset(tax, (Type == "Succession" | Type == "Donation" | Type == "Bail") & TaxRateFinal > 0 & Year > 0)
taxr <- aggregate(tax1$TaxRateFinal, by=list(tax1$Year), median)
taxr$count <- table(tax1$Year)
colnames(taxr) <- c("Year", "RentalTax", "CountRent")
tax2 <- subset(tax, (Type == "Adjudication" | Type == "Vente") & TaxRateFinal > 0 & Year > 0)
taxs <- aggregate(tax2$TaxRateFinal, by=list(tax2$Year), median)
taxs$count <- table(tax2$Year)
colnames(taxs) <- c("Year", "PriceTax", "CountPrice")
taxt <- merge(taxr, taxs, by="Year", all.x=TRUE, all.y=TRUE)
write.csv(taxt, file="taxesearlyparis.csv")

### Construct Secondary Series of Yields

### Read data
data <- read.csv("../data/DataParisSecondary.csv")

# Part 1: pre-1914 period
d <- subset(data, Year > 1869 & Year < 1915)
d <- subset(d, yield > 0)
d <- subset(d, !is.na(d$Asking.Price)) # remove asking prices, only minimum prices

# compute initial yield
median <- aggregate(d$yield, by=list(d$Year), median)
medianprice <- aggregate(d$Price, by=list(d$Year), median)
colnames(median) <- c("Year", "MedianYieldInit")
colnames(medianprice) <- c("Year", "MedianPriceInit")
d <- merge(d, median, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
d <- merge(d, medianprice, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")

d$yieldev <- log(d$yield) - log(d$MedianYieldInit)
d$avg <- log(d$Price) - log(d$MedianPriceInit)

# Remove extreme observations; modify for robustness checks
dy <- subset(d, yieldev > -1.386 & yieldev < 1.386 & avg < 2.198)

# create dummies for regression
dy$brut1870 <- 0
dy$brut1870[dy$brut == 0 & dy$Year < 1875] <- 1

dy$brut1875 <- 0
dy$brut1875[dy$brut == 0 & dy$Year > 1874 & dy$Year < 1880] <- 1

dy$brut1880 <- 0
dy$brut1880[dy$brut == 0 & dy$Year > 1880 & dy$Year < 1885] <- 1

dy$brut1885 <- 0
dy$brut1885[dy$brut == 0 & dy$Year > 1884 & dy$Year < 1890] <- 1

dy$brut1890 <- 0
dy$brut1890[dy$brut == 0 & dy$Year > 1889 & dy$Year < 1895] <- 1

dy$brut1895 <- 0
dy$brut1895[dy$brut == 0 & dy$Year > 1894 & dy$Year < 1900] <- 1

dy$brut1900 <- 0
dy$brut1900[dy$brut == 0 & dy$Year > 1899 & dy$Year < 1905] <- 1

dy$brut1905 <- 0
dy$brut1905[dy$brut == 0 & dy$Year > 1904 & dy$Year < 1910] <- 1

dy$brut1910 <- 0
dy$brut1910[dy$brut == 0 & dy$Year > 1909 & dy$Year < 1915] <- 1

# compute yields based on regressions
reg4 <- lm(log(dy$yield) ~ as.factor(dy$Year) + dy$brut1870 + dy$brut1875 + dy$brut1880 + dy$brut1885 + dy$brut1890 + dy$brut1895 + dy$brut1900 + dy$brut1905 + dy$brut1910 -1)
df <- data.frame(1872:1914, exp(reg4$coefficients[1:43]-0.33))
# write.csv(df, file="figaroyieldpre1914.csv")

# post-1916 yields, robustnesss series
d <- subset(data, Year > 1916 & Year < 1941)
d <- subset(d, yield > 0)

# create initial yield series
median <- aggregate(d$yield, by=list(d$Year), median)
medianprice <- aggregate(d$Price, by=list(d$Year), median)
colnames(median) <- c("Year", "MedianYieldInit")
colnames(medianprice) <- c("Year", "MedianPriceInit")
d <- merge(d, median, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
d <- merge(d, medianprice, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")

d$yieldev <- log(d$yield) - log(d$MedianYieldInit)
d$avg <- log(d$Price) - log(d$MedianPriceInit)

# Remove extreme observations; modify for robustness checks
dy <- subset(d, Real.Price == 0 & yieldev > -1.386 & yieldev < 1.386 & avg < 2.198)

dy$a1925 <- 0
dy$a1925[dy$Asking.Price == 0 & dy$Year > 1916 & dy$Year < 1926] <- 1

dy$a1930 <- 0
dy$a1930[dy$Asking.Price == 0 & dy$Year > 1925 & dy$Year < 1931] <- 1

dy$a1935 <- 0
dy$a1935[dy$Asking.Price == 0 & dy$Year > 1930 & dy$Year < 1936] <- 1

dy$a1940 <- 0
dy$a1940[dy$Asking.Price == 0 & dy$Year > 1935 & dy$Year < 1941] <- 1

dy$ba1925 <- 0
dy$ba1925[dy$Asking.Price == 0 & dy$brut == 0 & dy$Year > 1916 & dy$Year < 1926] <- 1

dy$ba1930 <- 0
dy$ba1930[dy$Asking.Price == 0 & dy$brut == 0 & dy$Year > 1925 & dy$Year < 1931] <- 1

dy$ba1935 <- 0
dy$ba1935[dy$Asking.Price == 0 & dy$brut == 0 & dy$Year > 1930 & dy$Year < 1936] <- 1

dy$ba1940 <- 0
dy$ba1940[dy$Asking.Price == 0 & dy$brut == 0 & dy$Year > 1935 & dy$Year < 1941] <- 1

dy$a <- 0
dy$a[dy$Asking.Price == 0 & dy$brit == 0 & dy$Year > 1915 & dy$Year < 1941] <- 1

# regress to get main yield series
reg4 <- lm(log(dy$yield) ~ as.factor(dy$Year) + dy$a1925 + dy$a1930 + dy$a1935 + dy$a1940 + dy$ba1925 + dy$ba1930 + dy$ba1935 + dy$ba1940 -1)

# total yield from Figaro, post-1916
tot1 <- data.frame(agg$year, exp(reg4$coefficients[1:24]))
colnames(tot1) <- c("Year", "RegYieldFig")

# create yield series  based on actual yields from Le Temps
ds <- subset(data, data$Real.Price == 1 & data$net == 0 & data$Year > 1923)
dsmed <- aggregate(ds$yields, by=list(ds$Year), median)
dsagg <- aggregate(ds$SalesPrice, by=list(ds$Year), sum)
dsagg$rent <- aggregate(ds$Revenu, by=list(ds$Year), sum)$x
dsagg$med <- dsmed$x
dsagg$pf <- dsagg$rent/dsagg$x
dsagg$year <- c(1932:1939)

tot2 <- data.frame(dsagg$year, dsagg$med, dsagg$pf)
colnames(tot2) <- c("Year", "MedYieldLeTemps", "PortfolioYieldLeTemps")

# Final Yields Post-1918
tot <- merge(tot1, tot2, by="Year", all.x=TRUE, all.y=TRUE)
# write.csv(tot, file="yieldspost1918.csv")

